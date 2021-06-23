;;; expanders.lisp
;;;
;;; Traversal expanders for standard sequence/container types

(in-package #:polymorph.traversable)


;;; Utilities

(defmacro with-constant-values ((&rest things) env &body clauses)
  "Check whether one or more forms are constant and retrieve their values.

THINGS is a list of bindings each of the form (VAR INITFORM). Each
INITFORM's is evaluated and the result is checked whether it is a
constant form in the environment ENV. If so the constant value is
stored in VAR otherwise the resulting form (to which INITFORM
evaluates) itself is stored in VAR. Alternatively each element of
THINGS can be a symbol in which case it is a shorthand for (VAR VAR).

ENV is the environment in which to check whether the forms are
constants.

CLAUSES is a list of clauses each of the form (VARS . FORMS), where
VARS is a list of variables, listed in THINGS, which should be
constant in order for the corresponding FORMS to be evaluated. If all
variables in VARS are constants FORMS are evaluated in an implicit
PROGN, with the result of the last form returned. If not all of VARS
are constant, FORMS are not evaluated and the next clause is tried. If
no clause succeeds NIL is returned."

  (let ((const-vars (pairlis (mapcar #'ensure-car things)
                             (make-gensym-list (length things) "CONST?"))))

    (labels ((make-get-value (thing body)
               (destructuring-bind (var &optional (form var))
                   (ensure-list thing)

                 (let ((const-var (cdr (assoc var const-vars))))
                   (assert const-var)

                   `(multiple-value-bind (,var ,const-var)
                        (constant-form-value ,form ,env)

                      ,body))))

             (const-var (var)
               (let ((cvar (cdr (assoc var const-vars))))
                 (unless cvar
                   (error "Variable ~s in clause not one of ~s."
                          var (mapcar #'ensure-car things)))

                 cvar))

             (make-clause (clause)
               (destructuring-bind (vars &rest body) clause
                 `((and ,@(mapcar #'const-var vars))
                   ,@body))))

      (cl:reduce
       #'make-get-value
       things
       :from-end t
       :initial-value
       `(cond ,@(mapcar #'make-clause clauses))))))


;;; Lists

(define-traverse-expander list (type form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-constant-values (from-end start end) env
      ((from-end start end)
       (cond
         (from-end
          (make-traverse-list
           `(cl:nreverse (cl:subseq ,form ,start ,end))
           body))

         (end
          (make-traverse-bounded-list form start end body))

         ((> start 0)
          (make-traverse-list `(nthcdr ,start ,form) body))

         (t
          (make-traverse-list form body))))

      ((start end)
       (cond
         (end
          (make-traverse-list
           `(if ,from-end
                (cl:nreverse (cl:subseq ,form ,start ,end))
                (cl:subseq ,form ,start ,end))
           body))

         ((> start 0)
          (make-traverse-list
           `(if ,from-end
                (cl:reverse (nthcdr ,start ,form))
                (nthcdr ,start ,form))
           body))

         (t
          (make-traverse-list
           `(if ,from-end
                (cl:reverse ,form)
                ,form)
           body))))

      (nil
       (make-traverse-list
        `(if ,from-end
             (cl:nreverse (cl:subseq ,form ,start ,end))
             (cl:subseq ,form ,start ,end))
        body)))))

(defun make-traverse-bounded-list (form start end body)
  "Generate a TRAVERSE expansion for a bounded list traversal, when END is non-NIL."

  (multiple-value-bind (bindings body bind-value)
      (make-traverse-list
       `(nthcdr ,start ,form)
       body)

    (with-gensyms (index)
      (values
       `((,index ,start) ,@bindings)

       body

       `(lambda (pattern body)
          (,bind-value
           pattern

           `((unless (< ,',index ,',end)
               (traverse-finish))


             (incf ,',index)
             ,@body)))))))

(defun make-traverse-list (form body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     body

     `(lambda (pattern body)
        (bind-list-element pattern body ',list)))))

(defun bind-list-element (pattern body list)
  "Generate a form which binds the next element of a list.

PATTERN is the binding pattern.

BODY is the list of forms to be evaluated, within the environment of
the binding.

LIST is the name of the variable storing the list."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-list-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))
        list)))

    (symbol
     `(progn
        (unless ,list
          (traverse-finish))

        (let ((,pattern (car ,list)))
          (setf ,list (cdr ,list))
          ,@body)))))


;;; Vectors

(define-traverse-expander vector (type form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-type-info (type (typename &optional (elt t)) env) form
      (with-gensyms (vec index end-index v-from-end v-start v-end)
        (values
         `((,v-from-end ,from-end :constant t)
           (,v-start ,start :constant t)
           (,v-end ,end :constant t)

           (,vec (the ,type ,form))
           (,end-index (if ,v-end ,v-end (cl:length ,vec)) :constant t)

           (,index
            (if ,v-from-end (cl:1- ,end-index) ,v-start)))

         body

         `(lambda (pattern body)
            (bind-vector-element pattern body ',vec ',elt ',index ',v-start ',end-index ',v-from-end)))))))

(defun bind-vector-element (pattern body vec elt-type index start end from-end)
  "Generate a form which binds the next element of a vector.

PATTERN is the binding pattern.

BODY is the list of forms to be evaluated, within the environment of
the binding.

VEC is the name of the variable storing the vector.

ELT-TYPE is the vector's element type.

INDEX is the name of the variable/symbol-macro storing the element
index.

START is the name of the variable/symbol-macro storing the starting
index.

END is the name of the variable/symbol-macro storing the ending index.

FROM-END is the name of the variable/symbol-macro storing the FROM-END
flag."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-vector-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))

        vec
        elt-type
        index
        start
        end
        from-end)))

    (symbol
     `(progn
        (unless (if ,from-end
                    (cl:>= ,index ,start)
                    (cl:< ,index ,end))
          (traverse-finish))

        (let ((,pattern (aref ,vec ,index)))
          (declare (type ,elt-type ,pattern))
          (if ,from-end
              (cl:decf ,index)
              (cl:incf ,index))
          ,@body)))))


;;; Hash-Tables

(define-traverse-expander hash-table (type form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index)
      (with-constant-values (start end) env
        ((start end)
         (let* ((counted? (or (> start 0) end))

                (test (if counted?
                          `(and ,more? (cl:< ,index ,size))
                          more?))

                (inc (when counted?
                       `((cl:incf ,index)))))

           (values
            `((,table ,form)
              ,@(when counted?
                  `((,index ,start)
                    (,size ,(or end `(hash-table-count ,table))))))

            `((with-hash-table-iterator (,next ,table)
                ,@body))

            `(lambda (pattern body)
               (bind-hash-table-element pattern body ',next ',more? ',test ',inc)))))

        (nil
         (values
          `((,table ,form)
            (,index ,start)
            (,size (or ,end (hash-table-count ,table))))

          `((with-hash-table-iterator (,next ,table)
              ,@body))

          `(lambda (pattern body)
             (bind-hash-table-element
              pattern body
              ',next ',more?
              '(and ,more? (cl:< ,index ,size))
              '((cl:incf ,index))))))))))

(defun bind-hash-table-element (pattern body next more? test inc)
  "Generate a form which binds the next element of a hash-table.

PATTERN is the binding pattern.

BODY is the list of forms to be evaluated, within the environment of
the binding.

NEXT is the hash-table iterator next element macro.

MORE? is the name of the variable to which the more elements flag
should be bound.

TEST is the loop termination test form.

INC is the list of index incrementation forms."

  (etypecase pattern
    (cons
     (if (and (symbolp (car pattern))
              (cdr pattern)
              (symbolp (cdr pattern)))

         (let ((key (or (car pattern) (gensym "KEY")))
               (value (or (cdr pattern) (gensym "VALUE"))))

           `(multiple-value-bind (,more? ,key ,value)
                (,next)

              (unless ,test
                (traverse-finish))

              ,@body))

         (with-gensyms (key value)
           (bind-hash-table-element
            (cons key value)

            `((destructuring-bind ,pattern (cons ,key ,value)
                ,@body))

            next more? test inc))))

    (symbol
     (with-gensyms (key value)
       (bind-hash-table-element
        (cons key value)

        `((let ((,pattern (cons ,key ,value)))
            ,@body))

        next more? test inc)))))


;;; Default

(define-traverse-expander t (type form args body env)
  (declare (ignore env))

  (with-gensyms (it)
    (values
     `((,it (make-iterator ,form ,@args)))

     body

     `(lambda (pattern body)
        (bind-iter-element pattern body ',it)))))

(defun bind-iter-element (pattern body iter)
  "Generate a form which binds the next element of a generic sequence.

PATTERN is the binding pattern.

BODY is the list of forms to be evaluated, within the environment of
the binding.

ITER is the variable storing the iterator object."

  (etypecase pattern
    (list
     (with-gensyms (var)
       (bind-iter-element
        var
        `((destructuring-bind ,pattern ,var
            ,@body))

        iter)))

    (symbol
     (with-gensyms (more?)
       `(multiple-value-bind (,more? ,pattern)
            (funcall ,iter)

          (unless ,more?
            (traverse-finish))

          ,@body)))))
