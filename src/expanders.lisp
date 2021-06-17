;;; expanders.lisp
;;;
;;; Traversal expanders for standard sequence/container types

(in-package #:polymorph.traversable)

;;; Utilities

(defun process-iterator-args (args env)
  "Extract the values of the FROM-END, START and END keyword arguments.

Checks whether the arguments evaluate to constant values, if so
returns their values, otherwise returns the forms themselves.

ARGS is the argument list.

ENV is the environment in which they occur.

Returns the following values:

  1. Value of FROM-END argument.

  2. True if the FROM-END argument is a constant, NIL otherwise.

  3. Value of START argument.

  4. True if the START argument is a constant, NIL otherwise.

  5. Value of END argument.

  6. True if the END argument is a constant, NIL otherwise."

  (destructuring-bind (&key from-end (start 0) (end nil))
      args

    (multiple-value-bind (from-end constant-from-end?)
        (constant-form-value from-end env)

      (multiple-value-bind (start constant-start?)
          (constant-form-value start env)

        (multiple-value-bind (end constant-end?)
            (constant-form-value end env)

          (values
           from-end
           constant-from-end?
           start
           constant-start?
           end
           constant-end?))))))

(defmacro oif (cond if-true &optional if-false &environment env)
  "Optimized IF expression.

If the condition form is a constant the IF expression is replaced with
the appropriate branch.

COND is the IF expression condition form.

IF-TRUE is the form to return if COND evaluates to true.

IF-FALSE is the form to return if COND evaluates to false."

  (multiple-value-bind (cond const?)
      (constant-form-value cond env)

    (if const?
        (if cond if-true if-false)
        `(if ,cond ,if-true ,if-false))))

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
no clauses succeeds NIL is returned."

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

(define-traverse-expander list (type element form args body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-constant-values (from-end start end) env
      ((from-end start end)
       (cond
         (from-end
          (make-traverse-list
           element
           `(cl:nreverse (cl:subseq ,form ,start ,end))
           body))

         (end
          (make-traverse-bounded element form start end body))

         ((> start 0)
          (make-traverse-list element `(nthcdr ,start form) body))

         (t
          (make-traverse-list element form body))))

      ((start end)
       (cond
         (end
          (make-traverse-list
           element
           `(if ,from-end
                (cl:nreverse (cl:subseq ,form ,start ,end))
                (cl:subseq ,form ,start ,end))
           body))

         ((> start 0)
          (make-traverse-list
           element
           `(if ,from-end
                (cl:reverse (nthcdr ,start ,form))
                (nthcdr ,start ,form))
           body))

         (t
          (make-traverse-list
           element
           `(if ,from-end
                (cl:reverse ,form)
                ,form)
           body))))

      (nil
       (make-traverse-list
        element
        `(if ,from-end
             (cl:nreverse (cl:subseq ,form ,start ,end))
             (cl:subseq ,form ,start ,end))
        body)))))

(defun make-traverse-bounded (element form start end body)
  "Generate a TRAVERSE expansion for a bounded list traversal, when END is non-NIL."

  (multiple-value-bind (bindings body parent)
      (make-traverse-list
       element
       `(nthcdr ,start ,form)
       body)

    (with-gensyms (index)
      (values
       `((,index ,start) ,@bindings)

       `(when (< ,index ,end)
          (incf ,index)
          ,body)

       parent))))

(defun make-traverse-list (element form body)
  "Generate a TRAVERSE expansion for a unbounded list traversal."

  (with-gensyms (list)
    (values
     `((,list ,form))

     `(when ,list
        (let ((,element (car ,list)))
          (setf ,list (cdr ,list))
          ,body))

     nil)))

;;; Vectors

(define-traverse-expander vector (type element form args body env)
  (multiple-value-bind (from-end c-from-end? start c-start? end c-end?)
      (process-iterator-args args env)

    (let ((v-from-end (if c-from-end? from-end (gensym "FROM-END")))
          (v-start (if c-start? start (gensym "START")))
          (v-end (if c-end? end (gensym "END"))))

      (with-gensyms (vec index end-index)
        (values
         `((,vec ,form)

           ,@(unless c-from-end?
               `((,v-from-end ,from-end)))

           ,@(unless c-start?
               `((,v-start ,start)))

           ,@(unless c-end?
               `((,v-end ,end)))

           (,end-index
            (oif ,v-end ,v-end (1- (cl:length ,vec))))

           (,index
            (oif ,v-from-end ,end-index ,v-start)))

         `(when (oif ,v-from-end
                     (cl:>= ,index ,start)
                     (cl:< ,index ,end-index))

            (let ((,element (aref ,vec ,index)))
              (oif ,v-from-end
                   (cl:decf ,index)
                   (cl:incf ,index))
              ,body))

         nil)))))


;;; Hash-Tables

(define-traverse-expander hash-table (type element form args body env)
  (multiple-value-bind (from-end c-from-end? start c-start? end c-end?)
      (process-iterator-args args env)

    (declare (ignore from-end c-from-end?))

    (with-gensyms (table more? next size index)
      (let* ((counted? (or (not (and c-start? c-end?))
                           (or (> start 0) end)))

             (test (if counted?
                       `(and ,more? (cl:< ,index ,size))
                       more?))

             (inc (when counted?
                    `((cl:incf ,index)))))

        (values
         `((,table ,form)
           ,@(when counted?
               `((,index ,start)
                 (,size (oif ,end ,end (hash-table-count ,table))))))

         (let* ((key (if (listp element) (first element) (gensym "KEY")))
                (value (if (listp element) (second element) (gensym "VALUE")))
                (whole (unless (listp element) element)))

           `(multiple-value-bind (,more? ,key ,value)
                (,next)

              (declare (ignorable ,key ,value))

              (let (,@(when whole `(,whole (list ,key ,value))))
                (when ,test
                  ,@inc
                  ,body))))

         `(with-hash-table-iterator (,next ,table) (&body)))))))


;;; Default

(define-traverse-expander t (type element form args body env)
  (declare (ignore env))

  (with-gensyms (it more?)
    (values
     `((,it (make-iterator ,form ,@args)))

     `(multiple-value-bind (,more? ,element)
          (funcall ,it)

        (when ,more?
          ,body))

     nil)))
