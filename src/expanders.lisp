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

(defmacro with-destructure-pattern ((var pattern) (body-var body) &body forms)
  "Automatically generate destructuring code if the binding pattern is
a destructuring-bind pattern.

The WITH-ITER-VALUE binding pattern, PATTERN, is checked whether it is
a symbol naming a variable or a list representing a DESTRUCTURING-BIND
pattern.

If PATTERN is a list, a variable name is generated, and bound to the
variable VAR. BODY-VAR is bound to forms which destructure the value
stored in the variable, wrapping the forms in BODY.

If PATTERN is a symbol, the variable given by VAR is bound directly to
that symbol and the variable given by BODY-VAR is bound directly to
BODY.

The body forms of the macro FORMS are evaluated in an implicit PROGN,
with the bindings to the variables given by VAR and BODY-VAR
visible. The return value of the last form is returned.

FORMS should generate code which binds the current sequence element to
   the variable with name stored in VAR."

  `(make-destructure-pattern
    ,pattern ,body
    (lambda (,var ,body-var)
      ,@forms)))

(defun make-destructure-pattern (pattern body fn)
  (etypecase pattern
    (list
     (with-gensyms (var)
       (funcall
        fn var
        `((destructuring-bind ,pattern ,var
            ,@body)))))

    (symbol
     (funcall fn pattern body))))

(defmacro iter-macro ((&rest vars) (&rest lambda-list) &body body)
  "Generate a lexical macro definition for WITH-ITER-VALUE/PLACE for an iterator.

This macro is intended to be used within a traverse expansion function
to facilitate the definition, by avoiding the need for nested
backquotes, of the lexical macros, serving as the expansion of
WITH-ITER-VALUE and WITH-ITER-PLACE for a given iterator type.

VARS is a list of variables to 'capture' from the lexical scope of the
ITER-MACRO form. Inside the generated macro definition, a symbol-macro
is introduced for each variable, by SYMBOL-MACROLET, which expands to
a QUOTE form which returns the value of the variable as it is in the
environment where the ITER-MACRO form occurs.

LAMBDA-LIST is the macro lambda-list (not evaluated).

BODY is the list of body forms of the generated macro. These are not
evaluated at the time the ITER-MACRO form is evaluated but are instead
quoted and become the body forms of the generated macro
definition. The body forms may reference the variables in the
LAMBDA-LIST and the values of the 'captured' variables listed in VARS.

Returns a lexical macro definition (excluding the name) suitable to be
returned from a traverse expander function as the macro definition for
the iterator's WITH-ITER-VALUE and WITH-ITER-PLACE."

  ``(,',lambda-list
     (symbol-macrolet
         ,(list ,@(loop for var in vars
                     collect ``(,',var ',,var)))

       ,@',body)))

(defmacro with-destructure-entry ((key value pattern) (body-var body) &body forms)
  "Like WITH-DESTRUCTURE-PATTERN, except that FORMS should generate
   code which binds the current entry key to KEY and the value to
   VALUE."

  `(make-destructure-entry
    ,pattern ,body
    (lambda (,key ,value ,body-var)
      ,@forms)))

(defun make-destructure-entry (pattern body fn)
  (etypecase pattern
    (cons
     (if (and (symbolp (car pattern))
              (cdr pattern)
              (symbolp (cdr pattern)))

         (let ((key (or (car pattern) (gensym "KEY")))
               (value (or (cdr pattern) (gensym "VALUE"))))

           (funcall fn key value body))

         (with-gensyms (key value)
           (funcall
            fn key value
            `((destructuring-bind ,pattern (cons ,key ,value)
                ,@body))))))

    (symbol
     (with-gensyms (key value)
       (funcall
        fn key value
        `((let ((,pattern (cons ,key ,value)))
            ,@body)))))))

(defmacro map-place (key table)
  (once-only (key)
    `(cons ,key (gethash ,key ,table))))

(defsetf map-place (key table) (new-value)
  `(setf (gethash ,key ,table) ,new-value))


;;; Lists

(define-traverse-expander list (type form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-gensyms (list index v-start v-end v-from-end with-value with-place place)
      (values
       `((,v-start ,start :constant t)
         (,v-end ,end :constant t)
         (,v-from-end ,from-end :constant t)

         (,list
          (if ,v-from-end
              (sublist ,form ,v-start ,v-end ,v-from-end)
              (nthcdr ,v-start ,form))))

       (let ((value-macro
              (iter-macro (tag list place)
                  (pattern &body body)

                (with-destructure-pattern (var pattern)
                    (body body)

                  `(progn
                     (unless ,list
                       (go ,tag))

                     (let ((,var (,place ,list)))
                       (setf ,list (cdr ,list))

                       ,@body)))))

             (place-macro
              (iter-macro (tag list place)
                  (name more? &body body)

                (let ((body
                       `(prog1 (progn ,@body)
                          (setf ,list (cdr ,list)))))

                  `(symbol-macrolet ((,name (,place ,list)))
                     ,(if more?
                          `(let ((,more? ,list))
                             ,body)

                          `(progn
                             (unless ,list
                               (go ,tag))

                             ,body)))))))

         (with-gensyms (list-value list-place)
           `((cond
               (,v-from-end
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(caar ,thing)))

                  ,@body))

               (,v-end
                (let ((,index ,v-start))
                  (macrolet ((,list-value . ,value-macro)
                             (,list-place . ,place-macro)
                             (,place (thing)
                               `(car ,thing))

                             (,with-value .
                               ,(iter-macro (tag index v-end list-value)
                                    (pattern &body body)

                                  `(,list-value
                                    ,pattern

                                    (unless (< ,index ,v-end)
                                      (go ,tag))

                                    (incf ,index)
                                    ,@body)))

                             (,with-place .
                               ,(iter-macro (tag index v-end list-place)
                                    (name more? &body body)

                                  `(,list-place
                                    ,name
                                    ,more?

                                    ,@(if more?
                                          `((let ((,more? (and ,more? (< ,index ,v-end))))
                                              (incf ,index)
                                              ,@body))

                                          `((unless (< ,index ,v-end)
                                              (go ,tag))

                                            (incf ,index)
                                            ,@body))))))
                    ,@body)))

               (t
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(car ,thing)))

                  ,@body))))))


       (iter-macro (with-value)
           (pattern &body body)
         `(,with-value ,pattern ,@body))

       (iter-macro (with-place)
           (name more? &body body)

         `(,with-place ,name ,more? ,@body))))))


;;; Vectors

(define-traverse-expander vector (type form args tag body env)
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

         (iter-macro (tag elt v-from-end v-start vec end-index index)
             (pattern &body body)

           (with-destructure-pattern (var pattern)
               (body body)

             `(progn
                (unless (if ,v-from-end
                            (>= ,index ,v-start)
                            (< ,index ,end-index))
                  (go ,tag))

                (let ((,var (aref ,vec ,index)))
                  (declare (type ,elt ,var))
                  (if ,v-from-end
                      (decf ,index)
                      (incf ,index))
                  ,@body))))

         (iter-macro (tag v-from-end v-start vec end-index index)
             (name more? &body body)

           (let ((test `(if ,v-from-end
                            (>= ,index ,v-start)
                            (< ,index ,end-index)))
                 (body `(prog1 (progn ,@body)
                          (if ,v-from-end
                              (decf ,index)
                              (incf ,index)))))

             `(symbol-macrolet ((,name (aref ,vec ,index)))
                ,(if more?
                     `(let ((,more? ,test))
                        ,body)

                     `(progn
                        (unless ,test
                          (go ,tag))

                        ,body))))))))))


;;; Hash-Tables

(define-traverse-expander hash-table (type form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index)
      (flet ((make-iter-value (test inc)
               (iter-macro (more? next test tag inc)
                   (pattern &body body)

                 (with-destructure-entry (key value pattern)
                     (body body)

                   `(multiple-value-bind (,more? ,key ,value)
                        (,next)
                      (declare (ignorable ,key ,value))

                      (unless ,test
                        (go ,tag))

                      ,@inc
                      ,@body))))

             (make-iter-place (test inc)
               (iter-macro (more? next test tag inc table)
                   (name more?-var &body body)

                 (let ((more? (or more?-var more?)))
                   (with-gensyms (key)
                     `(multiple-value-bind (,more? ,key)
                          (,next)

                        (symbol-macrolet ((,name (map-place ,key ,table)))
                          ,(if more?-var
                               body

                               `(progn
                                  (unless ,test
                                    (go ,tag))

                                  ,@inc
                                  ,@body)))))))))

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

              (make-iter-value test inc)
              (make-iter-place test inc))))

          (nil
           (values
            `((,table ,form)
              (,index ,start)
              (,size (or ,end (hash-table-count ,table))))

            `((with-hash-table-iterator (,next ,table)
                ,@body))

            (make-iter-value `(and ,more? (cl:< ,index ,size))
                             `((cl:incf ,index)))

            (make-iter-place `(and ,more? (cl:< ,index ,size))
                             `((cl:incf ,index))))))))))


;;; Default

(define-traverse-expander t (type form args tag body)
  (with-gensyms (it)
    (values
     `((,it (make-iterator ,form ,@args)))

     body

     (iter-macro (it tag)
         (pattern &body body)

       (with-destructure-pattern (var pattern)
           (body body)

         `(progn
            (unless (morep ,it)
              (go ,tag))

            (let ((,var (element ,it)))
              (advance ,it)
              ,@body))))

     (iter-macro (it tag)
         (name more? &body body)

       (let ((body `(prog1 (progn ,@body)
                      (advance ,it))))

         `(symbol-macrolet ((,name (element ,it)))
            ,(if more?
                 `(let ((,more? (morep ,it)))
                    ,body)

                 `(progn
                    (unless (morep ,it)
                      (go ,tag))

                    ,body))))))))
