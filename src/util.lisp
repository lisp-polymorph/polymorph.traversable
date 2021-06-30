;;; util.lisp
;;;
;;; Utilities

(in-package #:polymorph.traversable)

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

;;; Supporting destructuring-bind patterns in TRAVERSE

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

;;; Iterator Macros

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


;;; Hash-Tables

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
