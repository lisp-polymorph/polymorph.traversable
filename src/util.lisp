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

(defmacro with-destructure-pattern ((var pattern) (body-var decl-var body) &body forms)
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

DECL-VAR is the name of a variable which receives the list of DECLARE
expressions applying to the variable, given by VAR. These should be
inserted in the correct place, where VAR is bound, in the result
returned.

The body forms of the macro FORMS are evaluated in an implicit PROGN,
with the bindings to the variables given by VAR and BODY-VAR
visible. The return value of the last form is returned.

FORMS should generate code which binds the current sequence element to
the variable with name stored in VAR.

This macro automatically takes care of handling declarations, that is
if the list returned by BODY contains declarations, those applying to
the variables in the destructuring pattern are inserted in the
`destructuring-bind` form, DECL-VAR is bound to those which apply to
the variable VAR, and a LOCALLY form, wrapping the form returned by
FORMS, is generated in which the remaining declarations are inserted."

  `(make-destructure-pattern
    ,pattern ,body
    (lambda (,var ,decl-var ,body-var)
      ,@forms)))

(defmacro split-declarations-forms ((decl forms) body-form &body body)
  "Split a 'body' into declare expressions and the forms to be evaluate.

DECL is the name of the variable to receive the list of DECLARE
expressions.

FORMS is the name of the variable to receive the body forms.

BODY-FORM is a form (evaluated) which produces the body.

BODY is the list of forms, evaluated in an implicit PROGN with DECL
and FORMS bound to the declarations and forms respectively. The value
of the last form is returned."

  `(multiple-value-bind (,forms ,decl)
       (parse-iter-macro-body ,body-form)
     ,@body))

(defun parse-iter-macro-body (body)
  "Split the BODY of a WITH-ITER-VALUE/PLACE macro into declaration expressions and forms.

If BODY is a list of a single LOCALLY form, the body of the LOCALLY
form is parsed instead."

  (let ((body (if (and (length= body 1)
                       (listp (first body))
                       (eq (caar body) 'cl:locally))
                  (cdar body)
                  body)))

    (parse-body body :documentation nil)))

(defun make-destructure-pattern (pattern body fn)
  (split-declarations-forms (decl forms) body
    (etypecase pattern
      (list
       (multiple-value-bind (decl-xs decl-other)
           (partition-declarations (destructure-vars pattern) decl)

         (with-gensyms (var)
           `(locally ,@decl-other
              ,(funcall
                fn var nil
                `((destructuring-bind ,pattern ,var
                    ,@decl-xs
                    ,@forms)))))))

      (symbol
       (multiple-value-bind (decl-var decl-other)
           (partition-declarations (list pattern) decl)

         `(locally ,@decl-other
            ,(funcall fn pattern decl-var forms)))))))

(defun destructure-vars (lambda-list)
  "Return the list of variables introduced by a destructuring-lambda-list.

LAMBDA-LIST is a destructuring-lambda-list as found in
DESTRUCTURING-BIND. It should not have &ENVIRONMENT parameters.

Returns the list of all variables introduced by the lambda-list, not
in any particular order.

NOTE: If the lambda-list is malformed, or an unknown lambda-list
keyword is encountered, this function simply returns the variable
names it has encountered so far, and silently ignores the remaining
malformed part."

  (check-type lambda-list list)

  (labels ((process-required (list vars)
             (typecase list
               (null vars)
               (symbol (cons list vars))
               (list
                (destructuring-bind (arg &rest list) list
                  (cond
                    ((member arg lambda-list-keywords)
                     (process-next arg list vars))

                    ((symbolp arg)
                     (process-required list (cons arg vars)))

                    ((listp arg)
                     (process-required
                      list
                      (process-required arg vars)))

                    (t vars))))

               (otherwise vars)))

           (process-next (keyword list vars)
             (case keyword
               (&optional (process-optional list vars))
               (&whole (process-whole list vars))
               ((&rest &body) (process-rest list vars))
               (&key (process-key list vars))
               (&allow-other-keys (process-next-section list vars))
               (&aux (process-aux list vars))
               (otherwise
                ;; Unknown lambda-list keyword.
                vars)))

           (process-optional (list vars)
             (if (consp list)
                 (destructuring-bind (arg &rest list) list
                   (cond
                     ((member arg lambda-list-keywords)
                      (process-next arg list vars))

                     ((symbolp arg)
                      (process-optional list (cons arg vars)))

                     ((consp arg)
                      (destructuring-bind (var &optional init sp)
                          arg
                        (declare (ignore init))

                        (process-optional
                         list
                         (cons var (append (ensure-list sp) vars)))))

                     (t vars)))

                 vars))

           (process-rest (list vars)
             (if (consp list)
                 (destructuring-bind (arg &rest list) list
                   (typecase arg
                     (list
                      (process-next-section
                       list
                       (process-required arg vars)))

                     (symbol
                      (process-next-section list (cons arg vars)))

                     (otherwise vars)))

                 vars))

           (process-whole (list vars)
             (if (consp list)
                 (destructuring-bind (arg &rest list) list
                   (if (symbolp arg)
                       (process-required list (cons arg vars))
                       vars))
                 vars))

           (process-next-section (list vars)
             (if (and (consp list)
                      (member (first list) lambda-list-keywords))

                 (process-next (first list) (rest list) vars)
                 vars))

           (key-var (key)
             (cond
               ((and (listp key) (length= key 2))
                (destructuring-bind (keyword var) key
                  (declare (ignore keyword))

                  (when (symbolp var)
                    var)))

               ((symbolp key) key)))

           (process-key (list vars)
             (if (consp list)
                 (destructuring-bind (arg &rest list) list
                   (cond
                     ((member arg lambda-list-keywords)
                      (process-next arg list vars))

                     ((symbolp arg)
                      (process-key list (cons arg vars)))

                     ((consp arg)
                      (destructuring-bind (key &optional init sp)
                          arg
                        (declare (ignore init))

                        (if-let ((var (key-var key)))
                          (process-key
                           list
                           (cons var (append (ensure-list sp) vars)))

                          vars)))

                     (t vars)))

                 vars))

           (process-aux (list vars)
             (if (consp list)
                 (destructuring-bind (arg &rest list) list
                   (cond
                     ((member arg lambda-list-keywords)
                      (process-next arg list vars))

                     ((symbolp arg)
                      (process-aux list (cons arg vars)))

                     ((consp arg)
                      (destructuring-bind (var &optional init)
                          arg
                        (declare (ignore init))

                        (process-aux list (cons var vars))))

                     (t vars)))

                 vars)))

    (process-required lambda-list nil)))

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

(defmacro with-destructure-entry ((key value pattern) (body-var decl-var body) &body forms)
  "Like WITH-DESTRUCTURE-PATTERN, except that FORMS should generate
code which binds the current entry key to KEY and the value to VALUE."

  `(make-destructure-entry
    ,pattern ,body
    (lambda (,key ,value ,decl-var ,body-var)
      ,@forms)))

(defun make-destructure-entry (pattern body fn)
  (if (and (consp pattern)
           (symbolp (car pattern))
           (cdr pattern)
           (symbolp (cdr pattern)))

      (let ((key (or (car pattern) (gensym "KEY")))
            (value (or (cdr pattern) (gensym "VALUE"))))

        (split-declarations-forms (decl forms) body
          (multiple-value-bind (decls-vars decls-other)
              (partition-declarations (list key value) decl)

            `(locally ,@decls-other
               ,(funcall fn key value decls-vars forms)))))

      (with-destructure-pattern (var pattern)
          (forms decls body)

        (with-gensyms (key value)
          (funcall
           fn key value
           `((let ((,var (cons ,key ,value)))
               ,@decls
               ,@forms)))))))

(defmacro map-place (key table)
  (once-only (key)
    `(cons ,key (gethash ,key ,table))))

(defsetf map-place (key table) (new-value)
  `(setf (gethash ,key ,table) ,new-value))


;;;; Parsing declarations

;; From Serapeum / macro-tools.lisp

;; Copyright (c) 2014 Paul M. Rodriguez

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defun partition-declarations (xs declarations &optional env)
  "Split DECLARATIONS into those that do and do not apply to XS.
Return two values, one with each set.

Both sets of declarations are returned in a form that can be spliced
directly into Lisp code:

     (locally ,@(partition-declarations vars decls) ...)"
  (let ((env2 (parse-declarations declarations env)))
    (flet ((build (env)
             (build-declarations 'declare env)))
      (if (null xs)
          (values nil (build env2))
          (values
           (build (filter-declaration-env env2 :affecting xs))
           (build (filter-declaration-env env2 :not-affecting xs)))))))
