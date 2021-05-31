;;; traverse.lisp
;;;
;;; Implementation of the TRAVERSE macro

(in-package #:polymorph.traversable)

(defvar *traverse-expanders* nil
  "Association list of container traversal expanders for the TRAVERSE macro.

Each item is an entry of the form (TYPE . EXPANDER-FUNCTION) where
TYPE is a container type, and EXPANDER-FUNCTION is the corresponding
traversal expander function for the given container type.

The expander function is called with three arguments:

 1. The variable to which each successive elements of the container
    are bound.

 2. The container form.

 3. The environment in which the traverse macro form is found.

It should return the following values:

 1. A list of bindings which are established before the first
    iteration and remain throughout the entire traversal. Each element
    is of the form (VAR INIT-FORM) where VAR is the symbol naming the
    variable and INIT-FORM is the form which is evaluated to produce
    the value to which the variable is bound.

 2. A list of bindings which are established before each
    iteration. Each element is of the form (VAR INIT-FORM STEP-FORM),
    where VAR is the symbol naming the variable, INIT-FORM is a form
    which is evaluated to produce the value to which the variable is
    bound before the first iteration. STEP-FORM is a form which is
    evaluated, before each subsequent iteration, to produce the value
    to which the variable is bound after the first iteration. If the
    STEP-FORM is omitted, the variable is bound to INIT-FORM,
    reevaluated, before each iteration.

 3. The condition form. This is evaluated before each iteration and
    when it evaluates to false (NIL), the traversal is terminated.

 4. A list of additional LOOP keywords, which are inserted between the
 bindings and the loop condition form.

Traversal expander functions closer to the front of the list are
prioritized over those further from the front.")

(defmacro define-traverse-expander (type (var container &optional (env (gensym "ENV"))) &body body)
  "Define a container traversal expander function.

The traversal expander function is pushed to the front of the
*TRAVERSE-EXPANDERS* list, thus it takes priority over previously
defined traversal expanders.

TYPE is the container type to which this function applies. It will be
applied to all containers which are a subtype of this type.

VAR is the variable to which successive container elements should be
bound. This need not be a symbol but may also be a list to allow for
destructuring.

CONTAINER is the container form.

ENV is the environment in which the TRAVERSE macro form is found.

BODY is the list of forms, evaluated in an implicit PROGN, comprising
the expander function. See the documentation for *TRAVERSE-EXPANDERS*
for details on what values are expected to be returned by a traversal
expander function."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (if-let (cell (assoc ',type *traverse-expanders*))
       (setf (cdr cell)
             (lambda (,var ,container ,env)
	       (declare (ignorable ,env))

	       ,@body))

       (push
        (cons ',type
	      (lambda (,var ,container ,env)
	        (declare (ignorable ,env))

	        ,@body))
        *traverse-expanders*))))

(defun expand-traverse (var container env)
  "Expand a traversal for a container.

Calls the first function in *TRAVERSE-EXPANDERS* of which the type of
CONTAINER, in ENV, is a subtype.

VAR is the variable which should be bound to successive container
elements.

CONTAINER is the container form.

ENV is the lexical environment of the TRAVERSE macro form."

  (let ((seq-type (%form-type container env)))
    (loop for (type . expander) in *traverse-expanders*
       do
	 (when (subtypep seq-type type)
	   (return (make-traverse-expansion expander var container env))))))

(defun make-traverse-expansion (fn var container env)
  "Make a traversal expansion using a given expander function.

FN is the expander function.

VAR is the variable to which the items of the container are bound.

CONTAINER is the form which returns the container.

ENV is the environment in which the TRAVERSE macro from is found.

Returns a list of CL:LOOP keywords."

  (multiple-value-bind (withs fors condition keywords)
      (funcall fn var container env)

    (append
     (loop for with in withs
        append
          (destructuring-bind (var form) with
            (check-type var symbol)
            `(with ,var = ,form)))

     (loop for binding in fors
        append
          (destructuring-bind (var init &optional (step nil step-p))
              binding
            (check-type var symbol)

            `(for ,var = ,init
                  ,@(when step-p
                      `(then ,step)))))

     keywords

     (when condition
       `(while ,condition)))))


(defmacro traverse ((&rest bindings) &body body &environment env)
  "Traverse one or more containers with successive elements bound to a variable.

BINDINGS is a list of the form (VAR CONTAINER) where VAR is the name
of the variable(s) to which successive elements of
CONTAINER (evaluated) are bound.

BODY is the list of forms which are evaluated at each iteration,
during which each VAR in BINDINGS is bound to the next successive
element of the corresponding CONTAINER. The first elements of BODY may
be DECLARE expressions. RETURN may be used to return from the TRAVERSE
macro early."

  (multiple-value-bind (keywords decls)
      (loop
	 for (var sequence) in bindings
	 for (keyword-list decl) =
	   (multiple-value-list (expand-traverse var sequence env))

	 append keyword-list into keywords
	 append decl into decls

	 finally (return (values keywords decls)))

   `(loop
       ,@keywords
       do
       (locally ,@decls ,@body))))
