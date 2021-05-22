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

It should return two values:

 1. A list of loop keywords which are spliced into the resulting LOOP
    form.

 2. A list of DECLARE expressions which are added to the body of the
    loop.

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
the expander function. The function should return two values, a list
of loop keywords that is spliced in the LOOP form, and a list of
declarations."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push
      (cons ',type
	    (lambda (,var ,container ,env)
	      (declare (ignorable ,env))

	      ,@body))
      *traverse-expanders*)))

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
	   (return (funcall expander var container env))))))


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
