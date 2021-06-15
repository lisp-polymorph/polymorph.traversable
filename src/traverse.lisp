;;; traverse.lisp
;;;
;;; Implementation of the TRAVERSE macro

(in-package #:polymorph.traversable)

(define-polymorphic-function make-iterator (container)
  :overwrite t
  :documentation
  "Return an iterator function for a container.

Should return a function of no argument which when invoked returns the
following values:

1. True if there are more items in the container, i.e. a new item is
   returned in the second value. NIL if the end of the sequence has
   been reached, i.e. the second return value is not a new item of the
   sequence.

2. The next item in the container.")

(defvar *traverse-expanders* (make-hash-table :test #'equal)
  "Hash-table of container traversal expanders for the TRAVERSE macro.

Each key denotes a container type with the corresponding value storing
the traversal expander function for that container type.

The expander function is called with four arguments:

 1. The container type.

 2. The variable to which each successive elements of the container
    are bound. This may be a list to support destructuring.

 3. The container form.

 4. The additional arguments passed after the container. Should be
    able to interpret the following keyword arguments:

    :START - Starting index of the element from which to start the
             traversal.

    :END - Index of the element at which to stop the traversal. If NIL
           the entire container is traversed.

    :FROM-END - If true the container should be traversed in reverse,
                starting from the last element.

 5. The form to be executed for each iteration of the traversal.

 3. The environment in which the traverse macro form is found.

It should return the following values:

 1. A list of bindings, as by LET*, which are established before the
    first iteration and remain visible to the body forms throughout
    all iterations.

 3. The new body form to be evaluated at each iteration.

 4. A form to wrap the entire iteration code. The local macro (&BODY)
    is available to it which expands to the entire iteration code.

Traversal expander functions are ordered with those specialized on the
most derived type, i.e. a type which is a subtype of the types of the
other expander functions, ordered first.")

(defmacro define-traverse-expander (type (type-var element container args body &optional (env (gensym "ENV"))) &body forms)
  "Define a container traversal expander function.

TYPE is the container type to which this function applies. It will be
applied to all containers which are a subtype of this type.

TYPE-VAR is the variable which will receive the actual container type.

ELEMENT is the variable to which successive container elements should
be bound. This need not be a symbol but may also be a list to allow
for destructuring.

CONTAINER is the container form.

ARGS is the variable receiving the list of arguments passed after the
container form.

BODY is the variable receiving the list of forms to be executed at
each iteration.

ENV is the environment in which the TRAVERSE macro form is found.

FORMS are the list of forms, evaluated in an implicit PROGN, comprising
the expander function.

See the documentation for *TRAVERSE-EXPANDERS* for more details on the
arguments and on what values are expected to be returned by a
traversal expander function."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',type *traverse-expanders*)
           (lambda (,type-var ,element ,container ,args ,body ,env)
             (declare (ignorable ,type-var ,env))
             ,@forms))))

(defun traverse-expander (container-type env)
  "Return the most specific traversal expander function for a given container type.

CONTAINER-TYPE is the container type for which to return the expander
function.

ENV is the environment in which the TRAVERSE form is found.

Returns the expander function. If there is no function for the given
container type or a non-function value is stored in
*TRAVERSE-EXPANDERS*, a `type-error' is signalled."

  (let ((expander-type t)
        (expander (gethash t *traverse-expanders*)))

    (loop
       for type being the hash-keys of *traverse-expanders*
       for fn being the hash-values of *traverse-expanders*
       do
         (when (and (subtypep container-type type env)
                    (subtypep type expander-type env))

           (setf expander-type type
                 expander fn)))

    (check-type expander function)
    expander))

(defmacro traverse (name/bindings &body forms &environment env)
  "Iterate over the elements of one or more containers.

   The macro arguments can be in one of the following two forms:

    1. (NAME (&REST BINDINGS) &BODY BODY)

    2. ((&REST BINDINGS) &BODY BODY)

   NAME is a symbol serving as the name of the BLOCK from which the
   forms in BODY can return, using (RETURN-FROM NAME ...). If not
   given defaults to NIL.

   Each element of BINDINGS is a list of the form (VAR SEQUENCE
   . ARGS), where VAR is the variable to which the element of the
   container CONTAINER is bound, at each iteration, and ARGS are
   additional traversal arguments.

   FORMS is a list of forms evaluated at each iteration. RETURN-FROM to
   the block named NAME may be used to terminate the iteration early
   and return a value from the TRAVERSE form. NIL is returned if
   there is no explicit RETURN."

  (declare (ignore env))

  ;;TODO: Check SAFETY level in ENV, and if 3 expand directly to the
  ;;slow iterator based implementation?

  (let ((name (if (symbolp name/bindings)
                  name/bindings
                  nil))

        (bindings (if (symbolp name/bindings)
                      (first forms)
                      name/bindings))

        (forms (if (symbolp name/bindings)
                   (rest forms)
                   forms)))

    `(traverse-fast% ,name ,bindings ,@forms)))

(defmacro traverse-fast% (name (&rest bindings) &body forms &environment env)
  "Optimized expansion of TRAVERSE.

Generates optimized iteration code for the containers types, using the
traverse expander functions defined with DEFINE-TRAVERSE-EXPANDER."

  (labels ((expand-traverse (element container args body env)
             (with-type-info (type () env) container
               (multiple-value-list
                (funcall (traverse-expander type env)
                         type
                         element
                         container
                         args
                         body
                         env))))

           (wrap-parent (old new)
             (if new
                 `(macrolet ((&body ()
                               ,(make-&body-expansion old)))

                    ,new)
                 old))

           (make-&body-expansion (old)
             (if old
                 `',old
                 ''(loop-body)))

           (make-loop (bindings body parent)
             (make-bindings
              bindings
              (make-parent
               parent
               (make-block (make-tagbody body)))))

           (make-parent (parent body)
             (if parent
                 `(macrolet ((loop-body ()
                                ',body))
                    ,parent)
                 body))

           (make-tagbody (body)
             (with-gensyms (start)
               `(tagbody
                   ,start
                   (macrolet ((next-iter ()
                                `(go ,',start)))
                     ,body))))

           (make-block (body)
             `(block ,name
                ,body))

           (make-bindings (bindings body)
             `(let* ,bindings ,body)))

    (loop
       for (element container . args) in bindings
       for (bindings body parent) =
         (expand-traverse element container args `(progn ,@forms (next-iter)) env) then
         (expand-traverse element container args loop-body env)

       for loop-body = body
       for loop-parent = (wrap-parent loop-parent parent)

       append bindings into all-bindings

       finally
         (return
           (make-loop all-bindings loop-body loop-parent)))))
