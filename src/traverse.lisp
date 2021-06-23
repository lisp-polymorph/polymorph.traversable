;;; traverse.lisp
;;;
;;; Implementation of the TRAVERSE macro

(in-package #:polymorph.traversable)

;;; TRAVERSE Expanders

(defvar *traverse-expanders* (make-hash-table :test #'equal)
  "Hash-table of container traversal expanders for the TRAVERSE macro.

Each key denotes a container type with the corresponding value storing
the traversal expander function for that container type.")

(defmacro define-traverse-expander (type (type-var container args body &optional (env (gensym "ENV"))) &body forms)
  "Define a WITH-ITERATORS expansion for a given container type.

TYPE is the container type for which this expansion is defined. It
will be applied to all containers which are a subtype of this type.

TYPE-VAR is the variable which will receive the actual container type.

CONTAINER is the container form.

ARGS is the variable receiving the list of arguments passed after the
container form.

BODY is the variable receiving the list of forms comprising the body
of the WITH-ITERATORS form.

ENV is the environment in which the TRAVERSE macro form is found.

FORMS are the list of forms, evaluated in an implicit PROGN,
comprising the expander function. The value of the last form is
returned as the WITH-ITERATORS expansion.

The following values should be returned by the defined traverse
expansion function:

 1. A list of bindings, as by LET*, which are established before the
    first iteration and remain visible to the body forms throughout
    all iterations.

    Each binding, in this list, may optionally provide the following
    keyword arguments following the init-form:

    :CONSTANT

       Flag for whether the variable should be treated as a
       constant. If true and the init-form is a constant-form, the
       symbol is bound by SYMBOL-MACROLET, preceding the non-constant
       bindings, rather than LET.

       NOTE: A constant binding may only reference other bindings for
       which this flag is true.

 2. The new body of the WITH-ITERATORS form.

 3. A LAMBDA form of two arguments, representing the function to call
    when expanding WITH-ITER-VALUE for the sequence iterator.

    The first argument is the element binding pattern, to which
    elements of the sequence are bound. This may either be a list
    interpreted as a pattern to `destructuring-bind`.

    The second element is the list of forms comprising the body of the
    `WITH-ITER-VALUE` form.

    This function should return a form which binds the current
    sequence element to the variable(s) specified in the binding
    pattern, advances the position of the iterator to the next element
    in the sequence, and evaluates the body forms, with the element
    bindings visible to them.

    The form returned, should use the lexical TRAVERSE-FINISH macro to
    jump out of the WITH-ITERATORS form, if there are no more elements
    in the sequence."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',type *traverse-expanders*)
           (lambda (,type-var ,container ,args ,body ,env)
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


;;; WITH-ITERATORS Macro

(defmacro with-iterators (containers &body forms &environment env)
  "Create static iterators for one or more sequence.

This macro attempts to determine the type of each container and calls
the corresponding TRAVERSE expander function for the container type,
to generate optimal static iterator code.

CONTAINERS:

  A list of the form (ITER CONTAINER . ARGS) where ITER is a symbol
  identifying the iterator for the sequence, SEQUENCE is the form
  which evaluates to a sequence, and ARGS are the remaining iteration
  arguments (:START, :END, :FROM-END).

  Each iterator identifier (ITER) can be passed to WITH-ITER-VALUE,
  within the body forms of WITH-ITERATORS, to retrieve the value of
  the current element of the sequence.

  The iterator identifiers are in a namespace of their own that is
  they do not name lexical variables/symbol-macros nor
  functions/macros.

FORMS:

  A list of forms evaluated in an implicit TAGBODY, thus symbols are
  interpreted as tag names.

  The WITH-ITER-VALUE macro can be used, within FORMS, to retrieve the
  current element of the sequence and advance the iterator to the next
  position.

  The TRAVERSE-FINISH macro can be used to jump out of the
  WITH-ITERATORS form.

  NOTE: The value of the last form is not returned, due to it being
  evaluated in a TAGBODY, instead NIL is returned. RETURN-FROM, to an
  outer BLOCK, should be used to return a value from this form.

NOTE: Whilst the intended use of WITH-ITERATORS is to implement
iteration macros, such as DOSEQ, the FORMS are only evaluated once. It
is up to the user to implement the actual loop, using the provided
TAGBODY facility."

  (with-gensyms (end-tag)
    (labels ((expand-traverse (container args body env)
               (with-type-info (type () env) container
                 (multiple-value-list
                  (funcall (traverse-expander type env)
                           type
                           container
                           args
                           body
                           env))))

             (make-form (bindings get-values body)
               (make-bindings bindings (make-macros get-values body)))

             (make-macros (get-values body)
               (with-gensyms (pattern iter forms)
                 `(macrolet
                      ((with-iter-value ((,pattern ,iter) &body ,forms)
                         (case ,iter
                           ,@(loop for (it fn) in get-values
                                collect `(,it (,fn ,pattern ,forms)))
                           (otherwise
                            (error "In WITH-ITER-VALUE: ~s not one of ~s passed to WITH-ITERATORS."
                                   ,iter ',(mapcar #'car get-values))))))
                    ,@body)))

             ;; Tagbody

             (make-tagbody (body)
               `((macrolet ((traverse-finish () `(go ,',end-tag)))
                   (tagbody
                      ,@body
                      ,end-tag))))


             ;; Bindings

             (make-bindings (bindings body)
               (loop
                  for binding in bindings
                  for (init constant?) = (multiple-value-list (make-binding binding))
                  if constant? collect init into constants
                  else collect init into vars
                  finally
                    (return
                      `(symbol-macrolet ,constants
                         (let* ,vars ,body)))))

             (make-binding (binding)
               (destructuring-bind (var init &key constant) binding
                 (if constant
                     (multiple-value-bind (value constant?)
                         (constant-form-value init env)

                       (values
                        `(,var ,value)
                        constant?))

                     (values
                      `(,var ,init)
                      nil)))))

      (loop
         for (var container . args) in containers
         for (bindings body get-value) =
           (expand-traverse container args (make-tagbody forms) env) then
           (expand-traverse container args form-body env)

         for form-body = body
         append bindings into all-bindings
         collect (list var get-value) into get-values

         finally
           (return
             (make-form all-bindings get-values form-body))))))

(defmacro with-iter-value ((pattern iter) &body forms)
  "Bind the current element of a container pointed to by a static iterator, to a variable.

This macro may only be used within the body of a WITH-ITERATORS macro.

The current element of the container, with iterator ITER, is bound to
the variable(s) specified by PATTERN, with the bindings visible to
FORMS.

If the iterator is already at the end of the container, TRAVERSE-FINISH
is called to jump out of the enclosing WITH-ITERATORS form.

After binding, the iterator is advanced to the next element of the
sequence.

PATTERN:

  A binding pattern specifying the variable(s) to which the current
  element of the container is bound.

  This may either be a symbol, naming a variable, or a list which is
  interpreted as a pattern to `destructuring-bind`.

ITER:

  Symbol identifying the iterator, as established by the WITH-ITERATOR
  form.

  This must be one an iterator symbol passed in the first argument to
  the enclosing WITH-ITERATORS macro, otherwise an error is signalled.

  This may not be an iterator from a WITH-ITERATORS form other than
  the immediate WITH-ITERATORS form in which this form is nested.

FORMS:

  List of forms evaluated in an implicit PROGN. The binding(s) for the
  current element are visible to the forms.

NOTE: If there are no more elements in the container, the FORMS are
not evaluated and a non-local jump to the end of the WITH-ITERATORS
form is performed."

  (declare (ignore pattern iter forms))

  (error "Illegal usage of WITH-ITER-VALUE outside WITH-ITERATORS"))

(defmacro do-iter-values ((&rest iters) &body forms)
  "Iterate over the remaining elements of containers pointed to by static iterators.

Evaluate FORMS at each iteration, for each remaining element, until
one of the iterators reaches the end of its container.

NOTE: May only be used inside a WITH-ITERATORS form.

ITERS:

  List of bindings. Each element is of the form (PATTERN ITER),
  interpreted as if to WITH-ITER-VALUE, where PATTERN is a binding
  pattern, for the binding to the container element value, and ITER is
  a static iterator identifier symbol, which must have been created
  with the WITH-ITERATORS form in which this form is contained..

FORMS

  List of forms evaluated for each remaining element in the sequences
  pointed to by the static iterators.

  At each evaluation the next element of each container, pointed by
  each ITER in ITERS, is bound to the variable(s) specified by the
  corresponding PATTERN. After which the iterators are advanced to the
  next elements.

  If one of the iterators has reached the end of its sequence, a
  non-local jump to the end of the enclosing WITH-ITERATORS form is
  performed, i.e. any forms following this form, within the enclosing
  WITH-ITERATORS form, will not be evaluated."

  (flet ((make-iter-value (iter forms)
           (destructuring-bind (pattern iter) iter
             `((with-iter-value (,pattern ,iter)
                 ,@forms)))))

    (with-gensyms (start)
      `(tagbody
          ,start
          ,@(cl:reduce #'make-iter-value iters :initial-value forms :from-end t)
          (go ,start)))))


;;; TRAVERSE Macro

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

(defmacro traverse-fast% (name (&rest bindings) &body forms)
  "Optimized expansion of TRAVERSE.

Generates optimized iteration code for the containers types, using the
traverse expander functions defined with DEFINE-TRAVERSE-EXPANDER."

  (flet ((make-iter-binding (iter container)
           `(,iter ,@(rest container)))

         (make-value-binding (container iter)
           `(,(first container) ,iter)))

    (let ((iters (make-gensym-list (length bindings))))
      `(block ,name
         (with-iterators
             ,(mapcar #'make-iter-binding iters bindings)

           (do-iter-values ,(mapcar #'make-value-binding bindings iters)
             ,@forms))))))
