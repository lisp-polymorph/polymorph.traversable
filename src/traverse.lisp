;;; traverse.lisp
;;;
;;; Implementation of the TRAVERSE macro

(in-package #:polymorph.traversable)

;;; TRAVERSE Expanders

(defvar *traverse-expanders* (make-hash-table :test #'equal)
  "Hash-table of container traversal expanders for the TRAVERSE macro.

Each key denotes a container type with the corresponding value storing
the traversal expander function for that container type.")

(defmacro define-traverse-expander (type (type-var container args tag body &optional (env (gensym "ENV"))) &body forms)
  "Define a WITH-ITERATORS expansion for a given container type.

TYPE is the container type for which this expansion is defined. It
will be applied to all containers which are a subtype of this type.

TYPE-VAR is the variable which will receive the actual container type.

CONTAINER is the container form.

ARGS is the variable receiving the list of arguments passed after the
container form.

TAG is the variable receiving the name of the tag (in TAGBODY) to
which a non-local jump should be performed, by GO, when the end of the
container is reached.

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

 3. A lexical macro definition defining the expansion of
    WITH-ITER-VALUE for the sequence's iterator.

    This should be a list of the form:

       (LAMBDA-LIST . BODY)

    where LAMBDA-LIST is the macro lambda-list and BODY is the macro
    definition body. A name should not be provided as a name for the
    macro is generated.

    The lambda-list should have the following arguments:

       (PATTERN &BODY BODY)

    where PATTERN is the binding pattern, corresponding to the PATTERN
    argument of WITH-ITER-VALUE, describing which variable(s) to bind
    to the value of current sequence element.

    This may either be a symbol, naming a variable or a list which
    should be interpreted as a destructuring-bind pattern.

    BODY is the list of body forms of the WITH-ITER-VALUE form,
    corresponding to the BODY argument.

    The macro should expand to a form which binds the current sequence
    element to the variable(s) specified in PATTERN, advances the
    position of the iterator to the next element in the sequence, and
    evaluates the body forms.

    The expansion should jump out of the WITH-ITERATORS form, using a
    GO to the tag name given in the TAG argument.

 4. A lexical macro definition defining the expansion of
    WITH-ITER-PLACE for the sequence's iterator.

    This should be a list of the form:

       (LAMBDA-LIST . BODY)

    where LAMBDA-LIST is the macro lambda-list and BODY is the macro
    definition body. A name should not be provided as a name for the
    macro is generated.

    The lambda-list should have the following arguments:

       (NAME MOREP &BODY FORMS)

    where NAME is the name of the symbol-macro to be introduced,
    expanding to the 'place' of the current sequence element,
    corresponding to the NAME argument of WITH-ITER-PLACE.

    MOREP corresponds to the MOREP argument of WITH-ITER-PLACE which
    is the name of the variable which should be bound to true if there
    are more elements in the sequence and bound to NIL if there are no
    more elements. If MOREP is NIL, the expansion should jump out of
    the WITH-ITERATORS form, skipping the evaluation of FORMS, using a
    GO to the tag name given in the TAG argument.

    FORMS are the body forms of the WITH-ITER-PLACE form,
    corresponding to the FORMS argument of WITH-ITER-PLACE.

    If this return value is NIL it is assumed the sequence is
    immutable, and any uses of WITH-ITER-PLACE on it will result in an
    error being signalled."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',type *traverse-expanders*)
           (lambda (,type-var ,container ,args ,tag ,body ,env)
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


;;;; Local WITH-ITER-VALUE and WITH-ITER-PLACE expansions

(defun iter-value-macros (env)
  "Retrieve the association list mapping iterator identifiers to the
names of the macros serving as the expansion of WITH-ITER-VALUE when
given those iterator identifiers. This information is retrieved from
the environmnet ENV by macroexpanding the symbol-macro
ITER-VALUE-MACROS."

  (multiple-value-bind (macros expanded?)
      (macroexpand 'iter-value-macros env)

    (when expanded?
      macros)))

(defun iter-value-places (env)
  "Retrieve the association list mapping iterator identifiers to the
names of the macros serving as the expansion of WITH-ITER-PLACE when
given those iterator identifiers. This information is retrieved from
the environmnet ENV by macroexpanding the symbol-macro
ITER-VALUE-PLACES."

  (multiple-value-bind (macros expanded?)
      (macroexpand 'iter-value-places env)

    (when expanded?
      macros)))


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

  The WITH-ITER-PLACE macro can be used, within FORMS, both to
  retrieve and set the value of the current element of the sequence,
  and advance the iterator to the next position.

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
                           end-tag
                           body
                           env))))

             (make-form (bindings get-values places body)
               (make-bindings bindings (make-macros get-values places body)))

             (make-macros (get-values places body)
               `(symbol-macrolet
                    ((iter-value-macros
                      ,(append get-values (iter-value-macros env)))

                     (iter-value-places
                      ,(append places (iter-value-places env))))

                  ,@body))

             ;; Tagbody

             (make-tagbody (body)
               `((tagbody
                    ,@body
                    ,end-tag)))


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
         for (bindings body value place) =
           (expand-traverse container args (make-tagbody forms) env) then
           (expand-traverse container args form-body env)

         for iter-value = (gensym "ITER-VALUE")
         for iter-place = (gensym "ITER-PLACE")
         for form-body = `((macrolet ((,iter-value ,@value)
                                      (,iter-place
                                          ,@(or place
                                                `((&rest args)
                                                  (declare (ignore args))
                                                  (error "In WITH-ITER-PLACE: Iterator ~s points to immutable sequence." ',var)))))
                             ,@body))

         append bindings into all-bindings
         collect (cons var iter-value) into get-values
         collect (cons var iter-place) into places

         finally
           (return
             (make-form all-bindings get-values places form-body))))))

(defmacro with-iter-value ((pattern iter) &body body)
  "Bind the current element of a container pointed to by a static iterator, to a variable.

This macro may only be used within the body of a WITH-ITERATORS macro.

The current element of the container, with iterator ITER, is bound to
the variable(s) specified by PATTERN, with the bindings visible to
FORMS.

If the iterator is already at the end of the sequence a non-local
jump, to the end of the enclosing WITH-ITERATORS form, is performed.

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

  This must name an iterator introduced in a parent WITH-ITERATORS
  form.

BODY:

  The body of the WITH-ITER-VALUE form:

    (DECLARATION* FORM*)

  The body consists of a list of forms evaluated in an implicit PROGN,
  with the value of the last form returned from the WITH-ITER-VALUE
  form. The binding(s) introduced by PATTERN are visible to forms.

  The forms may be preceded by one or more declaration expressions,
  which may apply to the variables introduced in PATTERN.

NOTE: If there are no more elements in the container, the FORMS are
not evaluated and a non-local jump to the end of the WITH-ITERATORS
form is performed."

  `(with-iter-values ((,pattern ,iter)) ,@body))

(defmacro with-iter-values ((&rest bindings) &body body &environment env)
  "Like WITH-ITER-VALUE except multiple sequence elements are bound simultaneously.

BINDINGS:

  A list of element value bindings, corresponding to the first argument
  of WITH-ITER-VALUE, each of the form (PATTERN ITER).

    ((pattern-1 iter-1) (pattern-2 iter-2) ... (pattern-n iter-n))

  This form is functionally equivalent to a series of nested
  WITH-ITER-VALUE forms:

    (with-iter-value (pattern-1 iter-1)
      (with-iter-value (pattern-2 iter-2)
        (...
          (with-iter-value (pattern-n iter-n)
            ,@body))))

  However unlike simply nesting WITH-ITER-VALUE forms, declarations
  occurring in BODY are handled properly and associated with the
  correct WITH-ITER-VALUE form, depending on which variable(s) they
  apply to.

BODY:

  The body of the WITH-ITER-VALUES form:

    (DECLARATION* FORM*)

  The body consists of a list of forms evaluate in an implicit PROGN,
  with the value of the last form returned from the WITH-ITER-VALUE
  form. The binding(s) introduced by PATTERN are visible to forms.

  The forms may be preceded by one or more declaration expressions,
  which may apply to the variables introduced in any of the binding
  patterns, in BINDINGS.

NOTE: If there are no more elements in at least of the sequences, the
forms are not evaluated and a non-local jump to the end of the
enclosing WITH-ITERATORS form is performed."

  (let ((bind-macros (iter-value-macros env)))
    (unless bind-macros
      (error "Illegal use of WITH-ITER-VALUE outside WITH-ITERATORS."))

    (flet ((make-iter-value (binding body)
             (destructuring-bind (pattern iter) binding
               (check-type iter symbol)

               (let ((macro (assoc iter bind-macros)))
                 (unless macro
                   (error "In WITH-ITER-VALUE: ~s not one of ~{~s~^, ~} passed to WITH-ITERATORS."
                          iter (mapcar #'car bind-macros)))

                 (macroexpand (list* (cdr macro) pattern body) env)))))

      `(progn
         ,@(cl:reduce
            (lambda (binding body)
              (list (make-iter-value binding body)))

            bindings
            :from-end t
            :initial-value body)))))

(defmacro with-iter-place ((name iter &optional morep) &body forms &environment env)
  "Introduce an identifier serving as a place to the current sequence element.

This macro may only be used within the body of a WITH-ITERATORS macro.

A symbol-macro, with identifier NAME, is introduced which expands to a
place, suitable for use with SETF, to the current element of the
sequence, pointed by the iterator ITER. This symbol-macro is visible
to the body FORMS of the WITH-ITER-PLACE form.

If the iterator is already at the end of the sequence a non-local
jump, to the end of the enclosing WITH-ITERATORS form, is performed..

Simultaneously the iterator is also advanced to the next element of
the sequence. However, the iterator is only guaranteed to be advanced
on a normal exit from the WITH-ITER-PLACE form. If a non-local jump is
used, via GO, RETURN-FROM or THROW, the iterator might not be
advanced.

NAME:

  Identifier of the symbol-macro which is introduced.

  NOTE: Unlike in WITH-ITER-VALUE this must be a symbol, and cannot be
  a destructuring-bind pattern.

MOREP (Optional)

  The name of the variable which is bound to true if there are more
  elements in the sequence, and to NIL when there are no more elements
  in the sequence.

  If NON-NIL it is not checked whether the end of the sequence has
  been reached, and hence the body FORMS are not skipped. It is up to
  the programmer to check the value of this variable and perform
  whatever logic should be performed when the end of the sequence has
  been reached.

ITER:

  Symbol identifying the iterator, as established by the WITH-ITERATOR
  form.

  This must name an iterator introduced in a parent WITH-ITERATORS
  form.

FORMS:

  List of forms evaluated in an implicit PROGN. The binding(s) for the
  current element are visible to the forms.

  NOTE: If there are no more elements in the sequence, the FORMS are
  not evaluated and a non-local jump to the end of the WITH-ITERATORS
  form is performed."

  (let ((place-macros (iter-value-places env)))
    (unless place-macros
      (error "Illegal use of WITH-ITER-PLACE outside WITH-ITERATORS."))

    (if-let ((macro (assoc iter place-macros)))
      (list* (cdr macro) name morep forms)
      (error "In WITH-ITER-PLACE: ~s not one of ~{~s~^ or~} passed to WITH-ITERATORS."
             iter (mapcar #'car place-macros)))))

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
non-local jump outside the enclosing WITH-ITERATORS form is performed,
i.e. any forms following this form, within the enclosing
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

(defmacro do-iter-places ((&rest iters) &body forms)
  "Like DO-ITER-VALUES but instead of binding the value of each
sequence element, to variables, by WITH-ITER-VALUE, a symbol-macro
expanding to the 'place' of the current sequence element, is
introduced, as if by WITH-ITER-PLACE."

  (flet ((make-iter-place (iter forms)
           (destructuring-bind (var iter) iter
             `((with-iter-place (,var ,iter)
                 ,@forms)))))

    (with-gensyms (start)
      `(tagbody
          ,start
          ,@(cl:reduce #'make-iter-place iters :initial-value forms :from-end t)
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

;;; ITERATE Macro

(defmacro iterate (name/bindings &body forms &environment env)
  "Mutable version of TRAVERSE.

This is the same as TRAVERSE however each NAME, is the name of a
symbol-macro, that is introduced, which expands to the 'place',
suitable for use with SETF, of the current sequence element, rather
than a variable storing its value. This allows the elements of the
sequence to be mutated.

NOTE: This macro does not support destructuring of the sequence
elements."

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

    `(iterate-fast% ,name ,bindings ,@forms)))

(defmacro iterate-fast% (name (&rest bindings) &body forms)
  "Optimized expansion of ITERATE.

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

           (do-iter-places ,(mapcar #'make-value-binding bindings iters)
             ,@forms))))))
