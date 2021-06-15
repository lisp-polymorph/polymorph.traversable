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

(define-polymorphic-function traverse-container (container &key start end from-end)
  :documentation
  "Return the container (or sub-container of) to traverse.

CONTAINER is a container.

START is the index of the first element to traverse.

END is the index of the element passed the last element to
traverse. If NIL the entire container is traversed.

FROM-END is a flag, which if true the container is traversed from back
to front, otherwise it is traversed from front to back.")

(defpolymorph traverse-container ((seq list) &key (start 0) end from-end)
    list

  (let ((seq (cl:subseq seq start end)))
    (if from-end
        (cl:reverse seq)
        seq)))


(defpolymorph-compiler-macro traverse-container (list &key (:start t) (:end t) (:from-end t)) (&whole form list &key (start 0) end from-end &environment env)

  (multiple-value-bind (from-end c-from-end? start c-start? end c-end?)
      (process-iterator-args `(:from-end ,from-end :start ,start :end ,end) env)

    (if (and c-start? c-end?)
        (let ((list (cond
                      (end `(cl:subseq ,list ,start ,end))
                      ((> start 0) `(nthcdr ,start ,list))
                      (t list))))

          (if c-from-end?
              (if from-end
                  `(cl:reverse ,list)
                  list)

              `(if ,from-end
                   `(cl:reverse ,list)
                   ,list)))

        form)))

;;; Lists

(define-traverse-expander list (type element form args body)
  (with-gensyms (list)
    (values
     `((,list (traverse-container ,form ,@args)))

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
