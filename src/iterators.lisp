;;; iterators.lisp
;;;
;;; Implementation of iterator interface for standard Common Lisp
;;; containers

(in-package #:polymorph.traversable)

(define-polymorphic-function make-iterator (container &key start end from-end)
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


;;; Implementations

(defpolymorph make-iterator ((l list) &key (start 0) end from-end) t
  (let ((l (cond
             (from-end
              (cl:reverse
               (if (or end (plusp start))
                   (cl:subseq l start end)
                   l)))

             ((plusp start)
              (nthcdr start l))

             (t l))))


    (if (and (not from-end) end)
        (let ((i start))
          (lambda ()
            (when (and l (< i end))
              (values
               t
               (prog1 (car l)
                 (setf l (cdr l))
                 (incf i))))))

        (lambda ()
          (when l
            (values
             t
             (prog1 (car l)
               (setf l (cdr l)))))))))

(defpolymorph make-iterator ((v vector) &key (start 0) end from-end) t
  (let ((end (or end (1- (length v)))))
    (if (not from-end)
        (let ((i start))
          (lambda ()
            (when (< i end)
              (values
               t
               (prog1 (aref v i)
                 (incf i))))))

        (let ((i end))
          (lambda ()
            (when (>= i start)
              (values
               t
               (prog1 (aref v i)
                 (decf i)))))))))

(defpolymorph make-iterator ((table hash-table) &key (start 0) end from-end) t
  (declare (ignore from-end)) ; Ignore since hash-tables aren't ordered anyway

  (make-iterator
   (loop
      for key being the hash-key of table
      for value being the hash-value of table
      collect (list key value))

   :start start
   :end end))
