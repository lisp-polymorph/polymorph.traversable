;;; iterators.lisp
;;;
;;; Implementation of iterator interface for standard Common Lisp
;;; containers

(in-package #:polymorph.traversable)

(defpolymorph make-iterator ((l list)) t
  (lambda ()
    (when l
      (values
       t
       (prog1 (car l)
         (setf l (cdr l)))))))

(defpolymorph make-iterator ((v vector)) t
  (let ((i 0)
        (size (length v)))

    (lambda ()
      (when (< i size)
        (values
         t
         (prog1 (aref v i)
           (incf i)))))))

(defpolymorph make-iterator ((table hash-table)) t
  (make-iterator
   (loop
      for key being the hash-key of table
      for value being the hash-value of table
      collect (list key value))))
