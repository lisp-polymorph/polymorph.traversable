;;; iterators.lisp
;;;
;;; Implementation of iterator interface for standard Common Lisp
;;; containers

(in-package #:polymorph.traversable)

;;; Interface

(define-polymorphic-function make-iterator (container &key start end from-end)
  :overwrite t
  :documentation
  "Return an iterator function for a container.

An iterator is an object for which the ELEMENT, ADVANCE and MOREP
polymorphic-functions are implemented.")

(define-polymorphic-function element (iterator)
  :overwrite t
  :documentation
  "Return the element at the iterator's current position within the container.")

(define-polymorphic-function (setf element) (value iterator)
  :overwrite t
  :documentation
  "Set the value of the container element at the current position of
the iterator.")

(define-polymorphic-function advance (iterator)
  :overwrite t
  :documentation
  "Move the position of the iterator to the next element in the container.")

(define-polymorphic-function morep (iterator)
  :overwrite t
  :documentation
  "Return true if there are more elements in the container, from the
iterator's current position. Return NIL if the iterator is at the end
of the container.")


;;; List Iterators

(defstruct list-iterator
  list)

(defstruct (bound-list-iterator (:include list-iterator))
  index
  end)

(defstruct reverse-list-iterator
  cells)


;;;; ELEMENT

(defpolymorph element ((it list-iterator)) t
  (car (list-iterator-list it)))

(defpolymorph element ((it reverse-list-iterator)) t
  (caar (reverse-list-iterator-cells it)))


;;;; SETF ELEMENT

(defpolymorph (setf element) (value (it list-iterator)) t
  (setf (car (list-iterator-list it)) value))

(defpolymorph (setf element) (value (it reverse-list-iterator)) t
  (setf (caar (reverse-list-iterator-cells it)) value))


;;;; ADVANCE

(defpolymorph advance ((it list-iterator)) t
  (with-accessors ((list list-iterator-list)) it
    (setf list (cdr list))))

(defpolymorph advance ((it bound-list-iterator)) t
  (with-accessors ((list bound-list-iterator-list)
                   (index bound-list-iterator-index))
      it

    (setf list (cdr list))
    (incf index)))

(defpolymorph advance ((it reverse-list-iterator)) t
  (with-accessors ((cells reverse-list-iterator-cells)) it
    (setf cells (cdr cells))))


;;;; MOREP

(defpolymorph morep ((it list-iterator)) t
  (and (list-iterator-list it) t))

(defpolymorph morep ((it bound-list-iterator)) t
  (and (bound-list-iterator-list it)
       (< (bound-list-iterator-index it)
          (bound-list-iterator-end it))))

(defpolymorph morep ((it reverse-list-iterator)) t
  (and (reverse-list-iterator-cells it) t))


;;;; MAKE-ITERATOR

(defun sublist (list start end from-end)
  "Return the list of CONS cells making up a subsequence of a list.

LIST is the list.

START is the index of the first element of the subsequence.

END is the index 1 past the last element of the subsequence. If NIL
the subsequence extends till the end of the list.

If FROM-END is true the cells are collected starting from the last
element of the subsequence, otherwise they are collected starting from
the first element of the subsequence.

The return value is a list of CONS cells of the original list,
corresponding to the cells containing the elements of the
subsequence. This allows modifying the original list by modifying the
cons cells."

  (if from-end
      (let (cells)
        (loop
           for cell on (nthcdr start list)
           for i from start
           while (or (null end) (< i end))
           do
             (push cell cells))

        cells)

      (loop
         for cell on (nthcdr start list)
         for i from start
         while (or (null end) (< i end))
         collect cell)))

(defpolymorph make-iterator ((l list) &key (start 0) end from-end) t
  (cond
    (from-end
     (make-reverse-list-iterator :cells (sublist l start end t)))

    (end
     (make-bound-list-iterator
      :list (nthcdr start l)
      :index start
      :end end))

    (t
     (make-list-iterator :list (nthcdr start l)))))


;;; Vectors

(defstruct vector-iterator
  vector
  index
  end)

(defstruct (reverse-vector-iterator (:include vector-iterator)))


;;;; ELEMENT

(defpolymorph element ((it vector-iterator)) t
  (aref (vector-iterator-vector it) (vector-iterator-index it)))

;;;; SETF ELEMENT

(defpolymorph (setf element) (value (it vector-iterator)) t
  (setf (aref (vector-iterator-vector it)
              (vector-iterator-index it))
        value))

;;;; ADVANCE

(defpolymorph advance ((it vector-iterator)) t
  (incf (vector-iterator-index it)))

(defpolymorph advance ((it reverse-vector-iterator)) t
  (decf (vector-iterator-index it)))

;;;; MOREP

(defpolymorph morep ((it vector-iterator)) t
  (< (vector-iterator-index it)
     (vector-iterator-end it)))

(defpolymorph morep ((it reverse-vector-iterator)) t
  (>= (reverse-vector-iterator-index it)
      (reverse-vector-iterator-end it)))


;;;; MAKE-ITERATOR

(defpolymorph make-iterator ((v vector) &key (start 0) end from-end) t
  (if from-end
      (make-reverse-vector-iterator
       :vector v
       :index (1- (or end (length v)))
       :end start)

      (make-vector-iterator
       :vector v
       :index start
       :end (or end (length v)))))


;;; Hash Tables

(defstruct hash-table-iterator
  table
  entries)

(defpolymorph element ((it hash-table-iterator)) t
  (car (hash-table-iterator-entries it)))

(defpolymorph (setf element) (value (it hash-table-iterator)) t
  (with-accessors ((entries hash-table-iterator-entries)
                   (table hash-table-iterator-table))
      it

    (setf (gethash (caar entries) table) value)))

(defpolymorph morep ((it hash-table-iterator)) t
  (hash-table-iterator-entries it))

(defpolymorph advance ((it hash-table-iterator)) t
  (with-accessors ((entries hash-table-iterator-entries)) it
    (setf entries (cdr entries))))

(defpolymorph make-iterator ((table hash-table) &key (start 0) end from-end) t
  (declare (ignore from-end start end)) ; Ignore since hash-tables aren't ordered anyway

  (make-hash-table-iterator
   :table table
   :entries (hash-table-alist table)))
