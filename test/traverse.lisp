;;; Unit tests for TRAVERSE and ITERATE macros

(defpackage #:polymorph.traversable/test.traverse
  (:use #:cl
        #:alexandria
        #:fiveam
        #:polymorph.traversable/test)

  (:import-from #:polymorph.traversable
                #:traverse
                #:iterate))

(in-package #:polymorph.traversable/test.traverse)

;;; Test suite definition

(def-suite traverse
    :description "Test TRAVERSE and ITERATE macros"
    :in polymorph.traversable)

(in-suite traverse)


;;; Test TRAVERSE

;;;; Lists

(test traverse-list-unbounded
  "Test TRAVERSE macro on list (unbounded)"

  (let (result)
    (traverse ((elem '(1 2 3 4)))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(1 2 3 4) (nreverse result)))))

(test traverse-list-destructure
  "Test TRAVERSE with destructuring on list"

  (let (result)
    (traverse (((x y) '((1 2) (3 4) (5 6) (7 8))))
      (declare (type number x) (fixnum y))
      (declare (optimize speed))
      (push (+ x y) result))

    (is (equal '(3 7 11 15) (nreverse result)))))

(test traverse-list-reverse
  "Test TRAVERSE macro on list with :FROM-END t"

  (let* ((list '(1 2 3 4))
         (result nil))

    (traverse ((elem (the list list) :from-end t))
      (declare (number elem))
      (declare (optimize speed))
      (push elem result))

    (is (equal '(4 3 2 1) (nreverse result)))))

(test traverse-list-bounded
  "Test TRAVERSE macro on list with :START 1 and :END 4"

  (let* (result)
    (traverse ((elem '(1 2 3 4 5) :start 1 :end 4))
      (declare (integer elem) (optimize speed))
      (push elem result))

    (is (equal '(2 3 4) (nreverse result)))))

(test traverse-list-reverse-bounded
  "Test TRAVERSE macro on list with :START 1, :END 4, :FROM-END T"

  (let* (result)
    (traverse ((elem '(1 2 3 4 5) :start 1 :end 4 :from-end t))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(4 3 2) (nreverse result)))))

;;;; Vectors

(test traverse-vector-unbounded
  "Test TRAVERSE macro on vector (unbounded)"

  (let (result)
    (traverse ((elem #(1 2 3 4)))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(1 2 3 4) (nreverse result)))))

(test traverse-vector-destructure
  "Test TRAVERSE with destructuring on vector"

  (let (result)
    (traverse (((x y) #((1 2) (3 4) (5 6) (7 8))))
      (declare (type number x) (fixnum y))
      (declare (optimize speed))
      (push (+ x y) result))

    (is (equal '(3 7 11 15) (nreverse result)))))

(test traverse-vector-reverse
  "Test TRAVERSE macro on vector with :FROM-END t"

  (let* ((vec #(1 2 3 4))
         (result nil))

    (traverse ((elem (the vector vec) :from-end t))
      (declare (number elem))
      (declare (optimize speed))
      (push elem result))

    (is (equal '(4 3 2 1) (nreverse result)))))

(test traverse-vector-bounded
  "Test TRAVERSE macro on vector with :START 1 and :END 4"

  (let* (result)
    (traverse ((elem #(1 2 3 4 5) :start 1 :end 4))
      (declare (integer elem) (optimize speed))
      (push elem result))

    (is (equal '(2 3 4) (nreverse result)))))

(test traverse-vector-reverse-bounded
  "Test TRAVERSE macro on vector with :START 1, :END 4, :FROM-END T"

  (let* (result)
    (traverse ((elem #(1 2 3 4 5) :start 1 :end 4 :from-end t))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(4 3 2) (nreverse result)))))


;;;; Hash Tables

(test traverse-hash-table
  "Test TRAVERSE macro on hash-table"

  (let* ((map (alist-hash-table '((a . 1) (b . 2) (c . 3))))
	 (new-map (make-hash-table)))

    (traverse (((key . value) (the hash-table map)))
      (declare (type (or symbol null) key) (type (or number null) value))
      (setf (gethash key new-map) value))

    (is-every =
      (1 (gethash 'a new-map))
      (2 (gethash 'b new-map))
      (3 (gethash 'c new-map)))))


;;;; Iterator Based Implementation

(test traverse-generic
  "Test TRAVERSE macro on untyped sequence"

  (let* ((seq '(1 2 3 4 5 6))
         (result nil))

    (traverse ((elem seq))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(1 2 3 4 5 6) (nreverse result)))))

(test traverse-generic-destructure
  "Test TRAVERSE with destructuring on untyped sequence"

  (let ((seq '((1 2) (3 4) (5 6) (7 8)))
        (result nil))

    (traverse (((x y) seq))
      (declare (type number x) (fixnum y))
      (declare (optimize speed))
      (push (+ x y) result))

    (is (equal '(3 7 11 15) (nreverse result)))))

(test traverse-generic-reverse
  "Test TRAVERSE macro on untyped sequence with :FROM-END t"

  (let* ((list '(1 2 3 4))
         (result nil))

    (traverse ((elem list :from-end t))
      (declare (number elem))
      (declare (optimize speed))
      (push elem result))

    (is (equal '(4 3 2 1) (nreverse result)))))

(test traverse-generic-bounded
  "Test TRAVERSE macro on untyped sequence with :START 1 and :END 4"

  (let* ((list '(1 2 3 4 5))
         result)

    (traverse ((elem list :start 1 :end 4))
      (declare (integer elem) (optimize speed))
      (push elem result))

    (is (equal '(2 3 4) (nreverse result)))))

(test traverse-generic-reverse-bounded
  "Test TRAVERSE macro on untyped sequence with :START 1, :END 4, :FROM-END T"

  (let* ((seq '(1 2 3 4 5))
         result)

    (traverse ((elem seq :start 1 :end 4 :from-end t))
      (declare (type integer elem) (optimize speed))
      (push elem result))

    (is (equal '(4 3 2) (nreverse result)))))


;;;; DO-SEQUENCES macro

(test traverse-multiple
  "Test TRAVERSE on multiple sequences"

  (let ((res1 nil)
        (res2 nil)
        (res3 (make-hash-table)))

    (traverse ((x (list 1 2 3))
               (y #(a b c d e) :start 1)
               ((zkey . zval) (the hash-table (alist-hash-table '((k1 . 1) (k2 . 2) (k3 . 3))))))

      (declare (number x) (symbol y))
      (declare (optimize speed) (type (or symbol null) zkey))

      (push x res1)
      (push y res2)
      (setf (gethash zkey res3) zval))

    (is (equal '(1 2 3) (nreverse res1)))
    (is (equal '(b c d) (nreverse res2)))

    (is-every =
      (1 (gethash 'k1 res3))
      (2 (gethash 'k2 res3))
      (3 (gethash 'k3 res3)))))

;;; Test ITERATE

;;;; Lists

(test iterate-list-unbounded
  "Test ITERATE macro on list (unbounded)"

  (let ((l (list 1 2 3 4))
        (result)
        (i 0))

    (iterate ((elem (the list l)))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 3)
         (setf elem 'y)))

      (push elem result)

      (incf i))

    (is (equal '(1 x 3 y) (nreverse result)))
    (is (equal '(1 x 3 y) l))))

(test iterate-list-reverse
  "Test ITERATE macro on list with :FROM-END t"

  (let* ((list (list 1 2 3 4))
         (result nil)
         (i 0))

    (iterate ((elem (the list list) :from-end t))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 2)
         (setf elem 'y)))

      (push elem result)
      (incf i))

    (is (equal '(4 x y 1) (nreverse result)))
    (is (equal '(1 y x 4) list))))

(test iterate-list-bounded
  "Test ITERATE macro on list with :START 1 and :END 4"

  (let* ((list (list 1 2 3 4 5))
         (result nil)
         (i 0))

    (iterate ((elem (the list list) :start 1 :end 4))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equal '(1 a 3 b 5) list))))

(test iterate-list-reverse-bounded
  "Test ITERATE macro on list with :START 1, :END 4, :FROM-END T"

  (let* ((l (list 1 2 3 4 5))
         (result)
         (i 0))

    (iterate ((elem (the list l) :start 1 :end 4 :from-end t))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equal '(1 b 3 a 5) l))))


;;;; Vectors

(test iterate-vector-unbounded
  "Test ITERATE macro on vector (unbounded)"

  (let ((v (vector 1 2 3 4))
        (result)
        (i 0))

    (iterate ((elem (the vector v)))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 3)
         (setf elem 'y)))

      (push elem result)
      (incf i))

    (is (equal '(1 x 3 y) (nreverse result)))
    (is (equalp #(1 x 3 y) v))))

(test iterate-vector-reverse
  "Test ITERATE macro on vector with :FROM-END t"

  (let* ((vec (vector 1 2 3 4))
         (result)
         (i 0))

    (iterate ((elem (the vector vec) :from-end t))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 2)
         (setf elem 'y)))

      (push elem result)
      (incf i))

    (is (equal '(4 x y 1) (nreverse result)))
    (is (equalp #(1 y x 4) vec))))

(test iterate-vector-bounded
  "Test ITERATE macro on vector with :START 1 and :END 4"

  (let* ((v (vector 1 2 3 4 5))
         (result)
         (i 0))

    (iterate ((elem (the vector v) :start 1 :end 4))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equalp #(1 a 3 b 5) v))))

(test iterate-vector-reverse-bounded
  "Test ITERATE macro on vector with :START 1, :END 4, :FROM-END T"

  (let* ((v (vector 1 2 3 4 5))
         (result)
         (i 0))
    (iterate ((elem (the vector v) :start 1 :end 4 :from-end t))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equalp #(1 b 3 a 5) v))))

;;;; Hash Tables

(test iterate-hash-table
  "Test ITERATE macro on hash-table"

  (let* ((map (alist-hash-table '((a . 1) (b . 2) (c . 3))))
	 (new-map (make-hash-table)))

    (iterate ((item (the hash-table map)))
      (destructuring-bind (key . value) item
        (declare (ignore value))

        (if (eql key 'a)
            (setf item 'x1))

        (if (eql key 'c)
            (setf item 'x2))

        (setf (gethash key new-map) (cdr item))))

    (is-every eql
      ('x1 (gethash 'a new-map))
      (2 (gethash 'b new-map))
      ('x2 (gethash 'c new-map)))

    (is-every eql
      ('x1 (gethash 'a map))
      (2 (gethash 'b map))
      ('x2 (gethash 'c map)))))

;;;; Iterator Based Implementation

(test iterate-generic
  "Test ITERATE macro on untyped sequence"

  (let* ((seq (list 1 2 3 4 5 6))
         (result nil)
         (i 0))

    (iterate ((elem seq))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 3)
         (setf elem 'y)))

      (push elem result)
      (incf i))

    (is (equal '(1 x 3 y 5 6) (nreverse result)))
    (is (equal '(1 x 3 y 5 6) seq))))

(test iterate-generic-reverse
  "Test DOSEQ macro on untyped sequence with :FROM-END t"

  (let* ((list (list 1 2 3 4))
         (result nil)
         (i 0))

    (iterate ((elem list :from-end t))
      (cond
        ((= i 1)
         (setf elem 'x))

        ((= i 2)
         (setf elem 'y)))

      (push elem result)
      (incf i))

    (is (equal '(4 x y 1) (nreverse result)))
    (is (equal '(1 y x 4) list))))

(test iterate-generic-bounded
  "Test ITERATE macro on untyped sequence with :START 1 and :END 4"

  (let* ((list (list 1 2 3 4 5))
         (result)
         (i 0))

    (iterate ((elem list :start 1 :end 4))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equal '(1 a 3 b 5) list))))

(test iterate-generic-reverse-bounded
  "Test ITERATE macro on untyped sequence with :START 1, :END 4, :FROM-END T"

  (let* ((seq (list 1 2 3 4 5))
         (result)
         (i 0))

    (iterate ((elem seq :start 1 :end 4 :from-end t))
      (cond
        ((= i 0)
         (setf elem 'a))

        ((= i 2)
         (setf elem 'b)))

      (push elem result)
      (incf i))

    (is (equal '(a 3 b) (nreverse result)))
    (is (equal '(1 b 3 a 5) seq))))
