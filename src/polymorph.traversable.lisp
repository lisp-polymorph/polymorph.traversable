;;;; polymorph.traversable.lisp

(in-package #:polymorph.traversable)

;; Find
(define-polymorphic-function find (item container &key from-end start end
                                        key test) :overwrite t)

(defpolymorph (find :inline t) ((item t) (container t) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end (maybe ind)) nil)
                                ((key function) #'identity) ((test function) #'=))
    (values t boolean &optional)
  (traverse ((cand container :start start :end (or end (size container)) :from-end from-end))
    (let ((cand (funcall key cand)))
      (when (funcall test cand item)
        (return-from find (values cand t)))))
  (values nil nil))



(defpolymorph (find :inline t) ((item t) (container hash-table) &key ((from-end null) nil)
                                ((start (eql 0)) 0) ((end null) nil)
                                ((key function) #'identity)
                                ((test function) #'=))
    (values t boolean &optional)
  (declare (ignorable start end from-end))
  (cond
    ((or (eq key #'car) (eq key #'first))
     (loop :for k :being :the :hash-key :in container
           :when (funcall test item k)
             :do (return (values k t))
           :finally (return (values nil nil)))) ;;;TODO sometimes gethash is good
    ((or (eq key #'cdr) (eq key #'rest))
     (loop :for v :being :the :hash-value :in container
           :when (funcall test item v)
             :do (return (values v t))
           :finally (return (values nil nil))))
    (t
     (loop :for k :being :the :hash-keys :in container
             :using (hash-value v)
           :for pair := (funcall key (cons k v))
           :when (funcall test item pair)
             :do (return (values pair t))
           :finally (return (values nil nil))))))


(define-polymorphic-function find-if (predicate container &key from-end start end key) :overwrite t)


(defpolymorph (find-if :inline t) ((predicate function) (container array) &key ((from-end boolean) nil)
                                   ((start ind) 0) ((end (maybe ind)) nil)
                                   ((key function) #'identity))
    (values t boolean &optional)
   (traverse ((cand container :start start :end (or end (size container)) :from-end from-end))
    (let ((cand (funcall key cand)))
      (when (funcall predicate cand)
        (return-from find-if (values cand t)))))
   (values nil nil))




(defpolymorph (find-if :inline t) ((predicate function) (container hash-table) &key ((from-end null) nil)
                                   ((start (eql 0)) 0) ((end null) nil)
                                   ((key function) #'identity))
    (values t boolean &optional)
  (declare (ignorable start end from-end))
  (cond
    ((or (eq key #'car) (eq key #'first))
     (loop :for k :being :the :hash-key :in container
           :when (funcall predicate k)
             :do (return (values k t))
           :finally (return (values nil nil)))) ;;;TODO sometimes gethash is good
    ((or (eq key #'cdr) (eq key #'rest))
     (loop :for v :being :the :hash-value :in container
           :when (funcall predicate v)
             :do (return (values v t))
           :finally (return (values nil nil))))
    (t
     (loop :for k :being :the :hash-keys :in container
             :using (hash-value v)
           :for pair := (funcall key (cons k v))
           :when (funcall predicate pair)
             :do (return (values pair t))
           :finally (return (values nil nil))))))



;Count
(define-polymorphic-function count (item container &key from-end start end
                                         key test) :overwrite t)

(defpolymorph (count :inline t) ((item t) (container t) &key ((from-end boolean) nil)
                                 ((start ind) 0) ((end (maybe ind)) nil)
                                 ((key function) #'identity) ((test function) #'=))
   ind
  (let ((res 0))
    (declare (type ind res))
    (traverse ((cand container :start start :end (or end (size container)) :from-end from-end))
      (let ((cand (funcall key cand)))
        (when (funcall test cand item)
          (incf (the ind res)))))
    res))



(defpolymorph (count :inline t) ((item t) (container hash-table) &key ((from-end null) nil)
                                 ((start (eql 0)) 0) ((end null) nil)
                                 ((key function) #'identity)
                                 ((test function) #'=))
    ind
  (declare (ignorable start end from-end))
  (let ((res 0))
    (declare (type ind res))
    (cond
      ((or (eq key #'car) (eq key #'first))
       (loop :for k :being :the :hash-key :in container
             :when (funcall test item k)
               :do (incf (the ind res))
             :finally (return res)))
      ((or (eq key #'cdr) (eq key #'rest))
       (loop :for v :being :the :hash-value :in container
             :when (funcall test item v)
               :do (incf (the ind res))
             :finally (return res)))
      (t
       (loop :for k :being :the :hash-keys :in container
               :using (hash-value v)
             :for pair := (funcall key (cons k v))
             :when (funcall test item pair)
               :do (incf (the ind res))
             :finally (return res))))))


(define-polymorphic-function count-if (predicate container &key from-end start end key) :overwrite t)


(defpolymorph (count-if :inline t) ((predicate function) (container array) &key ((from-end boolean) nil)
                                    ((start ind) 0) ((end (maybe ind)) nil)
                                    ((key function) #'identity))
    ind
  (let ((res 0))
    (declare (type ind res))
    (traverse ((cand container :start start :end (or end (size container)) :from-end from-end))
      (let ((cand (funcall key cand)))
        (when (funcall predicate cand)
          (incf (the ind res)))))
    res))




(defpolymorph (count-if :inline t) ((predicate function) (container hash-table) &key ((from-end null) nil)
                                    ((start (eql 0)) 0) ((end null) nil)
                                    ((key function) #'identity))
    ind
  (declare (ignorable start end from-end))
  (let ((res 0))
    (declare (type ind res))
    (cond
      ((or (eq key #'car) (eq key #'first))
       (loop :for k :being :the :hash-key :in container
             :when (funcall predicate k)
               :do (incf (the ind res))
             :finally (return res)))
      ((or (eq key #'cdr) (eq key #'rest))
       (loop :for v :being :the :hash-value :in container
             :when (funcall predicate v)
               :do (incf (the ind res))
             :finally (return res)))
      (t
       (loop :for k :being :the :hash-keys :in container
               :using (hash-value v)
             :for pair := (funcall key (cons k v))
             :when (funcall predicate pair)
               :do (incf (the ind res))
             :finally (return res))))))
