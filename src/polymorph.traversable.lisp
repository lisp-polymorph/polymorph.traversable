;;;; polymorph.traversable.lisp

(in-package #:polymorph.traversable)

;; Find
(define-polymorphic-function find (item container &key from-end start end
                                        key test) :overwrite t)

(defpolymorph (find :inline t) ((item t) (container t) &key ((from-end boolean) nil)
                                  ((start ind) 0) ((end ind) (size container))
                                  ((key function) #'identity) ((test function) #'=))
    (values t boolean &optional)
  (traverse ((cand container :start start :end end :from-end from-end))
    (let ((cand (funcall key cand)))
      (when (funcall test cand item)
        (return-from find (values cand t)))))
  (values nil nil))

(defpolymorph (find :inline t) ((item t) (container hash-table) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end ind) (size container))
                                ((key (or null (member :key :value) function)))
                                ((test function) #'=))
    (values t boolean &optional)
  (declare (ignorable start end from-end))
  (typecase key
    (null (loop :for k :being :the :hash-keys :in container
                  :using (hash-value v)
                :for pair := (cons k v)
                :when (funcall test item pair)
                  :do (return (values pair t))
                :finally (return (values nil nil))))
    ((eql :key) (loop :for k :being :the :hash-key :in container
                      :when (funcall test item k)
                        :do (return (values k t))
                      :finally (return (values nil nil)))) ;;;TODO sometimes gethash is good
    ((eql :value) (loop :for v :being :the :hash-value :in container
                        :when (funcall test item v)
                          :do (return (values v t))
                        :finally (return (values nil nil))))
    (otherwise
     (loop :for k :being :the :hash-keys :in container
             :using (hash-value v)
           :for pair := (funcall key (cons k v))
           :when (funcall test item pair)
             :do (return (values pair t))
           :finally (return (values nil nil))))))



(define-polymorphic-function find-if (predicate container &key from-end start end key) :overwrite t)


(defpolymorph (find-if :inline t) ((predicate function) (container array) &key ((from-end boolean) nil)
                                   ((start ind) 0) ((end ind) (size container))
                                   ((key function) #'identity))
    (values t boolean &optional)
   (traverse ((cand container :start start :end end :from-end from-end))
    (let ((cand (funcall key cand)))
      (when (funcall predicate cand)
        (return-from find-if (values cand t)))))
   (values nil nil))




(defpolymorph (find-if :inline t) ((predicate function) (container hash-table) &key ((from-end boolean) nil)
                                   ((start ind) 0) ((end (maybe ind)) nil)
                                   ((key (or null (member :key :value) function)) nil))
    (values t boolean &optional)
  (if (and (= start 0) (not (or end from-end)))
      (typecase key
        (null (loop :for k :being :the :hash-keys :in container
                      :using (hash-value v)
                    :for pair := (cons k v)
                    :when (funcall predicate pair)
                      :do (return (values pair t))
                    :finally (return (values nil nil))))
        ((eql :key) (loop :for k :being :the :hash-key :in container
                          :when (funcall predicate k)
                            :do (return (values k t))
                          :finally (return (values nil nil)))) ;;;TODO sometimes gethash is good
        ((eql :value) (loop :for v :being :the :hash-value :in container
                            :when (funcall predicate v)
                              :do (return (values v t))
                            :finally (return (values nil nil))))
        (otherwise
         (loop :for k :being :the :hash-keys :in container
                 :using (hash-value v)
               :for pair := (funcall key (cons k v))
               :when (funcall predicate pair)
                 :do (return (values pair t))
               :finally (return (values nil nil)))))

      (error 'simple-error :format-control
             "Start, end or from-end arguments cannot be provided for hash-tables")))


