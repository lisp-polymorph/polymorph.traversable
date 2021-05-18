;;;; polymorph.traversable.lisp

(in-package #:polymorph.traversable)

;; Find
(define-polymorphic-function find (item container &key from-end start end
                                    key test) :overwrite t)



(defpolymorph (find :inline t) ((item t) (container array) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end ind) (size container))
                                ((key (maybe function)) nil) ((test (maybe function)) nil))
    t
  (if (= 1 (array-rank container))
      (if from-end
          (if key
              (if test
                  (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (funcall key (at container i))
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (funcall key (at container i))
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))
              (if test
                  (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (at container i)
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (at container i)
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))))
          (if key
              (if test
                  (loop :for i :of-type ind :from start :below end
                        :for elt := (funcall key (at container i))
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from start :below end
                        :for elt := (funcall key (at container i))
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))
              (if test
                  (loop :for i :of-type ind :from start :below end
                        :for elt := (at container i)
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from start :below end
                        :for elt := (at container i)
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))))

      (if (and (= start 0) (= end (size container)) (not from-end))
          (if key
              (if test
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (funcall key (row-major-at container i))
                        :when (funcall test cand item)
                          :do (return (values cand t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (funcall key (row-major-at container i))
                        :when (= cand item)
                          :do (return (values cand t))
                        :finally (return (values nil nil))))
              (if test
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (row-major-at container i)
                        :when (funcall test cand item)
                          :do (return (values cand t))
                        :finally (return (values nil nil)))
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (row-major-at container i)
                        :when (= cand item)
                          :do (return (values cand t))
                        :finally (return (values nil nil)))))
          (error 'simple-error :format-control
                 "Start, end or from-end arguments cannot be provided for multidimensional arrays"))))







(defpolymorph-compiler-macro find (t array &key (:from-end boolean) (:start ind) (:end (maybe ind))
                                     (:key (maybe function)) (:test (maybe function)))
    (&whole form item container &key from-end start end key test &environment env)
  (with-array-info (elt-type _) container env
    (declare (ignorable _))
    `(the (maybe ,elt-type) ,form)))



(defpolymorph (find :inline t) ((item t) (container list) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end (maybe ind)) nil)
                                ((key (maybe function)) nil) ((test (maybe function)) nil))
    t
  (if from-end
      (find item (reverse container) :start start :end end :key key :test test)
      (if end
          (if key
              (if test
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :repeat (- end start)
                        :for elt := (funcall key (first cursor))
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :repeat (- end start)
                        :for elt := (funcall key (first cursor))
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))
              (if test
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :repeat (- end start)
                        :for elt := (first cursor)
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :repeat (- end start)
                        :for elt := (first cursor)
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))))
          (if key
              (if test
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :for elt := (funcall key (first cursor))
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :for elt := (funcall key (first cursor))
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))
              (if test
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :for elt := (first cursor)
                        :when (funcall test item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))
                  (loop :for cursor := (nthcdr start container) :then (rest cursor)
                        :while cursor
                        :for elt := (first cursor)
                        :when (= item elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))))))



(defpolymorph (find :inline t) ((item t) (container hash-table) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end (maybe ind)) nil)
                                ((key (or null (member :key :value) function)) nil) ((test (maybe function)) nil))
    t
  (if (and (= start 0) (not (or end from-end)))
      (if test
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
                   :for pair := (cons k v)
                   :when (funcall test item (funcall key pair))
                     :do (return (values pair t))
                   :finally (return (values nil nil)))))
          (typecase key
            (null (loop :for k :being :the :hash-keys :in container
                          :using (hash-value v)
                        :for pair := (cons k v)
                        :when (= item pair)
                          :do (return (values pair t))
                        :finally (return (values nil nil))))
            ((eql :key) (loop :for k :being :the :hash-key :in container
                              :when (= item k)
                                :do (return (values k t))
                              :finally (return (values nil nil))))
            ((eql :value) (loop :for v :being :the :hash-value :in container
                                :when (= item v)
                                  :do (return (values v t))
                                :finally (return (values nil nil))))
            (otherwise
             (loop :for k :being :the :hash-keys :in container
                     :using (hash-value v)
                   :for pair := (cons k v)
                   :when (= item (funcall key pair))
                     :do (return (values pair t))
                   :finally (return (values nil nil))))))
      (error 'simple-error :format-control
             "Start, end or from-end arguments cannot be provided for hash-tables")))
