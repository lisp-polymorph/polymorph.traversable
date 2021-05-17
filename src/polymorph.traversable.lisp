;;;; polymorph.traversable.lisp

(in-package #:polymorph.traversable)

;; Find
(define-polymorphic-function find (item container &key from-end start end
                                    key test) :overwrite t)



(declaim (inline %inline-finder))
(defun %inline-finder (item container &key from-end start end key test)
  (if from-end
        (if end
            (if key
                (if test
                    (cl:find item container :from-end from-end
                                            :start start :end end
                                            :key key :test test)
                    (cl:find item container :from-end from-end
                                            :start start :end end
                                            :key key))
                (if test
                    (cl:find item container :from-end from-end
                                            :start start :end end
                                            :test test)
                    (cl:find item container :from-end from-end
                                            :start start :end end)))
            (if key
                (if test
                    (cl:find item container :from-end from-end
                                            :start start
                                            :key key :test test)
                    (cl:find item container :from-end from-end
                                            :start start
                                            :key key))
                (if test
                    (cl:find item container :from-end from-end
                                            :start start
                                            :test test)
                    (cl:find item container :from-end from-end
                                            :start start))))
        (if end
            (if key
                (if test
                    (cl:find item container :start start :end end
                                            :key key :test test)
                    (cl:find item container :start start :end end
                                            :key key))
                (if test
                    (cl:find item container :start start :end end
                                            :test test)
                    (cl:find item container :start start :end end)))
            (if key
                (if test
                    (cl:find item container :start start
                                            :key key :test test)
                    (cl:find item container :start start
                                            :key key))
                (if test
                    (cl:find item container :start start
                                            :test test)
                    (cl:find item container :start start))))))





(defpolymorph (find :inline t) ((item t) (container array) &key ((from-end boolean) nil)
                                ((start ind) 0) ((end (maybe ind)) nil)
                                ((key (maybe function)) nil) ((test (maybe function)) nil))
    t
  (if (= 1 (array-rank container))
      (%inline-finder item container :from-end from-end :start start :end end
                                     :key key :test test)
      (if (and (= start 0) (not (or end from-end)))
          (if key
              (if test
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (funcall key (row-major-at container i))
                        :when (funcall test cand item)
                          :do (return cand))
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (funcall key (row-major-at container i))
                        :when (= cand item)
                          :do (return cand)))
              (if test
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (row-major-at container i)
                        :when (funcall test cand item)
                          :do (return cand))
                  (loop :for i :of-type ind :from 0 :below (array-total-size container)
                        :for cand := (row-major-at container i)
                        :when (= cand item)
                          :do (return cand))))
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
  (%inline-finder item container :from-end from-end :start start :end end
                                 :key key :test test))


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
                          :do (return pair)))
            ((eql :key) (values (gethash item container))) ;;;TODO test is ignored here
            ((eql :value) (loop :for v :being :the :hash-value :in container
                             :when (funcall test item v)
                               :do (return v)))
            (otherwise
             (loop :for k :being :the :hash-keys :in container
               :using (hash-value v)
              :for pair := (cons k v)
              :when (funcall test item (funcall key pair))
                :do (return pair))))
          (typecase key
            (null (loop :for k :being :the :hash-keys :in container
                          :using (hash-value v)
                        :for pair := (cons k v)
                        :when (= item pair)
                          :do (return pair)))
            ((eql :key) (values (gethash item container)))
            ((eql :value) (loop :for v :being :the :hash-value :in container
                             :when (= item v)
                               :do (return v)))
            (otherwise
             (loop :for k :being :the :hash-keys :in container
               :using (hash-value v)
              :for pair := (cons k v)
              :when (= item (funcall key pair))
                :do (return pair)))))
      (error 'simple-error :format-control
             "Start, end or from-end arguments cannot be provided for hash-tables")))
