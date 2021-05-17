;;;; polymorph.traversable.lisp

(in-package #:polymorph.traversable)

(deftype maybe (typename) `(or null ,typename))


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
