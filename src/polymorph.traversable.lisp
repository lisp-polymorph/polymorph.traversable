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

(define-polymorphic-function find-if (predicate container &key from-end start end key) :overwrite t)
;; TODO It seems passing lambdas as predicates leads to them NOT being inlined
;; as opposed to what happens in cl:find-if. Same thing will happen for other predicate taking
;; functions. Possible solutions I can see here:
;; 1. compilers (sbcl in particular) just get better and learn to inline stuff from loops. Cool.
;; 2. We get better and do a compiler macro that specifically works with predicate
;; if it starts with (lambda ..) or (function (lambda ...)). This seems brittle to me
;; therefore it feels more like a hack? It will wrok in many cases though.
;; 3. We maybe do something compiler specific instead of starightforward loop? That seems
;; even worse

(defpolymorph (find-if :inline t) ((predicate function) (container array) &key ((from-end boolean) nil)
                                   ((start ind) 0) ((end ind) (size container))
                                   ((key (maybe function)) nil))
    (values t boolean &optional)
  (if (= 1 (array-rank container))
      (if from-end
          (if key
              (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (funcall key (at container i))
                        :when (funcall predicate elt)
                          :do (return (values elt t))
                    :finally (return (values nil nil)))
              (loop :for i :of-type ind :from (1- end) :downto start
                        :for elt := (at container i)
                        :when (funcall predicate elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil))))
          (if key
              (loop :for i :of-type ind :from start :below end
                        :for elt := (funcall key (at container i))
                        :when (funcall predicate elt)
                          :do (return (values elt t))
                    :finally (return (values nil nil)))
              (loop :for i :of-type ind :from start :below end
                        :for elt := (at container i)
                        :when (funcall predicate elt)
                          :do (return (values elt t))
                        :finally (return (values nil nil)))))
      (if (and (= start 0) (= end (size container)) (not from-end))
          (if key
              (loop :for i :of-type ind :from 0 :below (array-total-size container)
                    :for cand := (funcall key (row-major-at container i))
                    :when (funcall predicate cand)
                      :do (return (values cand t))
                    :finally (return (values nil nil)))
              (loop :for i :of-type ind :from 0 :below (array-total-size container)
                    :for cand := (row-major-at container i)
                    :when (funcall predicate cand)
                      :do (return (values cand t))
                    :finally (return (values nil nil))))
          (error 'simple-error :format-control
                 "Start, end or from-end arguments cannot be provided for multidimensional arrays"))))


(defpolymorph-compiler-macro find-if  (function array &key (:from-end boolean)
                                                (:start ind) (:end ind)
                                                (:key (maybe function)))
    (predicate container &key (from-end nil)
               (start 0) (end `(size ,container))
               (key nil))
  (let ((i (gensym "I"))
        (elt (gensym "ELT")))
    `(if (= 1 (array-rank ,container))
         (if ,from-end
             (if ,key
                 (loop :for ,i :of-type ind :from (1- ,end) :downto ,start
                       :for ,elt := (funcall ,key (at ,container ,i))
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil)))
                 (loop :for ,i :of-type ind :from (1- ,end) :downto ,start
                       :for ,elt := (at ,container ,i)
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil))))
             (if ,key
                 (loop :for ,i :of-type ind :from ,start :below ,end
                       :for ,elt := (funcall ,key (at ,container ,i))
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil)))
                 (loop :for ,i :of-type ind :from ,start :below ,end
                       :for ,elt := (at ,container ,i)
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil)))))
         (if ,(and (= start 0) (= end `(size ,container)) (not from-end))
             (if ,key
                 (loop :for ,i :of-type ind :from 0 :below (array-total-size ,container)
                       :for ,elt := (funcall ,key (row-major-at ,container ,i))
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil)))
                 (loop :for ,i :of-type ind :from 0 :below (array-total-size ,container)
                       :for ,elt := (row-major-at ,container ,i)
                       :when (funcall ,predicate ,elt)
                         :do (return (values ,elt t))
                       :finally (return (values nil nil))))
             (error 'simple-error :format-control
                    "Start, end or from-end arguments cannot be provided for multidimensional arrays")))))






(defpolymorph (find-if :inline t) ((predicate function) (container list) &key ((from-end boolean) nil)
                                   ((start ind) 0) ((end (maybe ind)) nil)
                                   ((key (maybe function)) nil))
    (values t boolean &optional)
  (if from-end
      (find predicate (reverse container) :start start :end end :key key)
      (if end
          (if key
              (loop :for cursor := (nthcdr start container) :then (rest cursor)
                    :while cursor
                    :repeat (- end start)
                    :for elt := (funcall key (first cursor))
                    :when (funcall predicate elt)
                      :do (return (values elt t))
                    :finally (return (values nil nil)))
              (loop :for cursor := (nthcdr start container) :then (rest cursor)
                    :while cursor
                    :repeat (- end start)
                    :for elt := (first cursor)
                    :when (funcall predicate elt)
                      :do (return (values elt t))
                    :finally (return (values nil nil))))

          (if key
              (loop :for cursor := (nthcdr start container) :then (rest cursor)
                    :while cursor
                    :for elt := (funcall key (first cursor))
                    :when (funcall predicate elt)
                      :do (return (values elt t))
                    :finally (return (values nil nil)))
              (loop :for cursor := (nthcdr start container) :then (rest cursor)
                    :while cursor
                    :for elt := (first cursor)
                    :when (funcall predicate elt)
                      :do (return (values elt t))
                    :finally (return (values nil nil)))))))


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


