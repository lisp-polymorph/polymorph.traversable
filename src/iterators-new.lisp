;;; iterators-new.lisp
;;;
;;; Updated implementation of iterator interface for standard Common Lisp
;;; containers

(in-package #:polymorph.traversable)


(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defparameter *unparamterize-name* (make-hash-table :test #'equalp))
  (defparameter *paramterize-name* (make-hash-table :test #'equalp))
  (defparameter *corresponding-ctype* (make-hash-table :test #'equalp)))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defclass c-iterator (ctype::ctype)
     ((%elem-type :initarg :element-type
                 :reader c-iter-element-type)
      (seq-type :initarg :seq-type
                :reader c-iter-seq-type))))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defstruct iterator)
  (defstruct (iterator-list (:include iterator)))
  (defstruct (iterator-vector (:include iterator)))

  (defgeneric gen-iter-defintion (seq-type elem-type name))

  (defmethod gen-iter-defintion ((seq-type (eql 'list)) elem-type name)
    (declare (ignorable seq-type elem-type))
    `(polymorph.macros::%def (,name (:include iterator-list)) (:copy)
       (:mut cur list (error "Empty iterator"))))

  (defmethod gen-iter-defintion ((seq-type (eql 'vector)) elem-type name)
    (declare (ignorable seq-type))
    `(polymorph.macros::%def (,name (:include iterator-vector)) (:copy)
       (seq (vector ,elem-type) (error "Empty iterator"))
       (:mut ind (or (eql -1) ind) 0)))


  (setf (gethash 'iterator *unparamterize-name*) 'iterator
        (gethash 'iterator *paramterize-name*) 'iterator
        (gethash 'iterator *corresponding-ctype*)
        (make-instance 'c-iterator :element-type t
                       :seq-type t)))


(defmacro define-iterator (seq-type elem-type &optional force-p)
  (unless (and (not force-p)
               (gethash (cons 'iterator
                              (cons seq-type
                                    (if (listp elem-type)
                                        elem-type
                                        (list elem-type))))
                        *unparamterize-name*))
    (let* ((iter-type (cons 'iterator
                              (cons seq-type
                                    (if (listp elem-type)
                                        elem-type
                                        (list elem-type)))))
           (iter-code (gentemp "ITERATOR")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,(gen-iter-defintion seq-type elem-type iter-code)
         (setf (gethash ',iter-type *unparamterize-name*) ',iter-code
               (gethash ',iter-code *paramterize-name*) ',iter-type
               (gethash ',iter-code *corresponding-ctype*)
               (make-instance 'c-iterator :element-type ',elem-type
                              :seq-type ',seq-type))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-iterator (seq-type elem-type)
    (eval `(define-iterator ,seq-type ,elem-type))))

(deftype iter (&optional seq-type elem-type)
  (cond ((and (eql seq-type 'cl:*) (eql elem-type 'cl:*))
         'iterator)
        ((and (eql seq-type 'cl:*) (not (eql elem-type 'cl:*))) ;; WARNING -- will NOT work if
         `(or ,@(loop :for (_ seqs elems)                       ;; used before defining all relevant
                        :being :the :hash-keys :in *unparamterize-name* ;; iterators
                          :using (hash-value v)                 ;; FIXME Most likely is not possible
                      :when (alexandria::type= elems elem-type) ;; to fix at all
                        :collect v)))                           ;; Just live with it
        (t (progn
            (unless (gethash (cons 'iterator (cons seq-type (list elem-type)))
                             *unparamterize-name*)
             (ensure-iterator seq-type elem-type))
            (gethash (cons 'iterator (cons seq-type (list elem-type)))
                     *unparamterize-name*)))))

(define-polymorphic-function iter (seq))

(defpolymorph (iter :inline :maybe) ((seq list)) (values iterator-list &optional)
  (let ((longname (cons 'iterator (cons 'list (list t)))))
    (unless (gethash longname *unparamterize-name*)
      (ensure-iterator 'list t))
    (funcall (intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
             :cur seq)))

(defpolymorph-compiler-macro iter (list) (seq)
  (let ((longname (cons 'iterator (cons 'list (list t)))))
    (unless (gethash longname *unparamterize-name*)
      (ensure-iterator 'list t))
    `(the ,(gethash longname *unparamterize-name*)
          (,(intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
           :cur ,seq))))

(defpolymorph (iter :inline :maybe) ((seq vector)) (values iterator-vector &optional)
  (let* ((elem-type (array-element-type seq))
         (longname (cons 'iterator (cons 'vector (list elem-type)))))
    (unless (gethash longname *unparamterize-name*)
      (ensure-iterator 'vector elem-type))
    (funcall (intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
             :seq seq)))

(defpolymorph-compiler-macro iter (vector) (seq &environment env)
  (with-type-info (_ (array-type &optional elem-type __) env) seq
    (declare (ignore __))
    (let ((longname (cons 'iterator (cons 'vector (list elem-type)))))
      (unless (gethash longname *unparamterize-name*)
        (ensure-iterator 'vector elem-type))
      `(the ,(gethash longname *unparamterize-name*)
            (,(intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
             :seq ,seq)))))


(define-polymorphic-function next (iter))

(defpolymorph (next :inline t) ((iter iterator-list)) (values t boolean)
  (let ((cur (cur iter)))
    (if (endp cur)
        (values nil nil)
        (progn
          (setf (cur iter) (rest cur))
          (values (first cur) t)))))

(defpolymorph (next :inline t) ((iter iterator-vector)) (values t boolean)
  (let ((ind (ind iter))
        (seq (seq iter)))
    (if (< ind (length seq))
        (progn
          (setf (ind iter) (the ind (+ ind 1)))
          (values (aref seq ind) t))
        (values nil nil))))

(defpolymorph-compiler-macro next (iterator-vector) (&whole form iter &environment env)
  (let* ((type (%form-type iter env)))
    (if (alexandria::type= type 'iterator-vector)
        form
        (let ((elem-type (c-iter-element-type (gethash type *corresponding-ctype*)))
              (itername (gensym "ITER"))
              (ind (gensym "IND"))
              (seq (gensym "SEQ")))
          `(the (values (or null ,elem-type) boolean)
               (let* ((,itername ,iter)
                      (,ind (ind ,itername))
                      (,seq (seq ,itername)))
                 (if (< ,ind (length ,seq))
                     (progn
                       (setf (ind ,itername) (the ind (+ ,ind 1)))
                       (values (aref ,seq ,ind) t))
                     (values nil nil))))))))

(defpolymorph (prev :inline t) ((iter iterator-vector)) (values t boolean)
  (let ((ind (ind iter))
        (seq (seq iter)))
    (if (< -1 ind)
        (progn
          (setf (ind iter) (the (or (eql -1) ind) (- ind 1)))
          (values (aref seq ind) t))
        (values nil nil))))

(defpolymorph-compiler-macro prev (iterator-vector) (&whole form iter &environment env)
  (let* ((type (%form-type iter env)))
    (if (alexandria::type= type 'iterator-vector)
        form
        (let ((elem-type (c-iter-element-type (gethash type *corresponding-ctype*)))
              (itername (gensym "ITER"))
              (ind (gensym "IND"))
              (seq (gensym "SEQ")))
          `(the (values (or null ,elem-type) boolean)
               (let* ((,itername ,iter)
                      (,ind (ind ,itername))
                      (,seq (seq ,itername)))
                 (if (< ,ind (length ,seq))
                     (progn
                       (setf (ind ,itername) (the (or (eql -1) ind) (- ,ind 1)))
                       (values (aref ,seq ,ind) t))
                     (values nil nil))))))))



#||
(def iter-list (:eq :copy)
  (:mut cur list (error "Empty iterator")))

(def iter-vec (:eq :copy)
  (seq vector (error "Empty iterator"))
  (:mut ind (or ind (eql -1))))

(define-polymorphic-function iter (seq))

(defpolymorph (iter :inline t) ((seq list)) (values iter-list &optional)
  (iter-list :cur seq))

(defpolymorph (iter :inline t) ((seq vector)) (values iter-vec &optional)
  (iter-vec :seq seq))


(define-polymorphic-function val (iter))

(defpolymorph (val :inline t) ((iter iter-list)) (values t boolean)
  (let ((cur (cur iter)))
    (if (endp cur)
        (values nil nil)
        (values (first cur) t))))

(defpolymorph (val :inline t) ((iter iter-vec)) (values t boolean)
  (let ((ind (ind iter))
        (seq (seq iter)))
    (if (< -1 ind (length seq))
        (values (aref seq ind) t)
        (values nil nil))))


(define-polymorphic-function next (iter))

(defpolymorph (next :inline t) ((iter iter-list)) (values t boolean)
  (let ((cur (cur iter)))
    (if (endp cur)
        (values nil nil)
        (progn
          (setf (cur iter) (rest cur))
          (values (first cur) t)))))

(defpolymorph (next :inline t) ((iter iter-vec)) (values t boolean)
  (let ((ind (ind iter))
        (seq (seq iter)))
    (if (< ind (length seq))
        (progn
          (setf (ind iter) (the ind (+ ind 1)))
          (values (aref seq ind) t))
        (values nil nil))))

(define-polymorphic-function prev (iter))

(defpolymorph prev ((iter iter-vec)) (values t boolean)
  (let ((ind (ind iter)))
    (if (< -1 ind)
        (progn
          (decf (ind iter))
          (values (aref (seq iter) ind) t))
        (values nil nil))))

(def imap (:eq :copy)
  (:mut iter t (error "Empty imap"))
  (fn function (error "Empty map")))

(defpolymorph lazymap (iter fn) imap
  (make-imap :iter (copy-structure iter) :fn fn))

(defpolymorph (val :inline t) ((iter imap)) (values t boolean)
  (bind ((val ok (val (iter iter))))
    (if ok
        (values (funcall (fn iter) val) t)
        (values nil nil))))


(defpolymorph (next :inline t) ((iter imap)) (values t boolean)
  (bind ((val ok (next (iter iter))))
    (if ok
        (values (funcall (fn iter) val) t)
        (values nil nil))))

(define-polymorphic-function collect (iter into))

(defpolymorph (collect :inline t) ((iter imap) (into (eql vector))) (values vector &optional)
  (declare (ignorable into))
  (let ((res (make-array 0 :adjustable t :fill-pointer 0)))
    (loop (bind ((val ok (next iter)))
            (if ok
                (vector-push-extend val res)
                (return res))))))

(defpolymorph (collect :inline t) ((iter imap) (into (eql list))) (values list &optional)
  (declare (ignorable into))
  (let ((res)
        (end))
    (loop (bind ((val ok (next iter)))
            (if ok
                (if (null res)
                    (progn (push val res)
                           (setf end res))
                    (progn (setf (cdr end) (cons val nil)
                                 end (cdr end))))
                (return res))))))


(defmacro iterate ((var iterator) &body body)
  (let ((start (gensym "START"))
        (it (gensym "IT"))
        (ok (gensym "OK")))
    `(let ((,it ,iterator))
       (block nil
         (tagbody
            ,start
            (multiple-value-bind (,var ,ok) (next ,it)
              (when ,ok
                (locally ,@body)
                (go ,start))))))))

(defpolymorph enumerate ((iter t)) (values t &optional)
  (let ((c -1))
    (lazymap iter (lambda (x) (cons x (incf c))))))

(define-polymorphic-function ifind (item iter))

(defpolymorph ifind ((item t) (iter iter-vec)) (values iter-vec &optional)
  (loop (bind ((val ok (val iter)))
          (if ok
              (if (= item val)
                  (return)
                  (next iter))
              (return))))
  iter)


(defpolymorph ifold ((iter t) (fn function) (init t)) (values t &optional)
  (loop (bind ((val ok (next iter)))
         (unless ok
            (return init))
         (setf init (funcall fn init val)))))
||#

;; Development area
(eval-when (:compile-toplevel
                     :load-toplevel
                     :execute)

  (defclass map-iterator (c-iterator)
     ((%fn-type :initarg :fn-map-type
                :reader fn-map-type)
      (%it-type :initarg :iter-type
                :reader iter-type)))


  (defun type-conforms-to-lambda-list-p (type lambda-list)
    (let ((req (ctype:lambda-list-required lambda-list))
          (op (ctype:lambda-list-optional lambda-list)))
      (cond ((ctype:lambda-list-rest lambda-list)
             (values t t))
            ((= (+ (length req) (length op)) 1)
             (if req
                 (ctype:subctypep type (first req))
                 (ctype:subctypep type (first op))))
            (t (values nil nil)))))



  (defstruct (lazymap (:include iterator)))

  (defmethod gen-map-definition (fn-type it-type name)
    `(polymorph.macros::%def (,name (:include lazymap)) (:copy)
       (fn ,fn-type (error "Need to provide a function for the map"))
       (it ,it-type (error "Need to provide an iterator for the map")))))

(defmacro define-map (fn-type it-type &optional force-p)
  (unless (and (not force-p)
               (gethash (cons 'lazymap
                              (cons it-type
                                    fn-type))
                        *unparamterize-name*))
    ;; type conformance check
    (let* ((in-type (ctype:specifier-ctype (c-iter-element-type (gethash it-type *corresponding-ctype*))))
           (fn-types (ctype:specifier-ctype fn-type)))
      ;(assert (type-conforms-to-lambda-list-p in-type
      ;                                        (ctype:cfunction-lambda-list fn-types))))
      (let* ((ret-type (if (ctype:cvalues-rest (ctype:cfunction-returns fn-types))
                           t
                           (ctype:unparse
                            (first
                             (or (ctype:cvalues-required
                                  (ctype:cfunction-returns fn-types))
                                 (ctype:cvalues-optional
                                  (ctype:cfunction-returns fn-types)))))))
             (map-type (cons 'lazymap
                             (cons it-type
                                   fn-type)))
             (map-code (gentemp "LAZYMAP")))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           ,(gen-map-definition fn-type it-type map-code)
           (setf (gethash ',map-type *unparamterize-name*) ',map-code
                 (gethash ',map-code *paramterize-name*) ',map-type
                 (gethash ',map-code *corresponding-ctype*)
                 (make-instance 'map-iterator :element-type ',ret-type
                                              :fn-map-type ',fn-type
                                              :iter-type ',it-type)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-map (fn-type it-type)
    (eval `(define-map ,fn-type ,it-type))))

(defpolymorph (lazymap :inline t) ((it iterator) (fn function)) (values lazymap &optional)
  (let* ((longname (cons 'lazymap (cons 'iterator 'function))))
    (unless (gethash longname *unparamterize-name*)
      (ensure-map 'function 'iterator))
    (funcall (intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
             :fn fn :it it)))

(defpolymorph-compiler-macro lazymap (iterator function) (it fn &environment env)
  (let* ((fn-type (%form-type fn env))
         (it-type (%form-type it env))
         (longname (cons 'lazymap (cons it-type fn-type))))
    (unless (gethash longname *unparamterize-name*)
      (ensure-map fn-type it-type))
    `(the ,(gethash longname *unparamterize-name*)
          (,(intern (format nil "MAKE-~s" (gethash longname *unparamterize-name*)))
           :fn ,fn :it ,it))))




(defpolymorph (val :inline t) ((map lazymap)) (values t boolean &optional)
  (bind ((val ok (val (it map))))
    (if ok
        (values (funcall (fn map) val) t)
        (values nil nil))))




(polymorph.macros::%def (%range-up (:include iterator)) (:eq :copy)
  (from real 0)
  (to real 0)
  (by real 1)
  (:mut cur real 0))


(polymorph.macros::%def (%range-down (:include iterator)) (:eq :copy)
  (from real 0)
  (to real 0)
  (by real 1)
  (:mut cur real 0))

(polymorph.macros::%def (%range-inf (:include iterator)) (:eq :copy)
  (from real 0)
  (by real 1)
  (:mut cur real 0))

(deftype range () `(or %range-up %range-down %range-inf))

(defpolymorph range ((from real) &optional (to nil) (step 1)) (values range &optional)
  (if to
      (cond ((>= to from)
             (if (> step 0)
                 (%range-up :from from :to to :by step :cur from)
                 (%range-inf :from from :by step :cur from)))
            (t
             (if (< step 0)
                 (%range-down :from from :to :by step :cur from)
                 (%range-inf :from from :by step :cur from))))
      (%range-inf :from from :by step :cur from)))

(defpolymorph (val :inline t) ((iter %range-up)) (values (or null real) boolean &optional)
  (let ((cur (cur iter))
        (to (to iter)))
    (if (>= cur to)
        (values nil nil)
        (values cur t))))

(defpolymorph (val :inline t) ((iter %range-down)) (values (or null real) boolean &optional)
  (let ((cur (cur iter))
        (to (to iter)))
    (if (<= cur to)
        (values nil nil)
        (values cur t))))


(defpolymorph (val :inline t) ((iter %range-inf)) (values real boolean &optional)
  (values (cur iter) t))

(defpolymorph (next :inline t) ((iter %range-up)) (values (or null real) boolean &optional)
  (let ((cur (cur iter))
        (to (to iter))
        (by (by iter)))
    (if (>= cur (- to by))
        (values nil nil)
        (values (incf (cur iter) by) t))))

(defpolymorph (next :inline t) ((iter %range-down)) (values (or null real) boolean &optional)
  (let ((cur (cur iter))
        (to (to iter))
        (by (by iter)))
    (if (<= cur (- to by))
        (values nil nil)
        (values (incf (cur iter) by) t))))

(defpolymorph (next :inline t) ((iter %range-inf)) (values real boolean &optional)
  (values (incf (cur iter) (by iter)) t))
