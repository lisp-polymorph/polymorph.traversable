;;; iterators-new.lisp
;;;
;;; Updated implementation of iterator interface for standard Common Lisp
;;; containers

(in-package #:polymorph.traversable)

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
