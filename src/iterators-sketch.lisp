


(in-package #:polymorph.traversable)

(define-condition iterator-end (condition)
  ())

(defmacro catch-iter-end (form)
  `(handler-case ,form
     (iterator-end (err)
       err)))

(defmacro iter-stop ()
  `(signal 'iterator-end))


(defstruct iter)

(define-polymorphic-function next (iter))

(defstruct (range (:include iter)))

(define-polymorphic-function iter (seq))

(polymorph.macros::%def (iter-vector (:include iter)) (:copy)
  (seq vector (error "Supply a vector"))
  (:mut cur ind))

(defpolymorph (next :inline t) ((v iter-vector)) (values t &optional)
  (if (< (cur v) (length (seq v)))
      (prog1 (aref (seq v) (cur v))
        (incf (cur v)))
      (iter-stop)))

(defpolymorph iter ((v vector)) (values iter-vector &optional)
  (iter-vector :seq v))


(polymorph.macros::%def (iter-list (:include iter)) (:copy)
  (:mut seq list (error "Supply a list")))

(defpolymorph (next :inline t) ((l iter-list)) (values t &optional)
  (if (null (seq l))
      (iter-stop)
      (prog1 (first (seq l))
        (pop (seq l)))))

(defpolymorph iter ((l list)) (values iter-list &optional)
  (iter-list :seq l))




(polymorph.macros::%def (map-iter (:include iter)) ()
  (fn function (error "Supply a function"))
  (it iter (error "Supply an iterator")))

(defpolymorph (next :inline t) ((m map-iter)) t
  (multiple-value-call (fn m) (next (it m))))

(defpolymorph map-iter-lazy ((it iter) (fn function)) (values map-iter &optional)
  (map-iter :it it :fn fn))

(polymorph.macros::%def (zip-iter (:include iter)) ()
  (fit iter (error "Supply first iterator"))
  (sit iter (error "Supply second iterator")))

(defpolymorph (next :inline t) ((z zip-iter)) t
  (multiple-value-call #'values (next (fit z)) (next (sit z))))

(polymorph.macros::%def (range-inf (:include range)) (:copy)
  (:mut from integer 0)
  (stp integer 1))

(defpolymorph (next :inline t) ((r range-inf)) (values integer &optional)
  (let ((from (from r))
        (stp (stp r)))
    (prog1 from
      (incf (from r) stp))))


(polymorph.macros::%def (range-lim (:include range)) (:copy)
  (:mut from integer 0)
  (lim integer 0)
  (stp integer 1))

(defpolymorph (next :inline t) ((r range-lim)) (values integer &optional)
  (let ((from (from r))
        (stp (stp r))
        (lim (lim r)))
    (if (< from lim)
        (prog1 from
          (incf (from r) stp))
        (iter-stop))))

(polymorph.macros::%def (range-char-lim (:include range)) (:copy)
  (:mut from integer 0)
  (lim integer 0))


(defpolymorph (next :inline t) ((r range-char-lim)) (values character &optional)
  (let ((from (from r))
        (lim (lim r)))
    (if (< from lim)
        (prog1 (code-char from)
          (incf (from r)))
        (iter-stop))))


(defpolymorph range ((from integer) &key ((to (or null integer)) nil) ((stp integer) 1))
    (values range &optional)
  (if to
      (range-lim :from from :lim to :stp stp)
      (range-inf :from from :stp stp)))

(defpolymorph range ((from character) &key ((to character) #\z) ((stp null) nil))
    (values range-char-lim &optional)
  (assert (not stp))
  (range-char-lim :from (char-code from) :lim (char-code to)))

#||
(defmacro rangef (&rest args)
  (assert (<= 2 (length args) 3))
  (if (= 2 (length args))
      (destructuring-bind (a b) args
        (assert (eql b '--))
        `(range ,a))))
||#

(defpolymorph (find-it :inline t) ((it iter) (fn function)) t
  (loop (let ((ls (multiple-value-list (next it))))
          (when (apply fn ls)
            (return (values-list ls))))))


(polymorph.macros::%def (taker (:include iter)) ()
  (it iter (error "Supply an iterator"))
  (:mut times ind))


(defpolymorph (take :inline t) ((it iter) (times ind)) (values taker &optional)
  (taker :it it :times times))


(defpolymorph (next :inline t) ((taker taker)) t
  (if (= 0 (times taker))
      (iter-stop)
      (progn (decf (times taker))
             (next (it taker)))))


(polymorph.macros::%def (taker-while (:include iter)) ()
  (it iter (error "Supply an iterator"))
  (pred function (error "Supply a function")))

(defpolymorph (take-while :inline t) ((it iter) (pred function)) (values taker-while &optional)
  (taker-while :it it :pred pred))


(defpolymorph (next :inline t) ((taker taker-while)) t
  (let ((ls (multiple-value-list (next (it taker)))))
    (if (apply (pred taker) ls)
        (values-list ls)
        (iter-stop))))



(polymorph.macros::%def (skipper (:include iter)) ()
  (it iter (error "Supply an iterator"))
  (:mut times ind))


(defpolymorph (skip :inline t) ((it iter) (times ind)) (values skipper &optional)
  (skipper :it it :times times))


(defpolymorph (next :inline t) ((skipper skipper)) t
  (loop (if (= 0 (times skipper))
            (return (next (it skipper)))
            (progn (next (it skipper))
                   (decf (times skipper))))))


(polymorph.macros::%def (skipper-while (:include iter)) ()
  (it iter (error "Supply an iterator"))
  (pred function (error "Supply a function"))
  (:mut done boolean nil))

(defpolymorph (skip-while :inline t) ((it iter) (pred function)) (values skipper-while &optional)
  (skipper-while :it it :pred pred))


(defpolymorph (next :inline t) ((skipper skipper-while)) t
  (if (done skipper)
      (next (it skipper))
      (loop (let ((ls (multiple-value-list (next (it skipper)))))
              (unless (apply (pred skipper) ls)
                (setf (done skipper) t)
                (return (values-list ls)))))))











(polymorph.macros::%def (filter-pred (:include iter)) ()
  (it iter (error "Supply an iterator"))
  (pred function (error "Supply a function")))

(defpolymorph (filter :inline t) ((it iter) (pred function)) (values filter-pred &optional)
  (filter-pred :it it :pred pred))


(defpolymorph (next :inline t) ((filter filter-pred)) t
  (loop (let ((ls (multiple-value-list (next (it filter)))))
          (when (apply (pred filter) ls)
            (return (values-list ls))))))




(defpolymorph (for-each :inline :maybe) ((it iter) (fn function)) (values null &optional)
  (handler-case (loop (multiple-value-call fn (next it)))
    (iterator-end (c)
      (declare (ignore c))
      nil)))

(polymorph.macros::%def (chain-it (:include iter)) ()
  (fit iter (error "Supply an iterator"))
  (sit iter (error "Supply an iterator"))
  (:mut flag boolean nil))

(defpolymorph (next :inline t) ((iter chain-it)) t
  (if (flag iter)
      (next (sit iter))
      (handler-case (next (fit iter))
        (iterator-end (c)
          (declare (ignore c))
          (setf (flag iter) t)
          (next (sit iter))))))

(defpolymorph chain ((fit iter) (sit iter)) (values iter &optional)
  (chain-it :fit fit :sit sit))


(defpolymorph (collect :inline t) ((it iter) (type (eql vector)) &optional ((combine function) #'identity))
    (values (and vector (not simple-array)) &optional)
  (declare (ignorable type))
  (let ((res (make-array 0 :adjustable t :fill-pointer 0)))
    (handler-case (loop (vector-push-extend (multiple-value-call combine (next it)) res))
      (iterator-end ()
        res))))

(defpolymorph (collect :inline t) ((it iter) (type (eql list)) &optional ((combine function) #'identity))
    (values list &optional)
  (declare (ignorable type))
  (let* ((ls (cons nil nil))
         (next ls))
    (handler-case (loop (setf (cdr next) (list (multiple-value-call combine (next it)))
                              next (cdr next)))
      (iterator-end ()
        (cdr ls)))))



(defpolymorph fold ((it iter) (init t) (fn function)) (values t &optional)
  (let ((res init))
    (handler-case (loop (setf res (multiple-value-call fn res (next it))))
      (iterator-end (c)
        (declare (ignore c))
        res))))


(polymorph.macros::%def (stream-iter-lines (:include iter)) ()
  (input stream (error "Supply a stream")))

(defpolymorph next ((it stream-iter-lines)) (values string &optional)
  (let ((line (read-line (input it) nil nil)))
    (or line
        (iter-stop))))


(defpolymorph lines ((in stream)) (values stream-iter-lines &optional)
  (stream-iter-lines :input in))

(polymorph.macros::%def (stream-iter-chars (:include iter)) ()
  (input stream (error "Supply a stream")))

(defpolymorph next ((it stream-iter-chars)) (values string &optional)
  (let ((char (read-char (input it) nil nil)))
    (or char
        (iter-stop))))


(defpolymorph chars ((in stream)) (values stream-iter-chars &optional)
  (stream-iter-chars :input in))


(polymorph.macros::%def (enum (:include iter)) ()
  (:mut i ind)
  (it iter (error "Supply an iterator")))


(defpolymorph (next :inline t) ((enum enum)) t
  (let ((i (i enum)))
    (incf (i enum))
    (multiple-value-call #'values i (next (it enum)))))

(defpolymorph enumerate ((it iter)) (values enum &optional)
  (enum :it it))
