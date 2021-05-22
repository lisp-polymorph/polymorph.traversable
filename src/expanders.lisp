;;; expanders.lisp
;;;
;;; Traversal expanders for standard sequence/container types

(in-package #:polymorph.traversable)

(define-traverse-expander list (var sequence)
  (values
   `(for ,var in ,sequence)
   nil))

(define-traverse-expander vector (var sequence)
  (values
   `(for ,var across ,sequence)
   nil))

(define-traverse-expander hash-table (var sequence)
  (with-gensyms (seq)
    (let* ((key (if (listp var) (first var) (gensym "KEY")))
	   (value (if (listp var) (second var) (gensym "VALUE")))
	   (whole (unless (listp var) var)))

      (append
       `(with ,seq = ,sequence)
       `(for ,key being the hash-key of ,seq)

       (when value
	 `(for ,value being the hash-value of ,seq))

       (when whole
	 `(for ,whole = (cons ,key ,value)))))))
