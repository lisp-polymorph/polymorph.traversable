;;; expanders.lisp
;;;
;;; Traversal expanders for standard sequence/container types

(in-package #:polymorph.traversable)

(define-traverse-expander list (var sequence)
  (values
   nil
   nil
   nil
   `(for ,var in ,sequence)))

(define-traverse-expander vector (var sequence)
  (values
   nil
   nil
   nil
   `(for ,var across ,sequence)))

(define-traverse-expander hash-table (var form)
  (with-gensyms (table)
    (let* ((key (if (listp var) (first var) (gensym "KEY")))
	   (value (if (listp var) (second var) (gensym "VALUE")))
	   (whole (unless (listp var) var)))

      (values
       `((,table ,form))

       nil
       nil

       (append
        (when key
          `(for ,key being the hash-key of ,table))

        (when value
          `(for ,value being the hash-value of ,table))

        (when whole
          `(for ,whole = (list ,key ,value))))))))
