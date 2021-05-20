;;; Unit tests for polymorph.traversable

(defpackage #:polymorph.traversable/test
  (:use #:cl #:alexandria #:fiveam)
  (:export #:polymorph.traversable
           #:test-polymorph.traversable))

(in-package #:polymorph.traversable/test)

;;; Test suite definition

(def-suite polymorph.traversable
    :description "Master test suite for polymorph.traversable")

(in-suite polymorph.traversable)

(defun test-polymorph.traversable ()
  (run! 'polymorph.traversable))
