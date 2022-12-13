;;;; polymorph.traversable.asd

(asdf:defsystem #:polymorph.traversable
  :description "Traversable sequences for polymorph.stl"
  :license  "MIT"
  :version "0.2"
  :serial t
  :depends-on (#:introspect-ctype
               #:polymorph.maths
               #:polymorph.access
               ;#:polymorph.callable
               #:polymorph.macros)
  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "traverse")
                 (:file "expanders")
                 (:file "iterators")
                 (:file "polymorph.traversable")
                 ;(:file "iterators-new")
                 (:file "iterators-sketch")))))

(asdf:defsystem #:polymorph.traversable/test
  :description "Unit tests for polymorph.traversable"
  :license "MIT"
  :serial t
  :depends-on (#:polymorph.traversable #:fiveam)
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "test"))))

  :perform (test-op (o s)
                    (uiop:symbol-call '#:polymorph.traversable/test '#:test-polymorph.traversable)))
