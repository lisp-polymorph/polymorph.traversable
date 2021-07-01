;;;; polymorph.traversable.asd

(asdf:defsystem #:polymorph.traversable
  :description "Describe polymorph.traversable here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:introspect-ctype
               #:polymorph.macros
               #:polymorph.maths
               #:polymorph.access
               #:polymorph.callable

               #:parse-declarations-1.0)

  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "traverse")
                 (:file "util")
                 (:file "expanders")
                 (:file "iterators")
                 (:file "polymorph.traversable"))))

  :in-order-to ((asdf:test-op (asdf:test-op :polymorph.traversable/test))))

(asdf:defsystem #:polymorph.traversable/test
  :description "Unit tests for polymorph.traversable"
  :license "MIT"
  :serial t
  :depends-on (#:polymorph.traversable #:fiveam)
  :components ((:module
                "test"
                :serial t
                :components
                ((:file "test")
                 (:file "traverse"))))

  :perform (test-op (o s)
                    (uiop:symbol-call '#:polymorph.traversable/test '#:test-polymorph.traversable)))
