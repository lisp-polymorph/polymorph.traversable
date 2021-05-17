;;;; polymorph.traversable.asd

(asdf:defsystem #:polymorph.traversable
  :description "Describe polymorph.traversable here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:polymorph.maths #:polymorph.access)
  :components ((:module
                "src"
                :serial t
                :components
                ((:file "package")
                 (:file "polymorph.traversable")))))
