;;;; package.lisp

(defpackage #:polymorph.traversable
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop)
                    (:ie :introspect-environment))
  (:shadow #:find #:find-if
           #:position #:position-if
           #:count #:count-if
           #:map #:reduce
           #:remove #:remove-if)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:shadowing-import-from #:polymorph.access #:at #:row-major-at #:size #:emptyp)
  (:export
   #:traverse
   #:define-traverse-expander
   #:make-iterator
   #:find #:find-if
   #:position #:position-if
   #:count #:count-if))
