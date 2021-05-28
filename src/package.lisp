;;;; package.lisp

(defpackage #:polymorph.traversable
  (:use #:cl #:polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop)
                    (:ie :introspect-environment))
  (:shadow #:find #:find-if
           #:position #:position-if
           #:count #:count-if)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:shadowing-import-from #:polymorph.access #:at #:row-major-at #:size)
  (:export #:find #:find-if
         #:position #:position-if
         #:count #:count-if))
