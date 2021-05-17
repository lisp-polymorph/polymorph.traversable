;;;; package.lisp

(defpackage #:polymorph.traversable
  (:use #:cl #:adhoc-polymorphic-functions #:alexandria #:polymorph.utility)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))
  (:shadow #:find #:find-if
           #:position #:position-if
           #:count #:count-if)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:shadowing-import-from #:polymorph.access #:at #:row-major-at)
  (:export #:find #:find-if
         #:position #:position-if
         #:count #:count-if))
