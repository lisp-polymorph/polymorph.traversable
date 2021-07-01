;;;; package.lisp

(defpackage #:polymorph.traversable
  (:use #:cl #:polymorphic-functions
        #:alexandria
        #:tcr.parse-declarations-1.0
        #:introspect-ctype
        #:polymorph.utility)

  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop)
                    (:ie :introspect-environment))

  (:import-from #:polymorph.macros
                #:setf*)

  (:shadow #:find #:find-if
           #:position #:position-if
           #:count #:count-if
           #:map #:reduce
           #:remove #:remove-if

           #:fill #:replace
           #:mismatch)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:shadowing-import-from #:polymorph.access #:at #:row-major-at #:size #:emptyp)
  (:export
   #:traverse
   #:define-traverse-expander
   #:make-iterator #:element #:advance #:morep
   #:find #:find-if
   #:position #:position-if
   #:count #:count-if

   #:fill
   #:replace
   #:mismatch))
