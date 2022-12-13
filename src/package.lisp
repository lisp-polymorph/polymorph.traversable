;;;; package.lisp

(defpackage #:polymorph.traversable
  (:use #:cl #:polymorphic-functions #:alexandria #:introspect-ctype #:polymorph.macros)
  (:local-nicknames (:mop :closer-mop)
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
   #:count #:count-if
   #:remove
   #:iter
   #:range
   #:iterator-end
   #:catch-iter-end
   #:iter-stop
   #:next
   #:iter-vector
   #:iter-list
   #:for-each
   #:chain-it
   #:chain
   #:lines
   #:chars
   #:range
   #:fold
   #:enumerate
   #:map-iter-lazy
   #:zip-iter))
