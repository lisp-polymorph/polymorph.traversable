;;; expanders.lisp
;;;
;;; Traversal expanders for standard sequence/container types

(in-package #:polymorph.traversable)

;;; Lists

(define-traverse-expander list (type form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-gensyms (list index v-start v-end v-from-end with-value with-place place)
      (values
       `((,v-start ,start :constant t)
         (,v-end ,end :constant t)
         (,v-from-end ,from-end :constant t)

         (,list
          (if ,v-from-end
              (sublist ,form ,v-start ,v-end ,v-from-end)
              (nthcdr ,v-start ,form))))

       (let ((value-macro
              (iter-macro (tag list place)
                  (pattern &body body)

                (with-destructure-pattern (var pattern)
                    (body decl body)

                  `(progn
                     (unless ,list
                       (go ,tag))

                     (let ((,var (,place ,list)))
                       ,@decl

                       (setf ,list (cdr ,list))
                       ,@body)))))

             (place-macro
              (iter-macro (tag list place)
                  (name more? &body body)

                (let ((body
                       `(prog1 (progn ,@body)
                          (setf ,list (cdr ,list)))))

                  `(symbol-macrolet ((,name (,place ,list)))
                     ,(if more?
                          `(let ((,more? ,list))
                             ,body)

                          `(progn
                             (unless ,list
                               (go ,tag))

                             ,body)))))))

         (with-gensyms (list-value list-place)
           `((cond
               (,v-from-end
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(caar ,thing)))

                  ,@body))

               (,v-end
                (let ((,index ,v-start))
                  (macrolet ((,list-value . ,value-macro)
                             (,list-place . ,place-macro)
                             (,place (thing)
                               `(car ,thing))

                             (,with-value .
                               ,(iter-macro (tag index v-end list-value)
                                    (pattern &body body)

                                  (split-declarations-forms (decl forms) body
                                    `(,list-value
                                      ,pattern

                                      ,@decl
                                      (unless (< ,index ,v-end)
                                        (go ,tag))

                                      (incf ,index)
                                      ,@forms))))

                             (,with-place .
                               ,(iter-macro (tag index v-end list-place)
                                    (name more? &body body)

                                  `(,list-place
                                    ,name
                                    ,more?

                                    ,@(if more?
                                          `((let ((,more? (and ,more? (< ,index ,v-end))))
                                              (incf ,index)
                                              ,@body))

                                          `((unless (< ,index ,v-end)
                                              (go ,tag))

                                            (incf ,index)
                                            ,@body))))))
                    ,@body)))

               (t
                (macrolet ((,with-value . ,value-macro)
                           (,with-place . ,place-macro)
                           (,place (thing)
                             `(car ,thing)))

                  ,@body))))))


       (iter-macro (with-value)
           (pattern &body body)
         `(,with-value ,pattern ,@body))

       (iter-macro (with-place)
           (name more? &body body)

         `(,with-place ,name ,more? ,@body))))))

;;; Vectors

(define-traverse-expander vector (type form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (with-type-info (type (typename &optional (elt t)) env) form
      (with-gensyms (vec index end-index v-from-end v-start v-end)
        (values
         `((,v-from-end ,from-end :constant t)
           (,v-start ,start :constant t)
           (,v-end ,end :constant t)

           (,vec (the ,type ,form))
           (,end-index (if ,v-end ,v-end (cl:length ,vec)) :constant t)

           (,index
            (if ,v-from-end (cl:1- ,end-index) ,v-start)))

         body

         (iter-macro (tag elt v-from-end v-start vec end-index index)
             (pattern &body body)

           (with-destructure-pattern (var pattern)
               (body decl body)

             `(progn
                (unless (if ,v-from-end
                            (>= ,index ,v-start)
                            (< ,index ,end-index))
                  (go ,tag))

                (let ((,var (aref ,vec ,index)))
                  (declare (type ,elt ,var))
                  ,@decl

                  (if ,v-from-end
                      (decf ,index)
                      (incf ,index))
                  ,@body))))

         (iter-macro (tag v-from-end v-start vec end-index index)
             (name more? &body body)

           (let ((test `(if ,v-from-end
                            (>= ,index ,v-start)
                            (< ,index ,end-index)))
                 (body `(prog1 (progn ,@body)
                          (if ,v-from-end
                              (decf ,index)
                              (incf ,index)))))

             `(symbol-macrolet ((,name (aref ,vec ,index)))
                ,(if more?
                     `(let ((,more? ,test))
                        ,body)

                     `(progn
                        (unless ,test
                          (go ,tag))

                        ,body))))))))))

;;; Hash-Tables

(define-traverse-expander hash-table (type form args tag body env)
  (destructuring-bind (&key from-end (start 0) end) args
    (declare (ignore from-end))

    (with-gensyms (table next more? size index)
      (flet ((make-iter-value (test inc)
               (iter-macro (more? next test tag inc)
                   (pattern &body body)

                 (with-destructure-entry (key value pattern)
                     (forms decl body)

                   `(multiple-value-bind (,more? ,key ,value)
                        (,next)
                      (declare (ignorable ,key ,value))
                      ,@decl

                      (unless ,test
                        (go ,tag))

                      ,@inc
                      ,@forms))))

             (make-iter-place (test inc)
               (iter-macro (more? next test tag inc table)
                   (name more?-var &body body)

                 (let ((more? (or more?-var more?)))
                   (with-gensyms (key)
                     `(multiple-value-bind (,more? ,key)
                          (,next)

                        (symbol-macrolet ((,name (map-place ,key ,table)))
                          ,(if more?-var
                               body

                               `(progn
                                  (unless ,test
                                    (go ,tag))

                                  ,@inc
                                  ,@body)))))))))

        (with-constant-values (start end) env
          ((start end)
           (let* ((counted? (or (> start 0) end))

                  (test (if counted?
                            `(and ,more? (cl:< ,index ,size))
                            more?))

                  (inc (when counted?
                         `((cl:incf ,index)))))

             (values
              `((,table ,form)
                ,@(when counted?
                    `((,index ,start)
                      (,size ,(or end `(hash-table-count ,table))))))

              `((with-hash-table-iterator (,next ,table)
                  ,@body))

              (make-iter-value test inc)
              (make-iter-place test inc))))

          (nil
           (values
            `((,table ,form)
              (,index ,start)
              (,size (or ,end (hash-table-count ,table))))

            `((with-hash-table-iterator (,next ,table)
                ,@body))

            (make-iter-value `(and ,more? (cl:< ,index ,size))
                             `((cl:incf ,index)))

            (make-iter-place `(and ,more? (cl:< ,index ,size))
                             `((cl:incf ,index))))))))))

;;; Default

(define-traverse-expander t (type form args tag body)
  (with-gensyms (it)
    (values
     `((,it (make-iterator ,form ,@args)))

     body

     (iter-macro (it tag)
         (pattern &body body)

       (with-destructure-pattern (var pattern)
           (body decl body)

         `(progn
            (unless (morep ,it)
              (go ,tag))

            (let ((,var (element ,it)))
              ,@decl

              (advance ,it)
              ,@body))))

     (iter-macro (it tag)
         (name more? &body body)

       (let ((body `(prog1 (progn ,@body)
                      (advance ,it))))

         `(symbol-macrolet ((,name (element ,it)))
            ,(if more?
                 `(let ((,more? (morep ,it)))
                    ,body)

                 `(progn
                    (unless (morep ,it)
                      (go ,tag))

                    ,body))))))))
