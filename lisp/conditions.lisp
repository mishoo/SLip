(in-package :ss)

(defun typep (obj type)
  (is-a obj type))

(defclass condition () (datum))
(defclass serious-condition (condition) ())
(defclass error (serious-condition) ())
(defclass warning (condition) ())

(defparameter *handler-clusters* '())
(defparameter *restart-clusters* '())

(defmacro handler-bind (bindings &body body)
  `(let ((*handler-clusters*
          (cons (list ,@(mapcar (lambda (binding)
                                  (destructuring-bind (type handler) binding
                                    `(cons ',type ,handler)))
                                bindings))
                *handler-clusters*)))
     ,@body))

