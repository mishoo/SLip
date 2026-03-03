(defpackage sl-thread
  (:use :sl)
  (:export #:make-thread #:current-thread #:thread-name
           #:make-lock #:acquire-lock #:release-lock
           #:with-lock-held))

(in-package :sl-thread)

(defun make-thread (func &key name arguments)
  (when arguments
    (setf func (lambda () (apply func arguments))))
  (%:%make-thread func name))

(defun current-thread ()
  (%:%current-thread))

(defun thread-name (thread)
  (%:%thread-name thread))

(defun make-lock ()
  (%:%make-mutex))

(defun acquire-lock (mutex &key (timeout t))
  (%:%mutex-acquire mutex timeout))

(defun release-lock (mutex)
  (%:%mutex-release mutex))

(defmacro with-lock-held ((mutex) &body body)
  (let ((_mutex (gensym "LOCK")))
    `(let ((,_mutex ,mutex))
       (when (acquire-lock ,_mutex)
         (unwind-protect
             (locally ,@body)
           (release-lock ,_mutex))))))
