(defpackage :test-conditions
  (:use :sl))

(in-package :test-conditions)

(defclass my-error (simple-error) ())

(defun throw-something ()
  (error (make-instance 'my-error
                        'sl::format-control "MY error ~A"
                        'sl::format-args '(bar))))

(defun test ()
  (handler-case
      (progn
        (throw-something)
        (%:console.log "not getting here"))
    (my-error ()
      (%:console.log "got the right handler"))
    (error (ex)
      (%:console.log "GOT HERE" (print-object-to-string ex))
      (%:console.dir ex))))