(in-package :sl-user)

(defun bar ()
  ;;(%:%step-debug-mode)
  (%:console.dir "foo"
                 (let ((x 10))
                   (ignore-errors (error "foo")) x)
                 "bar"))

;;(format t "~A~%" (disassemble #'bar))