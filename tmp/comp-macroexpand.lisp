(in-package :sl-user)

(defun foo (x)
  (setf (car x) 5)
  (return-from foo x))