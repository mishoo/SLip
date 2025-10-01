(in-package :sl-user)

(defparameter *test* nil)

(defun foo (n)
  (when (> n 0)
    (let ((*test* n))
      (format t "~A~%" n)
      (return-from foo (foo (- n 1))))))

(progn
  (foo 5)
  *test*)