(in-package :sl-user)

(defparameter *dynamic* 'unbound)

(defun next (val)
  (format t "~A~%" (+ val *dynamic*)))

(defun test-let-tco ()
  (%::%let* ((foo 1)
             (*dynamic* 10)
             (bar 2)
             (baz 3))
            (next (+ foo bar baz)))
  (format t "~A~%" *dynamic*))