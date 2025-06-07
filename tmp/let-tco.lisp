(in-package :sl-user)

(defparameter *dynamic* 'unbound)

(defun next (val)
  (format t "~A~%" (+ val *dynamic*))
  'result)

(defun test-let-tco ()
  (let* ((foo 1)
         (*dynamic* 10)
         (bar 2)
         (baz 3))
    (next (+ foo bar baz))))

(defmacro with-body (&body body)
  `(let ((*dynamic* 'bound))
     ,@body))

(defun moo ()
  (with-body
    (format t "~A~%" *dynamic*)))