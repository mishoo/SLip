(in-package :sl-user)

(declaim (inline test-func))
(defun test-func (a b &optional c (d 5) &key (e 6))
  (+ a b c d e))

(defun foo (a b)
  (format t "~A~%" (funcall 'test-func a b 3 5 :e 123)))

(declaim (inline test-rest))
(defun test-rest (a b &rest c)
  (apply '+ a b c))

(defun call-rest ()
  (test-rest 1 2 3 4 (+ 5 6 7 8)))