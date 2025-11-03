(in-package :sl-user)

(declaim (inline test-func))
(defun test-func (a b &optional c (d 5) &rest rest
                    &key (e 6 e-supplied-p) (f (compute-f) f-supplied-p))
  (+ a b c d e))

(defun foo (a b)
  (format t "~A~%" (funcall 'test-func a b 3 5 :e 123)))

(declaim (inline test-rest))
(defun test-rest (a b &rest c)
  (apply '+ a b c))

(defun call-rest ()
  (test-rest 1 2 3 4 (+ 5 6 7 8)))

(declaim (inline test-rest-and-key))
(defun test-rest-and-key (a b &rest rest &key foo (bar 'bar))
  (list a b foo bar :rest rest))
(defun call-rest-and-key ()
  (test-rest-and-key 1 2 :foo (progn
                                (format t "FOO EVALUATED~%")
                                'foo)))


(defglobal free-var 2)
(declaim (inline test))
(defun test (x) (* free-var x))

(let ((free-var 10))
  (defun call-test (x)
    (test x)))

