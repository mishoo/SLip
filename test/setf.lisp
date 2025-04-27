(in-package :sl-user)

(defun foo ()
  (macrolet ((bar (cell)
               `(car ,cell)))
    (let ((x (list 1 2)))
      (setf (bar x) 'changed)
      x)))