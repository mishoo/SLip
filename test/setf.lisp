(in-package :sl-user)

(defun foo ()
  (macrolet ((bar (cell)
               `(car ,cell)))
    (let ((x (list 1 2)))
      (setf (bar x) 'changed)
      x)))

(defun sm1 ()
  (let ((y 10))
    (symbol-macrolet ((x (list foo 'bar y)))
      (let ((foo 5))
        (format t "~A~%" x)))))

(defun sm2 ()
  (let ((y 10))
    (symbol-macrolet ((x y))
      (format t "~A~%" (list x
                             (let ((x 5))
                               x))))))

(defun setq-on-smac ()
  (symbol-macrolet ((x (cdr list)))
    (let ((list (list 1 2 3 4)))
      (setq x `(foo bar))
      (setf (car x) 're)
      list)))

(defsetf first-pair (lst) (v1 v2)
  (let ((vlst (gensym)))
    `(let ((,vlst ,lst))
       (values
        (setf (car ,vlst) ,v1)
        (setf (car (cdr ,vlst)) ,v2)))))

(defparameter x '(a b c d e))

(setf (first-pair x) (floor 100000001 17))

(defun (setf elt) (new-val list index)
  (setf (car (nthcdr index list)) new-val))