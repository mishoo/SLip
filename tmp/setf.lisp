(in-package :sl-user)

(defun foo ()
  (macrolet ((bar (cell)
               `(car ,cell)))
    (let ((x (list 1 2)))
      (setf (bar x) 'changed)
      x)))

(defun sm1 ()
  (let ((y 10))
    (symbol-macrolet ((x (list foo 'bar y))
                      (moo man))
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

(defun (setf nth) (new-val list index)
  (setf (car (nthcdr index list)) new-val))

(defun (setf nthcdr) (new-val list index)
  (setf (cdr (nthcdr index list)) new-val))

(defun tmp ()
  (setf (nth x 2) (pop x)))

(format t "~A~%~%" (disassemble #'tmp))

(defmacro second (x)
  `(car (cdr ,x)))

(defmacro third (x)
  `(car (cddr ,x)))

(setf (second (cddr x)) 2
      (third (cddr x)) 3)

(defun popping ()
  (let ((x `(a b c d e)))
    (let ((y (list (pop (cdr x))
                   (pop (cdr x)))))
      (values x y))))

(defmacro my-inc (place)
  (format t "Called for ~A~%" place)
  `(incf ,place))

(defun incf-sm ()
  (symbol-macrolet ((mac (car y)))
    (let ((y (list 1)))
      (my-inc mac)
      y)))