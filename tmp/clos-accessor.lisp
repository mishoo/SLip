(in-package :sl-user)

(defclass thing ()
  ((x :initarg :x :accessor thing-x)
   (y :initarg :y :accessor thing-y)))

(defmethod (setf thing-x) :before (new-x (thing thing))
  (format t "~&Changing X from ~A to ~A in ~S.~%"
          (thing-x thing) new-x thing))

(defparameter thing1 nil)
(defparameter thing2 nil)

(setq thing1 (make-instance 'thing :x 1 :y 2))
(setq thing2 (make-instance 'thing :x 7 :y 8))

%:EOF

(with-slots ((x1 x) (y1 y)) thing1
  (with-slots ((x2 x) (y2 y)) thing2
    (list (list x1 (thing-x thing1) y1 (thing-y thing1)
                x2 (thing-x thing2) y2 (thing-y thing2))
          (setq x1 (+ y1 x2))
          (list x1 (thing-x thing1) y1 (thing-y thing1)
                x2 (thing-x thing2) y2 (thing-y thing2))
          (setf (thing-x thing2) (list x1))
          (list x1 (thing-x thing1) y1 (thing-y thing1)
                x2 (thing-x thing2) y2 (thing-y thing2)))))

(setf (thing-x thing2) 'crap)
