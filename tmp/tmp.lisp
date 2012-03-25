(in-package :turtle)

(defun rect-spiral (max)
  (save-excursion
   (let rec ((n 0))
     (when (< n max)
       (forward n)
       (left 89.9)
       (rec (+ 5 n))))))

(defmacro animate ((&key (speed 50)) &body body)
  (let ((sym (gensym)))
    `(labels ((,sym ()
                ,@body
                (set-timeout ,speed #',sym)))
       (,sym))))

;;;; stuff

(defun test-func (func . args)
  (save-excursion
   (clear)
   (without-pen (backward (/ (%dom-canvas-width *canvas*) 2)))
   (apply func args)))

(defun tree (n)
  (when (> n 10)
    (forward (/ n 4))
    (left 10)
    (save-excursion
     (right 45)
     (tree (* n 0.6)))
    (forward (/ n 4))
    (right 25)
    (save-excursion
     (left 40)
     (tree (* n 0.7)))
    (left 10)
    (tree (- n 15))))

(defun leaf (n)
  (when (>= n 5)
    (let ((half (/ n 2)))
      (forward half)
      (save-excursion
       (right 60)
       (leaf half))
      (save-excursion
       (left 60)
       (leaf half))
      (right 3)
      (leaf (- n 5)))))

(defun circle args
  (destructuring-bind (r &optional (n 40)) args
    (save-excursion
     (without-pen (forward r))
     (let* ((len (* 2 r +PI+))
            (step (/ len n))
            (rot (/ 360 n)))
       (right (+ 90 (/ rot 2)))
       (let looop ((i n))
         (when (> i 0)
           (forward step)
           (right rot)
           (looop (1- i))))))))

(labels ((draw-pin (angle len thick)
           (save-excursion
            (right angle)
            (let* ((t2 (/ thick 2))
                   (x (sqrt (+ (* t2 t2)
                               (* len len))))
                   (a1 (* 180 (/ (asin (/ len x)) +PI+)))
                   (a2 (- 90 a1)))
              (left 90)
              (without-pen (forward t2))
              (backward thick)
              (right a1)
              (forward x)
              (right (+ 180 (* a2 2)))
              (forward x)))))
  (defun clock args
    (destructuring-bind (r &key
                           (hours-r 3)
                           (hours-pin 10)) args
      (let* ((len (* 2 r +PI+))
             (step (/ len 12)))
        (save-excursion
         (without-pen (forward r))
         (let looop ((i 12))
           (when (> i 0)
             (circle hours-r 4)
             (right 105)
             (without-pen (forward step))
             (left 75)
             (save-excursion (backward hours-pin))
             (looop (1- i)))))
        (destructuring-bind
            (year month date hour min sec msec)
            (%local-date)
          (incf sec (/ msec 1000))
          (incf min (/ sec 60))
          (incf hour (/ min 60))
          (save-excursion
           (right (* sec 6))
           (backward 10)
           (draw-pin 0 (+ r 10) 1))
          (draw-pin (* min 6) (* r 0.9) 3)
          (draw-pin (* hour 30) (* r 0.7) 5)
          min)))))

(let ((running nil))
  (defun start-clock ()
    (setf running t)
    (clear)
    (animate-clock))
  (defun stop-clock ()
    (setf running nil))
  (defun clock-running ()
    running))

(defun animate-clock ()
  (when (clock-running)
    (without-interrupts
      (clear)
      (let ((sec (clock 150)))
        (let ((*orientation* *orientation*))
          (right (* sec 6))
          (without-pen (backward 75)))
        (clock 35 :hours-r 2 :hours-pin 6)
        (set-timeout 150 #'animate-clock)))))

;; (defun animate-clock ()
;;   (when (clock-running)
;;     (without-interrupts
;;       (clear)
;;       (clock 150)
;;       (let rec ((i 10))
;;         (when (> i 0)
;;           (left (* i 2))
;;           (clock (- 150 (* 15 i)))
;;           (rec (- i 0.5))))
;;       (set-timeout 150 #'animate-clock))))