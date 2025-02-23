(in-package :turtle)

(init-canvas 700 700)

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

(defun ease-decelerate (f)
  (setf f (- 1 f))
  (- 1 (* f f)))

(defun ease-elastic (f)
  (- 1 (/ (cos (* 4 +pi+ f))
          (expt 2 (* 8 f)))))

(defun floatmap (f a b)
  (+ a (* f (- b a))))

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
                           (hours-r 6)
                           (hours-pin 10)) args
      (let* ((len (* 2 r +PI+))
             (step (/ len 12))
             sec0)
        (save-excursion
         (set-color "#ccc")
         (dotimes (i 60)
           (save-excursion
            (without-pen (forward (- r 3)))
            (forward 6))
           (right 6)))
        (save-excursion
         (let looop ((i 12))
           (when (> i 0)
             (save-excursion
              (without-pen (forward (* r 1.02)))
              (circle hours-r)
              (backward hours-pin))
             (right 30)
             (looop (1- i)))))
        (destructuring-bind
            (year month date hour min sec msec)
            (%local-date)
          (setf sec0 sec)
          (incf sec (/ msec 1000))
          (incf min (/ sec 60))
          (incf hour (/ min 60))
          (save-excursion
           (set-color "orange")
           (draw-pin (* min 6) (* r 0.9) 3)
           (set-color "red")
           (draw-pin (* hour 30) (* r 0.7) 5))
          (save-excursion
           (incf sec0 (ease-elastic (/ msec 300)))
           (right (* sec0 6))
           (set-thickness 2)
           (without-pen (backward (* r 0.07)))
           (set-thickness 1)
           (set-color "#445")
           (draw-pin 0 (* r 1.1) 2))
          sec)))))

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
      (let ((sec (clock 200)))
        ;; (let ((*orientation* *orientation*))
        ;;   (right (* sec 6))
        ;;   (without-pen (backward 280)))
        ;; (clock 40 :hours-r 2 :hours-pin 6)
        (set-timeout 16 #'animate-clock)))))

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