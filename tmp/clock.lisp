(load "lib/turtle.lisp")

(in-package :turtle)

(defun ease-decelerate (f)
  (setf f (- 1 f))
  (- 1 (* f f f f f)))

(defun ease-elastic (f)
  (- 1 (/ (cos (* 4 +pi+ f))
          (expt 2 (* 8 f)))))

(defun floatmap (f a b)
  (+ a (* f (- b a))))

(defun clock args
  (destructuring-bind (r &key
                         (hours-r (/ r 22))
                         (hours-pin (/ r 10))) args
    (let* ((len (* 2 r +PI+))
           (step (/ len 12))
           sec0)
      (destructuring-bind
          (year month date hour min sec msec
                &aux (anim-pos (/ msec 400)))
          (%:%local-date)
        (setf sec0 sec)
        (save-excursion
         (set-color "#aab")
         (dotimes (i 60)
           (save-excursion
            (without-pen (forward (- r 3)))
            (forward 6))
           (right 6)))
        (save-excursion
         (set-thickness 2)
         (let looop ((i 0))
           (when (< i 12)
             (save-excursion
              (without-pen
                (cond
                  ((= sec0 (* i 5))
                   (forward (floatmap (ease-elastic (/ msec 1000))
                                      (* r 1.09) (* r 1.02)))
                   (right (floatmap (ease-elastic (/ msec 1000))
                                    180 0)))
                  ((= (mod (1+ sec0) 60) (* i 5))
                   (forward (floatmap (ease-elastic (/ msec 1500))
                                      (* r 1.02) (* r 1.09)))
                   (left (floatmap (ease-elastic (/ msec 1000))
                                   0 180)))
                  (t
                   (forward (* r 1.02)))))
              (circle hours-r)
              (backward hours-pin))
             (right 30)
             (looop (1+ i)))))
        (incf sec (/ msec 1000))
        (incf min (/ sec 60))
        (incf hour (/ min 60))
        (save-excursion
         (set-color "orange")
         (right (* min 6))
         (triangle (* r 0.9) 3))
        (save-excursion
         (set-color "red")
         (right (* hour 30))
         (triangle (* r 0.7) 5))
        ;; (save-excursion
        ;;  (set-color "#ddd")
        ;;  (right (* sec 6))
        ;;  (forward r))
        (save-excursion
         (incf sec0 (ease-elastic anim-pos))
         (right (* sec0 6))
         (set-thickness 2)
         (without-pen (backward (* r 0.08)))
         (set-thickness 1)
         (set-color "#445")
         (triangle (* r 1.1) 2))
        (circle (* r 0.03))
        sec))))

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
      (let ((sec (clock 175)))
        ;; (let ((*orientation* *orientation*))
        ;;   (right (* sec 6))
        ;;   (without-pen (backward 280)))
        ;; (clock 40 :hours-r 2 :hours-pin 6)
        (set-timeout 16 #'animate-clock)))))

(make-thread
 (lambda ()
   (init-canvas 700 700)
   (hide-turtle)
   (start-clock)))
