(defpackage :turtle
  (:use :ss))

(in-package :turtle)

(import '(ss-ffi:defun-js))

(defparameter *canvas-id* "thy-canvas")
(defparameter *position* (cons 0 0))
(defparameter *orientation* 90)
(defparameter *pen* t)
(defparameter *canvas* nil)
(defparameter *context* nil)

(defun-js %dom-create (id width height)
  "var tmp = document.getElementById(id);
   if (tmp) tmp.parentNode.removeChild(tmp);
   tmp = document.createElement('canvas');
   tmp.id = id;
   tmp.width = width;
   tmp.height = height;
   tmp.style.border = '1px solid red';
   tmp.style.padding = '1px';
   document.body.appendChild(tmp);
   var ctx = tmp.getContext('2d');
   ctx.translate(width / 2 + 0.5, height / 2 + 0.5);
   ctx.scale(1, -1);
   return tmp;")

(defun-js %dom-canvas-context (canvas)
  "return canvas.getContext('2d')")

(defun-js %dom-save (context)
  "context.save()")

(defun-js %dom-restore (context)
  "context.restore()")

(defun-js %dom-line-to (context x0 y0 x1 y1)
  "context.beginPath();
   context.moveTo(x0, y0);
   context.lineTo(x1, y1);
   context.stroke();")

(defun-js %dom-clear (canvas)
  "var ctx = canvas.getContext('2d');
   ctx.clearRect(-canvas.width, -canvas.height, 2 * canvas.width, 2 * canvas.height);")

(defun-js %dom-canvas-width (canvas) "return canvas.width")
(defun-js %dom-canvas-height (canvas) "return canvas.height")

(defun init-canvas (width height)
  (setf *canvas* (%dom-create *canvas-id* width height)
        *context* (%dom-canvas-context *canvas*)))

(defglobal +PI+ (%js-eval "Math.PI"))

(defmacro save-excursion (&body body)
  (let ((context (gensym)))
    `(let ((,context *context*)
           (*position* *position*)
           (*orientation* *orientation*))
       (%dom-save ,context)
       (unwind-protect
           (progn ,@body)
         (%dom-restore ,context)))))

(defun orientation ()
  (* +PI+ (/ *orientation* 180)))

(defun forward (len)
  (let* ((radians (orientation))
         (x0 (car *position*))
         (y0 (cdr *position*))
         (x1 (+ x0 (* len (cos radians))))
         (y1 (+ y0 (* len (sin radians)))))
    (when *pen*
      (%dom-line-to *context* x0 y0 x1 y1))
    (setf *position* (cons x1 y1))))

(defun backward (n)
  (forward (- n)))

(defun left (angle) (incf *orientation* angle))
(defun right (angle) (decf *orientation* angle))
(defun penup () (setf *pen* nil))
(defun pendown () (setf *pen* t))

(defun clear args
  (destructuring-bind (&key keep-context) args
    (%dom-clear *canvas*)
    (unless keep-context
      (setf *position* (cons 0 0)
            *orientation* 90
            *pen* t))))

(defmacro without-pen (&body body)
  `(let ((*pen* nil))
     ,@body))

(init-canvas 400 400)

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
         (circle 2 4)
         (without-pen (forward r))
         (let looop ((i 12))
           (when (> i 0)
             (circle hours-r 10)
             (right 105)
             (without-pen (forward step))
             (left 75)
             (save-excursion (backward hours-pin))
             (looop (1- i)))))
        (destructuring-bind
            (year month date hour min sec msec)
            (%local-date)
          (incf sec (/ msec 1000))
          (decf sec)
          (incf min (/ sec 60))
          (incf hour (/ min 60))
          (save-excursion
           (right (* sec 6))
           (backward 10)
           (draw-pin 0 (+ r 10) 1))
          (draw-pin (* min 6) (* r 0.8) 3)
          (draw-pin (* hour 30) (* r 0.6) 5)
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
        (right (* sec 6))
        (without-pen (backward 75))
        (left (* sec 6))
        (clock 35 :hours-r 2 :hours-pin 6)))
    (set-timeout 100 #'animate-clock)))