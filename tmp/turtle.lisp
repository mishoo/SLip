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
(defparameter *turtle-canvas* nil)
(defparameter *turtle-context* nil)
(defparameter *show-turtle* t)

(defun-js %dom-create (id width height)
  "var tmp = document.getElementById(id);
   if (tmp) tmp.parentNode.removeChild(tmp);
   tmp = document.createElement('canvas');
   tmp.id = id;
   tmp.width = width;
   tmp.height = height;
   tmp.style.border = '1px solid red';
   tmp.style.padding = '1px';
   tmp.style.position = 'absolute';
   tmp.style.left = '5px';
   tmp.style.top = '5px';
   document.body.appendChild(tmp);
   var ctx = tmp.getContext('2d');
   ctx.translate(width / 2, height / 2);
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

(defun-js %dom-set-color (context color)
  "context.strokeStyle = color;")

(defun-js %dom-clear (canvas)
  "var ctx = canvas.getContext('2d');
   ctx.clearRect(-canvas.width, -canvas.height, 2 * canvas.width, 2 * canvas.height);")

(defun-js %dom-canvas-width (canvas) "return canvas.width")
(defun-js %dom-canvas-height (canvas) "return canvas.height")

;; forward declarations to avoid compiler warnings
;; should figure out a better way to do this
(defun draw-turtle ())
(defun hide-turtle ())

(defun init-canvas (width height)
  (setf *canvas* (%dom-create *canvas-id* width height)
        *context* (%dom-canvas-context *canvas*)
        *turtle-canvas* (%dom-create (strcat *canvas-id* "-turtle") width height)
        *turtle-context* (%dom-canvas-context *turtle-canvas*))
  (when *show-turtle* (draw-turtle)))

(defglobal +PI+ (%js-eval "Math.PI"))

(defmacro save-excursion (&body body)
  `(unwind-protect
       (let ((*position* *position*)
             (*orientation* *orientation*)
             (*show-turtle* nil))
         (%dom-save *context*)
         ,@body)
     (%dom-restore *context*)))

(defmacro without-pen (&body body)
  `(let ((*pen* nil)
         (*show-turtle* nil))
     ,@body))

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
    (setf *position* (cons x1 y1))
    (when *show-turtle* (draw-turtle))))

(defun backward (n)
  (forward (- n)))

(defun set-color (color) (%dom-set-color *context* color))

(defun left (angle)
  (incf *orientation* angle)
  (when *show-turtle* (draw-turtle)))

(defun right (angle)
  (decf *orientation* angle)
  (when *show-turtle* (draw-turtle)))

(defun penup () (setf *pen* nil))
(defun pendown () (setf *pen* t))

(defun clear args
  (destructuring-bind (&key keep-context) args
    (%dom-clear *canvas*)
    (unless keep-context
      (setf *position* (cons 0 0)
            *orientation* 90
            *pen* t)
      (if *show-turtle*
          (draw-turtle)
          (hide-turtle)))))

(defun triangle (h base)
  (let* ((x (/ base 2))
         (y (sqrt (+ (* x x) (* h h))))
         (α (* 180 (/ (asin (/ h y)) +PI+)))
         (β (* 2 (- 90 α))))
    (left 90)
    (without-pen (forward x))
    (right (- 180 α))
    (forward y)
    (right (- 180 β))
    (forward y)
    (right (- 180 α))
    (forward (* 2 x))))

(defun draw-turtle ()
  (let ((*canvas* *turtle-canvas*)
        (*context* *turtle-context*))
    (%dom-clear *canvas*)
    (save-excursion
     (set-color "#c03")
     (triangle 15 10))))

(defun hide-turtle ()
  (setf *show-turtle* nil)
  (%dom-clear *turtle-canvas*))

(defun show-turtle ()
  (setf *show-turtle* t)
  (draw-turtle))

(init-canvas 400 400)