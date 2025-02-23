(defpackage :turtle
  (:use :sl))

(in-package :turtle)

(export '(draw-turtle
          hide-turtle
          show-turtle
          save-excursion
          without-pen
          orientation
          orientation-radians
          set-orientation
          forward
          backward
          set-color
          set-thickness
          left
          right
          penup
          pendown
          clear
          triangle
          set-font
          set-text-baseline
          set-text-align
          measure-text
          fill-text
          set-rotation
          set-scale))

(import '(sl-ffi:defun-js))

(defparameter *canvas-id* "thy-canvas")
(defparameter *position* (cons 0 0))
(defparameter *orientation* 90)
(defparameter *pen* t)
(defparameter *canvas* nil)
(defparameter *context* nil)
(defparameter *turtle-canvas* nil)
(defparameter *turtle-context* nil)
(defparameter *show-turtle* t)

(defun-js %dom-create-canvas (id width height bg)
  "var tmp = document.getElementById(id);
   if (tmp) tmp.parentNode.removeChild(tmp);
   tmp = document.createElement('canvas');
   tmp.id = id;
   tmp.width = width;
   tmp.height = height;
   tmp.style.border = '1px solid red';
   tmp.style.padding = '1px';
   tmp.style.position = 'absolute';
   tmp.style.boxShadow = '0 0 10px #999';
   if (bg) tmp.style.background = bg;
   tmp.style.right = '10px';
   tmp.style.top = '10px';
   document.body.appendChild(tmp);
   var ctx = tmp.getContext('2d');
   ctx.translate(width / 2, height / 2);
   ctx.scale(1, -1);
   return tmp;")

(defun-js %dom-set-scale (context x y)
  "context.scale(x, y);")

(defun-js %dom-set-rotation (context angle)
  "context.rotate(angle);")

(defun-js %dom-reset-transform (context x y)
  "context.setTransform(1, 0, 0, 1, x, y);")

(defun-js %dom-set-translation (context x y)
  "context.translate(x, y);")

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

(defun-js %dom-set-fill (context color)
  "context.fillStyle = color;")

(defun-js %dom-set-thickness (context width)
  "context.lineWidth = width;")

(defun-js %dom-clear (canvas)
  "var ctx = canvas.getContext('2d');
   ctx.clearRect(-canvas.width, -canvas.height, 2 * canvas.width, 2 * canvas.height);")

(defun-js %dom-canvas-width (canvas) "return canvas.width")
(defun-js %dom-canvas-height (canvas) "return canvas.height")

(defun-js %dom-set-font (context font)
  "context.font = font;")

(defun-js %dom-set-text-baseline (context baseline)
  "context.textBaseline = baseline;")

(defun-js %dom-set-text-align (context align)
  "context.textAlign = align;")

(defun-js %dom-fill-text (context text x y)
  "context.fillText(text, x, y);")

(defun-js %dom-measure-text (context text)
  "var box = context.measureText(text);
   return [ box.width, box.height ];")

(defun-js %dom-circle (context x y radius)
  "context.beginPath();
   context.arc(x, y, radius, 0, 2 * Math.PI);
   context.stroke();")

(defun init-canvas (width height)
  (setf *canvas* (%dom-create-canvas *canvas-id* width height "#ffffffd0")
        *context* (%dom-canvas-context *canvas*)
        *turtle-canvas* (%dom-create-canvas (strcat *canvas-id* "-turtle") width height)
        *turtle-context* (%dom-canvas-context *turtle-canvas*))
  (when *show-turtle* (draw-turtle)))

(defglobal +PI+ (%js-eval "Math.PI"))
(defglobal +PI2+ (/ +PI+ 2))

(defmacro save-excursion (&body body)
  `(unwind-protect
       (let ((*position* *position*)
             (*orientation* *orientation*)
             (*show-turtle* nil))
         (%dom-save *context*)
         ,@body)
     (%dom-restore *context*)))

(defmacro without-pen (&body body)
  `(progn
     (let ((*pen* nil)
           (*show-turtle* nil))
       ,@body)
     (when *show-turtle*
       (show-turtle))))

(defun circle (r)
  (%dom-circle *context* (car *position*) (cdr *position*) r))

(defun orientation-radians ()
  (* +PI+ (/ *orientation* 180)))

(defun orientation ()
  *orientation*)

(defun set-orientation (val)
  (setf *orientation* val)
  (when *show-turtle* (draw-turtle)))

(defun forward (len)
  (let* ((radians (orientation-radians))
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
(defun set-thickness (width) (%dom-set-thickness *context* width))

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

(defun set-font (font)
  (%dom-set-font *context* font))

(defun set-text-baseline (baseline)
  (%dom-set-text-baseline *context* baseline))

(defun set-text-align (align)
  (%dom-set-text-align *context* align))

(defun measure-text (text)
  (as-list (%dom-measure-text *context* text)))

(defun set-rotation (angle)
  (%dom-set-rotation *context* angle))

(defun set-translation (x y)
  (%dom-set-translation *context* x y))

(defun set-scale (x y)
  (%dom-set-scale *context* x y))

(defun fill-text (text)
  (let ((x (car *position*))
        (y (cdr *position*))
        (w (%dom-canvas-width *canvas*))
        (h (%dom-canvas-height *canvas*)))
    (unwind-protect
        (progn
          (%dom-save *context*)
          (%dom-reset-transform *context*
                                (+ x (/ w 2))
                                (- h (+ y (/ h 2))))
          (%dom-set-rotation *context* (- (- (orientation-radians) +PI2+)))
          (%dom-fill-text *context* text 0 0))
      (%dom-restore *context*))))

(init-canvas 700 700)
