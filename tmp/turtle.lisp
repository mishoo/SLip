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
  `(unwind-protect
       (let ((*position* *position*)
             (*orientation* *orientation*))
         (%dom-save *context*)
         ,@body)
     (%dom-restore *context*)))

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