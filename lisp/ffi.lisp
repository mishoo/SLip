(defpackage :sl-ffi
  (:use :sl)
  (:nicknames :ffi))

(in-package :sl-ffi)

(export '(defun-js lambda-js))

(defun to-js-name (name)
  (setf name (downcase (replace-regexp #/-+/g name "_")))
  (aif (regexp-exec #/^%/ name)
       (setf name (strcat "_" (substr name 1))))
  (aif (regexp-exec #/^\*(.*)\*$/ name)
       (setf name (upcase (vector-ref it 1))))
  (aif (regexp-exec #/^\+(.*)\+$/ name)
       (setf name (strcat "$" (upcase (vector-ref it 1)))))
  name)

(defmacro defun-js (name args body)
  (let* ((js-name (to-js-name name))
         (sym (gensym))
         (js-argnames (mapcar #'to-js-name args)))
    `(let ((,sym (%js-eval ,(format nil "function ~A (~{~A~^, ~}) { ~A }"
                                    js-name
                                    js-argnames
                                    body))))
       (defun ,name args
         (%js-apply ,sym nil args)))))

(defmacro lambda-js (args body)
  (let* ((sym (gensym))
         (js-argnames (mapcar #'to-js-name args)))
    `(let ((,sym (%js-eval ,(format nil "function (~{~A~^, ~}) { ~A }"
                                    js-argnames
                                    body))))
       (lambda args
         (%js-apply ,sym nil args)))))
