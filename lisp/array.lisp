(in-package :sl)

(export '(make-array aref arrayp array-dimension array-dimensions
          array-rank array-element-type fill-pointer
          vector-push vector-push-extend))

(defpackage :sl-array
  (:use :sl :%))

(in-package :sl-array)

(defun make-array (dimensions &key
                              element-type
                              initial-element
                              initial-contents
                              adjustable
                              fill-pointer
                              displaced-to
                              displaced-index-offset)
  (cond
    ((and (not element-type)
          (or (numberp dimensions)
              (and (consp dimensions)
                   (not (cdr dimensions)))))
     (make-vector (if (consp dimensions) (car dimensions) dimensions)
                  initial-element initial-contents))
    (t
     (%:%make-array dimensions element-type initial-element initial-contents))))

(define-compiler-macro make-array (&whole form
                                          dimensions &key
                                          initial-element
                                          initial-contents
                                          element-type
                                          &allow-other-keys)
  (cond
    ((or element-type
         (consp dimensions))
     form)
    (t
     `(make-vector ,dimensions ,initial-element ,initial-contents))))

(defun aref (array &rest subscripts)
  (cond
    ((or (null subscripts)
         (cdr subscripts))
     (apply #'%:%array-ref array subscripts))
    (t
     (svref array (car subscripts)))))

(define-compiler-macro aref (array &rest subscripts)
  (cond
    ((or (null subscripts)
         (cdr subscripts))
     `(%:%array-ref ,array ,@subscripts))
    (t
     `(svref ,array ,(car subscripts)))))

(defun (setf aref) (value array &rest subscripts)
  (cond
    ((or (null subscripts)
         (cdr subscripts))
     (apply #'%:%array-set value array subscripts))
    (t
     (setf (svref array (car subscripts)) value))))

(define-compiler-macro (setf aref) (value array &rest subscripts)
  (cond
    ((or (null subscripts)
         (cdr subscripts))
     `(%:%array-set ,value ,array ,@subscripts))
    (t
     `(setf (svref ,array ,(car subscripts)) ,value))))

(defun arrayp (thing)
  (%:%arrayp thing))

(define-compiler-macro arrayp (thing)
  `(%:%arrayp ,thing))

(defun vector-push-extend (obj vector)
  (vector-push obj vector))

(define-compiler-macro vector-push-extend (obj vector)
  `(vector-push ,obj ,vector))
