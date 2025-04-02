(defpackage sl-tmp
  (:use :sl :%))

(in-package :sl-tmp)

(defun identity (x) x)

(defun find/list (item sequence &key
                       from-end
                       (test #'eql)
                       (start 0)
                       end
                       (key #'identity))
  (loop for el in sequence
        for val = (funcall key el)
        when (funcall test item val)
        do (return el)))

(defun find (item sequence &rest args)
  (cond
    ((listp sequence)
     (apply #'find/list item sequence args))))
