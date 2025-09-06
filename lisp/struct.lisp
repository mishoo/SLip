(in-package :sl)

(export '(defstruct))

(defpackage :sl-struct
  (:use :sl :%))

(in-package :sl-struct)

(defglobal *structures* (make-hash-table))
(defglobal *stag* '%struct)

(defun structurep (thing &optional name)
  (and (vectorp thing)
       (eq *stag* (svref thing 0))
       (or (not name)
           (eq name (structure-name (svref thing 1))))))

(defun assert-struct (thing name)
  (unless (structurep thing name)
    (error "Expected structure ~S." name)))

(defun find-structure (name &optional (errorp t))
  (or (gethash *structures* name)
      (when errorp
        (error "No such structure ~S." name))))

(defun (setf find-structure) (struct name)
  (setf (gethash *structures* name) struct))

(defun make-structure (name &key slots)
  (setf (find-structure name)
        (vector '%struct 'structure name slots)))

(defun structure-name (struct)
  (assert-struct struct 'structure)
  (svref struct 2))

(defun structure-slots (struct)
  (assert-struct struct 'structure)
  (svref struct 3))