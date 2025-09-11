(in-package :sl)

(export '(make-hash-table maphash clrhash hash-table-count with-hash-table-iterator))

(defpackage :sl-hash
  (:use :sl :%))

(defun (setf gethash) (newvalue key hash)
  (%hash-set newvalue key hash))

(define-compiler-macro (setf gethash) (newvalue key hash)
  `(%hash-set ,newvalue ,key ,hash))

(defun unsupported-test (test)
  (and test (not (or (eq test 'eq)
                     (eq test 'eql)
                     (eq test #'eq)
                     (eq test #'eql)))))

(defun make-hash-table (&key test size rehash-size rehash-threshold)
  (declare (ignore size rehash-size rehash-threshold))
  (when (unsupported-test test)
    (warn "MAKE-HASH-TABLE: only EQ or EQL test is supported"))
  (make-hash))

(define-compiler-macro make-hash-table (&whole form &key test size rehash-size rehash-threshold)
  (declare (ignore size rehash-size rehash-threshold))
  (if test form `(make-hash)))

(defmacro with-hash-table-iterator ((iterator hash-table) &body body)
  (let ((it (gensym)))
    `(let ((,it (hash-iterator ,hash-table)))
       (flet ((,iterator ()
                (multiple-value-bind (more entry) (iterator-next ,it)
                  (when more (values t (svref entry 0) (svref entry 1))))))
         ,@body))))

(defun maphash (func hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (tagbody
     :loop
     (multiple-value-bind (more key value) (next-entry)
       (when more
         (funcall func key value)
         (go :loop))))))
