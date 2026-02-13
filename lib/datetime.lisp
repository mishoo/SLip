(defpackage :sl-datetime
  (:use :sl :ffi)
  (:export #:datetimep #:from-timestamp #:now #:to-locale-string))

(in-package :sl-datetime)

(defun-js datetimep (obj) "return thing instanceof Date")

(defun-js from-timestamp (ts) "return new Date(ts)")

(defun-js now () "return new Date()")

(defun-js to-locale-string (date) "return date.toLocaleString()")
