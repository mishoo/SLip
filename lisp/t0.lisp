(defpackage :foo
  (:use :ss))

(in-package :foo)

(console.log (%symbol-package 'crap))
