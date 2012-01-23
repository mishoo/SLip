(defpackage :bar
  (:use :ss)
  (:import-from #:foo #:crap))

(in-package :bar)

(console.log (%symbol-package 'crap))
