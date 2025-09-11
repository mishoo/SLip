;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug 19 07:31:55 2002
;;;; Contains: Tests for COUNT

(in-package :sl-user)

(deftest count-list.1
  (count 'a '(a b c d e a e f))
  2)

(deftest count-list.2
  (count 'a '(a b c d e a e f) :test #'eql)
  2)

(deftest count-list.3
  (count 'a '(a b c d e a e f) :test 'eql)
  2)

(deftest count-list.4
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key #'1-)
  5)

(deftest count-list.5
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key '1-)
  5)

(deftest count-list.6
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key #'1- :test #'equal)
  5)

(deftest count-list.7
  (count 1 '(2 1 1 2 3 1 4 1 7 6 1 8) :from-end t)
  5)

(deftest count-list.8
  (let ((c 0))
    (count 1 '(1 2 3 1 4 1 7 6 1 8)
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  4)

(deftest count-list.9
  (let ((c 0))
    (count 1 '(1 2 3 7 4 5 7 6 2 8)
           :from-end t
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  3)

(deftest count-list.10
  (count 1 '(1 1 1 1 1 2 1 1) :start 3)
  4)

(deftest count-list.11
  (count 1 '(1 1 1 1 1 2 1 1) :end 6)
  5)

(deftest count-list.12
  (count 1 '(1 1 1 1 1 2 1 1) :start 2 :end 7)
  4)

(deftest count-list.13
  (count 1 '(1 1 1 1 1 2 1 1) :start 3 :end nil)
  4)

(deftest count-list.14
  (count 1 '(1 1 1 1 1 2 1 1)  :end nil)
  7)

(deftest count-list.15
  (count 1 '(1 1 1 1 1 2 1 1)  :test-not #'eql)
  1)

(deftest count-list.16
  (count 1 '(1 1 1 3 1 2 1 1) :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y))  t))
  5)

(deftest count-list.17
  (count 10 '(1 11 2 4 14 5 18 6 7) :test #'<)
  3)

(deftest count-list.18
  (count 10 '(1 11 2 4 14 5 18 6 7) :test-not #'>=)
  3)

(defharmless count-list.test-and-test-not.1
  (count 0 '(0 1 2 0 1 2 3 0 1) :test #'eql :test-not #'eql))

(defharmless count-list.test-and-test-not.2
  (count 0 '(0 1 2 0 1 2 3 0 1) :test-not #'eql :test #'eql))
