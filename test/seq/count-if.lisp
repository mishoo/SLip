;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 08:01:30 2002
;;;; Contains: Tests for COUNT-IF

(in-package :sl-user)

(deftest count-if-list.1
  (count-if #'identity '(a b nil c d nil e))
  5)

(deftest count-if-list.2
  (count-if #'not '(a b nil c d nil e))
  2)

(deftest count-if-list.3
  (count-if #'(lambda (x) (error "break")) nil)
  0)

(deftest count-if-list.4
  (count-if #'identity '(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-list.5
  (count-if 'identity '(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-list.6
  (count-if #'identity '(a b nil c d nil e) :key 'identity)
  5)

(deftest count-if-list.8
  (count-if #'identity '(a b nil c d nil e) :key 'not)
  2)

(deftest count-if-list.9
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-list.10
  (count-if #'evenp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-list.11
  (let ((c 0))
    (count-if #'evenp '(1 2 3 4 4 1 8 10 1)
              :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-list.12
  (let ((c 0))
    (count-if #'evenp '(0 1 2 3 4 4 1 7 10 1)
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-list.13
  (count-if #'(lambda (x) (eqt x 'a))
            '(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-list.14
  (count-if #'(lambda (x) (eqt x 'a))
            '(a b c d a e f a e f f a a) :end 7)
  2)

(deftest count-if-list.15
  (count-if #'(lambda (x) (eqt x 'a))
            '(a b c d a e f a e f f a a) :end 7
            :start 2)
  1)

(deftest count-if-list.16
  (count-if #'(lambda (x) (eqt x 'a))
            '(a b c d a e f a e f f a a) :end 7
            :start 2 :from-end t)
  1)
