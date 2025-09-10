;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 22:42:35 2002
;;;; Contains: Tests for COUNT-IF-NOT

(in-package :sl-user)

(deftest count-if-not-list.1
  (count-if-not #'identity '(a b nil c d nil e))
  2)

(deftest count-if-not-list.2
  (count-if-not #'not '(a b nil c d nil e))
  5)

(deftest count-if-not-list.3
  (count-if-not #'(lambda (x) (error "break")) nil)
  0)

(deftest count-if-not-list.4
  (count-if-not #'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.5
  (count-if-not 'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.6
  (count-if-not #'identity '(a b nil c d nil e) :key 'identity)
  2)

(deftest count-if-not-list.8
  (count-if-not #'identity '(a b nil c d nil e) :key 'not)
  5)

(deftest count-if-not-list.9
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-not-list.10
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-not-list.11
  (let ((c 0))
    (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1)
              :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-list.12
  (let ((c 0))
    (count-if-not #'oddp '(0 1 2 3 4 4 1 7 10 1)
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-list.13
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-not-list.14
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7)
  2)

(deftest count-if-not-list.15
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7
            :start 2)
  1)

(deftest count-if-not-list.16
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7
            :start 2 :from-end t)
  1)
