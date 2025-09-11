;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 24 07:10:05 2002
;;;; Contains: Tests for POSITION-IF-NOT-NOT

(in-package :sl-user)

(deftest position-if-not-list.1
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.2
  (position-if-not 'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.3
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-not-list.4
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-not-list.5
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-not-list.6
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-not-list.7
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-not-list.8
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-not-list.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key '1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key #'1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))
