;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 18:37:52 2002
;;;; Contains: Tests for FIND-IF

(in-package :sl-user)

(deftest find-if-list.1
  (find-if #'identity ())
  nil)

(deftest find-if-list.2
  (find-if #'identity '(a))
  a)

(deftest find-if-list.2a
  (find-if 'identity '(a))
  a)

(deftest find-if-list.3
  (find-if #'evenp '(1 2 4 8 3 1 6 7))
  2)

(deftest find-if-list.4
  (find-if #'evenp '(1 2 4 8 3 1 6 7) :from-end t)
  6)

(deftest find-if-list.5
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-list.6
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-list.7
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-list.8
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-list.9
  (loop for i from 0 to 8 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i))
  (nil nil 2 2 2 2 2 2 2))

(deftest find-if-list.10
  (loop for i from 0 to 8 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i :from-end t))
  (nil nil 2 4 8 8 8 6 6))

(deftest find-if-list.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.13
  (loop for i from 0 to 6
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :start i))
  (1 11 11 45 45 71 nil))

(deftest find-if-list.14
  (loop for i from 0 to 6
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))
  (71 71 71 71 71 71 nil))

(deftest find-if-list.15
  (loop for i from 0 to 7
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :end i))
  (nil 1 1 1 1 1 1 1))

(deftest find-if-list.16
  (loop for i from 0 to 7
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))
  (nil 1 1 11 11 45 71 71))

(deftest find-if-list.17
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.18
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t :key #'1+)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))
