;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 07:49:49 2002
;;;; Contains: Tests for FIND

(in-package :sl-user)

(deftest find-list.1
  (find 'c '(a b c d e c a))
  c)

(deftest find-list.2
  (find 'c '(a b c d e c a) :from-end t)
  c)

(deftest find-list.3
  (loop for i from 0 to 7 collect
        (find 'c '(a b c d e c a) :start i))
  (c c c c c c nil nil))

(deftest find-list.4
  (loop for i from 0 to 7 collect
        (find 'c '(a b c d e c a) :start i :end nil))
  (c c c c c c nil nil))

(deftest find-list.5
  (loop for i from 7 downto 0 collect
        (find 'c '(a b c d e c a) :end i))
  (c c c c c nil nil nil))

(deftest find-list.6
  (loop for i from 0 to 7 collect
        (find 'c '(a b c d e c a) :start i :from-end t))
  (c c c c c c nil nil))

(deftest find-list.7
  (loop for i from 0 to 7 collect
        (find 'c '(a b c d e c a) :start i :end nil :from-end t))
  (c c c c c c nil nil))

(deftest find-list.8
  (loop for i from 7 downto 0 collect
        (find 'c '(a b c d e c a) :end i :from-end t))
  (c c c c c nil nil nil))

(deftest find-list.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (find 'c '(a b c d e c a) :start i :end j)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-list.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (find 'c '(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil c c c c c)
   (nil c c c c c)
   (c c c c c)
   (nil nil c c)
   (nil c c)
   (c c)
   (nil)))

(deftest find-list.11
  (find 5 '(1 2 3 4 5 6 4 8) :key #'1+)
  4)

(deftest find-list.12
  (find 5 '(1 2 3 4 5 6 4 8) :key '1+)
  4)

(deftest find-list.13
  (find 5 '(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  4)

(deftest find-list.14
  (find 'a '(a a b a c e d a f a) :test (complement #'eql))
  b)

(deftest find-list.15
  (find 'a '(a a b a c e d a f a) :test (complement #'eql)
            :from-end t)
  f)

(deftest find-list.16
  (find 'a '(a a b a c e d a f a) :test-not #'eql)
  b)

(deftest find-list.17
  (find 'a '(a a b a c e d a f a) :test-not 'eql
            :from-end t)
  f)

(deftest find-list.18
  (find 'a '(a a b a c e d a f a) :test-not 'eql)
  b)

(deftest find-list.19
  (find 'a '(a a b a c e d a f a) :test-not #'eql
            :from-end t)
  f)

(deftest find-list.20
  (find 'a '(a a b a c e d a f a) :test-not #'eql)
  b)

(deftest find-list.21
  (find 'a '(a a b a c e d a f a) :test #'eql
            :start 2)
  a)

(deftest find-list.22
  (find 'a '(a a b a c e d a f a) :test #'eql
            :start 2 :end nil)
  a)

(deftest find-list.23
  (find 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5)
  b)

(deftest find-list.24
  (find 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5 :from-end t)
  c)

;; (deftest find-list.25
;;   (find "ab" '("a" #(#\b #\a) #(#\a #\b #\c) #(#\a #\b) #(#\d #\e) f) :test #'equalp)
;;   #(#\a #\b))

(deftest find-list.26
  (find 'a '((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  (a b c))

(deftest find-list.27
  (find 'a '((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
            :start 3)
  (a b))

(deftest find-list.28
  (find 'a '((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
            :start 2 :from-end t)
  (a b))

(deftest find-list.29
  (find 10 '(1 2 3 8 20 3 1 21 3) :test #'<)
  20)

(deftest find-list.30
  (find 10 '(1 2 3 8 20 3 1 21 3) :test-not #'>=)
  20)
