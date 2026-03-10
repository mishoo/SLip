;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 07:49:49 2002
;;;; Contains: Tests for POSITION

(in-package :sl-test)

(deftest position-list.1
  (position 'c '(a b c d e c a))
  2)

(deftest position-list.2
  (position 'c '(a b c d e c a) :from-end t)
  5)

(deftest position-list.3
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-list.4
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-list.5
  (loop for i from 7 downto 0 collect
        (position 'c '(a b c d e c a) :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-list.6
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-list.7
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-list.8
  (loop for i from 7 downto 0 collect
        (position 'c '(a b c d e c a) :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-list.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c '(a b c d e c a) :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-list.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c '(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-list.11
  (position 5 '(1 2 3 4 5 6 4 8) :key #'1+)
  3)

(deftest position-list.12
  (position 5 '(1 2 3 4 5 6 4 8) :key '1+)
  3)

(deftest position-list.13
  (position 5 '(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  6)

(deftest position-list.14
  (position 'a '(a a b a c e d a f a) :test (complement #'eql))
  2)

(deftest position-list.15
  (position 'a '(a a b a c e d a f a) :test (complement #'eql)
            :from-end t)
  8)

(deftest position-list.16
  (position 'a '(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-list.17
  (position 'a '(a a b a c e d a f a) :test-not 'eql
            :from-end t)
  8)

(deftest position-list.18
  (position 'a '(a a b a c e d a f a) :test-not 'eql)
  2)

(deftest position-list.19
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :from-end t)
  8)

(deftest position-list.20
  (position 'a '(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-list.21
  (position 'a '(a a b a c e d a f a) :test #'eql
            :start 2)
  3)

(deftest position-list.22
  (position 'a '(a a b a c e d a f a) :test #'eql
            :start 2 :end nil)
  3)

(deftest position-list.23
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-list.24
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-list.25
  (position '(a b) '(a (b a) (a b c) (a b) (d e) f) :test #'equal)
  3)

(deftest position-list.26
  (position 'a '((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  2)

(deftest position-list.27
  (position 'a '((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
            :start 3)
  4)

(deftest position-list.28
  (position 'a '((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
            :start 2 :from-end t)
  4)

(deftest position-list.29
  (position 10 '(1 4 8 10 15 20) :test #'<)
  4)

(deftest position-list.30
  (position 10 '(1 4 8 10 15 20) :test-not #'>=)
  4)

;;; Tests on vectors

(deftest position-vector.1
  (position 'c #(a b c d e c a))
  2)

(deftest position-vector.2
  (position 'c #(a b c d e c a) :from-end t)
  5)

(deftest position-vector.3
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-vector.4
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-vector.5
  (loop for i from 7 downto 0 collect
        (position 'c #(a b c d e c a) :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-vector.6
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-vector.7
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-vector.8
  (loop for i from 7 downto 0 collect
        (position 'c #(a b c d e c a) :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-vector.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c #(a b c d e c a) :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-vector.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c #(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-vector.11
  (position 5 #(1 2 3 4 5 6 4 8) :key #'1+)
  3)

(deftest position-vector.12
  (position 5 #(1 2 3 4 5 6 4 8) :key '1+)
  3)

(deftest position-vector.13
  (position 5 #(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  6)

(deftest position-vector.14
  (position 'a #(a a b a c e d a f a) :test (complement #'eql))
  2)

(deftest position-vector.15
  (position 'a #(a a b a c e d a f a) :test (complement #'eql)
            :from-end t)
  8)

(deftest position-vector.16
  (position 'a #(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-vector.17
  (position 'a #(a a b a c e d a f a) :test-not 'eql
            :from-end t)
  8)

(deftest position-vector.18
  (position 'a #(a a b a c e d a f a) :test-not 'eql)
  2)

(deftest position-vector.19
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :from-end t)
  8)

(deftest position-vector.20
  (position 'a #(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-vector.21
  (position 'a #(a a b a c e d a f a) :test #'eql
            :start 2)
  3)

(deftest position-vector.22
  (position 'a #(a a b a c e d a f a) :test #'eql
            :start 2 :end nil)
  3)

(deftest position-vector.23
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-vector.24
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-vector.25
  (position '(a b) #(a (b a) (a b c) (a b) (d e) f) :test #'equal)
  3)

(deftest position-vector.26
  (position 'a #((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  2)

(deftest position-vector.27
  (position 'a #((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
            :start 3)
  4)

(deftest position-vector.28
  (position 'a #((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
            :start 2 :from-end t)
  4)

;; (deftest position-vector.29
;;   (position 'a (make-array '(10) :initial-contents '(b b b b b a a a a a)
;;                            :fill-pointer 5))
;;   nil)

;; (deftest position-vector.30
;;   (position 'a (make-array '(10) :initial-contents '(b b b b a a a a a a)
;;                            :fill-pointer 5))
;;   4)

;; (deftest position-vector.31
;;   (position 'a (make-array '(10) :initial-contents '(b a b b a a a a a a)
;;                            :fill-pointer 5)
;;             :from-end t)
;;   4)

(deftest position-vector.32
  (position 10 #(1 4 8 10 15 20) :test #'<)
  4)

(deftest position-vector.33
  (position 10 #(1 4 8 10 15 20) :test-not #'>=)
  4)

;; (deftest position-vector.34
;;   (let* ((v1 #(x x x a b c d a b c d y y y y y))
;;          (v2 (make-array '(8) :displaced-to v1
;;                          :displaced-index-offset 3)))
;;     (values (position 'c v2)
;;             (position 'c v2 :from-end t)))
;;   2 6)
