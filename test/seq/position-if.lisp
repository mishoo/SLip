;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 22:08:57 2002
;;;; Contains: Tests for POSITION-IF

(in-package :sl-test)

(deftest position-if-list.1
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.2
  (position-if 'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.3
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-list.4
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-list.5
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-list.6
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-list.7
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-list.8
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-list.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-list.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-list.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-list.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

;;; Vector tests

(deftest position-if-vector.1
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.2
  (position-if 'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.3
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-vector.4
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-vector.5
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-vector.6
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-vector.7
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-vector.8
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-vector.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-vector.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-vector.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

;; (deftest position-if-vector.13
;;   (let ((a (make-array '(10) :initial-contents '(1 3 1 4 3 1 2 1 8 9)
;;                        :fill-pointer 5)))
;;     (flet ((%f (x) (eql x 1)))
;;       (values (position-if #'%f a)
;;               (position-if #'%f a :from-end t))))
;;   0 2)

;; (deftest position-if-vector.14
;;   (let* ((v1 #(x x x a b 1 d a b 2 d y y y y y))
;;          (v2 (make-array '(8) :displaced-to v1
;;                         :displaced-index-offset 3)))
;;     (values (position-if #'integerp v2)
;;             (position-if #'integerp v2 :from-end t)))
;;   2 6)
