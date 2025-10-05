;;;; Author:   Paul Dietz
;;;; Created:  Sun Apr 20 15:43:44 2003
;;;; Contains: Tests of SHIFTF

(in-package :sl-user)

(deftest shiftf-order.1
  (let ((x (vector 'a 'b 'c 'd 'e))
        (i 2))
    (values (shiftf (aref x (incf i)) (incf i)) x i))
  d #(a b c 4 e) 4)

(deftest shiftf-order.2
  (let ((x (vector 'a 'b 'c 'd 'e 'f 'g 'h))
        (i 2))
    (values (shiftf (aref x (incf i)) (aref x (incf i)) (incf i)) x i))
  d #(a b c e 5 f g h) 5)

(deftest shiftf.1
  (let ((x 0))
    (values
     x
     (shiftf x 1)
     x))
  0 0 1)

(deftest shiftf.2
  (let ((x 'a) (y 'b) (z 'c))
    (values
     x y z
     (shiftf x y z 'd)
     x y z))
  a b c
  a
  b c d)

(deftest shiftf.3
  (let ((x (vector 0 1 2 3)))
    (values
     (copy-seq x)
     (shiftf (aref x (aref x 0))
             (aref x (aref x 1))
             (aref x (aref x 2))
             (aref x (aref x 3))
             'foo)
     (copy-seq x)))
  #(0 1 2 3)
  0
  #(1 2 3 foo))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest shiftf.4
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (values
      (shiftf (expand-in-current-env (%m x)) y 'foo)
      x y)))
  1 2 foo)

(deftest shiftf.5
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (values
      (shiftf x (expand-in-current-env (%m y)) 'foo)
      x y)))
  1 2 foo)

(deftest shiftf.6
  (macrolet
   ((%m (z) z))
   (let ((x 1) (y 2))
     (values
      (shiftf x y (expand-in-current-env (%m 'foo)))
      x y)))
  1 2 foo)

;; #||
;; ;;;; Disabling this test.  See
;; ;;;; https://gitlab.common-lisp.net/ansi-test/ansi-test/-/issues/35
;; ;;;; for more information.  Several lisps return '(a b) and when
;; ;;;; viewed as a shift register it makes sense to have all the values
;; ;;;; from first place to be returned so that it can be used directly
;; ;;;; as the input to another shift register.

;; ;;; Test that SHIFTF returns a single value, even though the first
;; ;;; place has multiple values.
;; (deftest shiftf.7
;;   (let ((x 'a) (y 'b))
;;     (values
;;      (multiple-value-list (shiftf (values x y) (floor 10 3)))
;;      x y))
;;   (a) 3 1)
;; ||#

;;; Need to add more shiftf tests here
