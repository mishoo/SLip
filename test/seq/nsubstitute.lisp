;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 16:56:48 2002
;;;; Contains: Tests for NSUBSTITUTE

(in-package :sl-test)

(deftest nsubstitute-list.1
  (nsubstitute 'b 'a nil)
  nil)

(deftest nsubstitute-list.2
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x) x)
  (b b b c))

(deftest nsubstitute-list.3
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count nil))
  (b b b c))

(deftest nsubstitute-list.4
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 2))
  (b b b c))

(deftest nsubstitute-list.5
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 1))
  (b b a c))

(deftest nsubstitute-list.6
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 0))
  (a b a c))

(deftest nsubstitute-list.7
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count -1))
  (a b a c))

(deftest nsubstitute-list.8
  (nsubstitute 'b 'a nil :from-end t)
  nil)

(deftest nsubstitute-list.9
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :from-end t))
  (b b b c))

(deftest nsubstitute-list.10
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :from-end t :count nil))
  (b b b c))

(deftest nsubstitute-list.11
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 2 :from-end t))
  (b b b c))

(deftest nsubstitute-list.12
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 1 :from-end t))
  (a b b c))

(deftest nsubstitute-list.13
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 0 :from-end t))
  (a b a c))

(deftest nsubstitute-list.14
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count -1 :from-end t))
  (a b a c))

(deftest nsubstitute-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j :from-end t)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c)))
                      (equal y (nconc (make-list i :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c :from-end t)))
                      (equal y (nconc (make-list (- j c) :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest nsubstitute-list.19
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    result)
  (1 2 x x x x x 8 9))

(deftest nsubstitute-list.20
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    result)
  (1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-list.21
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    result)
  (1 2 3 4 5 6 7 x 9))

(deftest nsubstitute-list.22
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    result)
  (1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-list.23
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    result)
  (1 2 3 4 5 6 7 x 9))

;;; Tests on vectors

(deftest nsubstitute-vector.1
  (let ((x #())) (values (nsubstitute 'b 'a x) x))
  #() #())

(deftest nsubstitute-vector.2
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x))
  #(b b b c))

(deftest nsubstitute-vector.3
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count nil) x)
  #(b b b c))

(deftest nsubstitute-vector.4
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 2))
  #(b b b c))

(deftest nsubstitute-vector.5
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 1))
  #(b b a c))

(deftest nsubstitute-vector.6
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 0))
  #(a b a c))

(deftest nsubstitute-vector.7
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count -1))
  #(a b a c))

(deftest nsubstitute-vector.8
  (let ((x #())) (nsubstitute 'b 'a x :from-end t))
  #())

(deftest nsubstitute-vector.9
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :from-end t))
  #(b b b c))

(deftest nsubstitute-vector.10
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :from-end t :count nil))
  #(b b b c))

(deftest nsubstitute-vector.11
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 2 :from-end t))
  #(b b b c))

(deftest nsubstitute-vector.12
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 1 :from-end t))
  #(a b b c))

(deftest nsubstitute-vector.13
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 0 :from-end t))
  #(a b a c))

(deftest nsubstitute-vector.14
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count -1 :from-end t))
  #(a b a c))

;; (deftest nsubstitute-vector.15
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (let* ((orig #(a a a a a a a a a a))
;;                      (x (copy-seq orig))
;;                      (y (nsubstitute 'x 'a x :start i :end j)))
;;                 (equalp y (concatenate 'simple-vector
;;                                        (make-array i :initial-element 'a)
;;                                        (make-array (- j i) :initial-element 'x)
;;                                        (make-array (- 10 j) :initial-element 'a))))))
;;   t)

;; (deftest nsubstitute-vector.16
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (let* ((orig #(a a a a a a a a a a))
;;                      (x (copy-seq orig))
;;                      (y (nsubstitute 'x 'a x :start i :end j :from-end t)))
;;                 (equalp y (concatenate 'simple-vector
;;                                        (make-array i :initial-element 'a)
;;                                        (make-array (- j i) :initial-element 'x)
;;                                        (make-array (- 10 j) :initial-element 'a))))))
;;   t)

;; (deftest nsubstitute-vector.17
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (loop for c from 0 to (- j i) always
;;                     (let* ((orig #(a a a a a a a a a a))
;;                            (x (copy-seq orig))
;;                            (y (nsubstitute 'x 'a x :start i :end j :count c)))
;;                       (equalp y (concatenate 'simple-vector
;;                                              (make-array i :initial-element 'a)
;;                                              (make-array c :initial-element 'x)
;;                                              (make-array (- 10 (+ i c)) :initial-element 'a)))))))
;;   t)

;; (deftest nsubstitute-vector.18
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (loop for c from 0 to (- j i) always
;;                     (let* ((orig #(a a a a a a a a a a))
;;                            (x (copy-seq orig))
;;                            (y (nsubstitute 'x 'a x :start i :end j :count c :from-end t)))
;;                       (equalp y (concatenate 'simple-vector
;;                                              (make-array (- j c) :initial-element 'a)
;;                                              (make-array c :initial-element 'x)
;;                                              (make-array (- 10 j) :initial-element 'a)))))))
;;   t)

(deftest nsubstitute-vector.19
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    result)
  #(1 2 x x x x x 8 9))

(deftest nsubstitute-vector.20
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    result)
  #(1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-vector.21
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    result)
  #(1 2 3 4 5 6 7 x 9))

(deftest nsubstitute-vector.22
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    result)
  #(1 2 x 4 5 6 7 8 9))

(deftest nsubstitute-vector.23
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    result)
  #(1 2 3 4 5 6 7 x 9))

;; (deftest nsubstitute-vector.28
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute 'z 'a x)))
;;     result)
;;   #(z b z c b))

;; (deftest nsubstitute-vector.29
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute 'z 'a x :from-end t)))
;;     result)
;;   #(z b z c b))

;; (deftest nsubstitute-vector.30
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute 'z 'a x :count 1)))
;;     result)
;;   #(z b a c b))

;; (deftest nsubstitute-vector.31
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute 'z 'a x :from-end t :count 1)))
;;     result)
;;   #(a b z c b))
