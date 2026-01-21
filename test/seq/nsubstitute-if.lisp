;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 18:56:41 2002
;;;; Contains: Tests for NSUBSTITUTE-IF

(in-package :sl-user)

(deftest nsubstitute-if-list.1
  (nsubstitute-if 'b 'identity nil)
  nil)

(deftest nsubstitute-if-list.2
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x) x)
  (b b b c))

(deftest nsubstitute-if-list.3
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count nil))
  (b b b c))

(deftest nsubstitute-if-list.4
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 2))
  (b b b c))

(deftest nsubstitute-if-list.5
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 1))
  (b b a c))

(deftest nsubstitute-if-list.6
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 0))
  (a b a c))

(deftest nsubstitute-if-list.7
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count -1))
  (a b a c))

(deftest nsubstitute-if-list.8
  (nsubstitute-if 'b (is-eql-p 'a) nil :from-end t)
  nil)

(deftest nsubstitute-if-list.9
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :from-end t))
  (b b b c))

(deftest nsubstitute-if-list.10
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :from-end t :count nil))
  (b b b c))

(deftest nsubstitute-if-list.11
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 2 :from-end t))
  (b b b c))

(deftest nsubstitute-if-list.12
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 1 :from-end t))
  (a b b c))

(deftest nsubstitute-if-list.13
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 0 :from-end t))
  (a b a c))

(deftest nsubstitute-if-list.14
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count -1 :from-end t))
  (a b a c))

(deftest nsubstitute-if-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :from-end t)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :count c)))
                      (equal y (nconc (make-list i :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-if-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :count c :from-end t)))
                      (equal y (nconc (make-list (- j c) :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 j) :initial-element 'a)))))))
  t)

;;; Tests on vectors

(deftest nsubstitute-if-vector.1
  (let ((x #())) (nsubstitute-if 'b (is-eql-p 'a) x))
  #())

(deftest nsubstitute-if-vector.2
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x))
  #(b b b c))

(deftest nsubstitute-if-vector.3
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count nil) x)
  #(b b b c))

(deftest nsubstitute-if-vector.4
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 2))
  #(b b b c))

(deftest nsubstitute-if-vector.5
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 1))
  #(b b a c))

(deftest nsubstitute-if-vector.6
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 0))
  #(a b a c))

(deftest nsubstitute-if-vector.7
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count -1))
  #(a b a c))

(deftest nsubstitute-if-vector.8
  (let ((x #())) (nsubstitute-if 'b (is-eql-p 'a) x :from-end t))
  #())

(deftest nsubstitute-if-vector.9
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :from-end t))
  #(b b b c))

(deftest nsubstitute-if-vector.10
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :from-end t :count nil))
  #(b b b c))

(deftest nsubstitute-if-vector.11
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 2 :from-end t))
  #(b b b c))

(deftest nsubstitute-if-vector.12
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 1 :from-end t))
  #(a b b c))

(deftest nsubstitute-if-vector.13
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count 0 :from-end t))
  #(a b a c))

(deftest nsubstitute-if-vector.14
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if 'b (is-eql-p 'a) x :count -1 :from-end t))
  #(a b a c))

;; (deftest nsubstitute-if-vector.15
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (let* ((orig #(a a a a a a a a a a))
;;                      (x (copy-seq orig))
;;                      (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j)))
;;                 (equalp y (concatenate 'simple-vector
;;                                        (make-array i :initial-element 'a)
;;                                        (make-array (- j i) :initial-element 'x)
;;                                        (make-array (- 10 j) :initial-element 'a))))))
;;   t)

;; (deftest nsubstitute-if-vector.16
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (let* ((orig #(a a a a a a a a a a))
;;                      (x (copy-seq orig))
;;                      (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :from-end t)))
;;                 (equalp y (concatenate 'simple-vector
;;                                        (make-array i :initial-element 'a)
;;                                        (make-array (- j i) :initial-element 'x)
;;                                        (make-array (- 10 j) :initial-element 'a))))))
;;   t)

;; (deftest nsubstitute-if-vector.17
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (loop for c from 0 to (- j i) always
;;                     (let* ((orig #(a a a a a a a a a a))
;;                            (x (copy-seq orig))
;;                            (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :count c)))
;;                       (equalp y (concatenate 'simple-vector
;;                                              (make-array i :initial-element 'a)
;;                                              (make-array c :initial-element 'x)
;;                                              (make-array (- 10 (+ i c)) :initial-element 'a)))))))
;;   t)

;; (deftest nsubstitute-if-vector.18
;;   (loop for i from 0 to 9 always
;;         (loop for j from i to 10 always
;;               (loop for c from 0 to (- j i) always
;;                     (let* ((orig #(a a a a a a a a a a))
;;                            (x (copy-seq orig))
;;                            (y (nsubstitute-if 'x (is-eql-p 'a) x :start i :end j :count c :from-end t)))
;;                       (equalp y (concatenate 'simple-vector
;;                                              (make-array (- j c) :initial-element 'a)
;;                                              (make-array c :initial-element 'x)
;;                                              (make-array (- 10 j) :initial-element 'a)))))))
;;   t)

;; (deftest nsubstitute-if-vector.28
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute-if 'z (is-eql-p 'a) x)))
;;     result)
;;   #(z b z c b))

;; (deftest nsubstitute-if-vector.29
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute-if 'z (is-eql-p 'a) x :from-end t)))
;;     result)
;;   #(z b z c b))

;; (deftest nsubstitute-if-vector.30
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute-if 'z (is-eql-p 'a) x :count 1)))
;;     result)
;;   #(z b a c b))

;; (deftest nsubstitute-if-vector.31
;;   (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
;;                        :fill-pointer 5))
;;          (result (nsubstitute-if 'z (is-eql-p 'a) x :from-end t :count 1)))
;;     result)
;;   #(a b z c b))

;; (deftest nsubstitute-if-vector.32
;;   (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
;;          (v2 (make-array '(8) :displaced-to v1
;;                          :displaced-index-offset 3)))
;;     (values
;;      (nsubstitute-if 'x (is-eql-p 'c) v2 :count 1)
;;      v1))
;;   #(d a b x d a b c)
;;   #(a b c d a b x d a b c d a b c d))

;; (deftest nsubstitute-if-vector.33
;;   (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
;;          (v2 (make-array '(8) :displaced-to v1
;;                          :displaced-index-offset 3)))
;;     (values
;;      (nsubstitute-if 'x (is-eql-p 'c) v2 :count 1 :from-end t)
;;      v1))
;;   #(d a b c d a b x)
;;   #(a b c d a b c d a b x d a b c d))
