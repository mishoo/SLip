;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 21:15:33 2002
;;;; Contains: Tests for SUBSTITUTE

(in-package :sl-user)

(deftest substitute-list.1
  (let ((x '())) (values (substitute 'b 'a x) x))
  nil nil)

(deftest substitute-list.2
  (let ((x '(a b a c))) (values (substitute 'b 'a x) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.3
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.4
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 2) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.5
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 1) x))
  (b b a c)
  (a b a c))

(deftest substitute-list.6
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 0) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.7
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count -1) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.8
  (let ((x '())) (values (substitute 'b 'a x :from-end t) x))
  nil nil)

(deftest substitute-list.9
  (let ((x '(a b a c))) (values (substitute 'b 'a x :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.10
  (let ((x '(a b a c))) (values (substitute 'b 'a x :from-end t :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.11
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 2 :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.12
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 1 :from-end t) x))
  (a b b c)
  (a b a c))

(deftest substitute-list.13
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 0 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.14
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count -1 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j :from-end t)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c)))
                      (and (equal orig x)
                           (equal y (nconc (make-list i :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c :from-end t)))
                      (and (equal orig x)
                           (equal y (nconc (make-list (- j c) :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 j) :initial-element 'a))))))))
  t)

(deftest substitute-list.19
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (substitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    (and (equal orig x)
         result))
  (1 2 x x x x x 8 9))

(deftest substitute-list.20
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    (and (equal orig x)
         result))
  (1 2 x 4 5 6 7 8 9))


(deftest substitute-list.21
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    (and (equal orig x)
         result))
  (1 2 3 4 5 6 7 x 9))

(deftest substitute-list.22
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    (and (equal orig x)
         result))
  (1 2 x 4 5 6 7 8 9))


(deftest substitute-list.23
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    (and (equal orig x)
         result))
  (1 2 3 4 5 6 7 x 9))

(deftest substitute-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car)))
    (and (equal orig x)
         result))
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :start 1 :end 5)))
    (and (equal orig x)
         result))
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-list.26
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test (complement #'eql))))
    (and (equal orig x)
         result))
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest substitute-list.27
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test-not #'eql)))
    (and (equal orig x)
         result))
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))
