;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 17:42:04 2002
;;;; Contains: Tests for SUBSTITUTE-IF

(in-package :sl-user)

(deftest substitute-if-list.1
  (let ((x '())) (values (substitute-if 'b #'identity x) x))
  nil nil)

(deftest substitute-if-list.2
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.3
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.4
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.5
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 1) x))
  (b b a c)
  (a b a c))

(deftest substitute-if-list.6
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 0) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.7
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count -1) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.8
  (let ((x '())) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  nil nil)

(deftest substitute-if-list.9
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.10
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.11
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2 :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.12
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 1 :from-end t) x))
  (a b b c)
  (a b a c))

(deftest substitute-if-list.13
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 0 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.14
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count -1 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :from-end t)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :count c)))
                      (and (equal orig x)
                           (equal y (nconc (make-list i :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-if-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :count c :from-end t)))
                      (and (equal orig x)
                           (equal y (nconc (make-list (- j c) :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 j) :initial-element 'a))))))))
  t)
