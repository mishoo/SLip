(load "lib/dlx.lisp")

(defpackage :sudoku
  (:use :sl :dlx))

(in-package :sudoku)

(defun sudoku-from-string (str)
  (map 'list (lambda (c)
               (ecase c
                 ((#\0 #\. #\Space) 0)
                 (#\1 1)
                 (#\2 2)
                 (#\3 3)
                 (#\4 4)
                 (#\5 5)
                 (#\6 6)
                 (#\7 7)
                 (#\8 8)
                 (#\9 9)))
       str))

(defun block-index (row col)
  (+ (* 3 (floor row 3)) (floor col 3)))

(defun i2rc (index)
  (floor index 9))

(defun rc2i (row col)
  (+ col (* row 9)))

(defun make-board (&optional contents)
  (typecase contents
    (string
     (make-array 81 :initial-contents (sudoku-from-string contents)))
    (vector
     (copy-seq contents))
    (cons
     (make-array 81 :initial-contents contents))
    (t
     (make-array 81 :initial-element 0))))

(defparameter *sudoku-full-matrix*
  (let ((matrix (make-array 729 :initial-contents (loop repeat 729 collect (make-array 324 :initial-element 0)))))
    (loop for row from 0 to 8 do
          (loop for col from 0 to 8
                for blk = (block-index row col)
                for pos = (rc2i row col) do
                (loop for digit from 0 to 8
                      for constraints = (aref matrix (+ digit (* pos 9)))
                      for constraint-row = (+ 81 digit (* 9 row))
                      for constraint-col = (+ 162 digit (* 9 col))
                      for constraint-blk = (+ 243 digit (* 9 blk)) do
                      (setf (aref constraints pos) 1
                            (aref constraints constraint-row) 1
                            (aref constraints constraint-col) 1
                            (aref constraints constraint-blk) 1))))
    matrix))

(defun sudoku-dlx-matrix ()
  (let* ((head (make-dlx-matrix *sudoku-full-matrix*))
         (cols (apply #'vector
                      (loop for col = (dlx-node-right head) then (dlx-node-right col)
                            until (eq col head)
                            collect col))))
    (let ((covered '()))
      (flet ((cover (col)
               (let ((col (aref cols col)))
                 (dlx-cover-col col)
                 (push col covered))))
        (lambda (board)
          (mapc #'dlx-uncover-col covered)
          (setf covered (list))
          (loop with board = (make-board board)
                for row from 0 to 8 do
                (loop for col from 0 to 8
                      for pos = (rc2i row col)
                      for digit = (aref board pos)
                      unless (zerop digit) do
                      (setf digit (1- digit))
                      (let ((constraint-row (+ 81 digit (* 9 row)))
                            (constraint-col (+ 162 digit (* 9 col)))
                            (constraint-blk (+ 243 digit (* 9 (block-index row col)))))
                        (cover pos)
                        (cover constraint-row)
                        (cover constraint-col)
                        (cover constraint-blk))))
          head)))))

(defparameter *sudoku-dlx-matrix* (sudoku-dlx-matrix))

(defun solve-sudoku-dlx (board &key (solcount 1) (print t) (matrix *sudoku-dlx-matrix*))
  (let* ((board (make-board board))
         (sol (search-dlx (funcall matrix board)
                          :solcount solcount
                          :printer (when print #'print-dlx-solution))))
    (cond
      (print
       (mapcar (lambda (sol)
                 (print-sudoku-dlx-solution sol board))
               sol))
      (t sol))))

(defun print-sudoku-dlx-solution (sol board)
  (let ((b (make-board)))
    (loop for r in sol do
          (multiple-value-bind (pos digit) (floor r 9)
            (setf (aref b pos) (1+ digit))))
    (loop for idx from 0 to 80
          for digit across board
          unless (zerop digit)
          do (setf (aref b idx) digit))
    (concatenate 'list b)))

(defun print-board (board &optional (out t))
  (loop for row from 0 to 8 do
        (when (and (not (zerop row))
                   (zerop (mod row 3)))
          (format out "--~%"))
        (loop for col from 0 to 8
              for i = (rc2i row col)
              for d = (elt board i)
              do (when (and (not (zerop col))
                            (zerop (mod col 3)))
                   (write-string " |" out))
              (format out "~4,' D" d))
        (format out "~&")))