(defpackage :sl-benchmarks
  (:use :sl))

(in-package :sl-benchmarks)

(defun test1 (thunk)
  (let ((t1 (get-internal-run-time)))
    (funcall thunk)
    (- (get-internal-run-time) t1)))

(defun %test (thunk times)
  (loop repeat times
        for time = (test1 thunk)
        summing time into total
        ;; for total = 0 then (+ total time)
        minimizing time into min
        maximizing time into max
        finally (return (list min max (/ total times)))))

(defmacro test (form &key (times 5))
  `(%test (lambda () ,form) ,times))

(defun fib (n)
  (let fib ((n n))
    (if (< n 2)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))