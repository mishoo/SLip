(defpackage :sl-benchmarks
  (:use :sl))

(in-package :sl-benchmarks)

(defun test1 (thunk)
  (let ((t1 (get-internal-run-time)))
    (funcall thunk)
    (- (get-internal-run-time) t1)))

(defun %test (thunk times)
  (assert (> times 1) "TEST: you gotta run it at least twice, man..")
  (loop for i from 1 to times
        for time = (test1 thunk)
        do (format t "Sample ~D: ~Dms~%" i time)
        when (> i 1) sum time into total
        and minimize time into min
        and maximize time into max
        finally (return (list min max (/ total (1- times))))))

(defmacro test (form &key (times 5))
  `(%test (lambda () ,form) ,times))

(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(defun test-timers ()
  (let ((finished nil)
        (count 0))
    (labels ((timer ()
               (cond
                 (finished
                  (format t "Stopping. ~A!~%" count))
                 (t
                  (format t "Woot ~A!~%" (incf count))
                  (set-timeout 100 #'timer)))))
      (timer)
      (multiple-value-prog1 (test (fib 32))
        (setf finished 'done)))))

(defparameter x
  (loop repeat 3000 collect (random 10000)))

(defun test-fn (fn &rest args)
  (loop repeat 500 do (apply fn args)))

(defun len2 (list)
  (do ((p list (cdr p))
       (count 0 (1+ count)))
      ((null p) count)))

(defun len2* (list)
  (do* ((p list (cdr p))
        (count 0 (1+ count)))
       ((null p) count)))

(defun len2.1 (list)
  (let ((count 0))
    (tagbody
     :loop
     (when list
       (incf count)
       (setf list (cdr list))
       (go :loop)))
    count))

(defun len3 (list)
  (let rec ((p list)
            (count 0))
    (if p
        (rec (cdr p) (1+ count))
        count)))