;; ((lambda (sum)
;;    (set! sum (lambda (n)
;;                (if (= n 0) 0
;;                    (+ n (sum (- n 1))))))
;;    (clog (sum 100000)))
;;  nil)

(progn
  (clog 1)
  ((lambda (k)
     (clog 3)
     (k nil)
     (clog 4)) (c/c))
  (clog 2))
