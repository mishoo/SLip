`(a b ,(+ 3 4) ,@(list 5 6 7) t nil)

;; ((lambda (sum)
;;    (set! sum (lambda (n)
;;                (if (= n 0) 0
;;                    (+ n (sum (- n 1))))))
;;    (clog (sum 100000)))
;;  nil)

;; ((lambda (cont n)
;;    (clog (+ "//" ((lambda (k)
;;                     (set! cont k) n) (c/c))))
;;    (if (> n 0)
;;        (progn
;;          (set! n (- n 1))
;;          (cont (* n 2))))
;;    n)
;;  nil 10)
