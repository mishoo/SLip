((lambda (sum)
   (set! sum (lambda (n ret)
               (if (= n 0) ret
                   (sum (- n 1) (+ ret n)))))
   (sum 100000 0))
 nil)

;; (labels ((sum (n ret)
;;            (if (= n 0) ret
;;                (sum (- n 1) (+ ret n)))))
;;   (sum 10))
