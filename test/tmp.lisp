(labels ((sum (n ret)
           (if (= n 0) ret
               (sum (- n 1) (+ ret n)))))
  (console.print (sum 100000 0)))
