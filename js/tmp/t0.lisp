((lambda (q)
   (let ((a 1))
     (let ((b 2))
       (let ((c 3))
         (let ((d 4))
           (set! maka 10)
           (set! maka (+ maka 2))
           (set! a (+ maka 1))
           (+ a b c d q))))))
 10)

(progn
  (if nil
      (clog "true")
      (clog "false"))
  (not 'foo))
