(special '*foo*)
(special 'output)

(defun output (x)
  (clog x))

(defun test ()
  (output `(foo is ,*foo*)))

(let ((mak 0)
      (kont nil))

  (let ((*foo* 1))
    (test)
    (set! *foo* 2)
    (test)
    (let ((*foo* "crap"))
      (test)
      (with-cc k
        (set! kont k))
      (output `(MMMMAAAAAK ,mak))
      (let* ((*foo* "mak")
             (oldout output)
             (output (lambda (x)
                       (oldout "===")
                       (oldout x)
                       (oldout "+++"))))
        (test))
      (test))
    (set! *foo* (+ *foo* 1))
    (test))

  (set! mak (+ mak 1))
  (when (< mak 5)
    (kont nil)))

(labels ((sum (n ret)
           (if (= n 0) ret
               (sum (- n 1) (+ ret n)))))
  (sum 100000 0))
