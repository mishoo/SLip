(progn
  (let ((a 10)
        (b 20)
        (c 30)
        (kont)
        (n 0))
    (let ((a 'a)
          (b 'b)
          (c 'c))
      (with-cc k
        (set! kont k))
      (clog "inside" n)
      (clog a b c))
    (clog "outside")
    (clog a b c)
    (if (= n 0)
        (progn
          (set! n (+ n 1))
          (kont nil)))))
