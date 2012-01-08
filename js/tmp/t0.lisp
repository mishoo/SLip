(if (and 3 1 'a 'b 'c (+ 4 5 6))
    "true"
    "false")

(progn
  (set! crap (and 3 0 1 'a 'b 'c (+ 4 5 6)))
  t)

(and 3 0 1 'a 'b 'c (+ 4 5 6))

(set! crap (or nil 1 2 3 4 5 6))

crap

;; (if nil 1 2)
