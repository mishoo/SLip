(%special 'b)

(set! stuff (lambda ()
              (clog "in stuff" b)))

(let ((a 10))
  (let* ((b a)
         (a 3)
         (c (+ b b)))
    (stuff)
    (clog (* a b c)))
  (clog a)
  (stuff))
