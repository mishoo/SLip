(if (or (+ 1 2) 'foo 'baz)
    (clog "true")
    (clog "false"))

(progn (clog (and 1 2 3 (clog "check")))
       (clog "final"))
