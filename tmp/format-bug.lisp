(format t "~{~{~V,R~},CRAP~^~%~}~%" '((5 1000) (6 2000) (7 3000)))

(LET ((args517 (LIST '((5 1000) (6 2000)
                       (7 3000)))))
  (CATCH 'SL-FORMAT::ABORT-FORMAT-ITERATION
    (LET ((MYARGS518 (%:%POP args517)))
      (CATCH 'SL-FORMAT::ABORT-FORMAT-ITERATION
        (TAGBODY
         :LOOP
           (when MYARGS518
             (LET ((MYARGS520 (%:%POP MYARGS518)))
               (CATCH 'SL-FORMAT::ABORT-FORMAT-ITERATION
                 (TAGBODY
                  :LOOP
                    (when MYARGS520
                      (SL-FORMAT::%PRINT-INTEGER *STANDARD-OUTPUT*
                                                 NIL NIL
                                                 (%:%POP MYARGS520)
                                                 0 #\SPACE #\,
                                                 3
                                                 (%:%POP MYARGS520))
                      (GO :LOOP)))))
             (%:%STREAM-PUT *STANDARD-OUTPUT* ",CRAP")
             (unless MYARGS518
               (print "EXIT")
               (THROW 'SL-FORMAT::ABORT-FORMAT-ITERATION NIL))
             (%:%STREAM-PUT *STANDARD-OUTPUT* #\NEWLINE)
             (GO :LOOP))))
      args517)
    (SETQ args517 (PROGN
                    (%:%STREAM-PUT 
                     *STANDARD-OUTPUT* #\NEWLINE)
                    args517))))