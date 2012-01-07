(%special '*foo*)
(%special 'output)

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



(let* ((pak (%make-package "FOO"))
       (sym (%intern "CRAP" pak)))
  (eq (%find-symbol "CRAP" pak) sym))


(let ((str "abcdefgh"))
  (labels ((rec (i)
             (when (< i (length str))
               (clog (elt str i))
               (rec (+ i 1)))))
    (rec 0)))

(defmacro while (cond . body)
  (let ((rec (gensym)))
    `(labels ((,rec ()
                (when ,cond
                  ,@body
                  (,rec))))
       (,rec))))

(defmacro with-stream (input . body)
  (let ((str (gensym))
        (idx (gensym)))
    `(let ((,str ,input)
           (,idx 0))
       (flet ((peek ()
                (elt ,str ,idx))
              (next ()
                (let ((ch (elt ,str ,idx)))
                  (when ch
                    (set! ,idx (+ 1 ,idx))
                    ch))))
         ,@body))))

(with-stream "check this out"
  (while (peek)
    (clog (next))))
