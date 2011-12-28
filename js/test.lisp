;; `(a b ,(+ 3 4) ,@(list 5 6 7) t nil)

;; (set! qq-expand-list
;;       (lambda (x)
;;         (if (consp x)
;;             (if (eq (car x) 'qq-unquote)
;;                 (list 'list (cadr x))
;;                 (if (eq (car x) 'qq-splice)
;;                     (cadr x)
;;                     (if (eq (car x) 'quasiquote)
;;                         (qq-expand-list (qq-expand (cadr x)))
;;                         (list 'list (list 'append
;;                                           (qq-expand-list (car x))
;;                                           (qq-expand (cdr x)))))))
;;             (list 'quote (list x)))))

;; (set! qq-expand
;;       (lambda (x)
;;         (if (consp x)
;;             (if (eq (car x) 'qq-unquote)
;;                 (cadr x)
;;                 (if (eq (car x) 'quasiquote)
;;                     (qq-expand (qq-expand (cadr x)))
;;                     (list 'append
;;                           (qq-expand-list (car x))
;;                           (qq-expand (cdr x)))))
;;             (list 'quote x))))

;; (defmacro quasiquote (thing)
;;   (qq-expand thing))

;; (defmacro defun (name args body)
;;   `(set! ,name (lambda ,args ,body)))

;; (defun crap (a b)
;;   (+ a b))

;; (clog (crap 1 2))


;; ((lambda (sum)
;;    (set! sum (lambda (n)
;;                (if (= n 0) 0
;;                    (+ n (sum (- n 1))))))
;;    (clog (sum 100000)))
;;  nil)

;; ((lambda (cont n)
;;    (clog (+ "//" ((lambda (k)
;;                     (set! cont k) n) (c/c))))
;;    (if (> n 0)
;;        (progn
;;          (set! n (- n 1))
;;          (cont (* n 2))))
;;    n)
;;  nil 10)
