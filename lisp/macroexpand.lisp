;;; based on “Some Useful Lisp Algorithms: Part 2” by Richard C. Waters

(in-package :sl)

(export '(macroexpand-all))

(defpackage :sl-mexp
  (:use :sl :%))

(in-package :sl-mexp)

(defun %with-local-vars (names thunk)
  (if names
      (let ((defs (nreverse
                   (mapcar (lambda (name)
                             (list name :var))
                           names))))
        (let ((%:*compiler-env* (%:extend-compiler-env `(:lex ,(as-vector defs)))))
          (funcall thunk)))
      (funcall thunk)))

(defmacro with-local-vars (names &body body)
  `(%with-local-vars ,names (lambda () ,@body)))

(defun mexp (f)
  (let (m)
    (let rec ((f f)
              (flag t))
      (cond ((and f (symbolp f)
                  (not (eq t f)))
             (cond
               ((aif (%:find-var-in-compiler-env f)
                     (when (eq :smac (caddr it))
                       (mexp (cadddr it)))))
               (t f)))
            ((atom f)
             f)
            ((not (symbolp (car f)))
             (all-mexp f))
            ((setq m (%get-symbol-prop (car f) "MEXP"))
             (funcall m f))
            ((not flag)
             (funcall-mexp f))
            ((not (symbolp (car f)))
             (rec f nil))
            (t
             (let ((exp (macroexpand-1 f)))
               (rec exp (not (eq exp f)))))))))

(defun all-mexp (list)
  (let rec ((f list)
            (r '()))
    (if (atom f)
        (nreconc r f)
        (rec (cdr f)
             (cons (mexp (car f)) r)))))

(defun funcall-mexp (f)
  `(,(car f) ,@(all-mexp (cdr f))))

(defun setq-mexp (f)
  (let ((name (cadr f)))
    (cond
      ((aif (%:find-var-in-compiler-env name)
            (when (eq :smac (caddr it))
              (mexp `(setf ,(cadddr it) ,@(cddr f))))))
      ((funcall-mexp f)))))

(defun tagbody-mexp (f)
  `(,(car f) ,@(let rec ((f (cdr f))
                         (r '()))
                 (if (atom f)
                     (nreconc r f)
                     (rec (cdr f)
                          (cons (let ((exp (mexp (car f))))
                                  (if (and (not (eq exp (car f)))
                                           (or (symbolp exp)
                                               (numberp exp)))
                                      `(progn ,exp)
                                      exp))
                                r))))))

(defun quote-mexp (f)
  f)

(defun block-mexp (f)
  `(,(car f) ,(cadr f) ,@(all-mexp (cddr f))))

(defun bind-mexp (p)
  (if (and (consp p) (consp (cdr p)))
      (list (car p) (mexp (cadr p)))
      p))

(defun binding-name (x)
  (if (consp x)
      (car x)
      x))

(defun let-mexp (f)
  (cond
    ((null (cadr f))
     (progn-mexp `(progn ,@(all-mexp (cddr f)))))
    ((listp (cadr f))
     ;; normal let
     `(,(car f)
       ,(mapcar #'bind-mexp (cadr f))
       ,@(with-local-vars (mapcar #'binding-name (cadr f))
           (all-mexp (cddr f)))))
    (t
     ;; named let
     `(,(car f)
       ,(cadr f)
       ,(mapcar #'bind-mexp (caddr f))
       ,@(with-local-vars (mapcar #'binding-name (caddr f))
           (all-mexp (cdddr f)))))))

(defun mvb-mexp (f)
  `(,(car f)
    ,(cadr f)
    ,(mexp (caddr f))
    ,@(with-local-vars (cadr f)
        (all-mexp (cdddr f)))))

(defun arg-mexp (arg)
  (if (and (consp arg) (consp (cdr arg)))
      (list* (car arg) (mexp (cadr arg)) (cddr arg))
      arg))

(defun lambda-list-names (lst)
  (let rec ((lst lst)
            (ret nil))
    (cond
      ((null lst)
       (nreverse ret))
      ((atom lst)
       (nreconc ret (list lst)))
      ((consp (car lst))
       (rec (cdr lst) (cons (caar lst) ret)))
      ((and (symbolp (car lst))
            (not (%:lambda-keyword-p (car lst))))
       (rec (cdr lst) (cons (car lst) ret)))
      ((rec (cdr lst) ret)))))

(defun lambda-list-mexp (lst)
  (let rec ((lst lst)
            (ret nil))
    (cond
      ((not (consp lst))
       (nreconc ret lst))
      (t
       (rec (cdr lst)
            (cons (arg-mexp (car lst))
                  ret))))))

(defun lambda-mexp (f)
  `(,(car f)
    ,(lambda-list-mexp (cadr f))
    ,@(with-local-vars (lambda-list-names (cadr f))
        (all-mexp (cddr f)))))

(defun fn-mexp (f)
  `(,(car f)
    ,(mexp (cadr f))
    ,(lambda-list-mexp (caddr f))
    ,@(with-local-vars (lambda-list-names (caddr f))
        (all-mexp (cdddr f)))))

(defun all-lambda-mexp (list)
  (mapcar #'lambda-mexp list))

(defun flet-mexp (f)
  `(,(car f)
    ,(all-lambda-mexp (cadr f))
    ,@(all-mexp (cddr f))))

(defun progn-mexp (f)
  (if (cddr f)
      (%:flatten 'progn `(progn ,@(all-mexp (cdr f))))
      (mexp (cadr f))))

(defun macrolet-mexp (f)
  (let ((defs (nreverse
               (mapcar (lambda (md)
                         (let ((name (car md))
                               (args (cadr md))
                               (body (cddr md)))
                           (list name :func
                                 :macro (compile (%:macro-lambda name args body)))))
                       (cadr f)))))
    (let ((%:*compiler-env* (%:extend-compiler-env `(:lex ,(as-vector defs)))))
      (if (cdddr f)
          `(progn ,@(all-mexp (cddr f)))
          (mexp (caddr f))))))

(defun symbol-macrolet-mexp (f)
  (let ((defs (nreverse
               (mapcar (lambda (def)
                         (let ((name (car def))
                               (exp (cadr def)))
                           (list name :var :smac exp)))
                       (cadr f)))))
    (let ((%:*compiler-env* (%:extend-compiler-env `(:lex ,(as-vector defs)))))
      (progn-mexp `(progn ,@(all-mexp (cddr f)))))))

(foreach `((block                 block-mexp)
           (catch                 funcall-mexp)
           (flet                  flet-mexp)
           (labels                flet-mexp)
           (function              funcall-mexp)
           (go                    quote-mexp)
           (if                    funcall-mexp)
           (lambda                lambda-mexp)
           (%:%fn                 fn-mexp)
           (let                   let-mexp)
           (let*                  let-mexp)
           (progn                 progn-mexp)
           (quote                 quote-mexp)
           (return-from           block-mexp)
           (setq                  setq-mexp)
           (tagbody               tagbody-mexp)
           (throw                 funcall-mexp)
           (unwind-protect        funcall-mexp)
           (macrolet              macrolet-mexp)
           (symbol-macrolet       symbol-macrolet-mexp)
           (multiple-value-bind   mvb-mexp))
         (lambda (x)
           (%set-symbol-prop (car x) "MEXP" (cadr x))))

(defun macroexpand-all (f)
  (let ((%:*compiler-env* (%:make-compiler-env)))
    (mexp f)))
