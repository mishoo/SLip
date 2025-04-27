;;; based on “Some Useful Lisp Algorithms: Part 2” by Richard C. Waters

(in-package :sl)

(export '(macroexpand-all))

(defpackage :sl-mexp
  (:use :sl :%))

(in-package :sl-mexp)

(defun mexp (f)
  (let (m)
    (let rec ((f f)
              (flag t))
      (cond ((atom f)
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

(defun quote-mexp (f)
  f)

(defun block-mexp (f)
  `(,(car f) ,(cadr f) ,@(all-mexp (cddr f))))

(defun bind-mexp (p)
  (if (and (consp p) (consp (cdr p)))
      (list (car p) (mexp (cadr p)))
      p))

(defun let-mexp (f)
  `(,(car f)
    ,(if (listp (cadr f))
         (mapcar #'bind-mexp (cadr f))
         (cadr f))
    ,@(all-mexp (cddr f))))

(defun arg-mexp (arg)
  (if (and (consp arg) (consp (cdr arg)))
      (list* (car arg) (mexp (cadr arg)) (cddr arg))
      arg))

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
    ,@(all-mexp (cddr f))))

(defun fn-mexp (f)
  `(,(car f)
    ,(mexp (cadr f))
    ,(lambda-list-mexp (caddr f))
    ,@(all-mexp (cdddr f))))

(defun all-lambda-mexp (list)
  (mapcar #'lambda-mexp list))

(defun flet-mexp (f)
  `(,(car f)
    ,(all-lambda-mexp (cadr f))
    ,@(all-mexp (cddr f))))

(defun flatten-progn (forms)
  (let dig ((forms forms)
            (result nil)
            (rest nil))
    (cond
      ((null forms)
       (if rest
           (dig (car rest) result (cdr rest))
           (nreverse result)))
      ((and (consp (car forms))
            (eq 'progn (caar forms)))
       (dig (cdar forms)
            result
            (cons (cdr forms) rest)))
      (t
       (dig (cdr forms)
            (cons (car forms) result)
            rest)))))

(defun progn-mexp (f)
  (if (cddr f)
      (flatten-progn `(progn ,@(all-mexp (cdr f))))
      (mexp (cadr f))))

(defun macrolet-mexp (f)
  (let ((defs (nreverse
               (mapcar (lambda (md)
                         (let ((name (car md))
                               (func (compile (list* '%:%fn md))))
                           (list name :macro func)))
                       (cadr f)))))
    (let ((%:*compiler-env* (%:extend-compiler-env `(:macros ,defs))))
      (if (cdddr f)
          `(progn ,@(all-mexp (cddr f)))
          (mexp (caddr f))))))

(foreach `((block           ,#'block-mexp)
           (catch           ,#'funcall-mexp)
           (flet            ,#'flet-mexp)
           (labels          ,#'flet-mexp)
           (function        ,#'funcall-mexp)
           (go              ,#'quote-mexp)
           (if              ,#'funcall-mexp)
           (lambda          ,#'lambda-mexp)
           (%:%fn           ,#'fn-mexp)
           (let             ,#'let-mexp)
           (let*            ,#'let-mexp)
           (progn           ,#'progn-mexp)
           (quote           ,#'quote-mexp)
           (return-from     ,#'block-mexp)
           (setq            ,#'funcall-mexp)
           (tagbody         ,#'funcall-mexp)
           (throw           ,#'funcall-mexp)
           (unwind-protect  ,#'funcall-mexp)
           (macrolet        ,#'macrolet-mexp))
         (lambda (x)
           (%set-symbol-prop (car x) "MEXP" (cadr x))))

(defun macroexpand-all (f)
  (let ((%:*compiler-env* (%:make-compiler-env)))
    (mexp f)))
