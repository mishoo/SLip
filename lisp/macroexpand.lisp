;;; based on “Some Useful Lisp Algorithms: Part 2” by Richard C. Waters

(defpackage :sl-mexp
  (:use :sl))

(in-package :sl-mexp)

(import '(macroexpand-all) :sl)
(export '(macroexpand-all) :sl)

;; forward declarations to avoid warnings
(defun all-mexp ())
(defun funcall-mexp ())

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
            (t (if (and (symbolp (car f))
                        (%macro (car f)))
                   (rec (macroexpand-1 f) t)
                   (rec f nil)))))))

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

(defun lambda-mexp (f)
  `(,(car f)
    ,(mapcar #'arg-mexp (cadr f))
    ,@(all-mexp (cddr f))))

(defun all-lambda-mexp (list)
  (mapcar #'lambda-mexp list))

(defun flet-mexp (f)
  `(,(car f)
    ,(all-lambda-mexp (cadr f))
    ,@(all-mexp (cddr f))))

(foreach `((block           ,#'block-mexp)
           (catch           ,#'funcall-mexp)
           (flet            ,#'flet-mexp)
           (labels          ,#'flet-mexp)
           (function        ,#'funcall-mexp)
           (go              ,#'quote-mexp)
           (if              ,#'funcall-mexp)
           (lambda          ,#'lambda-mexp)
           (let             ,#'let-mexp)
           (let*            ,#'let-mexp)
           (progn           ,#'funcall-mexp)
           (quote           ,#'quote-mexp)
           (return-from     ,#'block-mexp)
           (setq            ,#'funcall-mexp)
           (tagbody         ,#'funcall-mexp)
           (throw           ,#'funcall-mexp)
           (unwind-protect  ,#'funcall-mexp))
         (lambda (x)
           (%set-symbol-prop (car x) "MEXP" (cadr x))))

(defun macroexpand-all (f)
  (mexp f))
