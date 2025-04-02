(defpackage sl-tmp
  (:use :sl :%))

(in-package :sl-tmp)

(defun lambda-keyword-p (sym)
  (case sym
    ((&optional &rest &key &aux &allow-other-keys)
     t)))

(defun parse-lambda-list (args)
  (let ((required nil)
        (optional nil)
        (rest nil)
        (key nil)
        (aux nil)
        (allow-other-keys nil))
    (labels
        ((assert (p msg)
           (if p p (error msg)))

         (symp (x)
           (and x (symbolp x)
                (not (eq x t))
                (not (lambda-keyword-p x))))

         (rec (args)
           (cond
             ((null args))
             ((consp args)
              (case (car args)
                (&optional (rec-opt (cdr args)))
                (&rest (rec-rest (cdr args)))
                (&key (rec-key (cdr args)))
                (&aux (rec-aux (cdr args)))
                (otherwise
                 (rec (cdr args))
                 (assert (symp (car args)) "Symbol expected in lambda list")
                 (push (car args) required))))
             ((symp args)
              (assert (not rest) "&rest already given")
              (setq rest args))
             (t
              (error "Bad lambda list"))))

         (rec-rest (args)
           (when (cdr args)
             (case (cadr args)
               (&key (rec-key (cddr args)))
               (&aux (rec-aux (cddr args)))
               (otherwise
                (error "Bad lambda list after &rest"))))
           (setq rest (car args)))

         (rec-opt (args)
           (case (car args)
             (&rest (rec-rest (cdr args)))
             (&key (rec-key (cdr args)))
             (&aux (rec-aux (cdr args)))
             (otherwise
              (when (cdr args)
                (rec-opt (cdr args)))
              (cond
                ((consp (car args))
                 (push (car args) optional))
                ((symp (car args))
                 (push (list (car args)) optional))
                (t
                 (error "Bad &optional parameter"))))))

         (key-arg-names (arg)
           (cond
             ((consp arg)
              arg)
             ((symp arg)
              (cons (intern (symbol-name arg) :keyword) arg))
             (t
              (error "Bad &key argument name"))))

         (rec-key (args)
           (case (car args)
             (&allow-other-keys
              (setq allow-other-keys t)
              (when (cdr args)
                (assert (eq '&aux (cadr args)) "Only &aux can follow in lambda list after &allow-other-keys")
                (rec-aux (cddr args))))
             (&aux (rec-aux (cdr args)))
             (otherwise
              (when (cdr args)
                (rec-key (cdr args)))
              (cond
                ((consp (car args))
                 (push (list* (key-arg-names (caar args)) (cdar args)) key))
                ((symp (car args))
                 (push (list (key-arg-names (car args))) key))
                (t
                 (error "Bad &key parameter in lambda list"))))))

         (rec-aux (args)
           (when (cdr args)
             (rec-aux (cdr args)))
           (cond
             ((consp (car args))
              (push (car args) aux))
             ((symp (car args))
              (push (list (car args)) aux))
             (t
              (error "Bad &aux parameter in lambda list")))))

      (rec args)
      (list :required required
            :optional optional
            :rest rest
            :key key
            :aux aux
            :aok allow-other-keys))))