(let ((main (%make-package "SS"))
      (boot (%find-package "%"))
      (user (%make-package "SS-USER")))
  (%export '(quasiquote defmacro defun when unless map labels flet foreach
             prog1 prog2 or and cond member case mapcar with-cc aif push error
             lisp-reader compile compile-string load
             funcall macrolet
             quote lambda let let* if progn setq t nil not
             tagbody go block return return-from
             *package* *read-table*
             &key &rest &body &whole &optional &aux)
           boot)
  (%use-package boot main)
  (%use-package main user)
  (setq *package* main))

;;;; destructuring-bind

(defmacro %next (lst)
  `(prog1 (car ,lst)
     (setq ,lst (cdr ,lst))))

(defun %fn-destruct (args values body)
  (let (names decls)
    (let ((topv (gensym)) rec)
      (labels
          ((add (name val)
             (push `(,name ,val) decls))
           (rec (optional? rest? key? aux? args values i)
             (when args
               (cond
                 ((symbolp args)
                  (add args values))
                 ((consp args)
                  (let ((thisarg (car args)))
                    (cond
                      ((symbolp thisarg)
                       (case thisarg
                         (&whole
                          (when (> i 0) (error "Misplaced &WHOLE"))
                          (let ((thisarg (cadr args)))
                            (unless (and thisarg (symbolp thisarg))
                              (error "Missing variable name for &WHOLE"))
                            (add thisarg values))
                          (rec nil nil nil nil (cddr args) values i))

                         (&optional
                          (when (or optional? rest? key? aux?)
                            (error "Invalid &OPTIONAL"))
                          (rec t nil nil nil (cdr args) values i))

                         ((&rest &body)
                          (when (or rest? key? aux?)
                            (error "Invalid &REST/&BODY"))
                          (let ((thisarg (cadr args)))
                            (unless (and thisarg (symbolp thisarg))
                              (error "Missing variable name for &REST"))
                            (add thisarg values))
                          (rec nil t nil nil (cddr args) values i))

                         (&key
                          (when (or key? aux?)
                            (error "Invalid &KEY"))
                          (rec nil nil t nil (cdr args) values i))

                         (&aux
                          (when aux?
                            (error "Invalid &AUX"))
                          (rec nil nil nil t (cdr args) values i))

                         (t
                          (when (member thisarg names)
                            (error (strcat "Argument seen twice: " thisarg)))
                          (push thisarg names)
                          (cond
                            (optional?
                             (add thisarg `(%next ,values)))
                            (aux?
                             (add thisarg nil))
                            (key?
                             (add thisarg `(%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")))))
                            (t
                             (add thisarg `(if ,values
                                               (%next ,values)
                                               (error ,(strcat "Missing required argument: " thisarg))))))
                          (rec optional? rest? key? aux? (cdr args) values (+ i 1)))))

                      ((consp thisarg)
                       (cond
                         ((or optional? key?)
                          (let ((thisarg (car thisarg))
                                (default (cadr thisarg))
                                (thisarg-p (caddr thisarg)))
                            (when thisarg-p
                              (add thisarg-p `(if ,values t nil)))
                            (add thisarg (if key?
                                             (let ((val (gensym)))
                                               `(let ((,val (%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")) 'not-found)))
                                                  (if (eq ,val 'not-found) ,default ,val)))
                                             `(if ,values (%next ,values) ,default)))))
                         (aux? (let ((thisarg (car thisarg))
                                     (value (cadr thisarg)))
                                 (add thisarg value)))
                         (rest? (error "Invalid argument list following &REST/&BODY"))
                         (t
                          (let ((sublist (gensym)))
                            (add sublist `(if ,values (%next ,values) (error "Missing sublist")))
                            (rec nil nil nil nil thisarg sublist 0))))
                       (rec optional? rest? key? aux? (cdr args) values (+ i 1))))))
                 (t (error "Invalid lambda-list"))))))
        (rec nil nil nil nil args topv 0))
      `(let* ((,topv ,values) ,@(reverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct args values body))

(defmacro defmacro (name lambda-list . body)
  (let ((args (gensym "ARGS")))
    `(labels ((,name ,args
                (destructuring-bind ,lambda-list ,args ,@body)))
       (%macro! ',name #',name))))

(defmacro export (symbols &optional (package *package*))
  `(%export ,symbols ,package))

(defmacro def-efun (name . rest)
  `(progn
     (defun ,name ,@rest)
     (export ',name)))

(defmacro def-emac (name . rest)
  `(progn
     (defmacro ,name ,@rest)
     (export ',name)))

(def-efun macroexpand (form)
  (if (and (consp form)
           (symbolp (car form))
           (%macro (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(def-efun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (map #'macroexpand-all form))
      form))

(def-emac time body
  (let ((t1 (gensym)))
    `(let ((,t1 (%get-time)))
       (prog1 (progn ,@body)
         (console.print "Evaluation time:" (- (%get-time) ,t1))))))

;; (def-emac defun (name lambda-list &body body)
;;   (let ((args (gensym "ARGS")))
;;     `(labels ((,name ,args
;;                 (destructuring-bind ,lambda-list ,args ,@body)))
;;        (set-symbol-function! ',name #',name))))

(def-emac intern (symbol-name &optional (package *package*))
  `(%intern ,symbol-name ,package))

(def-emac defpackage (name &rest options)
  (let ((nicknames nil)
        (use nil)
        (pak (gensym "DEFPACKAGE")))
    (foreach options
             (lambda (opt)
               (case (car opt)
                 (:nicknames
                  (setq nicknames (append nicknames (cdr opt))))
                 (:use
                  (setq use (append use (cdr opt)))))))
    `(let ((,pak (%make-package ',name ',use ',nicknames)))
       ,@(map (lambda (opt)
                (case (car opt)
                  (:export
                   `(%export (list ,@(cdr opt)) ,pak))
                  (:import-from
                   (destructuring-bind (source &rest names) (cdr opt)
                     (setq source (%find-package source))
                     `(%import ',(map (lambda (name)
                                        (%find-symbol name source))
                                      names)
                               ,pak)))))
              options)
       ,pak)))

(def-emac in-package (name)
  (setq *package* (%find-package name)) nil)

(def-emac defparameter (name val)
  `(progn
     (%special! ,name)
     (setq ,name ,val)))

;; only the long form is supported
(def-emac defsetf (access-fn lambda-list store-vars &body body)
  (let ((args1 (gensym))
        (args2 (gensym)))
    `(%set-symbol-prop ',access-fn :setf
                       (lambda (,args1 ,args2)
                         (destructuring-bind ,lambda-list ,args1
                           (destructuring-bind ,store-vars ,args2
                             ,@body))))))

(def-efun %setf (args)
  (when args
    (unless (cdr args)
      (error "Odd number of arguments in SETF"))
    (cons
     (cond
       ((listp (car args))
        (destructuring-bind ((sym &rest args1) &rest args2
                             &aux (expander (%get-symbol-prop sym :setf)))
            args
          (unless (functionp expander)
            (error (strcat "Unknown SETF expander for " sym)))
          (funcall expander args1 args2)))
       ((symbolp (car args))
        `(setq ,(car args) ,(cadr args)))
       (t (error "Unsupported SETF syntax")))
     (%setf (cddr args)))))

(def-emac setf args
  `(progn ,@(%setf args)))

;; this is `once-only' from Practical Common Lisp
(def-emac with-rebinds (names . body)
  (let ((gensyms (mapcar (lambda (_) (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                ,@body)))))

(export 'destructuring-bind)

(in-package :ss-user)
