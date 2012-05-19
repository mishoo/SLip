(setq *package* (%find-package "%"))

"
(in-package :%)
" ;; hack for Ymacs

(let ((main (%make-package "SL"))
      (boot (%find-package "%"))
      (user (%make-package "SL-USER")))
  (%export '(quasiquote defmacro defun when unless map labels flet foreach
             prog1 prog2 or and cond member case mapcar with-cc aif it push
             error warn without-interrupts
             lisp-reader compile load function unwind-protect
             funcall macrolet catch throw
             quote lambda let let* if progn setq t nil not
             tagbody go block return return-from
             *package* *read-table*
             &key &rest &body &whole &optional &aux)
           boot)
  (%use-package boot main)
  (%use-package main user)
  (setq *package* main))

"
(in-package :sl)
"

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
      `(let* ((,topv ,values) ,@(nreverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct args values body))

(defmacro defmacro (name lambda-list . body)
  (%::maybe-xref-info name "DEFMACRO")
  (let ((args (gensym "ARGS")))
    `(%macro! ',name (%::%fn ',name ,args
                             (destructuring-bind ,lambda-list ,args
                               ,@body)))))

(defmacro import (symbols &optional (package *package*))
  `(%import ,symbols ,package))

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
  (%special! name)
  (%::maybe-xref-info name "DEFPARAMETER")
  `(progn (%special! ',name)
          (setq ,name ,val)))

(def-emac defglobal (name val)
  (%global! name)
  (%::maybe-xref-info name "DEFGLOBAL")
  `(progn (%global! ',name)
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

(def-emac incf (place &optional (inc 1))
  `(setf ,place (+ ,place ,inc)))

(def-emac decf (place &optional (dec 1))
  `(setf ,place (- ,place ,dec)))

(defsetf car (x) (val)
  `(rplaca ,x ,val))

(defsetf cdr (x) (val)
  `(rplacd ,x ,val))

(def-emac push (obj place)
  `(setf ,place (cons ,obj ,place)))

;; this is `once-only' from Practical Common Lisp
(def-emac with-rebinds (names . body)
  (let ((gensyms (mapcar (lambda (_) (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
             ,@body)))))

(def-emac pop (place)
  (let ((v (gensym)))
    `(let ((,v ,place))
       (setf ,place (cdr ,v))
       (car ,v))))

(def-efun collect-if (test list)
  (cond ((not list) nil)
        ((funcall test (car list)) (cons (car list) (collect-if test (cdr list))))
        (t (collect-if test (cdr list)))))

(def-efun remove (item list)
  (collect-if (lambda (x)
                (not (eq x item))) list))

(labels ((finished (tails)
           (when tails
             (if (car tails) (finished (cdr tails)) t))))

  ;; every returns false as soon as any invocation of predicate
  ;; returns false. If the end of a sequence is reached, every returns
  ;; true. Thus, every returns true if and only if every invocation of
  ;; predicate returns true.
  (def-efun every (test . lists)
    (let scan ((tails lists))
      (if (finished tails) t
          (and (%apply test (map #'car tails))
               (scan (map #'cdr tails))))))

  ;; some returns the first non-nil value which is returned by an
  ;; invocation of predicate. If the end of a sequence is reached
  ;; without any invocation of the predicate returning true, some
  ;; returns false. Thus, some returns true if and only if some
  ;; invocation of predicate returns true.
  (def-efun some (test . lists)
    (let scan ((tails lists))
      (if (finished tails) nil
          (or (%apply test (map #'car tails))
              (scan (map #'cdr tails))))))

  ;; notany returns false as soon as any invocation of predicate
  ;; returns true. If the end of a sequence is reached, notany returns
  ;; true. Thus, notany returns true if and only if it is not the case
  ;; that any invocation of predicate returns true.
  (def-efun notany (test . lists)
    (let scan ((tails lists))
      (if (finished tails) t
          (if (%apply test (map #'car tails))
              nil
              (scan (map #'cdr tails))))))

  ;; notevery returns true as soon as any invocation of predicate
  ;; returns false. If the end of a sequence is reached, notevery
  ;; returns false. Thus, notevery returns true if and only if it is
  ;; not the case that every invocation of predicate returns true.
  (def-efun notevery (test . lists)
    (let scan ((tails lists))
      (if (finished tails) nil
          (if (%apply test (map #'car tails))
              (scan (map #'cdr tails))
              t)))))

(def-efun remove-duplicates args
  (destructuring-bind (list &key (test #'eq) from-end) args
    (labels ((rmv (list ret)
               (if list
                   (let ((current (car list)))
                     (rmv (collect-if (lambda (x)
                                        (not (funcall test current x)))
                                      (cdr list))
                          (cons current ret)))
                   ret)))
      (if from-end
          (nreverse (rmv list nil))
          (rmv (reverse list) nil)))))

(def-efun last (list)
  (when list
    (if (not (cdr list))
        list
        (last (cdr list)))))

(defun nhalf-list (a)
  (cons a (when a
            (let rec ((a a)
                      (b (cddr a)))
              (cond
                ((eq a b) (error "Circular list detected"))
                (b (rec (cdr a) (cddr b)))
                (t (prog1 (cdr a)
                     (setf (cdr a) nil))))))))

(def-efun merge (list1 list2 predicate)
  (let (ret p)
    (labels ((add (cell)
               (if p
                   (setf (cdr p) cell)
                   (setf ret cell))
               (setf p cell))
             (rec (a b)
               (cond ((and a b)
                      (if (funcall predicate (car b) (car a))
                          (progn
                            (add (list (car b)))
                            (rec a (cdr b)))
                          (progn
                            (add (list (car a)))
                            (rec (cdr a) b))))
                     (a (add a))
                     (b (add b)))))
      (rec list1 list2)
      ret)))

(def-efun stable-sort (list predicate)
  (let sort ((list list))
    (cond ((not list) nil)
          ((not (cdr list)) list)
          (t (let* ((a list)
                    (b (labels ((sub (list i)
                                  (if (zerop (decf i))
                                      (prog1 (cdr list)
                                        (setf (cdr list) nil))
                                      (sub (cdr list) i))))
                         (sub list (floor (length list) 2)))))
               (merge (sort a) (sort b) predicate))))))

(set-symbol-function! 'sort #'stable-sort)
(export '(sort export import))

(def-emac with-append-list ((var append &key (tail (gensym))) &body body)
  `(let (,var ,tail)
     (flet ((,append (x)
              (setf ,tail
                    (last (if ,tail
                              (setf (cdr ,tail) x)
                              (setf ,var x))))))
       ,@body)))

(def-emac dotimes ((var count-form &optional result-form) &body body)
  (let ((tag (gensym))
        (end (gensym)))
    `(block nil
       (let ((,end ,count-form)
             (,var 0))
         (tagbody
          ,tag (when (< ,var ,end)
                 ,@body
                 (incf ,var)
                 (go ,tag))))
       ,result-form)))

(def-emac use-package (source &optional (target *package*))
  `(%use-package (%find-package ,source) (%find-package ,target)))

(defparameter *standard-output* (%make-output-stream))
(defparameter *error-output* (%make-output-stream))
(defparameter *trace-output* (%make-output-stream))

(export '(*standard-output*
          *error-output*
          *trace-output*
          destructuring-bind))
