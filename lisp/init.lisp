(setq *package* (find-package "%"))

"
(in-package :%)
" ;; hack for Ymacs

;; XXX: for some reason setting this badly affects the compiler, so
;; LET expressions in the compiler won't have proper tail calls. It's
;; still good that we can set it here, but I Just Don't Understand why
;; the compiler breaks with it!! :-/
(setq %:*let-tco* t)

(let ((main (make-package "SL"))
      (boot (find-package "%"))
      (user (make-package "SL-USER"))
      exported)
  (%use-package boot main)
  (%use-package main user)
  (setq exported
        '(atom quasiquote defmacro defun when unless labels flet foreach
          prog1 prog2 or and cond member case mapcar with-cc aif it push
          error warn without-interrupts
          lisp-reader compile load function unwind-protect
          apply funcall macrolet symbol-macrolet catch throw
          quote lambda λ let let* if progn setq t nil not
          tagbody go block return return-from
          *package* *read-table*
          elt rplaca rplacd nthcdr last reverse nreverse append nconc nreconc revappend
          char-name char-code name-char code-char upcase downcase
          charp char-equal char= char< char<= char> char>= char/= letterp digitp
          stringp string-equal string= string< string<= string> string>= string/=
          make-regexp regexp-test regexp-exec replace-regexp quote-regexp regexpp
          vectorp vector vector-ref vector-set vector-push vector-pop make-vector
          getf
          list list* copy-list listp cons consp eq eql equal equalp gensym length

          numberp zerop plusp minusp evenp oddp parse-number parse-integer number-fixed number-string
          < <= > >= + - * / = /= null 1+ 1- floor ceiling round mod
          abs sin asin cos acos tan atan exp log sqrt expt random
          min max

          make-hash hashp hash-get hash-set hash-add hash-copy hash-keys hash-values
          hash-iterator iterator-next

          macroexpand-1 disassemble

          make-package find-package make-symbol symbol-name symbol-package symbol-function
          packagep symbolp keywordp intern shadow find-symbol package-name
          threadp make-thread current-thread set-timeout clear-timeout

          car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
          cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr

          get-internal-run-time

          values multiple-value-bind multiple-value-call values-list
          multiple-value-list multiple-value-setq multiple-value-prog1

          &key &rest &body &whole &optional &aux &allow-other-keys))
  (%export exported boot)
  (%export exported main)
  (setq *package* main))

"
(in-package :sl)
"

;;;; destructuring-bind

(defmacro %next (place)
  (let ((v (gensym)))
    `(let ((,v ,place))
       (setq ,place (cdr ,v))
       (car ,v))))

(defun %dbind-error-missing-arg (arg)
  (error (strcat "Missing required argument: " arg)))

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
                             (add thisarg `(getf ,values ,(intern (symbol-name thisarg) (find-package "KEYWORD")))))
                            (t
                             (add thisarg `(if ,values
                                               (%next ,values)
                                               (%dbind-error-missing-arg ',thisarg)))))
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
                                               `(let ((,val (getf ,values ,(intern (symbol-name thisarg) (find-package "KEYWORD")) 'not-found)))
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

(defun ordinary-lambda-list-p (args)
  (cond
    ((or (null args)
         (symbolp args)))
    ((not (consp args))
     (error "Bad macro lambda list"))
    ((not (symbolp (car args)))
     nil)
    ((%::lambda-keyword-p (car args))
     t)
    ((ordinary-lambda-list-p (cdr args)))))

(defmacro defmacro (name lambda-list . body)
  (%::maybe-xref-info name 'defmacro)
  (if (%primitivep name)
      (error (strcat "We shall not DEFMACRO on " name " (primitive function)")))
  (if (ordinary-lambda-list-p lambda-list)
      `(%macro! ',name (%::%fn ,name ,lambda-list ,@body))
      (let ((args (gensym "ARGS")))
        `(%macro! ',name (%::%fn ,name ,args
                                 (destructuring-bind ,lambda-list ,args
                                   ,@body))))))

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

(def-efun macroexpand-1 (form)
  (cond
    ((atom form) form)
    ((and (eq 'progn (car form))
          (null (cdr form)))
     nil)
    (t (aif (or (%:find-macrolet-in-compiler-env (car form))
                (%macro (car form)))
            (apply it (cdr form))
            form))))

(def-efun macroexpand (form)
  (let ((result (macroexpand-1 form)))
    (if (eq result form)
        result
        (macroexpand result))))

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
    `(let ((,pak (make-package ',name ',use ',nicknames)))
       ,@(map (lambda (opt)
                (case (car opt)
                  (:export
                   `(%export (list ,@(cdr opt)) ,pak))
                  (:import-from
                   (destructuring-bind (source &rest names) (cdr opt)
                     (setq source (find-package source))
                     `(%import ',(map (lambda (name)
                                        (find-symbol name source))
                                      names)
                               ,pak)))))
              options)
       ,pak)))

(def-emac in-package (name)
  (setq *package* (find-package name)) nil)

(def-emac defparameter (name val)
  (%special! name)
  (%::maybe-xref-info name 'defparameter)
  `(progn (%special! ',name)
          (setq ,name ,val)))

(def-emac defglobal (name val)
  (%global! name)
  (%::maybe-xref-info name 'defglobal)
  `(progn (%global! ',name)
          (setq ,name ,val)))

;;; setf

(def-emac defsetf (access-fn lambda-list &optional store-vars &body body)
  (maybe-xref-info access-fn 'setf)
  (cond
    ((symbolp lambda-list)
     `(%set-symbol-prop ',access-fn :setf ',lambda-list))
    ((> (length store-vars) 1)
     (let ((arg (gensym "setfarg")))
       `(%set-symbol-prop ',access-fn :setf
                          (%fn ,access-fn (,@lambda-list ,arg)
                               (symbol-macrolet
                                   (,@(mapcar (lambda (sym)
                                                `(,sym ',sym))
                                              store-vars))
                                 `(multiple-value-bind ,'(,@store-vars) ,,arg
                                    ,,@body))))))
    (t
     `(%set-symbol-prop ',access-fn :setf
                        (%fn ,access-fn (,@lambda-list ,@store-vars) ,@body)))))

(defun %get-setf-place (form)
  (let ((expander (let dig ()
                    (or (%get-symbol-prop (car form) :setf)
                        (let ((exp (macroexpand-1 form)))
                          (unless (eq exp form)
                            (setq form exp)
                            (dig)))))))
    (values expander form)))

(defun %setf (args)
  (when args
    (unless (cdr args)
      (error "Odd number of arguments in SETF"))
    (cons
     (cond
       ((consp (car args))
        (multiple-value-bind (expander form) (%get-setf-place (car args))
          (cond
            ((null expander)
             (error (strcat "No SETF expander for " (caar args))))
            ((functionp expander)
             (apply expander (append (cdr form) (list (cadr args)))))
            ((symbolp expander)
             `(,expander ,@(cdr form) ,(cadr args)))
            ((error (strcat "Unknown SETF expander for " (caar args) ": " expander))))))
       ((symbolp (car args))
        `(setq ,(car args) ,(cadr args)))
       (t (error "Unsupported SETF syntax")))
     (%setf (cddr args)))))

(def-emac setf args
  `(progn ,@(%setf args)))

(def-emac psetf args
  (let ((temps nil)
        (places nil)
        (values nil))
    (let rec ((args args))
      (cond
        ((null args))
        ((null (cadr args))
         (error "Odd number of forms in psetf"))
        (t
         (push (gensym "psetf") temps)
         (push (car args) places)
         (push (cadr args) values)
         (rec (cddr args)))))
    (setf temps (nreverse temps)
          places (nreverse places)
          values (nreverse values))
    `(let ,(mapcar #'list temps values)
       ,@(mapcar (lambda (var set)
                   `(setf ,var ,set))
                 places temps))))

(def-emac incf (place &optional (inc 1))
  `(setf ,place (+ ,place ,inc)))

(def-emac decf (place &optional (dec 1))
  `(setf ,place (- ,place ,dec)))

(defsetf car (x) (val)
  `(rplaca ,x ,val))

(defsetf cdr (x) (val)
  `(rplacd ,x ,val))

(defsetf getf (place indicator) (value)
  (let ((vval (gensym)))
    `(let (,vval)
       (setf ,place (%:%putf ,place ,indicator
                             (setq ,vval ,value)))
       ,vval)))

(defmacro defun (name args . body)
  (cond
    ((and (consp name)
          (eq 'setf (car name))
          (symbolp (cadr name))
          (null (cddr name)))
     (let* ((setter (intern (strcat "(SETF " (cadr name) ")")
                            (symbol-package (cadr name))))
            (value-arg (car args))
            (rest-args (cdr args)))
       `(progn
          (set-symbol-function! ',setter
                                (%fn ,setter (,value-arg ,@rest-args)
                                     ,@body))
          (defsetf ,(cadr name) ,rest-args (,value-arg)
            `(,',setter (%:%ooo ,,value-arg) ,,@rest-args)))))
    (t
     (maybe-xref-info name 'defun)
     `(set-symbol-function! ',name (%fn ,name ,args ,@body)))))

(defun (setf symbol-function) (func sym)
  (%::maybe-xref-info sym 'defun)
  (set-symbol-function! sym func))

(def-emac push (obj place)
  `(setf ,place (cons ,obj ,place)))

(def-emac pop (place)
  (let ((v (gensym)))
    `(let ((,v ,place))
       (setf ,place (cdr ,v))
       (car ,v))))

;;; lists

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
          (and (apply test (map #'car tails))
               (scan (map #'cdr tails))))))

  ;; some returns the first non-nil value which is returned by an
  ;; invocation of predicate. If the end of a sequence is reached
  ;; without any invocation of the predicate returning true, some
  ;; returns false. Thus, some returns true if and only if some
  ;; invocation of predicate returns true.
  (def-efun some (test . lists)
    (let scan ((tails lists))
      (if (finished tails) nil
          (or (apply test (map #'car tails))
              (scan (map #'cdr tails))))))

  ;; notany returns false as soon as any invocation of predicate
  ;; returns true. If the end of a sequence is reached, notany returns
  ;; true. Thus, notany returns true if and only if it is not the case
  ;; that any invocation of predicate returns true.
  (def-efun notany (test . lists)
    (let scan ((tails lists))
      (if (finished tails) t
          (if (apply test (map #'car tails))
              nil
              (scan (map #'cdr tails))))))

  ;; notevery returns true as soon as any invocation of predicate
  ;; returns false. If the end of a sequence is reached, notevery
  ;; returns false. Thus, notevery returns true if and only if it is
  ;; not the case that every invocation of predicate returns true.
  (def-efun notevery (test . lists)
    (let scan ((tails lists))
      (if (finished tails) nil
          (if (apply test (map #'car tails))
              (scan (map #'cdr tails))
              t)))))

(def-efun remove-duplicates (list &key (test #'eql) from-end)
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
        (rmv (reverse list) nil))))

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
  (let* ((ret (list nil))
         (p ret))
    (macrolet ((add (cell)
                 `(setf p (setf (cdr p) ,cell))))
      (let rec ((a list1)
                (b list2))
        (cond ((and a b)
               (if (funcall predicate (car b) (car a))
                   (progn
                     (add (list (car b)))
                     (rec a (cdr b)))
                   (progn
                     (add (list (car a)))
                     (rec (cdr a) b))))
              (a (add a))
              (b (add b))))
      (cdr ret))))

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

(setf (symbol-function 'sort) #'stable-sort)
(export '(sort export import))

;;; basic looping

(def-emac dotimes ((var count-form &optional result-form) &body body)
  (let ((count (gensym))
        (next (gensym "next"))
        (end (gensym "end")))
    `(block nil
       (let ((,count ,count-form)
             (,var 0))
         (tagbody
          ,next
          (unless (< ,var ,count)
            (go ,end))
          ,@body
          (incf ,var)
          (go ,next)
          ,end))
       ,result-form)))

(def-emac dolist ((var list-form &optional result-form) &body body)
  (let ((list (gensym "list"))
        (next (gensym "next"))
        (end (gensym "end")))
    `(block nil
       (let ((,list ,list-form)
             (,var))
         (tagbody
          ,next
          (unless ,list
            (setf ,var nil)
            (go ,end))
          (setf ,var (car ,list)
                ,list (cdr ,list))
          ,@body
          (go ,next)
          ,end)
         ,result-form))))

;;; do and do* differ by exactly two characters, but oh well... copy-paste FTW.

(def-emac do (vars (end-test-form &optional result-form) &body body)
  (let ((next (gensym "next"))
        (end (gensym "end"))
        (step (apply #'nconc (mapcar (lambda (var)
                                       (when (> (length var) 2)
                                         (list (car var) (caddr var))))
                                     vars))))
    `(let ,(mapcar (lambda (var)
                     (list (car var) (cadr var)))
                   vars)
       (tagbody
        ,next
        (when ,end-test-form
          (go ,end))
        ,@body
        (psetf ,@step)
        (go ,next)
        ,end)
       ,result-form)))

(def-emac do* (vars (end-test-form &optional result-form) &body body)
  (let ((next (gensym "next"))
        (end (gensym "end"))
        (step (apply #'nconc (mapcar (lambda (var)
                                       (when (> (length var) 2)
                                         (list (car var) (caddr var))))
                                     vars))))
    `(let* ,(mapcar (lambda (var)
                      (list (car var) (cadr var)))
                    vars)
       (tagbody
        ,next
        (when ,end-test-form
          (go ,end))
        ,@body
        (setf ,@step)
        (go ,next)
        ,end)
       ,result-form)))

(def-emac use-package (source &optional (target *package*))
  `(%use-package (find-package ,source) (find-package ,target)))

;;; multiple values

(defmacro multiple-value-call (fn &rest values)
  `(apply ,fn (nconc ,@(mapcar (lambda (form)
                                 `(multiple-value-list ,form))
                               values))))

(defun values-list (list)
  (apply #'values list))

(defmacro multiple-value-setq ((&rest vars) value-form)
  (let ((syms (mapcar (lambda (var)
                        (gensym (symbol-name var)))
                      vars)))
    `(multiple-value-bind ,syms ,value-form
       ,@(mapcar (lambda (var sym)
                   `(setf ,var ,sym))
                 vars syms))))

(defmacro multiple-value-prog1 (form &rest body)
  (let ((list (gensym "list")))
    `(let ((,list (multiple-value-list ,form)))
       ,@body
       (values-list ,list))))

(defparameter *standard-output* (%make-output-stream))
(defparameter *error-output* (%make-output-stream))
(defparameter *trace-output* (%make-output-stream))

(def-emac with-output-to-string ((var &optional string) &body body)
  `(let ((,var (%make-output-stream)))
     ,@(when string
         `((%stream-put ,var ,string)))
     ,@body
     (%stream-get ,var)))

(export '(*standard-output*
          *error-output*
          *trace-output*
          destructuring-bind))
