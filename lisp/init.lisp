(setq *package* (find-package "%"))

"
(in-package :%)
" ;; hack for Ymacs

(let ((main (make-package "SL"))
      (boot (find-package "%"))
      (user (make-package "SL-USER"))
      exported)
  (%use-package boot main)
  (%use-package main user)
  (setq exported
        '(atom quasiquote defmacro defun when unless labels flet foreach
          prog1 prog2 or and cond member case otherwise mapcar with-cc aif it push
          error warn without-interrupts
          lisp-reader eval compile load function unwind-protect
          apply funcall macrolet symbol-macrolet catch throw
          quote lambda λ let let* if progn progv setq t nil not
          tagbody go block return return-from
          *package* *read-table*
          elt rplaca rplacd nth nthcdr last reverse nreverse append nconc nreconc revappend
          char-name char-code name-char code-char upcase downcase
          charp char-equal char= char< char<= char> char>= char/= letterp digitp
          stringp string-equal string= string< string<= string> string>= string/=
          make-regexp regexp-test regexp-exec replace-regexp quote-regexp regexpp
          vectorp vector svref vector-push vector-pop make-vector
          getf
          list list* copy-list listp cons consp eq eql equal equalp gensym length
          declare locally type ignore special optimize speed debug space fixnum integer unsigned-byte
          identity

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
          first second third fourth rest

          boundp makunbound fboundp fmakunbound

          get-internal-run-time
          compiler-macro-function
          symbol-value

          values multiple-value-bind multiple-value-call values-list
          multiple-value-list multiple-value-setq multiple-value-prog1

          lambda-list-keywords
          &key &rest &body &whole &optional &aux &allow-other-keys))
  (%export exported boot)
  (%export exported main)
  (setq *package* main))

"
(in-package :sl)
"

(defmacro import (symbols &optional (package *package*))
  `(%import ,symbols ,package))

(defmacro export (symbols &optional (package *package*))
  `(%export ,symbols ,package))

(defmacro def-efun (name . rest)
  `(progn
     (export ',name)
     (defun ,name ,@rest)))

(defmacro def-emac (name . rest)
  `(progn
     (defmacro ,name ,@rest)
     (export ',name)))

(def-efun macroexpand-1 (form)
  (cond
    ((atom form)
     (or (%:find-symbol-macrolet-in-compiler-env form) form))
    ((and (eq 'progn (car form))
          (null (cdr form)))
     nil)
    ((aif (or (%:find-macrolet-in-compiler-env (car form))
              (%macro (car form)))
          (let ((%:*whole-form* form))
            (apply it (cdr form)))
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
       ,@(map1 (lambda (opt)
                 (case (car opt)
                   (:export
                    `(%export (list ,@(cdr opt)) ,pak))
                   (:import-from
                    (destructuring-bind (source &rest names) (cdr opt)
                      (setq source (find-package source))
                      `(%import ',(map1 (lambda (name)
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

(defun finished (tails)
  (when tails
    (if (car tails) (finished (cdr tails)) t)))

(defun mapcar (f . lists)
  (let rec (ret (tails lists))
    (if (finished tails)
        (nreverse ret)
        (rec (cons (apply f (map1 #'car tails)) ret)
             (map1 #'cdr tails)))))

;;; setf

(defmacro %mvb-internal (names form &body body)
  `(multiple-value-bind ,names ,form ,@body))

(def-emac defsetf (access-fn lambda-list &optional store-vars &body body)
  (maybe-xref-info access-fn 'defsetf)
  (cond
    ((symbolp lambda-list)
     `(%set-symbol-prop ',access-fn :setf ',lambda-list))
    ((> (length store-vars) 1)
     (let ((arg (gensym "setfarg")))
       `(%set-symbol-prop ',access-fn :setf
                          (%fn ,access-fn (,@lambda-list ,arg)
                               (symbol-macrolet
                                   (,@(map1 (lambda (sym)
                                              `(,sym ',sym))
                                            store-vars))
                                 `(%mvb-internal ,'(,@store-vars) ,,arg
                                    ,,@body))))))
    (t
     `(%set-symbol-prop ',access-fn :setf
                        (%fn ,access-fn (,@lambda-list ,@store-vars) ,@body)))))

(defun %get-setf-place (form)
  (let ((expander (let dig ()
                    ;; XXX: return T when form is a symbol (for some reason).
                    ;;      not sure this is correct, but it works.
                    (or (symbolp form)
                        (%get-symbol-prop (car form) :setf)
                        (let ((exp (macroexpand-1 form)))
                          (unless (eq exp form)
                            (setq form exp)
                            (dig)))))))
    (values expander form)))

(def-efun get-setf-expansion (form)
  (cond
    ((symbolp form)
     ;; could be symbol macro.
     (let ((exp (macroexpand-1 form)))
       (unless (eq exp form)
         (return-from get-setf-expansion (get-setf-expansion exp))))
     (let ((vals (list (gensym "val"))))
       (values nil nil vals `(setq ,form ,@vals) form)))
    ((consp form)
     (multiple-value-bind (expander form) (%get-setf-place form)
       (when (eq expander t)
         ;; XXX: see above in `%get-setf-place'. We receive T when we got down
         ;; to a symbol. By recursing here we also expand an eventual
         ;; symbol-macrolet on that symbol.
         (return-from get-setf-expansion (get-setf-expansion form)))
       (let* ((temps (map1 (lambda (subform)
                             (gensym "temp"))
                           (cdr form)))
              (vals (list (gensym "new")))
              (store-form (cond
                            ((functionp expander)
                             (apply expander (append temps vals)))
                            ((and expander
                                  (symbolp expander))
                             `(,expander ,@temps ,@vals))
                            ((symbolp (car form))
                             (let ((setter (intern (strcat "(SETF " (car form) ")")
                                                   (symbol-package (car form)))))
                               `(,setter ,@vals ,@temps)))
                            ((error (strcat "Unknown SETF expander " (car form)))))))
         (when (eq '%mvb-internal (car store-form))
           ;; hack: for multiple store vars, fetch names directly from
           ;; the code and unwrap it from the multiple-value-bind.
           (setq vals (cadr store-form))
           (setq store-form `(progn ,@(cdddr store-form))))
         (values temps
                 (cdr form)
                 vals
                 store-form
                 `(,(car form) ,@temps)))))
    ((error (strcat "Invalid SETF place " form)))))

(defun %setf (args)
  (when args
    (unless (cdr args)
      (error "Odd number of arguments in SETF"))
    (cons
     (cond
       ((consp (car args))
        (multiple-value-bind (expander form) (%get-setf-place (car args))
          (cond
            ((and (null expander)
                  (symbolp (car form)))
             ;; no global setter defined, but maybe there is a (SETF FOO) somewhere.
             (let ((setter (intern (strcat "(SETF " (car form) ")")
                                   (symbol-package (car form)))))
               `(,setter ,(cadr args) ,@(cdr form))))
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
    `(let ,(map2 #'list temps values)
       ,@(map2 (lambda (var set)
                 `(setf ,var ,set))
               places temps))))

(defsetf car (x) (val)
  `(rplaca ,x ,val))

(defsetf cdr (x) (val)
  `(rplacd ,x ,val))

(defun (setf symbol-function) (func sym)
  (%::maybe-xref-info sym 'defun)
  (set-symbol-function! sym func))

(defun (setf symbol-value) (value sym)
  (set-symbol-value! sym value))

(def-emac push (obj place)
  (cond
    ((safe-atom-p place)
     `(setq ,place (cons ,obj ,place)))
    ((multiple-value-bind (temps value-forms store-vars store-form get-form)
                          (get-setf-expansion place)
       (let ((item (gensym "item")))
         `(let ((,item ,obj)
                ,@(map2 #'list temps value-forms))
            (let ((,(car store-vars) (cons ,item ,get-form)))
              ,store-form)))))))

(def-emac pop (place)
  (let ((v (gensym "place")))
    (cond
      ((safe-atom-p place)
       `(%:%pop ,place))
      ((multiple-value-bind (temps value-forms store-vars store-form get-form)
                            (get-setf-expansion place)
         `(let* (,@(map2 #'list temps value-forms)
                 (,v ,get-form)
                 (,(car store-vars) (cdr ,v)))
            ,store-form
            (car ,v)))))))

(defsetf getf (place indicator) (value)
  (let ((vval (gensym)))
    (cond
      ((safe-atom-p place)
       (if (safe-atom-p value)
           `(progn
              (setf ,place (%:%putf ,place ,indicator ,value))
              ,value)
           `(let (,vval)
              (setf ,place (%:%putf ,place ,indicator (setq ,vval ,value)))
              ,vval)))
      ((multiple-value-bind (temps value-forms store-vars store-form get-form)
                            (get-setf-expansion place)
         `(let* (,vval
                 ,@(map2 #'list temps value-forms)
                 (,(car store-vars) (%:%putf ,get-form ,indicator (setq ,vval ,value))))
            ,store-form
            ,vval))))))

(defun (setf compiler-macro-function) (handler name)
  (setf (getf %:*compiler-macros* name) handler))

(def-emac define-compiler-macro (name args &body body)
  (multiple-value-bind (setter name) (%:maybe-setter name)
    (%:maybe-xref-info name (if setter
                                'compiler-macro-setf
                                'compiler-macro))
    (let ((form (if (eq '&whole (car args))
                    (prog1
                        (cadr args)
                      (setq args (cddr args)))
                    (gensym "form"))))
      `(setf (compiler-macro-function ',(or setter name))
             (%fn ,name (,form)
                  (destructuring-bind ,args (if (eq 'funcall (car ,form))
                                                (cddr ,form)
                                                (cdr ,form))
                    ,@body))))))

(defun (setf svref) (value vector index)
  (vector-set vector index value))

(define-compiler-macro (setf svref) (value vector index)
  (cond
    ((or (safe-atom-p value)
         (and (safe-atom-p vector)
              (safe-atom-p index)))
     `(vector-set ,vector ,index ,value))
    ((let ((v (gensym)))
       `(let ((,v ,value))
          (vector-set ,vector ,index ,v))))))

(define-compiler-macro mapcar (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to MAPCAR"))
    ((null (cdr lists))
     `(map1 ,func ,@lists))
    ((null (cddr lists))
     `(map2 ,func ,@lists))
    (form)))

(labels
    ((reduce-form (form)
       (aif (and (consp form)
                 (compiler-macro-function (car form)))
            (funcall it form)
            form))
     (reduce-sum (nums)
       (cond
         ((not nums) 0)
         ((not (cdr nums)) (car nums))
         ((when (and (numberp (car nums))
                     (not (numberp (cadr nums))))
            (setf nums (list (cadr nums) (car nums)))
            nil))
         ((numberp (cadr nums))
          (cond
            ((numberp (car nums))
             (+ (car nums) (cadr nums)))
            ((= 1 (cadr nums))
             `(1+ ,(car nums)))
            ((= -1 (cadr nums))
             `(1- ,(car nums)))
            ((> 0 (cadr nums))
             `(- ,(car nums) ,(- (cadr nums))))))))
     (reduce-sub (nums)
       (cond
         ((not (cdr nums))
          (when (numberp (car nums))
            (- (car nums))))
         ((numberp (cadr nums))
          (cond
            ((numberp (car nums))
             (- (car nums) (cadr nums)))
            ((= 1 (cadr nums))
             `(1- ,(car nums))))))))
  (define-compiler-macro + (&whole form &rest nums)
    (or (and (null (cddr nums))
             (reduce-sum (mapcar #'reduce-form nums)))
        form))
  (define-compiler-macro - (&whole form &rest nums)
    (or (and (null (cddr nums))
             (reduce-sub (mapcar #'reduce-form nums)))
        form)))

(labels ((make-dementor (place delta inc)
           (symbol-macrolet ((dement `(,inc ,place ,delta)))
             (cond
               ((safe-atom-p place)
                `(setq ,place ,dement))
               ((multiple-value-bind (temps value-forms store-vars store-form place)
                                     (get-setf-expansion place)
                  `(let* (,@(mapcar #'list temps value-forms)
                          (,(car store-vars) ,dement))
                     ,store-form)))))))
  (def-emac incf (place &optional (add 1))
    (make-dementor place add '+))
  (def-emac decf (place &optional (sub 1))
    (make-dementor place sub '-)))

;;; lists

(def-emac dolist ((var list-form &rest result-form) &body body)
  (let ((list (gensym "list"))
        (next (gensym "next"))
        (end (gensym "end")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(block nil
         (let (,var (,list ,list-form))
           (declare ,@declarations)
           (tagbody
            ,next
            (unless ,list
              (go ,end))
            (setf ,var (pop ,list))
            ,@body
            (go ,next)
            ,end
            (setf ,var nil))
           ,@result-form)))))

(defmacro with-collectors ((&rest names) &body body)
  (let (lists tails syms adders)
    (flet ((mk-collector (name)
             (let ((vlist (gensym))
                   (vtail (gensym))
                   (vadd name))
               (push vlist lists)
               (push vtail tails)
               (push `(,name (cdr ,vlist)) syms)
               (push `(,vadd (el)
                       `(setf ,',vtail (setf (cdr ,',vtail)
                                             (cons ,el nil))))
                     adders))))
      (foreach names #'mk-collector)
      `(let* (,@(mapcar (lambda (name) `(,name (list nil)))
                        lists)
              ,@(mapcar #'list tails lists))
         (macrolet (,@adders)
           (symbol-macrolet (,@syms)
             ,@body))))))

(def-efun collect-if (test list)
  (with-collectors (elements)
    (dolist (el list)
      (when (funcall test el)
        (elements el)))
    elements))

(def-efun remove (item list)
  (collect-if (lambda (x)
                (not (eq x item))) list))

;; every returns false as soon as any invocation of predicate
;; returns false. If the end of a sequence is reached, every returns
;; true. Thus, every returns true if and only if every invocation of
;; predicate returns true.
(def-efun every (test . lists)
  (let scan ((tails lists))
    (if (finished tails) t
        (and (apply test (map1 #'car tails))
             (scan (map1 #'cdr tails))))))

;; some returns the first non-nil value which is returned by an
;; invocation of predicate. If the end of a sequence is reached
;; without any invocation of the predicate returning true, some
;; returns false. Thus, some returns true if and only if some
;; invocation of predicate returns true.
(def-efun some (test . lists)
  (let scan ((tails lists))
    (if (finished tails) nil
        (or (apply test (map1 #'car tails))
            (scan (map1 #'cdr tails))))))

;; notany returns false as soon as any invocation of predicate
;; returns true. If the end of a sequence is reached, notany returns
;; true. Thus, notany returns true if and only if it is not the case
;; that any invocation of predicate returns true.
(def-efun notany (test . lists)
  (let scan ((tails lists))
    (if (finished tails) t
        (if (apply test (map1 #'car tails))
            nil
            (scan (map1 #'cdr tails))))))

;; notevery returns true as soon as any invocation of predicate
;; returns false. If the end of a sequence is reached, notevery
;; returns false. Thus, notevery returns true if and only if it is
;; not the case that every invocation of predicate returns true.
(def-efun notevery (test . lists)
  (let scan ((tails lists))
    (if (finished tails) nil
        (if (apply test (map1 #'car tails))
            (scan (map1 #'cdr tails))
            t))))

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

(def-efun merge (a b predicate)
  (let* ((ret (list nil))
         (p ret))
    (macrolet ((add (cell)
                 `(setf p (setf (cdr p) ,cell))))
      (tagbody
       next
       (cond ((and a b)
              (if (funcall predicate (car b) (car a))
                  (setf b (cdr (add b)))
                  (setf a (cdr (add a))))
              (go next))
             (a (add a))
             (b (add b))))
      (cdr ret))))

(def-efun stable-sort (list predicate)
  (let sort ((list list))
    (cond ((not list) nil)
          ((not (cdr list)) list)
          (t (let ((a list)
                   (b (%nhalf-list list)))
               (merge (sort a) (sort b) predicate))))))

(setf (symbol-function 'sort) #'stable-sort)
(export '(sort export import))

(defmacro first (list)
  `(car ,list))

(defmacro second (list)
  `(car (cdr ,list)))

(defmacro third (list)
  `(car (cddr ,list)))

(defmacro fourth (list)
  `(car (cdddr ,list)))

(defmacro rest (list)
  `(cdr ,list))

;;; basic looping

(def-emac dotimes ((var count-form &rest result-form) &body body)
  (let ((count (gensym))
        (next (gensym "next"))
        (end (gensym "end")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(block nil
         (let ((,count ,count-form)
               (,var 0))
           (declare ,@declarations)
           (tagbody
            ,next
            (unless (< ,var ,count)
              (go ,end))
            ,@body
            (incf ,var)
            (go ,next)
            ,end)
           ,@result-form)))))

;;; do and do* differ by exactly two characters, but oh well... copy-paste FTW.

(def-emac do (vars (end-test-form &rest result-form) &body body)
  (let ((next (gensym "next"))
        (end (gensym "end"))
        (step (apply #'nconc (mapcar (lambda (var)
                                       (when (> (length var) 2)
                                         (list (car var) (caddr var))))
                                     vars))))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(block nil
         (let ,(mapcar (lambda (var)
                         (list (car var) (cadr var)))
                       vars)
           (declare ,@declarations)
           (tagbody
            ,next
            (when ,end-test-form
              (go ,end))
            ,@body
            (psetf ,@step)
            (go ,next)
            ,end)
           ,@result-form)))))

(def-emac do* (vars (end-test-form &rest result-form) &body body)
  (let ((next (gensym "next"))
        (end (gensym "end"))
        (step (apply #'nconc (mapcar (lambda (var)
                                       (when (> (length var) 2)
                                         (list (car var) (caddr var))))
                                     vars))))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(block nil
         (let* ,(mapcar (lambda (var)
                          (list (car var) (cadr var)))
                        vars)
           (declare ,@declarations)
           (tagbody
            ,next
            (when ,end-test-form
              (go ,end))
            ,@body
            (setf ,@step)
            (go ,next)
            ,end)
           ,@result-form)))))

(def-efun nth (n list)
  (car (nthcdr n list)))

(defun (setf nth) (value n list)
  (setf (car (nthcdr n list)) value))

(def-emac use-package (source &optional (target *package*))
  `(%use-package (find-package ,source) (find-package ,target)))

;;; multiple values

(defmacro multiple-value-call (fn &rest values)
  `(apply ,fn (nconc ,@(mapcar (lambda (form)
                                 `(multiple-value-list ,form))
                               values))))

(defun values-list (list)
  (apply #'values list))

(defmacro multiple-value-setq ((&rest places) value-form)
  (cond
    ((null places)
     `(values ,value-form))
    (t
     (let ((syms (mapcar (lambda (place)
                           (list* (gensym "mvs")
                                  (multiple-value-list
                                   (get-setf-expansion place))))
                         places)))
       `(let* (,@(apply #'append (mapcar (lambda (exp)
                                           (let ((temps (cadr exp))
                                                 (value-forms (caddr exp)))
                                             (mapcar #'list temps value-forms)))
                                         syms)))
          (multiple-value-bind ,(mapcar #'car syms) ,value-form
            (let (,@(append (mapcar (lambda (exp)
                                      (let ((store-vars (cadddr exp)))
                                        `(,(car store-vars) ,(car exp))))
                                    syms)))
              ,@(mapcar (lambda (exp)
                          (elt exp 4))
                        syms)
              ,(caar syms))))))))

(def-emac with-output-to-string ((var &optional string) &body body)
  `(let ((,var (%make-output-stream)))
     ,@(when string
         `((%stream-put ,var ,string)))
     ,@body
     (%stream-get ,var)))

(defmacro defun-memoize (name args &body body)
  (let ((memo (gensym "memo")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(let ((,memo (make-hash)))
         (defun ,name ,args
           (declare ,@declarations)
           (or (hash-get ,memo ,(car args))
               (hash-add ,memo ,(car args) (progn ,@body))))))))

(define-compiler-macro identity (x) x)

(def-emac prog (bindings &body body)
  (multiple-value-bind (body declarations) (%:dig-declarations body)
    `(block nil
       (let ,bindings
         (declare ,@declarations)
         (tagbody ,@body)))))

(def-emac prog* (bindings &body body)
  (multiple-value-bind (body declarations) (%:dig-declarations body)
    `(block nil
       (let* ,bindings
         (declare ,@declarations)
         (tagbody ,@body)))))

(def-efun make-list (size &key initial-element)
  (let ((list nil))
    (dotimes (i size list)
      (setq list (cons initial-element list)))))

(def-efun make-array (dimensions &key
                                 element-type
                                 initial-element
                                 initial-contents
                                 adjustable
                                 fill-pointer
                                 displaced-to
                                 displaced-index-offset)
  (when (listp dimensions)
    (error "make-array: multi-dimensional arrays not supported (yet?)"))
  (make-vector dimensions initial-element initial-contents))

(define-compiler-macro make-array (&whole form
                                          dimensions &key
                                          initial-element
                                          initial-contents
                                          &allow-other-keys)
  (cond
    ((listp dimensions)
     form)
    (t
     `(make-vector ,dimensions ,initial-element ,initial-contents))))

(def-efun aref (array &rest pos)
  (when (cdr pos)
    (error "aref: multi-dimensional arrays not supported (yet?)"))
  (svref array (car pos)))

(define-compiler-macro aref (&whole form array &rest pos)
  (cond
    ((cdr pos) form)
    (t
     `(svref ,array ,(car pos)))))

(defun (setf aref) (value array &rest pos)
  (when (cdr pos)
    (error "(setf aref): multi-dimensional arrays not supported (yet?)"))
  (setf (svref array (car pos)) value))

(define-compiler-macro (setf aref) (&whole form value array &rest pos)
  (cond
    ((cdr pos) form)
    (t
     `(setf (svref ,array ,(car pos)) ,value))))

(defglobal lambda-list-keywords '(&key &rest &body &whole &optional &aux &allow-other-keys))

(export '(*standard-output*
          *error-output*
          *trace-output*
          destructuring-bind))
