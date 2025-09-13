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
          prog1 prog2 or and cond member case ecase otherwise mapcar mapc aif it push
          error warn assert without-interrupts
          eval compile load function functionp unwind-protect
          apply funcall macrolet symbol-macrolet catch throw
          quote lambda λ let let* if progn progv setq t nil not
          tagbody go block return return-from
          *package* *read-table*
          elt rplaca rplacd nth nthcdr last reverse nreverse append nconc nreconc revappend
          char schar char-name char-code name-char code-char upcase downcase string-upcase string-downcase
          charp char= char< char<= char> char>= char/= letterp digitp
          stringp string= string< string<= string> string>= string/=
          char-equal char-not-equal char-lessp char-greaterp char-not-lessp char-not-greaterp
          string-equal string-not-equal string-lessp string-greaterp string-not-lessp string-not-greaterp
          make-regexp regexp-test regexp-exec replace-regexp quote-regexp regexpp
          vectorp vector svref vector-push vector-pop make-vector
          getf
          list list* copy-list copy-seq listp cons consp eq eql equal equalp gensym length
          declare locally type ignore special optimize speed debug space fixnum integer unsigned-byte
          identity constantly

          export import

          most-positive-fixnum most-negative-fixnum

          numberp zerop plusp minusp evenp oddp parse-number parse-integer number-fixed number-string
          < <= > >= + - * / = /= null 1+ 1- floor ceiling round mod
          abs sin asin cos acos tan atan exp log sqrt expt random
          min max

          make-hash hash-table-p gethash remhash hash-copy hash-keys hash-values
          hash-iterator iterator-next

          macroexpand-1 disassemble

          make-package find-package make-symbol symbol-name symbol-package symbol-function
          packagep symbolp keywordp intern unintern shadow find-symbol package-name
          threadp make-thread current-thread set-timeout clear-timeout

          car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
          cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
          cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
          first second third fourth fifth rest destructuring-bind

          boundp makunbound fboundp fmakunbound

          get-internal-run-time
          compiler-macro-function
          symbol-value

          values multiple-value-bind multiple-value-call values-list
          multiple-value-list multiple-value-setq multiple-value-prog1

          *standard-output* *error-output* *trace-output*

          lambda-list-keywords
          &key &rest &body &whole &optional &aux &allow-other-keys

          define-modify-macro macroexpand-1 macroexpand defpackage in-package
          defparameter defvar defconstant defglobal every some notany notevery defsetf
          define-setf-expander get-setf-expansion setf psetf psetq push pop
          define-compiler-macro incf decf dolist dotimes do do* complement adjoin
          pushnew nth use-package with-output-to-string prog prog* make-list
          make-array aref copy-tree

          sxhash))

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

(defun assert (cond . arguments)
  (unless cond (apply #'error arguments)))

(defun macroexpand-1 (form)
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

(defun macroexpand (form)
  (let ((result (macroexpand-1 form)))
    (if (eq result form)
        result
        (macroexpand result))))

(defmacro defpackage (name &rest options)
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

(defmacro in-package (name)
  (setq *package* (find-package name)) nil)

(defmacro defglobal (name val)
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

(define-compiler-macro mapcar (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to MAPCAR"))
    ((null (cdr lists))
     `(map1 ,func ,@lists))
    ((null (cddr lists))
     `(map2 ,func ,@lists))
    (form)))

(defun mapc (f . lists)
  (let ((first (car lists)))
    (tagbody
     :loop
     (unless (finished lists)
       (apply f (mapcar #'car lists))
       (setq lists (mapcar #'cdr lists))
       (go :loop)))
    first))

;; every returns false as soon as any invocation of predicate
;; returns false. If the end of a sequence is reached, every returns
;; true. Thus, every returns true if and only if every invocation of
;; predicate returns true.
(defun every (test . lists)
  (let scan ((tails lists))
    (if (finished tails) t
        (and (apply test (mapcar #'car tails))
             (scan (mapcar #'cdr tails))))))

(defun every1 (test list)
  (tagbody
   :loop
   (cond
     ((null list)
      (return-from every1 t))
     ((funcall test (%pop list))
      (go :loop)))))

(define-compiler-macro every (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to EVERY"))
    ((null (cdr lists))
     `(every1 ,func ,@lists))
    (form)))

;; some returns the first non-nil value which is returned by an
;; invocation of predicate. If the end of a sequence is reached
;; without any invocation of the predicate returning true, some
;; returns false. Thus, some returns true if and only if some
;; invocation of predicate returns true.
(defun some (test . lists)
  (let scan ((tails lists))
    (if (finished tails) nil
        (or (apply test (mapcar #'car tails))
            (scan (mapcar #'cdr tails))))))

(defun some1 (test list)
  (tagbody
   :loop
   (when list
     (aif (funcall test (%pop list))
          (return-from some1 it))
     (go :loop))))

(define-compiler-macro some (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to SOME"))
    ((null (cdr lists))
     `(some1 ,func ,@lists))
    (form)))

;; notany returns false as soon as any invocation of predicate
;; returns true. If the end of a sequence is reached, notany returns
;; true. Thus, notany returns true if and only if it is not the case
;; that any invocation of predicate returns true.
(defun notany (test . lists)
  (let scan ((tails lists))
    (if (finished tails) t
        (if (apply test (mapcar #'car tails))
            nil
            (scan (mapcar #'cdr tails))))))

(defun notany1 (test list)
  (tagbody
   :loop
   (cond
     ((null list)
      (return-from notany1 t))
     ((funcall test (%pop list))
      (return-from notany1 nil)))
   (go :loop)))

(define-compiler-macro notany (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to NOTANY"))
    ((null (cdr lists))
     `(notany1 ,func ,@lists))
    (form)))

;; notevery returns true as soon as any invocation of predicate
;; returns false. If the end of a sequence is reached, notevery
;; returns false. Thus, notevery returns true if and only if it is
;; not the case that every invocation of predicate returns true.
(defun notevery (test . lists)
  (let scan ((tails lists))
    (if (finished tails) nil
        (if (apply test (mapcar #'car tails))
            (scan (mapcar #'cdr tails))
            t))))

(defun notevery1 (test list)
  (tagbody
   :loop
   (cond
     ((null list)
      (return-from notevery1 nil))
     ((funcall test (%pop list))
      (go :loop))
     (t
      (return-from notevery1 t)))))

(define-compiler-macro notevery (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to NOTEVERY"))
    ((null (cdr lists))
     `(notevery1 ,func ,@lists))
    (form)))

(defmacro with-collectors ((&rest names) &body body)
  (let (lists tails syms adders)
    (flet ((mk-collector (arg)
             (let* ((vlist (gensym))
                    (vtail (gensym))
                    (name (if (consp arg) (car arg) arg))
                    (vadd (if (consp arg) (cadr arg) arg))
                    (vconc (if (consp arg) (caddr arg))))
               (push vlist lists)
               (push vtail tails)
               (push `(,name (cdr ,vlist)) syms)
               (when vadd
                 (push `(,vadd (el)
                         `(setq ,',vtail (setf (cdr ,',vtail) (cons ,el nil))))
                       adders))
               (when vconc
                 (push `(,vconc (lst)
                         `(setq ,',vtail (last (setf (cdr ,',vtail) ,lst))))
                       adders)))))
      (foreach names #'mk-collector)
      `(let (,@(mapcar (lambda (name) `(,name (list nil))) lists))
         (let (,@(mapcar #'list tails lists))
           (macrolet (,@adders)
             (symbol-macrolet (,@syms)
               ,@body)))))))

;;; setf

(defmacro %mvb-internal (names form &body body)
  `(multiple-value-bind ,names ,form ,@body))

(defmacro defsetf (access-fn lambda-list &optional store-vars &body body)
  (maybe-xref-info access-fn 'defsetf)
  (cond
    ((symbolp lambda-list)
     `(%set-symbol-prop ',access-fn :setf ',lambda-list))
    ((> (length store-vars) 1)
     (let ((arg (gensym "setfarg")))
       `(%set-symbol-prop ',access-fn :setf
                          (%fn ,access-fn (,@lambda-list)
                               (lambda (,arg)
                                 (symbol-macrolet
                                     (,@(mapcar (lambda (sym)
                                                  `(,sym ',sym))
                                                store-vars))
                                   `(%mvb-internal ,'(,@store-vars) ,,arg
                                                   ,,@body)))))))
    (t
     `(%set-symbol-prop ',access-fn :setf
                        (%fn ,access-fn (,@lambda-list)
                             (lambda ,store-vars ,@body))))))

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (maybe-xref-info access-fn 'define-setf-expander)
  `(%set-symbol-prop ',access-fn :setf-exp
                     (%fn ,access-fn (,@lambda-list)
                          ,@body)))

(defun %get-setf-place (form)
  (let dig ((form form))
    (if (symbolp form)
        ;; XXX: return T when form is a symbol (for some reason).
        ;;      not sure this is correct, but it works.
        (values t form)
        (aif (%get-symbol-prop (car form) :setf-exp)
             (values it form t)
             (aif (%get-symbol-prop (car form) :setf)
                  (values it form)
                  (let ((exp (macroexpand-1 form)))
                    (if (eq exp form)
                        (values nil form)
                        (dig exp))))))))

(defun get-setf-expansion (form)
  (cond
    ((symbolp form)
     ;; could be symbol macro.
     (let ((exp (macroexpand-1 form)))
       (unless (eq exp form)
         (return-from get-setf-expansion (get-setf-expansion exp))))
     (let ((vals (list (gensym "new"))))
       (values nil nil vals `(setq ,form ,@vals) form)))
    ((consp form)
     (multiple-value-bind (expander form setf-exp)
                          (%get-setf-place form)
       (when (eq expander t)
         ;; XXX: see above in `%get-setf-place'. We receive T when we got down
         ;; to a symbol. By recursing here we also expand an eventual
         ;; symbol-macrolet on that symbol.
         (return-from get-setf-expansion (get-setf-expansion form)))
       (when setf-exp
         (return-from get-setf-expansion (apply expander (cdr form))))
       (let* ((temps (mapcar (lambda (subform)
                               (gensym "temp"))
                             (cdr form)))
              (vals (list (gensym "new")))
              (store-form (cond
                            ((functionp expander)
                             (funcall (apply expander temps) (car vals)))
                            ((and expander
                                  (symbolp expander))
                             `(,expander ,@temps ,@vals))
                            ((symbolp (car form))
                             (let ((setter (maybe-setter `(setf ,(car form)))))
                               `(,setter ,(car vals) ,@temps)))
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

(defun %call-default-setter (form value)
  (let ((setter (maybe-setter `(setf ,(car form)))))
    (cond
      ((or (safe-atom-p value)
           (every #'safe-atom-p (cdr form)))
       ;; it's safe to compute the value first.
       `(,setter ,value ,@(cdr form)))
      (t
       (let ((tmpvars (mapcar (lambda (val)
                                (list (gensym) val))
                              (cdr form))))
         `(let (,@tmpvars)
            (,setter ,value ,@(mapcar #'car tmpvars))))))))

(defun %call-setf-expansion (temps vals stores set value)
  ;; this got ugly, because I'd like to dig the values and bind
  ;; whatever is “simple” via symbol-macrolet so the generated code
  ;; would be smaller.
  (let rec ((simple nil)
            (complex nil))
    (cond
      ((null temps)
       ;; this is the final setter form.
       `(let ,(nreverse complex)
          (symbol-macrolet ,(nreverse simple)
            ,(cond
               ((cdr stores)
                `(multiple-value-bind ,stores ,value ,set))
               ((safe-atom-p value)
                `(symbol-macrolet ((,(car stores) ,value)) ,set))
               (t
                `(let ((,(car stores) ,value)) ,set))))))
      ((let* ((val (%pop vals))
              (safe? (safe-atom-p val)))
         (rec (if safe?
                  ;; if safe, we add it to the “simple” list (symbol-macrolet)
                  (cons (list (%pop temps) val) simple)
                  simple)
              (if safe?
                  complex
                  ;; if not safe, we add it to the “complex” list (let)
                  (cons (list (%pop temps) val) complex))))))))

(defun %make-set-form (place value)
  (cond
    ((consp place)
     (multiple-value-bind (expander form setf-exp) (%get-setf-place place)
       (cond
         ((eq expander t)
          ;; form is a symbol.
          `(setq ,form ,value))
         (setf-exp
          (multiple-value-bind (temps vals stores set)
                               (apply expander (cdr form))
            (%call-setf-expansion temps vals stores set value)))
         ((and (null expander)
               (symbolp (car form)))
          ;; no global setter defined, but maybe there is a (SETF FOO) somewhere.
          (%call-default-setter form value))
         ((functionp expander)
          (funcall (apply expander (cdr form)) value))
         ((symbolp expander)
          `(,expander ,@(cdr form) ,value))
         ((error (strcat "Unknown SETF expander for " (car place) ": " expander))))))
    ((symbolp place)
     `(setq ,place ,value))
    (t (error "Unsupported SETF syntax"))))

(defun %setf (args)
  (when args
    (unless (cdr args)
      (error "Odd number of arguments in SETF"))
    (cons (%make-set-form (car args) (cadr args))
          (%setf (cddr args)))))

(defmacro setf args
  `(progn ,@(%setf args)))

(defun %make-modify-macro (place args function)
  (cond
    ((safe-atom-p place)
     `(setq ,place (,function ,place ,@args)))
    (t
     (multiple-value-bind (temp-vars temp-vals stores set get)
                          (get-setf-expansion place)
       (%call-setf-expansion
        temp-vars temp-vals stores set
        `(,function ,get ,@args))))))

(defmacro define-modify-macro (name lambda-list function &optional docstring)
  (let* ((place (make-symbol "place"))
         (parsed (parse-lambda-list lambda-list))
         (args (append (getf parsed :required)
                       (mapcar (lambda (arg)
                                 (if (consp arg) (car arg) arg))
                               (getf parsed :optional)))))
    (when (or (getf parsed :has-key)
              (getf parsed :aux)
              (getf parsed :aok))
      (error "DEFINE-MODIFY-MACRO: only &optional and &rest lambda words are expected"))
    `(defmacro ,name (,place ,@lambda-list)
       ,@(when docstring (list docstring))
       (%make-modify-macro ,place
                           (list* ,@args ,(getf parsed :rest))
                           ',function))))

(defsetf car %rplaca)
(defsetf cdr %rplacd)

(defun (setf symbol-function) (func sym)
  (%::maybe-xref-info sym 'defun)
  (set-symbol-function! sym func))

(defun (setf symbol-value) (value sym)
  (set-symbol-value! sym value))

(defmacro push (obj place)
  (cond
    ((safe-atom-p place)
     `(setq ,place (cons ,obj ,place)))
    ((multiple-value-bind (temps value-forms store-vars store-form get-form)
                          (get-setf-expansion place)
       (let ((item (gensym "item"))
             (newval (gensym "newval")))
         `(let* ((,item ,obj)
                 ,@(mapcar #'list temps value-forms)
                 (,newval (cons ,item ,get-form)))
            (symbol-macrolet ((,(car store-vars) ,newval))
              ,store-form)))))))

(defmacro pop (place)
  (let ((v (gensym "place")))
    (cond
      ((safe-atom-p place)
       `(%:%pop ,place))
      ((multiple-value-bind (temps value-forms store-vars store-form get-form)
                            (get-setf-expansion place)
         `(let* (,@(mapcar #'list temps value-forms)
                 (,v ,get-form))
            (symbol-macrolet ((,(car store-vars) (cdr ,v)))
              ,store-form)
            (car ,v)))))))

(define-setf-expander getf (place indicator &optional default)
  (multiple-value-bind (place-tempvars place-tempvals stores set get)
                       (get-setf-expansion place)
    (let ((vindicator (gensym "indicator"))
          (vdefault (gensym "default"))
          (newval (gensym "newval")))
      (values `(,@place-tempvars ,vindicator ,vdefault)
              `(,@place-tempvals ,indicator ,default)
              `(,newval)
              `(symbol-macrolet ((,(car stores) (%:%putf ,get ,vindicator ,newval)))
                 ,set
                 ,newval)
              `(getf ,get ,vindicator ,vdefault)))))

(defun constantly (value)
  (lambda args
    (declare (ignore args))
    value))

(defun (setf svref) (value vector index)
  (vector-set value vector index))

(define-compiler-macro (setf svref) (value vector index)
  `(vector-set ,value ,vector ,index))

(define-modify-macro incf (&optional (add 1)) +)
(define-modify-macro decf (&optional (add 1)) -)

;;; lists

(defmacro first (list)
  `(car ,list))

(defmacro second (list)
  `(car (cdr ,list)))

(defmacro third (list)
  `(car (cddr ,list)))

(defmacro fourth (list)
  `(car (cdddr ,list)))

(defmacro fifth (list)
  `(car (cddddr ,list)))

(defmacro rest (list)
  `(cdr ,list))

;;; basic looping

(defmacro dolist ((var list-form &rest result-form) &body body)
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

(defmacro dotimes ((var count-form &rest result-form) &body body)
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

(defmacro do* (vars (end-test-form &rest result-form) &body body)
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

(defmacro psetf args
  (with-collectors (temps places vals)
    (do* ((args args (cddr args)))
         ((null args) `(let ,(mapcar #'list temps vals)
                         ,@(mapcar (lambda (var set)
                                     `(setf ,var ,set))
                                   places temps)
                         nil))
      (when (null (cadr args))
        (error "PSETF: Odd number of forms"))
      (temps (gensym "psetf"))
      (places (car args))
      (vals (cadr args)))))

(defmacro psetq args
  `(psetf ,@args))

(defmacro do (vars (end-test-form &rest result-form) &body body)
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

(defun member (item lst &key test test-not key)
  (when test-not
    (when test
      (error "MEMBER: both TEST and TEST-NOT are given"))
    (setf test (complement test-not)))
  (let ((no-test (or (not test) (eq test #'eq) (eq test #'eql)))
        (no-key (or (not key) (eq key #'identity))))
    (cond
      (no-key
       (if no-test
           (%:%memq item lst)
           (let rec ((lst lst))
             (when lst
               (if (funcall test item (car lst))
                   lst
                   (rec (cdr lst)))))))
      (no-test
       (let rec ((lst lst))
         (when lst
           (if (eq item (funcall key (car lst)))
               lst
               (rec (cdr lst))))))
      (t
       (let rec ((lst lst))
         (when lst
           (if (funcall test item (funcall key (car lst)))
               lst
               (rec (cdr lst)))))))))

(defconstant +member-safe-test+ (list `#'eql #'eql `#'eq #'eq 'eql 'eq ''eql ''eq))
(defconstant +member-safe-key+ (list `#'identity #'identity 'identity ''identity))

(define-compiler-macro member (&whole form item lst &key test test-not key)
  (cond
    ((and (not test-not)
          (or (not test) (member test +member-safe-test+ :test #'equal))
          (or (not key) (member key +member-safe-key+ :test #'equal)))
     `(%:%memq ,item ,lst))
    (t form)))

(defun adjoin (item lst &rest args &key test test-not key)
  (if (apply #'member (if key (funcall key item) item) lst args)
      lst
      (cons item lst)))

(defmacro pushnew (obj place &rest args &key test test-not key)
  (let ((vobj (gensym "obj"))
        (vcurr (gensym "curr")))
    (multiple-value-bind (temps vals stores set get)
                         (get-setf-expansion place)
      `(let* ((,vobj ,obj)
              ,@(mapcar #'list temps vals)
              (,vcurr ,get))
         (symbol-macrolet ((,(car stores) (adjoin ,vobj ,vcurr ,@args)))
           ,set)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun (setf nth) (value n list)
  (setf (car (nthcdr n list)) value))

(defmacro use-package (source &optional (target *package*))
  `(%use-package (find-package ,source) (find-package ,target)))

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

(defmacro with-output-to-string ((var &optional string) &body body)
  `(let ((,var (%make-output-stream)))
     ,@(when string
         `((%stream-put ,var ,string)))
     ,@body
     (%stream-get ,var)))

(defmacro defun-memoize (name args &body body)
  (let ((memo (gensym "memo")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(let ((,memo (make-hash)))
         (%:%set-symbol-prop ',name "MEMOIZE" ,memo)
         (defun ,name ,args
           (declare ,@declarations)
           (or (gethash ,(car args) ,memo)
               (%hash-set (progn ,@body) ,(car args) ,memo)))))))

(defmacro defun-memoize2 (name args &body body)
  (let ((memo (gensym "memo")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(let ((,memo (make-hash)))
         (%:%set-symbol-prop ',name "MEMOIZE" ,memo)
         (defun ,name ,args
           (declare ,@declarations)
           (aif (gethash ,(car args) ,memo)
                (or (gethash ,(cadr args) it)
                    (%hash-set (progn ,@body) ,(cadr args) it))
                (let ((val (progn ,@body)))
                  (%hash-set (make-hash ,(cadr args) val) ,(car args) ,memo)
                  val)))))))

(defmacro prog (bindings &body body)
  (multiple-value-bind (body declarations) (%:dig-declarations body)
    `(block nil
       (let ,bindings
         (declare ,@declarations)
         (tagbody ,@body)))))

(defmacro prog* (bindings &body body)
  (multiple-value-bind (body declarations) (%:dig-declarations body)
    `(block nil
       (let* ,bindings
         (declare ,@declarations)
         (tagbody ,@body)))))

(defun make-list (size &key initial-element)
  (let ((list nil))
    (dotimes (i size list)
      (setq list (cons initial-element list)))))

(defun make-array (dimensions &key
                              element-type
                              initial-element
                              initial-contents
                              adjustable
                              fill-pointer
                              displaced-to
                              displaced-index-offset)
  (when (listp dimensions)
    (error "MAKE-ARRAY: multi-dimensional arrays not supported (yet?)"))
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

(defun aref (array &rest pos)
  (when (cdr pos)
    (error "AREF: multi-dimensional arrays not supported (yet?)"))
  (svref array (car pos)))

(define-compiler-macro aref (&whole form array &rest pos)
  (cond
    ((cdr pos) form)
    (t
     `(svref ,array ,(car pos)))))

(defun (setf aref) (value array &rest pos)
  (when (cdr pos)
    (error "SETF AREF: multi-dimensional arrays not supported (yet?)"))
  (setf (svref array (car pos)) value))

(defun (setf elt) (value seq index)
  (cond
    ((vectorp seq)
     (setf (svref seq index) value))
    ((listp seq)
     (let ((cell (nthcdr index seq)))
       (unless cell (error (strcat "SETF ELT: Index " index " too large")))
       (setf (car cell) value)))
    (t
     (error "SETF ELT: Unknown sequence"))))

(defun copy-tree (tree)
  (if (atom tree) tree
      (cons (copy-tree (car tree))
            (copy-tree (cdr tree)))))

(defun (setf cadr) (value list)
  (setf (car (cdr list)) value))

(define-setf-expander values (&rest places)
  (with-collectors (temp-vars temp-vals store-main-vars store-other-vars setters getters)
    (dolist (place places)
      (multiple-value-bind (place-temps place-vals place-stores place-set place-get)
                           (get-setf-expansion place)
        (dolist (el place-temps) (temp-vars el))
        (dolist (el place-vals) (temp-vals el))
        (dolist (el (cdr place-stores)) (store-other-vars el))
        (store-main-vars (car place-stores))
        (setters place-set)
        (getters place-get)))
    (values temp-vars
            temp-vals
            store-main-vars
            `(let (,@store-other-vars)
               (values ,@setters))
            `(values ,@getters))))

(defglobal lambda-list-keywords '(&key &rest &body &whole &optional &aux &allow-other-keys))
