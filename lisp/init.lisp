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
  (setq
   exported
   '(

     &allow-other-keys &aux &body &key &optional &rest &whole &environment *
     *error-output* *package* *read-table* *standard-input* *standard-output*
     *trace-output* + - / /= 1+ 1- < <= = > >= abs acos adjoin aif and append
     apply aref asin assert atan atom block boolean boundp caaaar caaadr caaar
     caadar caaddr caadr caar cadaar cadadr cadar caddar cadddr caddr cadr car
     case catch cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar cddadr
     cddar cdddar cddddr cdddr cddr cdr ceiling char character characterp
     char-code char-equal char-greaterp char-lessp char-name char-not-equal
     char-not-greaterp char-not-lessp char/= char< char<= char= char> char>=
     char-upcase char-downcase charp clear-timeout code-char compile
     compiler-macro-function complement cond cons consp constantly copy-list
     copy-seq copy-tree cos current-thread debug decf declare declaim inline
     defconstant defglobal define-compiler-macro define-modify-macro
     define-setf-expander defmacro defpackage defparameter defsetf defun
     defvar destructuring-bind digitp digit-char-p disassemble do do* dolist
     dotimes downcase ecase elt eq eql equal equalp error eval evenp every exp
     export expt fboundp fdefinition fifth find-package find-symbol first
     fixnum flet float floatp floor fmakunbound foreach fourth funcall
     function functionp gensym get get-internal-run-time get-setf-expansion
     getf gethash go hash-copy hash-iterator hash-keys hash-table hash-table-p
     hash-values identity if ignore import in-package incf integer integerp
     intern it iterator-next keywordp labels lambda lambda-list-keywords last
     length let let* letterp list list* listp load locally log macroexpand
     macroexpand-1 macrolet make-array make-hash make-list make-package
     make-regexp make-symbol make-thread make-vector makunbound mapc mapcar
     maplist max member min minusp mod most-negative-fixnum
     most-positive-fixnum multiple-value-bind multiple-value-call
     multiple-value-list multiple-value-prog1 multiple-value-setq name-char
     nconc nil not notany notevery nreconc nreverse nth nthcdr null endp
     number-fixed number-string number numberp oddp optimize or otherwise
     package-name package packagep parse-integer parse-number plusp pop prog
     prog* prog1 prog2 progn progv psetf psetq push pushnew quasiquote quote
     quote-regexp random regexp regexp-exec regexp-test regexpp remhash
     replace-regexp rest return return-from revappend reverse rotatef round
     rplaca rplacd schar second set-timeout setf setq shadow shiftf sin sleep
     some space special speed sqrt standard-object string string-capitalize
     string-downcase string-equal string-greaterp string-lessp
     string-not-equal string-not-greaterp string-not-lessp string-upcase
     string/= string< string<= string= string> string>= stringp structure
     svref sxhash symbol symbol-function symbol-macrolet symbol-name
     symbol-package symbol-plist symbol-value symbolp t tagbody tan third
     thread threadp throw type type-of typep unintern unless unsigned-byte
     unwind-protect upcase use-package values values-list vector vector-pop
     vector-push vectorp warn when with-output-to-string without-interrupts
     zerop λ

     stream input-stream output-stream text-input-stream text-output-stream

     ))

  (export exported boot)
  (export exported main)
  (setq *package* main))

"
(in-package :sl)
"

(defun assert (test . arguments)
  (unless test (apply #'error arguments)))

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
                    `(export ',(cdr opt) ,pak))
                   (:import-from
                    (destructuring-bind (source &rest names) (cdr opt)
                      (setq source (find-package source))
                      `(import ',(map1 (lambda (name)
                                         (find-symbol name source))
                                       names)
                               ,pak)))))
               options)
       ,pak)))

(defmacro in-package (name)
  `(setq *package* (find-package ',name)))

(defun mapc1 (f list)
  (let rec ((p list))
    (if (not p) list
        (progn
          (funcall f (%pop p))
          (rec p)))))

(defun mapc2 (f list1 list2)
  (let rec ((p list1)
            (q list2))
    (if (not (and p q)) list1
        (progn
          (funcall f (%pop p) (%pop q))
          (rec p q)))))

(defun mapc (f . lists)
  (let ((first (car lists)))
    (let rec ((lists lists))
      (if (finished lists) first
          (progn
            (apply f (mapcar #'car lists))
            (rec (mapcar #'cdr lists)))))))

(define-compiler-macro mapc (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to MAPC"))
    ((null (cdr lists))
     `(mapc1 ,func ,@lists))
    ((null (cddr lists))
     `(mapc2 ,func ,@lists))
    (form)))

(defun some1 (test list)
  (let rec ((list list))
    (when list
      (or (funcall test (%pop list))
          (rec list)))))

(defun finished (tails)
  (some1 #'null tails))

(define-compiler-macro finished (tails)
  `(some1 #'null ,tails))

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

(defun maplist1 (func lst)
  (let rec ((ret nil) (lst lst))
    (if lst
        (rec (cons (funcall func lst) ret)
             (cdr lst))
        (nreverse ret))))

(defun maplist2 (func lst1 lst2)
  (let rec ((ret nil) (lst1 lst1) (lst2 lst2))
    (if (and lst1 lst2)
        (rec (cons (funcall func lst1 lst2) ret)
             (cdr lst1) (cdr lst2))
        (nreverse ret))))

(defun maplist (f . lists)
  (let rec (ret (tails lists))
    (if (finished tails)
        (nreverse ret)
        (rec (cons (apply f tails) ret)
             (map1 #'cdr tails)))))

(define-compiler-macro maplist (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to MAPLIST"))
    ((null (cdr lists))
     `(maplist1 ,func ,@lists))
    ((null (cddr lists))
     `(maplist2 ,func ,@lists))
    (form)))

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
  (let rec ((list list))
    (if (not list) t
        (when (funcall test (%pop list))
          (rec list)))))

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
  (let rec ((list list))
    (if (not list) t
        (if (funcall test (%pop list))
            nil
            (rec list)))))

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
  (let rec ((list list))
    (when list
      (if (funcall test (%pop list))
          (rec list)
          t))))

(define-compiler-macro notevery (&whole form func &rest lists)
  (cond
    ((null lists)
     (error "Missing list argument to NOTEVERY"))
    ((null (cdr lists))
     `(notevery1 ,func ,@lists))
    (form)))

;;; setf

(defmacro with-collectors ((&rest names) &body body)
  (let (lists tails syms adders)
    (flet ((mk-collector (arg)
             (let* ((name (if (consp arg) (car arg) arg))
                    (vlist (gensym (strcat "L" name)))
                    (vtail (gensym (strcat "T" name)))
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

(defun get-setf-expansion (form &optional (*compiler-env* *compiler-env*))
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
           (and (consp value)
                (eq 'quote (car value)))
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

(defun fdefinition (sym)
  (symbol-function sym))

(defun (setf fdefinition) (func sym)
  (set-symbol-function! sym func))

(defun (setf symbol-value) (value sym)
  (set-symbol-value! sym value))

(defun (setf symbol-plist) (plist sym)
  (%:%set-symbol-plist plist sym))

(define-compiler-macro (setf symbol-plist) (plist sym)
  `(%:%set-symbol-plist ,plist ,sym))

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

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun (setf get) (value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) value))

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
            (setq ,@step)
            (go ,next)
            ,end)
           ,@result-form)))))

(defmacro psetf args
  ;; if the places to set are plain symbols (not bound by symbol-macrolet)
  ;; turn it into a %psetq, which is handled more efficiently by the compiler.
  (when (= 2 (length args))
    (return-from psetf `(progn (setf ,@args) nil)))
  (let rec ((p args))
    (if (not p) (return-from psetf `(%:%psetq ,@args))
        (if (%:safe-atom-p (car p))
            (if (cdr p) (rec (cddr p))
                (error "PSETF: missing last value")))))
  (with-collectors (setters)
    (let* ((code (list nil))
           (cell code))
      (do* ((args args (cddr args)))
           ((null args) (progn
                          (setters nil)
                          (setf (car cell) (car setters)
                                (cdr cell) (cdr setters))
                          (car code)))
        (let ((place (car args))
              (value (cadr args)))
          (multiple-value-bind (temps vals stores set get)
                               (get-setf-expansion place)
            (setters set)
            (let ((next-cell (list nil)))
              (setf (car cell)
                    `(let (,@(mapcar #'list temps vals))
                       (multiple-value-bind ,stores ,value
                         . ,next-cell)))
              (setf cell next-cell))))))))

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
            (psetq ,@step)
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

(define-compiler-macro nth (n list)
  `(car (nthcdr ,n ,list)))

(defun (setf nth) (value n list)
  (setf (car (nthcdr n list)) value))

(defmacro use-package (source &optional (target *package*))
  `(%use-package (find-package ,source) (find-package ,target)))

(defmacro with-output-to-string ((var &optional string) &body body)
  `(let ((,var (%make-text-memory-output-stream)))
     ,@(when string
         `((%stream-put ,var ,string)))
     ,@body
     (%get-output-stream-string ,var)))

(defmacro defun-memoize (name args &body body)
  (let ((memo (gensym "memo")))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(let ((,memo (make-hash)))
         (%:%set-symbol-prop ',name :MEMOIZE ,memo)
         (defun ,name ,args
           (declare ,@declarations)
           (or (gethash ,(car args) ,memo)
               (%hash-set (progn ,@body) ,(car args) ,memo)))))))

(defmacro defun-memoize2 (name args &body body)
  (let ((memo (gensym "memo"))
        (val (gensym "val"))
        (h1 (gensym))
        (weak1 (if (consp name) (member :weak1 name)))
        (weak2 (if (consp name) (member :weak2 name)))
        (name (if (consp name) (car name) name)))
    (unless (symbolp name)
      (error "DEFUN-MEMOIZE2: name must be a symbol"))
    (multiple-value-bind (body declarations) (%:dig-declarations body)
      `(let ((,memo (,(if weak1 'make-weak-hash 'make-hash))))
         (%:%set-symbol-prop ',name :MEMOIZE ,memo)
         (defun ,name ,args
           (declare ,@declarations)
           (let ((,h1 (gethash ,(car args) ,memo)))
             (if ,h1
                 (or (gethash ,(cadr args) ,h1)
                     (%hash-set (progn ,@body) ,(cadr args) ,h1))
                 (let ((,val (progn ,@body)))
                   (%hash-set (,(if weak2 'make-weak-hash 'make-hash)
                               ,(cadr args) ,val) ,(car args) ,memo)
                   ,val))))))))

(defun clear-memoize-cache (symbol)
  (clrhash (%get-symbol-prop symbol :MEMOIZE)))

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

;; We do this for completion, to have (SETF CAR) and (SETF CDR) defined. It
;; will not endless-loop, because we have a DEFSETF for CAR and CDR somewhere
;; above (the macro instilled by DEFSETF takes precedence over the function).
(defun (setf car) (val list)
  (setf (car list) val))

(defun (setf cdr) (val list)
  (setf (cdr list) val))

(defun (setf caar) (val list)
  (setf (car (car list)) val))

(defun (setf cadr) (val list)
  (setf (car (cdr list)) val))

(defun (setf cdar) (val list)
  (setf (cdr (car list)) val))

(defun (setf cddr) (val list)
  (setf (cdr (cdr list)) val))

(defun (setf caaar) (val list)
  (setf (car (caar list)) val))

(defun (setf caadr) (val list)
  (setf (car (cadr list)) val))

(defun (setf cadar) (val list)
  (setf (car (cdar list)) val))

(defun (setf caddr) (val list)
  (setf (car (cddr list)) val))

(defun (setf cdaar) (val list)
  (setf (cdr (caar list)) val))

(defun (setf cdadr) (val list)
  (setf (cdr (cadr list)) val))

(defun (setf cddar) (val list)
  (setf (cdr (cdar list)) val))

(defun (setf cdddr) (val list)
  (setf (cdr (cddr list)) val))

(defun (setf caaaar) (val list)
  (setf (car (caaar list)) val))

(defun (setf caaadr) (val list)
  (setf (car (caadr list)) val))

(defun (setf caadar) (val list)
  (setf (car (cadar list)) val))

(defun (setf caaddr) (val list)
  (setf (car (caddr list)) val))

(defun (setf cadaar) (val list)
  (setf (car (cdaar list)) val))

(defun (setf cadadr) (val list)
  (setf (car (cdadr list)) val))

(defun (setf caddar) (val list)
  (setf (car (cddar list)) val))

(defun (setf cadddr) (val list)
  (setf (car (cdddr list)) val))

(defun (setf cdaaar) (val list)
  (setf (cdr (caaar list)) val))

(defun (setf cdaadr) (val list)
  (setf (cdr (caadr list)) val))

(defun (setf cdadar) (val list)
  (setf (cdr (cadar list)) val))

(defun (setf cdaddr) (val list)
  (setf (cdr (caddr list)) val))

(defun (setf cddaar) (val list)
  (setf (cdr (cdaar list)) val))

(defun (setf cddadr) (val list)
  (setf (cdr (cdadr list)) val))

(defun (setf cdddar) (val list)
  (setf (cdr (cddar list)) val))

(defun (setf cddddr) (val list)
  (setf (cdr (cdddr list)) val))

(defmacro with-places ((places &key
                               (temp-vars 'temp-vars)
                               (temp-vals 'temp-vals)
                               (store-main-vars 'store-main-vars)
                               (store-other-vars 'store-other-vars)
                               (setters 'setters)
                               (getters 'getters))
                       &body body)
  `(with-collectors (,temp-vars ,temp-vals ,store-main-vars ,store-other-vars ,setters ,getters)
     (dolist (place ,places)
       (multiple-value-bind (place-temps place-vals place-stores place-set place-get)
                            (get-setf-expansion place)
         (dolist (el place-temps) (,temp-vars el))
         (dolist (el place-vals) (,temp-vals el))
         (dolist (el (cdr place-stores)) (,store-other-vars el))
         (,store-main-vars (car place-stores))
         (,setters place-set)
         (,getters place-get)))
     ,@body))

(define-setf-expander values (&rest places)
  (with-places (places)
    (values temp-vars
            temp-vals
            store-main-vars
            `(let (,@store-other-vars)
               (values ,@setters))
            `(values ,@getters))))

(defmacro multiple-value-setq ((&rest places) value-form)
  (if (null places)
      `(values ,value-form)
      `(values (setf (values ,@places) ,value-form))))

(defmacro rotatef (&rest places)
  (with-places (places)
    `(let (,@(mapcar #'list temp-vars temp-vals))
       (let (,@(mapcar #'list
                       store-main-vars
                       (nconc (cdr getters) (list (car getters)))))
         (let (,@store-other-vars)
           ,@setters
           nil)))))

(defmacro shiftf (&rest args)
  (let* ((oldvalue (gensym))
         (args (reverse args))
         (newvalue (pop args)))
    (with-places ((nreverse args))
      (let ((getters getters))
        `(let (,@(mapcar #'list temp-vars temp-vals))
           (let ((,oldvalue ,(pop getters)))
             (let (,@(mapcar #'list
                             store-main-vars
                             (nconc getters (list newvalue))))
               (let (,@store-other-vars)
                 ,@setters
                 ,oldvalue))))))))

(defglobal lambda-list-keywords '(&key &rest &body &whole &optional &aux &allow-other-keys))

(defmacro with-backtrace (&rest body)
  `(handler-bind ((error (lambda (error)
                           (format *standard-output* "~S~%" error)
                           (format *standard-output* "~S~%" (%:%backtrace)))))
     ,@body))

(export 'with-backtrace)