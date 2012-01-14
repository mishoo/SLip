(%special '*package*)

;; props to http://norstrulde.org/ilge10/
(set! qq
      (lambda (x)
        (if (consp x)
            (if (eq 'qq-unquote (car x))
                (cadr x)
                (if (eq 'quasiquote (car x))
                    (qq (qq (cadr x)))
                    (if (consp (car x))
                        (if (eq 'qq-splice (caar x))
                            (list 'append (cadar x) (qq (cdr x)))
                            (list 'cons (qq (car x)) (qq (cdr x))))
                        (list 'cons (qq (car x)) (qq (cdr x))))))
            (list 'quote x))))

(defmacro quasiquote (thing)
  (qq thing))

;;;; let the show begin

(defmacro defun (name args . body)
  `(%set-function-name (set! ,name (lambda ,args ,@body)) ',name))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defun map (func lst)
  (when lst
    (cons (func (car lst)) (map func (cdr lst)))))

(defmacro labels (defs . body)
  `(let ,(map (lambda (x) (car x)) defs)
     ,@(map (lambda (x)
              `(set! ,(car x) (%set-function-name
                               (lambda ,(cadr x) ,@(cddr x))
                               ',(car x)))) defs)
     ,@body))

(defun foreach (lst func)
  (when lst
    (func (car lst))
    (foreach (cdr lst) func)))

(defmacro prog1 (exp . body)
  (let ((ret (gensym)))
    `(let ((,ret ,exp))
       ,@body
       ,ret)))

(defmacro prog2 (exp1 exp2 . body)
  `(progn
     ,exp1
     (prog1 ,exp2 ,@body)))

(defmacro flet (defs . body)
  `(let ,(map (lambda (x)
                `(,(car x) (%set-function-name
                            (lambda ,(cadr x) ,@(cddr x))
                            ',(car x)))) defs)
     ,@body))

(defmacro or exps
  (when exps
    (let ((x (gensym "OR")))
      `(let ((,x ,(car exps)))
         (if ,x ,x (or ,@(cdr exps)))))))

(defmacro and exprs
  (if exprs
      (let ((x (gensym "AND")))
        `(let ((,x ,(car exprs)))
           (when ,x
             ,(if (cdr exprs) `(and ,@(cdr exprs)) x))))
      t))

(defmacro cond cases
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,@(cdr cases)))))

(defmacro member (item lst)
  `(%memq ,item ,lst))

(defmacro case (expr . cases)
  (let ((vexpr (gensym "CASE")))
    `(let ((,vexpr ,expr))
       ,(labels ((recur (cases)
                        (when cases
                          (if (listp (caar cases))
                              `(if (member ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))
                              (if (and (not (cdr cases))
                                       (member (caar cases) '(otherwise t)))
                                  `(progn ,@(cdar cases))
                                  `(if (eq ,vexpr ',(caar cases))
                                       (progn ,@(cdar cases))
                                       ,(recur (cdr cases))))))))
                (recur cases)))))

(defmacro mapcar (func . lists)
  (let ((rec (gensym))
        (fname (gensym))
        (args (map (lambda (el) (gensym)) lists))
        (val (gensym)))
    `(let ((,fname ,func))
       (labels ((,rec (,val ,@args)
                  (if (and ,@args)
                      (,rec (cons (,fname ,@(map (lambda (l)
                                                   `(car ,l)) args))
                                  ,val)
                            ,@(map (lambda (l)
                                     `(cdr ,l)) args))
                      ,val)))
         (,rec nil ,@lists)))))

(defmacro call/cc (func)
  `(,func (c/c)))

(defmacro with-cc (name . body)
  `((lambda (,name) ,@body) (c/c)))

(defmacro awhen (cond . body)
  `(let ((it ,cond))
     (when it ,@body)))

(defmacro aif (cond . rest)
  `(let ((it ,cond))
     (if it ,@rest)))

(defmacro %incf (var)
  `(set! ,var (+ ,var 1)))

(defmacro %decf (var)
  `(set! ,var (- ,var 1)))


(defmacro push (obj place)
  `(set! ,place (cons ,obj ,place)))

(defmacro pushnew (obj place)
  (let ((sym (gensym)))
    `(let ((,sym ,obj))
       (unless (member ,sym ,place)
         (push ,sym ,place)))))

(defmacro error (msg)
  `(%error ,msg))

(defun macroexpand (form)
  (if (and (consp form)
           (symbolp (car form))
           (%macrop (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(defun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (map macroexpand-all form))
      form))

;; this is `once-only' from Practical Common Lisp
(defmacro with-rebinds (names . body)
  (let ((gensyms (mapcar (lambda (_) (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                ,@body)))))

;;; amb

(set! *amb-fail* (lambda (arg)
                   (clog "TOTAL FAILURE")))

(defmacro amb alternatives
  (if alternatives
      `(let ((+prev-amb-fail *amb-fail*))
         (with-cc +sk
           ,@(map (lambda (alt)
                    `(with-cc +fk
                       (set! *amb-fail* +fk)
                       (+sk ,alt)))
                  alternatives)
           (set! *amb-fail* +prev-amb-fail)
           (+prev-amb-fail nil)))
      `(*amb-fail* nil)))

(defmacro while (cond . body)
  (let ((rec (gensym "while")))
    `(labels ((,rec ()
                (when ,cond
                  ,@body
                  (,rec))))
       (,rec))))

;;;; destructuring-bind

(defmacro %next (lst)
  `(prog1 (car ,lst)
     (set! ,lst (cdr ,lst))))

(defun %fn-destruct (args values body)
  (let (optional? rest? key? aux? names decls)
    (let ((topv (gensym)) rec)
      ((set! rec
             (lambda (args values i)
               (when args
                 (let ((thisarg (car args)))
                   (cond
                     ((symbolp thisarg)
                      (case thisarg
                        (&whole
                         (when (> i 0) (error "Misplaced &WHOLE"))
                         (let ((thisarg (cadr args)))
                           (unless (and thisarg (symbolp thisarg))
                             (error "Missing variable name for &WHOLE"))
                           (push `(,thisarg ,values) decls))
                         (rec (cddr args) values i))

                        (&optional
                         (when (or optional? rest? key? aux?)
                           (error "Invalid &OPTIONAL"))
                         (set! optional? t)
                         (rec (cdr args) values i))

                        ((&rest &body)
                         (when (or rest? key? aux?)
                           (error "Invalid &REST/&BODY"))
                         (set! rest? t)
                         (set! optional? nil)
                         (let ((thisarg (cadr args)))
                           (unless (and thisarg (symbolp thisarg))
                             (error "Missing variable name for &REST"))
                           (push `(,thisarg ,values) decls))
                         (rec (cddr args) values i))

                        (&key
                         (when (or key? aux?)
                           (error "Invalid &KEY"))
                         (set! key? t)
                         (set! optional? nil)
                         (set! rest? nil)
                         (rec (cdr args) values i))

                        (&aux
                         (when aux?
                           (error "Invalid &AUX"))
                         (set! aux? t)
                         (set! optional? nil)
                         (set! rest? nil)
                         (set! key? nil)
                         (rec (cdr args) values i))
                        (t
                         (when (member thisarg names)
                           (error (strcat "Argument seen twice: " thisarg)))
                         (push thisarg names)
                         (cond
                           (optional?
                            (push `(,thisarg (%next ,values)) decls))
                           (aux?
                            (push thisarg decls))
                           (key?
                            (push `(,thisarg (%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")))) decls))
                           (t
                            (push `(,thisarg (if ,values
                                                 (%next ,values)
                                                 (error ,(strcat "Missing required argument: " thisarg))))
                                  decls)))
                         (rec (cdr args) values (+ i 1)))))
                     ((consp thisarg)
                      (cond
                        ((or optional? key?)
                         (let ((thisarg (car thisarg))
                               (default (cadr thisarg))
                               (thisarg-p (caddr thisarg)))
                           (when thisarg-p
                             (push `(,thisarg-p (if ,values t nil)) decls))
                           (push `(,thisarg ,(if key?
                                                 (let ((val (gensym)))
                                                   `(let ((,val (%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")) 'not-found)))
                                                      (if (eq ,val 'not-found) ,default ,val)))
                                                 `(if ,values (%next ,values) ,default)))
                                 decls)))
                        ((or rest? aux?) (error "Invalid argument list following &REST/&BODY or &AUX"))
                        (t
                         (let ((sublist (gensym)))
                           (push `(,sublist (if ,values (%next ,values) (error "Missing sublist"))) decls)
                           (rec thisarg sublist 0))))
                      (rec (cdr args) values (+ i 1))))))))
       args topv 0)
      `(let* ((,topv ,values) ,@(reverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct args values body))

;;;; parser/compiler

(defun lisp-reader (text eof)
  (let ((input (%make-input-stream text))
        (in-qq 0))
    (labels
        ((peek ()
           (%stream-peek input))

         (next ()
           (%stream-next input))

         (read-while (pred)
           (let ((out (%make-output-stream))
                 (ch))
             (while (and (set! ch (peek))
                         (pred ch))
               (%stream-put out (next)))
             (%stream-get out)))

         (croak (msg)
           (error (strcat msg ", line: " (%stream-line input) ", col: " (%stream-col input))))

         (skip-ws ()
           (read-while (lambda (ch)
                         (member ch '(#\Space
                                      #\Newline
                                      #\Tab
                                      #\Page
                                      #\Line_Separator
                                      #\Paragraph_Separator)))))

         (skip (expected)
           (unless (eq (next) expected)
             (croak (strcat "Expecting " expected))))

         (read-escaped (start end inces)
           (skip start)
           (let ((out (%make-output-stream)))
             (labels ((rec (ch escaped)
                        (cond
                          ((not ch)
                           (croak "Unterminated string or regexp"))
                          ((eq ch end)
                           (%stream-get out))
                          (escaped
                           (%stream-put out ch)
                           (rec (next) nil))
                          ((eq ch #\\)
                           (if inces (%stream-put out #\\))
                           (rec (next) t))
                          (t (%stream-put out ch)
                             (rec (next) nil)))))
               (rec (next) nil))))

         (read-string ()
           (read-escaped #\" #\" nil))

         (read-regexp ()
           (let ((str (read-escaped #\/ #\/ t))
                 (mods (downcase (read-while (lambda (ch)
                                               (member ch '(#\g #\m #\i #\y)))))))
             (make-regexp str mods)))

         (skip-comment ()
           (read-while (lambda (ch)
                         (not (eq ch #\Newline)))))

         (read-symbol ()
           (let ((str (read-while
                       (lambda (ch)
                         (or
                          (char<= #\a ch #\z)
                          (char<= #\A ch #\Z)
                          (char<= #\0 ch #\9)
                          (member ch
                                  '(#\% #\$ #\_ #\- #\: #\. #\+ #\*
                                    #\@ #\! #\? #\& #\= #\< #\>
                                    #\[ #\] #\{ #\} #\/ #\^ #\# )))))))
             (set! str (upcase str))
             (if (regexp-test #/^[0-9]*\.?[0-9]*$/ str)
                 (parse-number str)
                 (aif (regexp-exec #/^(.*?)::?(.*)$/ str)
                      (let ((pak (elt it 1))
                            (sym (elt it 2)))
                        (set! pak (%find-package (if (zerop (length pak))
                                                     "KEYWORD"
                                                     pak)))
                        (%intern sym pak))
                      (%intern str *package*)))))

         (read-char ()
           (let ((name (strcat (next)
                               (read-while (lambda (ch)
                                             (or (char<= #\a ch #\z)
                                                 (char<= #\A ch #\Z)
                                                 (char<= #\0 ch #\9)
                                                 (eq ch #\-)
                                                 (eq ch #\_)))))))
             (if (regexp-test #/^U[0-9a-f]{4}$/i name)
                 (code-char (parse-integer (substr name 1) 16))
                 (name-char name))))

         (read-sharp ()
           (skip #\#)
           (case (peek)
             (#\\ (next) (read-char))
             (#\/ (read-regexp))
             (#\( `(vector ,(read-list)))
             (otherwise (croak (strcat "Unsupported sharp syntax #" (peek))))))

         (read-quote ()
           (skip #\')
           `(quote ,(read-token)))

         (read-quasiquote ()
           (skip #\`)
           (skip-ws)
           (if (member (peek) '(#\( #\`))
               (prog2
                   (%incf in-qq)
                   (list 'quasiquote (read-token))
                 (%decf in-qq))
               `(quote ,(read-token))))

         (read-comma ()
           (when (zerop in-qq) (croak "Comma outside quasiquote"))
           (skip #\,)
           (skip-ws)
           (prog2
               (%decf in-qq)
               (if (eq (peek) #\@)
                   (progn (next)
                          (list 'qq-splice (read-token)))
                   (list 'qq-unquote (read-token)))
             (%incf in-qq)))

         (read-list ()
           (let ((ret nil)
                 (p nil))
             (labels ((rec ()
                        (skip-ws)
                        (case (peek)
                          (#\) ret)
                          (#\; (skip-comment) (rec))
                          (#\. (next)
                               (rplacd p (read-token))
                               (skip-ws)
                               ret)
                          (nil (croak "Unterminated list"))
                          (otherwise (let ((cell (cons (read-token) nil)))
                                       (set! p (if ret
                                                   (rplacd p cell)
                                                   (set! ret cell))))
                                     (rec)))))
               (prog2
                   (skip #\()
                   (rec)
                 (skip #\))))))

         (read-token ()
           (skip-ws)
           (case (peek)
             (#\; (skip-comment) (read-token))
             (#\" (read-string))
             (#\( (read-list))
             (#\# (read-sharp))
             (#\` (read-quasiquote))
             (#\, (read-comma))
             (#\' (read-quote))
             (nil eof)
             (otherwise (read-symbol)))))

      read-token)))







(destructuring-bind (&whole all a (b c) &optional (d (* a b c)) &key foo (bar (* d d))) `(1 (2 3) 7 :foo ,(+ 2 3 4))
  (clog all)
  (clog (list a b c d))
  (clog "foo" foo)
  (clog "bar" bar))

(macroexpand-1 '(destructuring-bind (n &optional (ret 0)) list))

;; (defmacro fn (args . body)
;;   (let ((sym (gensym)))
;;     `(lambda ,sym
;;        (destructuring-bind ,args ,sym
;;          ,@body))))

;; (set! sss (fn (n &optional (ret 0))
;;               (if (= n 0)
;;                   ret
;;                   (sss (- n 1) (+ ret n)))))

;; (set! sss2 (lambda (n ret)
;;              (if (= n 0)
;;                  ret
;;                  (sss2 (- n 1) (+ ret n)))))

;; (sss 100000)
;; (sss2 100000 0)
;; (sss 100000)
;; (sss2 100000 0)
;; (sss 100000)
;; (sss2 100000 0)



;; (defun %do-loop (stuff)
;;   (let (vars init body next conds final)
;;     (labels
;;         ((parse-for (stuff)
;;            (let ((var (car stuff)))
;;              (pushnew var vars)
;;              (case (cadr stuff)
;;                ((in :in) (let ((lst-sym (gensym)))
;;                            (push `(,lst-sym ,(caddr stuff)) vars)
;;                            (pushnew var conds)
;;                            (push `(set! ,var (cdr ,var)) next)
;;                            (cdddr stuff)))
;;                ((from :from) (let ((from-sym (gensym))
;;                                    (to-sym (gensym)))
;;                                (let ((from (caddr stuff))
;;                                      (to (cadddr stuff)))
;;                                  (push `(,from-sym ,from) vars)
;;                                  (push `(,to-sym ,to) vars))
;;                                (cddddr stuff)))
;;                (t (%error "Unsupported FOR LOOP syntax")))))
;;          (rec (stuff)
;;            (when stuff
;;              (case (car stuff)
;;                ((for :for) (rec (parse-for (cdr stuff))))
;;                (t (push (car stuff) body)
;;                   (rec (cdr stuff)))))))
;;       (rec stuff))
;;     (let ((loopsym (gensym)))
;;       `(with-cc return
;;          (let ,vars
;;            ,@init
;;            (labels ((,loopsym ()
;;                       (when (and ,@conds)
;;                         ,@body
;;                         ,@next)
;;                       (,loopsym)))
;;              (,loopsym))
;;            ,@final)))))

;; (defmacro loop stuff
;;   (%do-loop stuff))

;; (macroexpand-1 '(loop :for i :in '(a b c) (crap)))





;; (let ((reader (lisp-reader
;;                (%js-eval "window.CURRENT_FILE")
;;                ;;"(a b . c)"
;;                ;;"#\\Newline mak"
;;                'EOF)))
;;   (labels ((rec (q)
;;              (let ((tok (reader)))
;;                (if (eq tok 'EOF)
;;                    q
;;                    (rec (cons tok q))))))
;;     (reverse (rec nil))))






;; (labels
;;     ((assert (p msg)
;;        (unless p (%error msg)))

;;      (arg-count (x min max)
;;        (unless max (set! max min))
;;        (let ((len (length x)))
;;          (assert (<= min len max) "Wrong number of arguments")))

;;      (gen cmd
;;        #( (as-vector cmd) ))

;;      (find-var (name env)
;;        )

;;      (comp (x env val? more?)
;;        (cond
;;          ((symbolp x) (cond
;;                         ((eq x nil) (comp-const nil val? more?))
;;                         ((eq x t) (comp-const t val? more?))
;;                         ((keywordp x) (comp-const x val? more?))
;;                         (t (comp-var x env val? more?))))
;;          ((atom x) (comp-const x val? more?))
;;          (t (case (car x)
;;               (quote (arg-count x 1)
;;                      (comp-const (cadr x) val? more?))
;;               (progn (comp-seq (cdr x) env val? more?))
;;               (set! (arg-count x 2)
;;                     (assert (symbolp (cadr x)) "Only symbols can be SET!")
;;                     (%seq (comp (caddr x) env t t)
;;                           (gen-set (cadr x) env)
;;                           (unless val? (gen "POP"))
;;                           (unless more? (gen "RET"))))))))
;;      )
;;   (defun compile (func)
;;     (assert (and (consp func)
;;                  (eq (car func) 'lambda)
;;                  (cadr func))
;;             "Expecting (LAMBDA (...) ...) in COMPILE")))
