;;;; This file implements the compiler.  To bootstrap, it should be
;;;; compiled using the JavaScript version (compiler.js).  That version
;;;; will stall so to keep this working with compiler.js you should
;;;; rely only on the following operators:
;;;;
;;;; IF, QUOTE, PROGN, SET!, NOT, C/C, LET, LET*, LABELS, FLET,
;;;; LAMBDA, FUNCTION and %FN
;;;;
;;;; Don't customize the reader in this file.

;; props to http://norstrulde.org/ilge10/
(set-symbol-function!
 'qq
 (labels ((qq (x)
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
   #'qq))

;; better to avoid quasiquote here:
(%macro! 'defmacro
         (labels ((defmacro (name args . body)
                      (list '%macro!
                       (list 'quote name)
                       (list* '%fn name args body))))
           #'defmacro))

(defmacro quasiquote (thing)
  (qq thing))

;;;; let the show begin

(defmacro funcall (f . args)
  `((progn ,f) ,@args))

(defmacro defun (name args . body)
  `(set-symbol-function! ',name (labels ((,name ,args ,@body))
                                  #',name)))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defun map (func lst)
  (when lst
    (cons (funcall func (car lst)) (map func (cdr lst)))))

(defun foreach (lst func)
  (when lst
    (funcall func (car lst))
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
                          (if (and (listp (caar cases)) (caar cases))
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
        (args (map (lambda (el) (gensym)) lists)))
    `(let ((,fname ,func))
       (labels ((,rec (,@args)
                  (when (and ,@args)
                    (cons (funcall ,fname ,@(map (lambda (l)
                                                   `(car ,l)) args))
                          (,rec ,@(map (lambda (l)
                                         `(cdr ,l)) args))))))
         (,rec ,@lists)))))

(defmacro with-cc (name . body)
  `((lambda (,name) ,@body) (c/c)))

(defmacro aif (cond . rest)
  `(let ((it ,cond))
     (if it ,@rest)))

(defmacro %incf (var)
  `(set! ,var (+ ,var 1)))

(defmacro %decf (var)
  `(set! ,var (- ,var 1)))

(defmacro push (obj place)
  `(set! ,place (cons ,obj ,place)))

(defmacro error (msg)
  `(%error ,msg))

;;;; parser/compiler

(%special! '*read-table* '*package*)

(defun lisp-reader (text eof)
  (let ((input (%make-input-stream text))
        (in-qq 0))
    (labels
        ((peek ()
           (%stream-peek input))

         (next ()
           (%stream-next input))

         (read-while (pred)
           (let ((out (%make-output-stream)) rec)
             (labels ((rec (ch)
                        (when (and ch (funcall pred ch))
                          (%stream-put out (next))
                          (rec (peek)))))
               (rec (peek)))
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
             (#\( (list* 'vector (read-list)))
             (#\' (next) (list 'function (read-token)))
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

      (lambda (what)
        (case what
          (next (read-token))
          (pos (%stream-pos input))
          (col (%stream-col input))
          (line (%stream-line input)))))))

(defun make-environment ()
  (make-hash :vars nil :funcs nil))

(defun environment-vars (env)
  (hash-get env :vars))

(defun environment-funcs (env)
  (hash-get env :funcs))

(defun environment-extend (env key val)
  (let ((h (hash-extend env)))
    (hash-add h key (cons val (hash-get h key)))
    h))

(labels
    ((assert (p msg)
       (unless p (%error msg)))

     (arg-count (x min max)
       (assert (<= min (- (length x) 1) max) "Wrong number of arguments"))

     (gen cmd
       #( (as-vector cmd) ))

     (find-var (name env)
       (with-cc return
         (labels ((position (lst i j)
                    (when lst
                      (if (eq name (car lst)) (funcall return (cons i j))
                          (position (cdr lst) i (+ j 1)))))
                  (frame (env i)
                    (when env
                      (position (car env) i 0)
                      (frame (cdr env) (+ i 1)))))
           (frame env 0))))

     (gen-var (name env)
       (if (%specialp name)
           (gen "GVAR" name)
           (aif (find-var name (environment-vars env))
                (gen "LVAR" (car it) (cdr it))
                (gen "GVAR" name))))

     (gen-set (name env)
       (if (%specialp name)
           (gen "GSET" name)
           (aif (find-var name (environment-vars env))
                (gen "LSET" (car it) (cdr it))
                (gen "GSET" name))))

     (mklabel ()
       (gensym "label"))

     (comp (x env val? more?)
       (cond
         ((symbolp x) (cond
                        ((eq x nil) (comp-const nil val? more?))
                        ((eq x t) (comp-const t val? more?))
                        ((keywordp x) (comp-const x val? more?))
                        (t (comp-var x env val? more?))))
         ((atom x) (comp-const x val? more?))
         (t (case (car x)
              (quote (arg-count x 1 1)
                     (comp-const (cadr x) val? more?))
              (progn (comp-seq (cdr x) env val? more?))
              (set! (arg-count x 2 2)
                    (assert (symbolp (cadr x)) "Only symbols can be SET!")
                    (%seq (comp (caddr x) env t t)
                          (gen-set (cadr x) env)
                          (unless val? (gen "POP"))
                          (unless more? (gen "RET"))))
              (if (arg-count x 2 3)
                  (comp-if (cadr x) (caddr x) (cadddr x) env val? more?))
              (not (arg-count x 1 1)
                   (if val?
                       (%seq (comp (cadr x) env t t)
                             (gen "NOT")
                             (if more? nil (gen "RET")))
                       (comp (cadr x) env val? more?)))
              (c/c (arg-count x 0 0)
                   (if val? (%seq (gen "CC"))))
              (let (comp-let (cadr x) (cddr x) env val? more?))
              (let* (comp-let* (cadr x) (cddr x) env val? more?))
              (labels (comp-labels (cadr x) (cddr x) env val? more?))
              (flet (comp-flet (cadr x) (cddr x) env val? more?))
              (lambda (if val?
                          (%seq (comp-lambda nil (cadr x) (cddr x) env)
                                (if more? nil (gen "RET")))))
              (function (arg-count x 1 1)
                        (let ((sym (cadr x)))
                          (assert (symbolp sym) "FUNCTION requires a symbol")
                          (let ((local (find-var sym (environment-funcs env))))
                            (if local
                                (gen "FVAR" (car local) (cdr local))
                                (gen "FGVAR" sym)))))
              (%fn (if val?
                       (%seq (comp-lambda (cadr x) (caddr x) (cdddr x) env)
                             (if more? nil (gen "RET")))))
              (t (if (and (symbolp (car x))
                          (%macro (car x)))
                     (comp-macroexpand (car x) (cdr x) env val? more?)
                     (comp-funcall (car x) (cdr x) env val? more?)))))))

     (comp-const (x val? more?)
       (if val? (%seq (gen "CONST" x)
                      (if more? nil (gen "RET")))))

     (comp-var (x env val? more?)
       (if val? (%seq (gen-var x env)
                      (if more? nil (gen "RET")))))

     (comp-seq (exps env val? more?)
       (cond
         ((not exps) (comp-const nil val? more?))
         ((not (cdr exps)) (comp (car exps) env val? more?))
         (t (%seq (comp (car exps) env nil t)
                  (comp-seq (cdr exps) env val? more?)))))

     (comp-list (exps env)
       (when exps
         (%seq (comp (car exps) env t t)
               (comp-list (cdr exps) env))))

     (comp-if (pred then else env val? more?)
       (cond
         ((not pred) (comp else env val? more?))
         ((or (numberp pred)
              (stringp pred)
              (regexp pred)
              (charp pred)
              (vectorp pred)) (comp then env val? more?))
         ((and (consp pred)
               (eq (car pred) 'not))
          (comp-if (cadr pred) else then env val? more?))
         (t
          (let ((pcode (comp pred env t t))
                (tcode (comp then env val? more?))
                (ecode (comp else env val? more?)))
            (cond
              ((equal tcode ecode)
               (%seq (comp pcode env nil t) ecode))
              ((zerop (length tcode)) (let ((l2 (mklabel)))
                                        (%seq pcode (gen "TJUMP" l2) ecode
                                              #( l2 )
                                              (if more? nil (gen "RET")))))
              ((zerop (length ecode)) (let ((l1 (mklabel)))
                                        (%seq pcode (gen "FJUMP" l1) tcode
                                              #( l1 )
                                              (if more? nil (gen "RET")))))
              (t (let ((l1 (mklabel))
                       (l2 (if more? (mklabel))))
                   (%seq pcode (gen "FJUMP" l1) tcode
                         (if more? (gen "JUMP" l2))
                         #( l1 ) ecode (if more? #( l2 ))))))))))

     (comp-funcall (f args env val? more?)
       (labels ((mkret (the-function)
                  (cond
                    (more? (let ((k (mklabel)))
                             (%seq (gen "SAVE" k)
                                   (comp-list args env)
                                   the-function
                                   (gen "CALL" (length args))
                                   #( k )
                                   (if val? nil (gen "POP")))))

                    (t (%seq (comp-list args env)
                             the-function
                             (gen "CALL" (length args)))))))
         (cond
           ((symbolp f)
            (let ((localfun (find-var f (environment-funcs env))))
              (cond
                (localfun
                 (mkret (gen "FVAR" (car localfun) (cdr localfun))))
                ((%primitivep f)
                 (if (and (not val?) (not (%prim-side-effects f)))
                     (comp-seq args env nil more?)
                     (%seq (comp-list args env)
                           (gen "PRIM" f (length args))
                           (if val? nil (gen "POP"))
                           (if more? nil (gen "RET")))))
                (t
                 (unless (symbol-function f)
                   (%warn "Undefined function" f))
                 (mkret (gen "FGVAR" f))))))
           ((and (consp f)
                 (eq (car f) 'lambda)
                 (not (cadr f)))
            (assert (not args) "Too many arguments")
            (comp-seq (cddr f) env val? more?))
           (t (mkret (comp f env t t))))))

     (gen-args (args n)
       (cond
         ((not args) (gen "ARGS" n))
         ((symbolp args) (gen "ARG_" n))
         ((and (consp args) (symbolp (car args)))
          (gen-args (cdr args) (+ n 1)))
         (t (error "Illegal argument list"))))

     (make-true-list (l)
       (when l
         (if (atom l)
             (list l)
             (cons (car l) (make-true-list (cdr l))))))

     (comp-lambda (name args body env)
       (gen "FN"
            (%seq (gen-args args 0)
                  (let (dyn (i 0) (args (make-true-list args)))
                    (foreach args (lambda (x)
                                    (when (%specialp x)
                                      (push #("BIND" x i) dyn))
                                    (%incf i)))
                    (%seq dyn
                          (comp-seq body (environment-extend env :vars args) t nil))))
            name))

     (get-bindings (bindings vars?)
       (let (names vals specials (i 0))
         (foreach bindings (lambda (x)
                             (if (consp x)
                                 (progn (push (if vars? (cadr x) (cdr x)) vals)
                                        (set! x (car x)))
                                 (if vars?
                                     (push nil vals)
                                     (%error "Malformed LABELS/FLET/MACROLET")))
                             (when (member x names)
                               (error "Duplicate name in LET/LABELS/FLET/MACROLET"))
                             (push x names)
                             (when (and vars? (%specialp x))
                               (push (cons x i) specials))
                             (%incf i)))
         (list (reverse names) (reverse vals) i (reverse specials))))

     (comp-labels (bindings body env val? more?)
       (if bindings
           (let* ((bindings (get-bindings bindings nil))
                  (names (car bindings))
                  (funcs (cadr bindings))
                  (len (caddr bindings)))
             (set! env (environment-extend env :funcs names))
             (let ((i 0))
               (%seq (gen "FUNCS" (- len))
                     (%prim-apply
                      '%seq (mapcar (lambda (name func)
                                      (%seq (comp-lambda name (car func) (cdr func) env)
                                            (gen "FSET" 0 (prog1 i (%incf i)))))
                                    names funcs))
                     (comp-seq body env val? t)
                     (gen "UNFR" 0 0 1)
                     (if more? nil (gen "RET")))))
           (comp-seq body env val? more?)))

     (comp-flet (bindings body env val? more?)
       (if bindings
           (let* ((bindings (get-bindings bindings nil))
                  (names (car bindings))
                  (funcs (cadr bindings))
                  (len (caddr bindings)))
             (%seq (%prim-apply
                    '%seq (mapcar (lambda (name func)
                                    (comp-lambda name (car func) (cdr func) env))
                                  names funcs))
                   (gen "FUNCS" len)
                   (comp-seq body (environment-extend env :funcs names) val? t)
                   (gen "UNFR" 0 0 1)
                   (if more? nil (gen "RET"))))
           (comp-seq body env val? more?)))

     (comp-macroexpand (sym args env val? more?)
       (comp (%apply (%macro sym) args) env val? more?))

     (comp-let (bindings body env val? more?)
       (if bindings
           (let* ((bindings (get-bindings bindings t))
                  (names (car bindings))
                  (vals (cadr bindings))
                  (len (caddr bindings))
                  (specials (cadddr bindings)))
             (%seq (%prim-apply '%seq (map (lambda (x)
                                             (comp x env t t))
                                           vals))
                   (gen "ARGS" len)
                   (%prim-apply '%seq (map (lambda (x)
                                             (gen "BIND" (car x) (cdr x)))
                                           specials))
                   (comp-seq body (environment-extend env :vars names) val? t)
                   (gen "UNFR" 1 (length specials) 0)
                   (if more? nil (gen "RET"))))
           (comp-seq body env val? more?)))

     (comp-let* (bindings body env val? more?)
       (if bindings
           (let* ((bindings (get-bindings bindings t))
                  (names (car bindings))
                  (vals (cadr bindings))
                  (specials (cadddr bindings))
                  (i 0)
                  newargs)
             (%seq (%prim-apply
                    '%seq (mapcar (lambda (name x)
                                    (prog1
                                        (%seq (comp x env t t)
                                              (gen (if newargs "VAR" "FRAME"))
                                              (when (%specialp name)
                                                (gen "BIND" name i)))
                                      (%incf i)
                                      (let ((cell (cons name nil)))
                                        (if newargs
                                            (rplacd newargs cell)
                                            (set! env (environment-extend env :vars cell)))
                                        (set! newargs cell))))
                                  names vals))
                   (comp-seq body env val? t)
                   (gen "UNFR" 1 (length specials) 0)
                   (if more? nil (gen "RET"))))
           (comp-seq body env val? more?))))

  (defun compile (exp)
    (assert (and (consp exp)
                 (eq (car exp) 'lambda))
            "Expecting (LAMBDA (...) ...) in COMPILE")
    (%eval-bytecode (comp exp (make-environment) t nil))))

(defun compile-string (str)
  (let ((reader (lisp-reader str 'EOF))
        (out (%make-output-stream)))
    (labels ((rec (is-first)
               (let ((form (funcall reader 'next)))
                 (unless (eq form 'EOF)
                   (let ((f (compile `(lambda () ,form))))
                     (let ((code (%serialize-bytecode f t)))
                       (unless is-first (%stream-put out #\,))
                       (%stream-put out code #\Newline))
                     (funcall f))
                   (rec nil)))))
      (rec t)
      (%stream-get out))))

(defun load-lisp-file (url)
  (compile-string (%get-file-contents url)))

;;;

EOF

(console.print (compile-string (%js-eval "window.CURRENT_FILE")))
