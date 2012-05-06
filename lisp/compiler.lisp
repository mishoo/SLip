;;;; This file implements the compiler.  To bootstrap, it should be
;;;; compiled using the JavaScript version (compiler.js).  That version
;;;; will stall so to keep this working with compiler.js you should
;;;; rely only on the following operators:
;;;;
;;;; IF, QUOTE, PROGN, SETQ, NOT, C/C, LET, LET*, LABELS, FLET,
;;;; LAMBDA, FUNCTION and %FN
;;;;
;;;; Don't customize the reader in this file.

"
(in-package :%)
" ;; hack for Ymacs to get the right package

(setq %::*package* (%find-package "%"))
(%special! '*read-table*
           '*package*
           '*standard-input*
           '*current-file*
           '*current-pos*
           '*url-prefix*
           '*defining-functions*
           '*xref-info*)

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

(set-symbol-function!
 'maybe-xref-info
 (%fn maybe-xref-info (name type)
      (if *xref-info*
          (vector-push *xref-info*
                       #(name type *current-pos*)))))

;; better to avoid quasiquote here:
(%macro! 'defmacro
         (%fn defmacro (name args . body)
              (maybe-xref-info name "DEFMACRO")
              (list '%macro!
                    (list 'quote name)
                    (list* '%fn name args body))))

(defmacro quasiquote (thing)
  (qq thing))

;;;; let the show begin

;; is there a good reason why CL doesn't allow this syntax?
;;
;;   ((progn foo) ...)  ==  (funcall foo ...)
;;
;; basically, if a list occurs in function position, then it's
;; evaluated as an ordinary expression and the returned value is
;; expected to be a closure, and it's then called.  Makes sense for
;; (lambda) too, although our compiler intercepts that as a special
;; case.
(defmacro funcall (f . args)
  `((progn ,f) ,@args))

(defmacro defun (name args . body)
  (maybe-xref-info name "DEFUN")
  `(set-symbol-function! ',name (%fn ,name ,args ,@body)))

(defun error (msg)
  (%error msg))

(defun warn (msg)
  (%warn msg))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defmacro cond cases
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,@(cdr cases)))))

(defun map (func lst)
  (labels ((rec (lst ret)
             (if lst
                 (rec (cdr lst) (cons (funcall func (car lst)) ret))
                 ret)))
    (nreverse (rec lst nil))))

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

(labels ((finished (tails)
           (when tails
             (if (car tails) (finished (cdr tails)) t))))
  (defun mapcar (f . lists)
    (labels ((looop (ret tails)
               (if (finished tails)
                   (nreverse ret)
                   (looop (cons (%apply f (map #'car tails)) ret)
                          (map #'cdr tails)))))
      (looop nil lists))))

(defmacro aif (cond . rest)
  `(let ((it ,cond))
     (if it ,@rest)))

(defmacro %incf (var)
  `(setq ,var (+ ,var 1)))

(defmacro %decf (var)
  `(setq ,var (- ,var 1)))

(defmacro push (obj place)
  `(setq ,place (cons ,obj ,place)))

(%global! '+keyword-package+)
(setq +keyword-package+ (%find-package "KEYWORD"))

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
                                      #\Paragraph_Separator
                                      #\NO-BREAK_SPACE)))))

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
                          (escaped
                           (%stream-put out ch)
                           (rec (next) nil))
                          ((eq ch end)
                           (%stream-get out))
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
           (%stream-skip-to input #\Newline))

         (read-symbol-name ()
           (let ((str (read-while
                       (lambda (ch)
                         (or
                          (letterp ch)
                          (char<= #\0 ch #\9)
                          (member ch
                                  '(#\% #\$ #\_ #\- #\: #\. #\+ #\*
                                    #\@ #\! #\? #\& #\= #\< #\>
                                    #\[ #\] #\{ #\} #\/ #\^ #\# )))))))
             (upcase str)))

         (read-symbol ()
           (let ((str (read-symbol-name)))
             (aif (and (regexp-test #/^-?[0-9]*\.?[0-9]*$/ str)
                       (parse-number str))
                  it
                  (aif (regexp-exec #/^(.*?)(::?)(.*)$/ str)
                       (let ((pak (elt it 1))
                             (sym (elt it 3))
                             (internal (string= (elt it 2) "::")))
                         (when (zerop (length sym))
                           (error (strcat "Bad symbol name in " str)))
                         (cond
                           ((zerop (length pak))
                            ;; KEYWORD
                            (setq sym (%intern sym +keyword-package+))
                            (%export sym +keyword-package+)
                            sym)
                           (t
                            (setq pak (%find-package pak))
                            (if internal
                                (%intern sym pak)
                                (or (%find-exported-symbol sym pak)
                                    (error (strcat "Symbol " sym " not accessible in package " pak)))))))
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
             (#\: (next) (%make-symbol (read-symbol-name)))
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
                                       (setq p (if ret
                                                   (rplacd p cell)
                                                   (setq ret cell))))
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
             (otherwise (read-symbol))))

         (read-toplevel ()
           (labels ((fwd ()
                      (skip-ws)
                      (when (eq (peek) #\;)
                        (skip-comment)
                        (fwd))))
             (fwd)
             (cons (%stream-pos input)
                   (read-token)))))

      (lambda (what)
        (case what
          (next (read-toplevel))
          (pos (%stream-pos input))
          (col (%stream-col input))
          (line (%stream-line input)))))))

(defmacro with-seq-output (out . body)
  (let ((seq (gensym)))
    `(let ((,seq #()))
       (flet ((,out args
                (%seq-cat ,seq args)))
         ,@body)
       ,seq)))

(defmacro without-interrupts body
  `(let ((old (%no-interrupts t)))
     (unwind-protect
         (progn ,@body)
       (%no-interrupts old))))

(defun defun-en-course (name)
  (member name *defining-functions*))

(labels
    ((assert (p msg)
       (unless p (error msg)))

     (arg-count (x min max)
       (assert (<= min (- (length x) 1) max) "Wrong number of arguments"))

     ;; :lex should match the runtime lexical environment; both
     ;; variables and functions are stored there, but the compiler
     ;; distinguiesh them and will emit different frame,index pairs.
     ;;
     ;; :macros and :tags are necessry only at compile-time.
     (make-environment ()
       (make-hash :lex nil
                  :macros nil
                  :tags nil))

     (extenv (env . rest)
       (let ((h (hash-copy env)))
         (labels ((rec (stuff)
                    (when stuff
                      (let ((key (car stuff))
                            (val (cadr stuff)))
                        (hash-add h key (cons val (hash-get h key)))
                        (rec (cddr stuff))))))
           (rec rest))
         h))

     (gen cmd
       #( (as-vector cmd) ))

     (find-in-env (name type env)
       (labels ((position (lst i j)
                  (when lst
                    (let ((x (car lst)))
                      (if (and (eq name (car x))
                               (eq type (cadr x)))
                          (list* i j (cddr x))
                          (position (cdr lst) i (+ j 1))))))
                (frame (env i)
                  (when env
                    (or (position (car env) i 0)
                        (frame (cdr env) (+ i 1))))))
         (frame env 0)))

     (find-var (name env)
       (find-in-env name :var (hash-get env :lex)))

     (find-func (name env)
       (find-in-env name :func (hash-get env :lex)))

     (find-tag (name env)
       (find-in-env name :tag (hash-get env :tags)))

     (find-tagbody (name env)
       (find-in-env name :tagbody (hash-get env :lex)))

     (find-block (name env)
       (find-in-env name :block (hash-get env :lex)))

     (find-macrolet (name env)
       (find-in-env name :macro (hash-get env :macros)))

     (gen-var (name env)
       (if (%globalp name)
           (gen "GVAR" name)
           (aif (find-var name env)
                (gen "LVAR" (car it) (cadr it))
                (progn
                  (warn (strcat "Undefined variable " name))
                  (gen "GVAR" name)))))

     (gen-set (name env)
       (if (%globalp name)
           (gen "GSET" name)
           (aif (find-var name env)
                (gen "LSET" (car it) (cadr it))
                (progn
                  (warn (strcat "Undefined variable " name))
                  (gen "GSET" name)))))

     (mklabel ()
       (gensym "label"))

     (macro (sym env)
       (aif (find-macrolet sym env)
            (caddr it)
            (%macro sym)))

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
              (setq (arg-count x 2 2)
                    (assert (symbolp (cadr x)) "Only symbols can be SETQ")
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
                   (if val? (gen "CC")))
              (let (comp-let (cadr x) (cddr x) env val? more?))
              (let* (comp-let* (cadr x) (cddr x) env val? more?))
              (labels (comp-flets (cadr x) (cddr x) env t val? more?))
              (flet (comp-flets (cadr x) (cddr x) env nil val? more?))
              (macrolet (comp-macrolet (cadr x) (cddr x) env val? more?))
              (lambda (if val?
                          (%seq (comp-lambda nil (cadr x) (cddr x) env)
                                (if more? nil (gen "RET")))))
              (function (arg-count x 1 1)
                        (let ((sym (cadr x)))
                          (assert (symbolp sym) "FUNCTION requires a symbol")
                          (let ((local (find-func sym env)))
                            (%seq (when val? (if local
                                                 (gen "LVAR" (car local) (cadr local))
                                                 (progn
                                                   (unless (or (symbol-function sym)
                                                               (defun-en-course sym))
                                                     (warn (strcat "Undefined function " sym)))
                                                   (gen "FGVAR" sym))))
                                  (if more? nil (gen "RET"))))))
              (%fn (let ((*defining-functions* (cons (cadr x) *defining-functions*)))
                     (%seq (if val? (comp-lambda (cadr x) (caddr x) (cdddr x) env))
                           (if more? nil (gen "RET")))))
              (tagbody (comp-tagbody (cdr x) env val? more?))
              (go (arg-count x 1 1)
                  (comp-go (cadr x) env))
              (block (comp-block (cadr x) (cddr x) env val? more?))
              (return (arg-count x 0 1) (comp-return nil (cadr x) env))
              (return-from (arg-count x 1 2) (comp-return (cadr x) (caddr x) env))
              (catch (comp-catch (cadr x) (cddr x) env val? more?))
              (throw (comp-throw (cadr x) (caddr x) env))
              (unwind-protect (comp-unwind-protect (cadr x) (cddr x) env val? more?))
              (t (if (and (symbolp (car x))
                          (macro (car x) env))
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

     (comp-block (name forms env val? more?)
       (assert (symbolp name) "BLOCK expects a symbol")
       (let ((label (gensym "block")))
         (%seq (gen "BLOCK")
               (comp-seq forms (extenv env :lex (list (list name :block label))) val? t)
               (gen "UNFR" 1 0)
               #( label )
               (if more? nil (gen "RET")))))

     (comp-return (name value env)
       (assert (symbolp name) "RETURN-FROM expects a symbol")
       (let ((block (find-block name env)))
         (assert block (strcat "BLOCK " name " not found"))
         (%seq (comp value env t t)
               (gen "LVAR" (car block) (cadr block))
               (gen "LRET" (caddr block)))))

     (comp-tagbody (forms env val? more?)
       ;; a TAGBODY introduces a single return point in the lexical
       ;; environment; this is necessary because we can jump to a tag
       ;; from a nested environment, so the runtime will need to save
       ;; the stack length and current environment for each tagbody.
       ;; a GO instruction will fetch the TAGBODY variable that it
       ;; refers to, restore that environment and jump to the
       ;; specified index.
       (with-seq-output <<
         (let ((tags nil)
               (tbody (gensym "tagbody")))
           ;; pass 1: fetch tags
           (labels ((rec (forms p)
                      (when forms
                        (if (symbolp (car forms))
                            (let ((cell (list (list (car forms)
                                                    :tag
                                                    tbody
                                                    (gensym "tag")))))
                              (if p
                                  (rplacd p cell)
                                  (setq tags cell))
                              (rec (cdr forms) cell))
                            (rec (cdr forms) p)))))
             (rec forms nil))
           (setq env (extenv env
                             :lex (list (list tbody :tagbody))
                             :tags tags))
           (<< (gen "BLOCK"))           ; define the tagbody entry
           (foreach forms (lambda (x)
                            (if (symbolp x)
                                (progn
                                  (<< #( (cadddr (car tags)) ))
                                  (setq tags (cdr tags))) ; label
                                (<< (comp x env nil t)))))
           (when val? (<< (gen "NIL"))) ; tagbody returns NIL
           (<< (gen "UNFR" 1 0))        ; pop the tagbody from the env
           (unless more? (<< (gen "RET"))))))

     (comp-go (tag env)
       (let ((pos (find-tag tag env)))
         (assert pos (strcat "TAG " tag " not found"))
         (let* ((tbody (find-tagbody (caddr pos) env))
                (i (car tbody))
                (j (cadr tbody)))
           (%seq (gen "LVAR" i j)
                 (gen "LJUMP" (cadddr pos))))))

     (comp-if (pred then else env val? more?)
       (cond
         ((not pred) (comp else env val? more?))
         ((or (numberp pred)
              (stringp pred)
              (regexp pred)
              (charp pred)
              (vectorp pred)
              (eq pred t)) (comp then env val? more?))
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
            (let ((localfun (find-func f env)))
              (cond
                (localfun
                 (mkret (gen "LVAR" (car localfun) (cadr localfun))))
                ((%primitivep f)
                 (if (and (not val?) (not (%prim-side-effects f)))
                     (comp-seq args env nil more?)
                     (%seq (comp-list args env)
                           (gen "PRIM" f (length args))
                           (if val? nil (gen "POP"))
                           (if more? nil (gen "RET")))))
                (t
                 (unless (or (symbol-function f)
                             (defun-en-course f))
                   (warn (strcat "Undefined function " f)))
                 (mkret (gen "FGVAR" f))))))
           ((and (consp f)
                 (eq (car f) 'lambda)
                 (not (cadr f)))
            (assert (not args) "Too many arguments")
            (comp-seq (cddr f) env val? more?))
           (t (mkret (comp f env t t))))))

     (gen-simple-args (args n)
       (cond
         ((not args) (gen "ARGS" n))
         ((symbolp args) (gen "ARG_" n))
         ((and (consp args) (symbolp (car args)))
          (gen-simple-args (cdr args) (+ n 1)))
         (t (error "Illegal argument list"))))

     (make-true-list (l)
       (when l
         (if (atom l)
             (list l)
             (cons (car l) (make-true-list (cdr l))))))

     (comp-lambda (name args body env)
       (gen "FN"
            (%seq (when args (gen-simple-args args 0))
                  (let (dyn (i 0) (args (make-true-list args)))
                    (foreach args (lambda (x)
                                    (when (%specialp x)
                                      (push #("BIND" x i) dyn))
                                    (%incf i)))
                    (%seq dyn
                          (comp-seq body (if args
                                             (extenv env :lex (map (lambda (name) (list name :var)) args))
                                             env) t nil))))
            name))

     (get-bindings (bindings vars?)
       (let (names vals specials (i 0))
         (foreach bindings (lambda (x)
                             (if (consp x)
                                 (progn (push (if vars? (cadr x) (cdr x)) vals)
                                        (setq x (car x)))
                                 (if vars?
                                     (push nil vals)
                                     (error "Malformed LABELS/FLET/MACROLET")))
                             (when (member x names)
                               (error "Duplicate name in LET/LABELS/FLET/MACROLET"))
                             (push x names)
                             (when (and vars? (%specialp x))
                               (push (cons x i) specials))
                             (%incf i)))
         (list (nreverse names) (nreverse vals) i (nreverse specials))))

     (comp-flets (bindings body env labels? val? more?)
       (if bindings
           (with-seq-output <<
             (let* ((bindings (get-bindings bindings nil))
                    (names (car bindings))
                    (funcs (cadr bindings))
                    (len (caddr bindings)))
               (flet ((extenv ()
                        (extenv env :lex (map (lambda (name) (list name :func)) names))))
                 (when labels?
                   (setq env (extenv))
                   (<< (gen "FRAME")))
                 (mapcar (lambda (name func)
                           (<< (comp-lambda name (car func) (cdr func) env)))
                         names funcs)
                 (unless labels? (<< (gen "FRAME")))
                 (<< (if (> len 1) (gen "VARS" len) (gen "VAR"))
                     (comp-seq body (if labels? env (extenv)) val? t)
                     (gen "UNFR" 1 0)
                     (if more? nil (gen "RET"))))))
           (comp-seq body env val? more?)))

     (comp-macrolet (bindings body env val? more?)
       (when bindings
         (setq env (extenv env :macros (map (lambda (def)
                                              (list (car def) :macro (compile (list* '%fn def))))
                                            bindings))))
       (comp-seq body env val? more?))

     (comp-macroexpand (sym args env val? more?)
       (comp (%apply (macro sym env) args) env val? more?))

     (comp-let (bindings body env val? more?)
       (if bindings
           (if (symbolp bindings)
               (let* ((looop bindings)
                      (bindings (car body))
                      (body (cdr body))
                      (names (map (lambda (x)
                                    (if (consp x) (car x) x))
                                  bindings)))
                 (comp `(labels ((,looop ,names
                                   ,@body))
                          (,looop ,@(map (lambda (x)
                                           (if (consp x) (cadr x)))
                                      bindings)))
                       env val? more?))
               (with-seq-output <<
                 (let* ((bindings (get-bindings bindings t))
                        (names (car bindings))
                        (vals (cadr bindings))
                        (len (caddr bindings))
                        (specials (cadddr bindings)))
                   (foreach vals (lambda (x)
                                   (<< (comp x env t t))))
                   (<< (gen "LET" len))
                   (foreach specials (lambda (x)
                                       (<< (gen "BIND" (car x) (cdr x)))))
                   (<< (comp-seq body (extenv env :lex (map (lambda (name) (list name :var)) names)) val? t)
                       (gen "UNFR" 1 (length specials))
                       (if more? nil (gen "RET"))))))
           (comp-seq body env val? more?)))

     (comp-let* (bindings body env val? more?)
       (if bindings
           (with-seq-output <<
             (let* ((bindings (get-bindings bindings t))
                    (names (car bindings))
                    (vals (cadr bindings))
                    (specials (cadddr bindings))
                    (i 0)
                    newargs)
               (mapcar (lambda (name x)
                         (<< (comp x env t t)
                             (unless newargs (gen "FRAME"))
                             (gen "VAR")
                             (when (%specialp name)
                               (gen "BIND" name i)))
                         (%incf i)
                         (let ((cell (list (list name :var))))
                           (if newargs
                               (rplacd newargs cell)
                               (setq env (extenv env :lex cell)))
                           (setq newargs cell)))
                       names vals)
               (<< (comp-seq body env val? t)
                   (gen "UNFR" 1 (length specials))
                   (if more? nil (gen "RET")))))
           (comp-seq body env val? more?)))

     (comp-catch (tag body env val? more?)
       (if body
           (let ((k (mklabel)))
             (%seq (comp tag env t t)
                   (gen "CATCH" k)
                   (comp-seq body env t t)
                   #( k )
                   (if val? nil (gen "POP"))
                   (if more? nil (gen "RET"))))
           (%seq (if more? nil (gen "RET")))))

     (comp-throw (tag ret env)
       (%seq (comp tag env t t)
             (comp ret env t t)
             (gen "THROW")))

     (comp-unwind-protect (form cleanup env val? more?)
       (if form
           (let ((k (mklabel)))
             (%seq (gen "UPOPEN" k)
                   (comp form env val? t) ; if val? is T, this leaves it on the stack
                   (gen "UPEXIT")
                   #( k )
                   (comp-seq cleanup env nil t) ; result of cleanup code not needed
                   (gen "UPCLOSE")
                   (if more? nil (gen "RET"))))
           (comp-seq cleanup env val? more?)))

     (compile (exp)
       (assert (and (consp exp)
                    (member (car exp) '(%fn lambda)))
               "Expecting (LAMBDA (...) ...) in COMPILE")
       (%eval-bytecode (comp exp (make-environment) t nil)))

     (compile-string (str)
       (let ((reader (lisp-reader str 'EOF))
             (out (%make-output-stream))
             (is-first t)
             (link-addr 0)
             (*xref-info* #())
             (env (make-environment)))
         (labels ((comp1 (form)
                    (let ((code (comp form env nil t)))
                      (when code
                        (setq code (%exec-code code))
                        (%relocate-code code link-addr)
                        (setq link-addr (+ link-addr (length code)))
                        (if is-first
                            (setq is-first nil)
                            (%stream-put out #\,))
                        (%stream-put out (%serialize-code code) #\Newline))))
                  (rec ()
                    (let* ((token (funcall reader 'next))
                           (*current-pos* (car token))
                           (form (cdr token)))
                      (unless (eq form 'EOF)
                        (comp1 form)
                        (rec)))))
           (rec)
           (let* ((xref *xref-info*)
                  (*xref-info* nil))
             (when (and *current-file* (> (length xref) 0))
               (comp1 `(%grok-xref-info ,*current-file* ,xref))))
           (%stream-get out)))))

  (set-symbol-function! 'compile #'compile)
  (set-symbol-function! 'compile-string #'compile-string))

(defun read1-from-string (str)
  (let ((reader (lisp-reader str 'EOF)))
    #( (cdr (funcall reader 'next)) (funcall reader 'pos) )))

(defun eval (expr)
  ((compile (list 'lambda nil expr))))

(defun eval-string (str)
  (let ((reader (lisp-reader str 'EOF)))
    (labels ((rec (last expr)
               (if (eq expr 'EOF) last
                   (rec ((compile (list 'lambda nil expr)))
                        (cdr (funcall reader 'next))))))
      (rec nil (cdr (funcall reader 'next))))))

(defun %load (url)
  (let ((*current-file* url)
        (code (%get-file-contents (make-url url))))
    (unless code (error (strcat "Unable to load file: " url)))
    (compile-string code)))

(defun make-url (url)
  (if *url-prefix*
      (strcat *url-prefix* url)
      url))

(defun load (url)
  (let ((*package* *package*)
        (*read-table* *read-table*))
    (%load url)))

;;;

EOF

(let ((*current-file* "compiler.lisp"))
  (console.print (compile-string (%js-eval "window.CURRENT_FILE"))))
