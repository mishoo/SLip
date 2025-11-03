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

(setq %::*package* (find-package "%"))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defmacro cond clauses
  (when clauses
    (let ((first (car clauses)))
      (if (cdr first)
          `(if ,(car first)
               (progn ,@(cdr first))
               (cond ,@(cdr clauses)))
          `(or ,(car first)
               (cond ,@(cdr clauses)))))))

;; props to http://norstrulde.org/ilge10/ - pasting here the original version,
;; because it's small and beautiful and it's the heart of quasiquotation:
;;
;; (set-symbol-function!
;;  'qq
;;  (labels ((qq (x)
;;             (if (consp x)
;;                 (if (eq 'qq-unquote (car x))
;;                     (cadr x)
;;                     (if (eq 'quasiquote (car x))
;;                         (qq (qq (cadr x)))
;;                         (if (consp (car x))
;;                             (if (eq 'qq-splice (caar x))
;;                                 (list 'append (cadar x) (qq (cdr x)))
;;                                 (list 'cons (qq (car x)) (qq (cdr x))))
;;                             (list 'cons (qq (car x)) (qq (cdr x))))))
;;                 (list 'quote x))))
;;    #'qq))
;;
;; The version below has a bunch of optimizations and uses `cond'.
(set-symbol-function!
 'qq
 (labels
     ((opt-splice (x)
        (cond
          ((cdr x)
           (let ((rest (qq (cdr x))))
             (if (if (consp rest) (eq 'append (car rest)))
                 (list* 'append (cadar x) (cdr rest))
                 (list 'append (cadar x) rest))))
          (t
           (cadar x))))
      (opt-list (first second rest)
        (cond
          ((not rest)
           (list 'list first second))
          ((not (consp rest))
           (list 'list* first second rest))
          ((eq 'list* (car rest))
           (list* 'list* first second (cdr rest)))
          ((eq 'list (car rest))
           (list* 'list first second (cdr rest)))
          (t
           (list 'list* first second rest))))
      (opt-cons (x)
        (let ((first (qq (car x)))
              (second (qq (cdr x))))
          (cond
            ((not second)
             (list 'list first))
            ((not (consp second))
             (list 'cons first second))
            ((eq 'list (car second))
             (list* 'list first (cdr second)))
            ((eq 'list* (car second))
             (list* 'list* first (cdr second)))
            ((eq 'cons (car second))
             (opt-list first (cadr second) (caddr second)))
            (t
             (list 'cons first second)))))
      (qq (x)
        (cond
          ((or (numberp x)
               (stringp x)
               (regexpp x)
               (vectorp x))
           x)
          ((not (consp x))
           (if x (list 'quote x)))
          ((eq 'qq-unquote (car x))
           (cadr x))
          ((eq 'quasiquote (car x))
           (qq (qq (cadr x))))
          ((consp (car x))
           (if (eq 'qq-splice (caar x))
               (opt-splice x)
               (opt-cons x)))
          (t
           (opt-cons x)))))
   #'qq))

(defun declaim-inline (names)
  (foreach names
    (lambda (name)
      (multiple-value-bind (setter name) (%:maybe-setter name)
        ;; XXX: disabled. Not safe for now.
        ;; (%set-symbol-prop (or setter name) :inline-request t)
        ))))

(defmacro declaim (&rest declarations)
  (foreach declarations
    (lambda (decl)
      (case (car decl)
        ((inline) (declaim-inline (cdr decl)))))))

(defmacro defun (name args . body)
  (multiple-value-bind (setter name) (%:maybe-setter name)
    (let ((target (or setter name)))
      (maybe-xref-info name (if setter 'setf 'defun))
      (let ((parsed-args (parse-lambda-list args)))
        (when (%get-symbol-prop target :inline-request)
          (cond
            ((null-lexenv-p)
             (%set-symbol-prop target :inline t))
            (t
             (warn (strcat "Cannot inline function " target " (non-null lexenv)")))))
        (%set-symbol-prop target :lambda-list parsed-args)
        (%set-symbol-prop target :lambda-body body)
        `(progn
           ,@(when (%get-symbol-prop target :inline)
               `((%set-symbol-prop ',target :inline t)
                 (%set-symbol-prop ',target :lambda-list ',parsed-args)
                 (%set-symbol-prop ',target :lambda-body ',body)))
           (set-symbol-function! ',target
                                 (%fn ,name ,args ,@body)))))))

(defun maybe-xref-info (name type)
  (when *xref-info*
    (%vector-push *xref-info*
                  (vector name type *current-pos*))))

(defmacro quasiquote (thing)
  (qq thing))

(defmacro defparameter (name val &optional documentation)
  (%special! name)
  (%::maybe-xref-info name 'defparameter)
  `(progn
     (%special! ',name)
     (setq ,name ,val)))

(defmacro defvar (name &optional (val nil val-passed-p) documentation)
  (%special! name)
  (%::maybe-xref-info name 'defvar)
  `(progn
     (%special! ',name)
     ,@(when val-passed-p
         `((unless (boundp ',name)
             (setq ,name ,val))))))

(defmacro defconstant (name val &optional documentation)
  (%global! name)
  (%set-symbol-prop name :constant t)
  (%::maybe-xref-info name 'defconstant)
  `(progn
     (%global! ',name)
     (%set-symbol-prop ',name :constant t)
     (setq ,name ,val)))

(defmacro defglobal (name &optional (val nil val-passed-p))
  (%global! name)
  (%::maybe-xref-info name 'defglobal)
  `(progn
     (%global! ',name)
     ,@(when val-passed-p
         `((setq ,name ,val)))))

(defvar *read-table* nil)
(defvar *package* nil)
(defvar *current-file* nil)
(defvar *current-pos* nil)
(defvar *url-prefix* nil)
(defvar *unknown-functions* nil)
(defvar *unknown-variables* nil)
(defvar *compiler-env* nil)
(defvar *xref-info* nil)
(defvar *load-timing* nil)
(defvar *delay-eval* nil)

(defvar *compiler-macros* (make-hash))
(defvar *macroexpand-cache* nil)

(defvar *standard-output* (%make-text-memory-output-stream))
(defvar *error-output* (%make-text-memory-output-stream))
(defvar *trace-output* (%make-text-memory-output-stream))
(defvar *standard-input*)

(defvar *build-count* 0)
(defmacro delay-eval body
  (if (zerop *build-count*)
      (setq *delay-eval* t))
  (list* 'progn body))

(defmacro and exps
  (cond
    ((cdr exps) `(if ,(car exps) (and ,@(cdr exps))))
    (exps (car exps))
    (t t)))

(defun maybe-setter (sym)
  (cond
    ((and (consp sym)
          (eq 'setf (car sym))
          (symbolp (cadr sym))
          (null (cddr sym)))
     (values (intern (strcat "(SETF " (cadr sym) ")")
                     (symbol-package (cadr sym)))
             (cadr sym)))
    (t
     (values nil sym))))

(defun error (msg)
  (%error msg))

(defun warn (msg)
  (%warn msg))

(defun error/wp (msg)
  (error (if *current-pos*
             (strcat msg " (" (or *current-file* "line") ":" *current-pos* ")")
             msg)))

(defun map1 (func lst)
  (let rec ((ret nil) (lst lst))
    (if lst
        (rec (cons (funcall func (%pop lst)) ret)
             lst)
        (nreverse ret))))

(defun map1-vector (func lst)
  (let ((ret (vector)))
    (let rec ((lst lst))
      (if lst (progn
                (%vector-push ret (funcall func (%pop lst)))
                (rec lst))
          ret))))

(defun map2 (func lst1 lst2)
  (let rec ((ret nil) (lst1 lst1) (lst2 lst2))
    (if (and lst1 lst2)
        (rec (cons (funcall func (%pop lst1) (%pop lst2)) ret)
             lst1 lst2)
        (nreverse ret))))

(defun foreach (lst func)
  (let rec ((lst lst))
    (when lst
      (funcall func (%pop lst))
      (rec lst))))

(defun foreach-index (lst func)
  (let rec ((lst lst) (index 0))
    (when lst
      (funcall func (%pop lst) index)
      (rec lst (1+ index)))))

(defmacro incf (var)
  `(setq ,var (1+ ,var)))

(defmacro decf (var)
  `(setq ,var (1- ,var)))

(defun identity (x) x)

(defun complement (f)
  (lambda args
    (not (apply f args))))

(defun constantly (value)
  (lambda args
    (declare (ignore args))
    value))

(defun filter (lst &optional (pred #'identity))
  (let rec ((lst lst)
            (ret nil))
    (cond
      ((null lst) (nreverse ret))
      ((funcall pred (car lst))
       (rec (cdr lst) (cons (car lst) ret)))
      (t
       (rec (cdr lst) ret)))))

(defmacro prog2 (exp1 exp2 . body)
  `(progn
     ,exp1
     (prog1 ,exp2 ,@body)))

(defun %ecase-error (expr cases)
  (error (strcat (%dump expr)
                 " fell through ECASE expression. Wanted: "
                 (%dump cases))))

(defun %case (expr cases errorp)
  (let* ((safe (safe-atom-p expr))
         (vexpr (if safe
                    expr
                    (gensym "CASE")))
         (exps nil)
         (code (let recur ((cases cases))
                 (cond
                   ((null cases)
                    (when errorp
                      `(%ecase-error ,vexpr ',(nreverse exps))))
                   ((consp (caar cases))
                    (cond
                      ((cdaar cases)
                       (when errorp
                         (foreach (caar cases) (lambda (x)
                                                 (push x exps))))
                       `(if (%memq ,vexpr ',(caar cases))
                            (progn ,@(cdar cases))
                            ,(recur (cdr cases))))
                      (t
                       (when errorp
                         (push (caar cases) exps))
                       `(if (eq ,vexpr ',(caaar cases))
                            (progn ,@(cdar cases))
                            ,(recur (cdr cases))))))
                   ((and (not (cdr cases))
                         (%memq (caar cases) '(otherwise t)))
                    `(progn ,@(cdar cases)))
                   ((not (caar cases))
                    (recur (cdr cases)))
                   (t
                    (when errorp
                      (push (caar cases) exps))
                    `(if (eq ,vexpr ',(caar cases))
                         (progn ,@(cdar cases))
                         ,(recur (cdr cases))))))))
    (if safe
        code
        `(let ((,vexpr ,expr)) ,code))))

(defmacro case (expr . cases)
  (%case expr cases nil))

(defmacro ecase (expr . cases)
  (%case expr cases t))

(defmacro aif (cond . rest)
  `(let ((it ,cond))
     (if it ,@rest)))

(defmacro push (obj place)
  `(setq ,place (cons ,obj ,place)))

(defconstant +keyword-package+ (find-package "KEYWORD"))

;;;; parser/compiler

(defvar in-qq)

(defun lisp-reader (input eof)
  (let ((input (if (stringp input)
                   (%make-text-memory-input-stream input)
                   input)))
    (labels
        ((peek ()
           (%stream-peek input))

         (next ()
           (%stream-next input))

         (read-while (pred)
           (let ((out (%make-text-memory-output-stream)))
             (labels ((rec (ch)
                        (when (and ch (funcall pred ch))
                          (%stream-put out (next))
                          (rec (peek)))))
               (rec (peek)))
             (%get-output-stream-string out)))

         (croak (msg)
           (when *current-file*
             (setq *current-pos* (%stream-line input)))
           (error/wp (strcat msg ", line: " (%stream-line input) ", col: " (%stream-col input))))

         (skip-ws ()
           (read-while #'whitespacep))

         (skip (expected)
           (unless (eq (next) expected)
             (croak (strcat "Expecting " expected))))

         (read-escaped (start end inces)
           (skip start)
           (let ((out (%make-text-memory-output-stream)))
             (labels ((rec (ch escaped)
                        (cond
                          ((not ch)
                           (croak "Unterminated string or regexp"))
                          (escaped
                           (%stream-put out ch)
                           (rec (next) nil))
                          ((eq ch end)
                           (%get-output-stream-string out))
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
                 (mods (read-while
                        (lambda (ch)
                          (%memq (downcase ch)
                                 '(#\g #\m #\i #\y #\u))))))
             (make-regexp str (downcase mods))))

         (skip-comment ()
           (read-while (lambda (ch) (not (eq ch #\Newline)))))

         (read-symbol-name ()
           (read-while
            (lambda (ch)
              (or
               (letterp ch)
               (digitp ch)
               (%memq ch
                      ;; XXX: this list should be greatly enlarged.. or better
                      ;; said, our reader should be greatly rewritten.
                      '(#\% #\$ #\_ #\- #\: #\. #\+ #\*
                        #\@ #\! #\? #\& #\= #\< #\>
                        #\[ #\] #\{ #\} #\/ #\^ #\#
                        #\« #\» #\❰ #\❱ #\♥ #\▪ #\§))))))

         (read-symbol ()
           (let ((str (upcase (read-symbol-name))))
             (when (zerop (length str))
               (croak (strcat "Bad character (or reader bug) in read-symbol: " (peek))))
             (aif (and (regexp-test #/^[+-]?[0-9]*\.?[0-9]*$/ str)
                       (parse-number str))
                  it
                  (aif (regexp-exec #/^(.*?)(::?)(.*)$/ str)
                       (let ((pak (elt it 1))
                             (sym (elt it 3))
                             (internal (string= (elt it 2) "::")))
                         (when (zerop (length sym))
                           (croak (strcat "Bad symbol name in " str)))
                         (cond
                           ((zerop (length pak))
                            ;; KEYWORD
                            (setq sym (intern sym +keyword-package+))
                            (export sym +keyword-package+)
                            sym)
                           (t
                            (setq pak (find-package pak))
                            (if internal
                                (intern sym pak)
                                (or (%find-exported-symbol sym pak)
                                    (croak (strcat "Symbol " sym " not accessible in package " pak)))))))
                       (intern str *package*)))))

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
             (#\( (apply #'vector (read-list)))
             (#\' (next) (list 'function (read-token)))
             (#\: (next) (make-symbol (upcase (read-symbol-name))))
             (#\. (next) (eval (read-token)))
             ((#\b #\B) (next) (read-base2-number))
             ((#\o #\O) (next) (read-base8-number))
             ((#\x #\X) (next) (read-base16-number))
             (otherwise (croak (strcat "Unsupported sharp syntax #" (peek))))))

         (read-base2-number ()
           (let ((digits (read-symbol-name)))
             (if (regexp-test #/^[01]+$/ digits)
                 (parse-integer digits 2)
                 (croak (strcat "Bad base2 number: " digits)))))

         (read-base8-number ()
           (let ((digits (read-symbol-name)))
             (if (regexp-test #/^[0-7]+$/ digits)
                 (parse-integer digits 8)
                 (croak (strcat "Bad base8 number: " digits)))))

         (read-base16-number ()
           (let ((digits (read-symbol-name)))
             (if (regexp-test #/^[0-9a-f]+$/i digits)
                 (parse-integer digits 16)
                 (croak (strcat "Bad hex number: " digits)))))

         (read-quote ()
           `(quote ,(read-token)))

         (read-quasiquote ()
           (skip #\`)
           (skip-ws)
           (if (%memq (peek) '(#\( #\` #\' #\,))
               (let* ((qq (list nil))
                      (in-qq (cons qq in-qq))
                      (token (read-token)))
                 (if (car qq)
                     (list 'quasiquote token)
                     (list 'quote token)))
               (list 'quote (read-token))))

         (read-comma ()
           (unless in-qq (croak "Comma outside quasiquote"))
           (skip #\,)
           (skip-ws)
           (let ((qq (car in-qq))
                 (in-qq (cdr in-qq)))
             (%rplaca qq t)
             (if (eq (peek) #\@)
                 (progn (next)
                        (list 'qq-splice (read-token)))
                 (list 'qq-unquote (read-token)))))

         (read-list ()
           (let* ((ret (list nil))
                  (p ret))
             (skip #\()
             (let rec ()
               (skip-ws)
               (case (peek)
                 (#\) (next) (cdr ret))
                 (#\; (skip-comment) (rec))
                 (#\. (next)
                      (%rplacd p (read-token))
                      (skip-ws)
                      (skip #\))
                      (cdr ret))
                 ((nil) (croak "Unterminated list"))
                 (otherwise
                  (setq p (%rplacd p (cons (read-token) nil)))
                  (rec))))))

         (read-token ()
           (skip-ws)
           (let ((reader (and *read-table* (gethash (peek) *read-table*))))
             (cond
               (reader
                (let ((tok (multiple-value-list
                            (funcall reader input (next) #'the-reader))))
                  (if tok (car tok) (read-token))))
               (t
                (case (peek)
                  (#\; (skip-comment) (read-token))
                  (#\" (read-string))
                  (#\( (read-list))
                  (#\# (read-sharp))
                  (#\` (read-quasiquote))
                  (#\, (read-comma))
                  ((#\' #\’) (next) (read-quote))
                  ((nil) eof)
                  (otherwise (read-symbol)))))))

         (read-toplevel ()
           (labels ((fwd ()
                      (skip-ws)
                      (when (eq (peek) #\;)
                        (skip-comment)
                        (fwd))))
             (fwd)
             (cons (%stream-line input)
                   (read-token))))

         (the-reader (what)
           (case what
             (next (let ((in-qq nil))
                     (read-toplevel)))
             (pos (%stream-pos input))
             (col (%stream-col input))
             (line (%stream-line input)))))

      #'the-reader)))

(defmacro with-seq-output (out . body)
  (let ((seq (gensym)))
    `(let ((,seq (vector)))
       (flet ((,out args
                (%seq-cat ,seq args)))
         ,@body)
       ,seq)))

(defmacro without-interrupts body
  `(let ((old (%no-interrupts t)))
     (unwind-protect
         (progn ,@body)
       (%no-interrupts old))))

(defmacro return (&optional val)
  `(return-from nil ,val))

(defun lambda-keyword-p (sym)
  (%memq sym '(&optional &rest &body &key &aux &allow-other-keys)))

(defun macro-keyword-p (sym)
  (%memq sym '(&whole &environment)))

(defun ordinary-lambda-list-p (args)
  (let dig ((args args)
            (seen nil))
    (cond
      ((symbolp args))
      ((not (consp args))
       (error/wp "Bad macro lambda list"))
      ((listp (car args))
       (when (and seen (symbolp (caar args)))
         (dig (cdr args) t)))
      ((%memq (car args) '(&rest &body))
       (when (symbolp (cadr args))
         (dig (cddr args) seen)))
      ((lambda-keyword-p (car args))
       (dig (cdr args) t))
      ((macro-keyword-p (car args))
       nil)
      (t
       (dig (cdr args) seen)))))

(defun parse-lambda-list (args)
  (let ((all nil)
        (required nil)
        (optional nil)
        (rest nil)
        (key nil)
        (has-key nil)
        (aux nil)
        (allow-other-keys nil))
    (labels
        ((assert (p msg)
           (if p p (error/wp msg)))

         (symp (x)
           (and x (symbolp x)
                (not (eq x t))
                (not (lambda-keyword-p x))))

         (add (name)
           (unless (symp name)
             (error/wp (strcat "Invalid name in lambda list " name)))
           (when (%memq name all)
             (error/wp (strcat "Duplicate name in lambda list " name)))
           (push name all)
           name)

         (rec (args)
           (cond
             ((null args))
             ((consp args)
              (case (car args)
                (&optional (rec-opt (cdr args)))
                ((&rest &body) (rec-rest (cdr args)))
                (&key (rec-key (cdr args)))
                (&aux (rec-aux (cdr args)))
                (otherwise
                 (assert (symp (car args)) "Symbol expected in lambda list")
                 (rec (cdr args))
                 (push (add (car args)) required))))
             ((symp args)
              (assert (not rest) "&rest already given")
              (setq rest (add args)))
             (t
              (error/wp "Bad lambda list"))))

         (rec-rest (args)
           (when (cdr args)
             (case (cadr args)
               (&key (rec-key (cddr args)))
               (&aux (rec-aux (cddr args)))
               (otherwise
                (error/wp "Bad lambda list after &rest"))))
           (setq rest (add (car args))))

         (rec-opt (args)
           (case (car args)
             ((&rest &body) (rec-rest (cdr args)))
             (&key (rec-key (cdr args)))
             (&aux (rec-aux (cdr args)))
             (otherwise
              (when (cdr args)
                (rec-opt (cdr args)))
              (cond
                ((consp (car args))
                 (push (car args) optional)
                 (add (caar args)))
                ((symp (car args))
                 (push (list (add (car args))) optional))
                (t
                 (error/wp "Bad &optional parameter"))))))

         (key-arg-names (arg)
           (cond
             ((consp arg)
              (add (cadr arg))
              arg)
             ((symp arg)
              (list (intern (symbol-name arg) :keyword) (add arg)))
             (t
              (error/wp "Bad &key argument name"))))

         (rec-key (args)
           (setq has-key t)
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
                (args
                 (error/wp "Bad &key parameter in lambda list"))))))

         (rec-aux (args)
           (when (cdr args)
             (rec-aux (cdr args)))
           (cond
             ((consp (car args))
              (add (caar args))
              (push (car args) aux))
             ((symp (car args))
              (push (list (add (car args))) aux))
             (t
              (error/wp "Bad &aux parameter in lambda list")))))

      (rec args)
      (list :required required
            :optional optional
            :rest rest
            :key key
            :has-key has-key
            :aux aux
            :aok allow-other-keys
            :names all))))

;; INLINE-CALL takes a parsed lambda list (as returned by PARSE-LAMBDA-LIST
;; above), a list of VALUES to bind arguments to (forms), and a BODY, and
;; returns a LET form that executes BODY, as if executed by a function with
;; the given lambda list when called with VALUES. It returns 'DECLINE to
;; refuse inlining when the call site (VALUES) includes non-constant argument
;; names.
;;
;; XXX: disabled, for now (see declaim-inline earlier). It isn't sound, since
;; free variables in body might become clobbered by the current lexical
;; environment. Can't do proper analysis here, we'd have to call this after
;; the body has been transformed to some IR (to be defined) which clearly
;; marks references to local vs. free variables.
(defun inline-call (lambda-list values &optional body)
  (cond
    ((symbolp lambda-list)
     (setq body (%get-symbol-prop lambda-list :lambda-body))
     (setq lambda-list (%get-symbol-prop lambda-list :lambda-list)))
    ((eq 'lambda (car lambda-list))
     (setq body (cddr lambda-list))
     (setq lambda-list (parse-lambda-list (cadr lambda-list)))))
  (let ((required (getf lambda-list :required))
        (optional (getf lambda-list :optional))
        (rest (getf lambda-list :rest))
        (key (getf lambda-list :key))
        (aok (getf lambda-list :aok))
        (aux (getf lambda-list :aux))
        (parallel nil)
        (sequential nil)
        (resolved nil)
        (call-aok nil))

    (foreach required
      (lambda (varname)
        (unless values
          (error/wp (strcat "Missing required argument " varname)))
        (push (list varname (%pop values)) parallel)))

    (foreach optional
      (lambda (argdef)
        (destructuring-bind (varname &optional default suppname) argdef
          (cond
            ((null values)
             (push (list varname default) sequential)
             (when suppname
               (push (list suppname nil) sequential)))
            (t
             (let ((temp (gensym)))
               (push (list temp (%pop values)) parallel)
               (push (list varname temp) sequential)
               (when suppname
                 (push (list suppname t) sequential))))))))

    (when rest
      (push (list rest `(list ,@values)) parallel))

    (when key
      (labels ((find-key-arg (argname)
                 (let rec ((key key))
                   (when key
                     (if (eq argname (caaar key))
                         (car key)
                         (rec (cdr key))))))

               (find-key-value (argname)
                 (let rec ((v values))
                   (when v
                     (if (eq argname (car v))
                         (cadr v)
                         (rec (cddr v))))))

               (constant (argname)
                 (multiple-value-bind (const? value) (constant-value argname)
                   (if const? value
                       (return-from inline-call 'decline)))))

        (unless (evenp (length values))
          (error/wp "INLINE-CALL: odd number of &key arguments"))

        (setq call-aok (constant (find-key-value :allow-other-keys)))

        (let rec ((v values)
                  (index 1))
          (when v
            (let* ((argname (constant (car v)))
                   (argval (cadr v))
                   (keyarg (find-key-arg argname)))
              (cond
                (keyarg
                 ;; known argument. The internal name is in CADAR.
                 (cond
                   (rest
                    (push (list (cadar keyarg) `(nth ,index ,rest))
                          sequential))
                   (t
                    (let ((temp (gensym (caar keyarg))))
                      (push (list temp argval) parallel)
                      (push (list (cadar keyarg) temp)
                            sequential))))
                 (when (caddr keyarg) ;; supplied-p, define as T
                   (push (list (caddr keyarg) t) sequential))
                 (push (cadar keyarg) resolved)
                 (rec (cddr v) (+ index 2)))

                ((or aok call-aok)
                 (unless (or rest (%:safe-atom-p argval))
                   ;; should eval for side-effects, invent a name for it
                   (let ((temp (gensym "EVAL")))
                     (push (list temp argval) parallel)))
                 (rec (cddr v) (+ index 2)))

                (t
                 ;; seems in order to croak here if the argname
                 ;; isn't known and we don't have AOK.
                 (error/wp (strcat "INLINE-CALL: unknown keyword argument " argname)))))))

        ;; default values and supplied-p NIL for unsupplied key arguments
        (let ((remaining
               (filter key (lambda (keyarg)
                             (not (%memq (cadar keyarg) resolved))))))
          (foreach remaining
            (lambda (keyarg)
              (push (list (cadar keyarg) (cadr keyarg))
                    sequential)
              (when (caddr keyarg) ;; supplied-p, define as NIL
                (push (list (caddr keyarg) nil)
                      sequential)))))))

    ;; &AUX list is cute too, just dump them all to SEQUENTIAL.
    (foreach aux
      (lambda (argdef)
        (destructuring-bind (varname &optional value) argdef
          (push (list varname value) sequential))))

    `(let ,(nreverse parallel)
       (let* ,(nreverse sequential)
         ,@body))))

(defun %log (data)
  (console.dir data)
  data)

;;;; This has been implemented as primitive (%:%find-in-env) because it's called
;;;; a lot during compilation and it's embarrasingly slow.
;;
;; (defun find-in-compiler-env (name type env)
;;   (labels ((position (v i j)
;;              (tagbody
;;               next
;;               (when (< j 0)
;;                 (return-from position nil))
;;               (let ((x (svref v j)))
;;                 (if (and (eq name (car x))
;;                          (eq type (cadr x)))
;;                     (return-from position
;;                       (list* i j (cddr x)))))
;;               (decf j)
;;               (go next)))
;;            (frame (env i)
;;              (when env
;;                (let* ((v (car env))
;;                       (skip (and (consp v)
;;                                  (eq '%skip-count (car v))
;;                                  (%pop v))))
;;                  (or (position v i (1- (length v)))
;;                      (frame (cdr env) (if skip i (1+ i))))))))
;;     (frame env 0)))

(defun null-lexenv-p (&optional (env *compiler-env*))
  (not (and env (gethash :lex env))))

(defun find-in-compiler-env (name type env)
  (%:%find-in-env name type env))

(defun find-macrolet-in-compiler-env (name &optional (env *compiler-env*))
  (when env
    (aif (find-in-compiler-env name :func (gethash :lex env))
         (getf (cddr it) :macro))))

(defun find-symbol-macrolet-in-compiler-env (name &optional (env *compiler-env*))
  (and env (symbolp name)
       (aif (find-var-in-compiler-env name)
            (getf (cddr it) :smac))))

(defun find-var-in-compiler-env (name &optional (env *compiler-env*))
  (when env
    (find-in-compiler-env name :var (gethash :lex env))))

(defun safe-atom-p (thing &optional (env *compiler-env*))
  "Tells us if we can safely evaluate THING multiple times during macro
   expansion. That is, if THING is a constant, or a symbol which is not bound
   via SYMBOL-MACROLET in the current compiler environment."
  (and (atom thing)
       (not (and (symbolp thing)
                 (aif (find-var-in-compiler-env thing)
                      (eq :smac (caddr it)))))))

(defun %dbind-error-missing-arg (arg)
  (error (strcat "DESTRUCTURING-BIND: Missing required argument: " arg)))

(defun %dbind-error-missing-sublist ()
  (error/wp "DESTRUCTURING-BIND: Missing sublist"))

(defun %fn-destruct (macro? args values body &key default-value)
  (let (names decls)
    (let ((topv (gensym))
          (whole nil)
          (env nil))
      (labels
          ((whole ()
             (or whole (setq whole (gensym))))
           (add (name val)
             (cond
               ((symbolp name)
                (push `(,name ,val) decls))
               ((consp name)
                (let ((current (gensym "dstr")))
                  (add current val)
                  (rec nil nil nil nil name current 0)))
               (t
                (error/wp (strcat "Unknown destructuring pattern: " name)))))
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
                          (when (plusp i) (error/wp "Misplaced &WHOLE"))
                          (let ((thisarg (cadr args)))
                            (unless thisarg
                              (error/wp "Missing variable name for &WHOLE"))
                            (add thisarg (if (and macro? (eq topv values))
                                             (whole) values)))
                          (rec nil nil nil nil (cddr args) values i))

                         (&environment
                          (unless macro?
                            (error/wp "&environment can only appear in macro lambda lists"))
                          (when env
                            (error/wp "&environment seen more than once"))
                          (setq env (cadr args))
                          (rec nil nil nil nil (cddr args) values i))

                         (&optional
                          (when (or optional? rest? key? aux?)
                            (error/wp "Invalid &OPTIONAL"))
                          (rec t nil nil nil (cdr args) values i))

                         ((&rest &body)
                          (when (or rest? key? aux?)
                            (error/wp "Invalid &REST/&BODY"))
                          (let ((thisarg (cadr args)))
                            (unless thisarg
                              (error/wp "Missing variable name for &REST"))
                            (add thisarg values))
                          (rec nil t nil nil (cddr args) values i))

                         (&key
                          (when (or key? aux?)
                            (error/wp "Invalid &KEY"))
                          (rec nil nil t nil (cdr args) values i))

                         (&aux
                          (when aux?
                            (error/wp "Invalid &AUX"))
                          (rec nil nil nil t (cdr args) values i))

                         (t
                          (when (%memq thisarg names)
                            (error/wp (strcat "Argument seen twice: " thisarg)))
                          (push thisarg names)
                          (cond
                            (optional?
                             (add thisarg (if default-value
                                              `(or (%pop ,values) ,default-value)
                                              `(%pop ,values))))
                            (aux?
                             (add thisarg nil))
                            (key?
                             (add thisarg (if default-value
                                              `(getf ,values ,(intern (symbol-name thisarg)
                                                                      #.+keyword-package+)
                                                     ,default-value)
                                              `(getf ,values ,(intern (symbol-name thisarg)
                                                                      #.+keyword-package+)))))
                            (t
                             (add thisarg `(if ,values
                                               (%pop ,values)
                                               (%dbind-error-missing-arg ',thisarg)))))
                          (rec optional? rest? key? aux? (cdr args) values (1+ i)))))

                      ((consp thisarg)
                       (cond
                         (optional?
                          (let ((thisarg (car thisarg))
                                (default (if (cdr thisarg)
                                             (cadr thisarg)
                                             default-value))
                                (thisarg-p (caddr thisarg)))
                            (when thisarg-p
                              (add thisarg-p `(if ,values t nil)))
                            (add thisarg `(if ,values (%pop ,values) ,default))))
                         (key?
                          (let ((thisarg (car thisarg))
                                (default (if (cdr thisarg)
                                             (cadr thisarg)
                                             default-value))
                                (thisarg-p (caddr thisarg)))
                            (when thisarg-p
                              (add thisarg-p nil))
                            (let* ((val (gensym))
                                   (setdef `(if (eq ,val '%not-found)
                                                ,default
                                                (progn
                                                  ,@(when thisarg-p
                                                      `((setq ,thisarg-p t)))
                                                  ,val))))
                              (cond
                                ((consp thisarg)
                                 (add (cadr thisarg)
                                      `(let ((,val (getf ,values ,(car thisarg)
                                                         '%not-found)))
                                         ,setdef)))
                                ((add thisarg
                                      `(let ((,val (getf ,values ,(intern (symbol-name thisarg)
                                                                          #.+keyword-package+)
                                                         '%not-found)))
                                         ,setdef)))))))
                         (aux? (let ((thisarg (car thisarg))
                                     (value (cadr thisarg)))
                                 (add thisarg value)))
                         (rest? (error/wp "Invalid argument list following &REST/&BODY"))
                         (t
                          (let ((sublist (gensym)))
                            (add sublist `(if ,values (%pop ,values) (%dbind-error-missing-sublist)))
                            (rec nil nil nil nil thisarg sublist 0))))
                       (rec optional? rest? key? aux? (cdr args) values (1+ i))))))
                 (t (error/wp "Invalid lambda-list"))))))
        (rec nil nil nil nil args topv 0))
      `(let* (,@(if whole `((,whole ,values)))
              ,@(if env `((,env *compiler-env*)))
              (,topv ,(if macro?
                          (if whole
                              `(cdr ,whole)
                              `(cdr ,values))
                          values))
              ,@(nreverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct nil args values body))

(defun macro-lambda (name lambda-list body)
  (let ((form (gensym "FORM")))
    (if (ordinary-lambda-list-p lambda-list)
        `(%::%fn ,name (,form)
                 (apply (lambda ,lambda-list ,@body)
                        (cdr ,form)))
        `(%::%fn ,name (,form)
                 ,(%fn-destruct t lambda-list form body)))))

(defmacro defmacro (name lambda-list . body)
  (when (%primitivep name)
    (error/wp (strcat "We shall not DEFMACRO on " name " (primitive function)")))
  (%::maybe-xref-info name 'defmacro)
  `(%macro! ',name ,(macro-lambda name lambda-list body)))

;; let's define early some compiler macros, so that the compiler itself can
;; benefit from them.

(defmacro define-compiler-macro (name args &body body)
  (multiple-value-bind (setter name) (%:maybe-setter name)
    (%:maybe-xref-info name (if setter
                                'compiler-macro-setf
                                'compiler-macro))
    (let ((form (if (eq '&whole (car args))
                    (prog1
                        (cadr args)
                      (setq args (cddr args)))
                    (gensym "form"))))
      `(%hash-set (%fn ,name (,form)
                       (destructuring-bind ,args (if (eq 'funcall (car ,form))
                                                     (cddr ,form)
                                                     (cdr ,form))
                         ,@body))
                  ',(or setter name)
                  *compiler-macros*))))

(labels
    ((reduce-form (form)
       (aif (and (consp form)
                 (compiler-macro-function (car form)))
            (let ((exp (funcall it form)))
              (if (eq exp form)
                  exp
                  (reduce-form exp)))
            form))

     (reduce-sum (nums)
       (cond
         ((not nums) 0)
         ((not (cdr nums)) (car nums))
         ((when (and (numberp (car nums))
                     (not (numberp (cadr nums))))
            (setq nums (list (cadr nums) (car nums)))
            nil))
         ((numberp (cadr nums))
          (cond
            ((numberp (car nums))
             (+ (car nums) (cadr nums)))
            ((= 1 (cadr nums))
             `(%:%op INC ,(car nums)))
            ((= -1 (cadr nums))
             `(%:%op DEC ,(car nums)))
            ((minusp (cadr nums))
             `(%:%op SUB ,(car nums) ,(- (cadr nums))))
            (t
             `(%:%op ADD ,(car nums) ,(cadr nums)))))
         (t
          `(%:%op ADD ,(car nums) ,(cadr nums)))))

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
             `(%:%op DEC ,(car nums)))
            (t
             `(%:%op SUB ,(car nums) ,(cadr nums)))))
         (t
          `(%:%op SUB ,(car nums) ,(cadr nums))))))

  (define-compiler-macro + (&whole form &rest nums)
    (or (and (null (cddr nums))
             (reduce-sum (map1 #'reduce-form nums)))
        form))

  (define-compiler-macro - (&whole form &rest nums)
    (or (and (null (cddr nums))
             (reduce-sub (map1 #'reduce-form nums)))
        form))

  (define-compiler-macro 1+ (value)
    `(%op INC ,(reduce-form value)))

  (define-compiler-macro 1- (value)
    `(%op DEC ,(reduce-form value)))

  (define-compiler-macro < (&whole form &rest nums)
    (cond
      ((cddr nums) form)
      ((eq 0 (reduce-form (car nums)))
       `(%op PLUSP ,(reduce-form (cadr nums))))
      ((eq 0 (reduce-form (cadr nums)))
       `(%op MINUSP ,(reduce-form (car nums))))
      (t
       `(%op LT ,(reduce-form (car nums)) ,(reduce-form (cadr nums))))))

  (define-compiler-macro <= (&whole form &rest nums)
    (if (cddr nums) form
        `(%op LTE ,(reduce-form (car nums)) ,(reduce-form (cadr nums)))))

  (define-compiler-macro > (&whole form &rest nums)
    (cond
      ((cddr nums) form)
      ((eq 0 (reduce-form (car nums)))
       `(%op MINUSP ,(reduce-form (cadr nums))))
      ((eq 0 (reduce-form (cadr nums)))
       `(%op PLUSP ,(reduce-form (car nums))))
      (t
       `(%op GT ,(reduce-form (car nums)) ,(reduce-form (cadr nums))))))

  (define-compiler-macro >= (&whole form &rest nums)
    (if (cddr nums) form
        `(%op GTE ,(reduce-form (car nums)) ,(reduce-form (cadr nums)))))

  (define-compiler-macro = (&whole form &rest nums)
    (if (cddr nums) form
        `(%op NUMEQ ,(reduce-form (car nums)) ,(reduce-form (cadr nums)))))

  (define-compiler-macro /= (&whole form &rest nums)
    (if (cddr nums) form
        `(%op NUMNEQ ,(reduce-form (car nums)) ,(reduce-form (cadr nums)))))

  (define-compiler-macro plusp (val)
    `(%op PLUSP ,(reduce-form val)))

  (define-compiler-macro minusp (val)
    `(%op MINUSP ,(reduce-form val))))

(define-compiler-macro identity (x) x)

(define-compiler-macro zerop (value)
  (cond
    ((eq value 0) t)
    (t `(= 0 ,value))))

(define-compiler-macro eq (&whole form a b)
  (cond
    ((not a) `(not ,b))
    ((not b) `(not ,a))
    (t form)))

(define-compiler-macro endp (lst)
  `(not ,lst))

(define-compiler-macro null (lst)
  `(not ,lst))

(defun make-compiler-env ()
  ;; :lex should match the runtime lexical environment; both
  ;; variables and functions are stored there, but the compiler
  ;; distinguiesh them and will emit different frame,index pairs.
  (make-hash :lex nil :tags nil))

(defun extend-compiler-env (rest &optional (env *compiler-env*))
  (cond
    (rest
     (let ((h (hash-copy env)))
       (let rec ((stuff rest))
         (when stuff
           (let ((key (car stuff))
                 (val (cadr stuff)))
             (%hash-set (cons val (gethash key h)) key h)
             (rec (cddr stuff)))))
       h))
    (env)))

(defmacro with-env body
  `(let ((*compiler-env* env))
     ,@body))

(defmacro with-extenv (forms . body)
  `(let ((env (extenv env ,@forms)))
     (with-env ,@body)))

(defun constant-value (form)
  (cond
    ((or (eq form t)
         (eq form nil)
         (numberp form)
         (stringp form)
         (keywordp form)
         (regexpp form)
         (charp form)
         (vectorp form)
         (functionp form)
         (%:%std-instance-p form)
         (%:%structp form))
     (values t form))

    ((and (consp form)
          (eq 'quote (car form)))
     (values t (cadr form)))

    ((symbolp form)
     (cond
       ((find-var-in-compiler-env form)
        ;; XXX: support for local constants to be added when we have Smart
        ;; Compiler™, if ever.
        nil)
       ((%:%get-symbol-prop form :constant)
        (values t (symbol-value form)))))))

(defun always-true-p (form)
  (multiple-value-bind (const? val) (constant-value form)
    (when const? (not (not val)))))

(defun always-false-p (form)
  (multiple-value-bind (const? val) (constant-value form)
    (when const? (not val))))

(defun compiler-macro-function (name &optional (env *compiler-env*))
  (unless (and env (find-in-compiler-env name :func (gethash :lex env)))
    (gethash name *compiler-macros*)))

(defun dig-declarations (exps)
  (let ((declarations nil)
        (documentation nil))
    (let rec ()
      (cond
        ((null exps))
        ((and (stringp (car exps))
              (cdr exps))
         (setq documentation (append documentation (list (%pop exps))))
         (rec))
        ((and (consp (car exps))
              (eq 'declare (caar exps)))
         (setq declarations (append declarations (cdr (%pop exps))))
         (rec))))
    (values exps declarations documentation)))

(defun zip-declarations (declarations)
  (when declarations
    (let ((specials nil))
      (let rec ((decl declarations))
        (when decl
          (case (caar decl)
            (special (setq specials (append specials (cdar decl)))))
          (rec (cdr decl))))
      `(:special ,(filter specials (lambda (sym)
                                     (not (%specialp sym))))))))

(defmacro with-declarations (exps &body body)
  `(multiple-value-bind (,exps declarations) (dig-declarations ,exps)
     (setq declarations (zip-declarations declarations))
     (let ((locally-special (getf declarations :special)))
       (labels ((maybe-special (name)
                  (if (%memq name locally-special)
                      (list name :var :special t)
                      (list name :var)))
                (declare-locally-special (&key except)
                  (when locally-special
                    (setq env (extenv env :lex
                                      (cons '%skip-count
                                            (map1-vector
                                             (lambda (name)
                                               (list name :var :special t))
                                             (if except
                                                 (filter locally-special
                                                         (lambda (name)
                                                           (not (%memq name except))))
                                                 locally-special))))))))
         ,@body))))

(defun flatten (sym forms)
  (let dig ((forms forms)
            (result nil)
            (rest nil))
    (cond
      ((null forms)
       (if rest
           (dig (car rest) result (cdr rest))
           (nreverse result)))
      ((and (consp (car forms))
            (eq sym (caar forms)))
       (dig (cdar forms)
            result
            (cons (cdr forms) rest)))
      (t
       (dig (cdr forms)
            (cons (car forms) result)
            rest)))))

(defglobal *lambda-syms* '(lambda λ %fn))

(labels
    ((assert (p msg)
       (if p p (error/wp msg)))

     (arg-count (x min &optional max)
       (if max
           (assert (<= min (- (length x) 1) max) "Wrong number of arguments")
           (assert (<= min (- (length x) 1)) "Wrong number of arguments")))

     (make-environment ()
       (or *compiler-env*
           (make-compiler-env)))

     (extenv (env . rest)
       (extend-compiler-env rest env))

     (gen cmd
       (vector (as-vector cmd)))

     (find-in-env (name type env)
       (find-in-compiler-env name type env))

     (find-var (name env)
       (find-in-env name :var (gethash :lex env)))

     (find-func (name env)
       (find-in-env name :func (gethash :lex env)))

     (find-tag (name env)
       (find-in-env name :tag (gethash :tags env)))

     (find-tagbody (name env)
       (find-in-env name :tagbody (gethash :lex env)))

     (find-block (name env)
       (find-in-env name :block (gethash :lex env)))

     (find-macrolet (name env)
       (find-macrolet-in-compiler-env name env))

     (find-special (name env)
       (or (%specialp name)
           (let ((v (find-var name env)))
             (when v (getf v :special)))))

     (gen-var (name env)
       (if (find-special name env)
           (gen "GVAR" name)
           (aif (find-var name env)
                (if (eq :smac (caddr it))
                    (comp (cadddr it) env t t) ;; symbol macro expansion
                    (gen "LVAR" (car it) (cadr it)))
                (cond
                  ((when (and (boundp name)
                              (%get-symbol-prop name :constant))
                     (let ((val (symbol-value name)))
                       (when (or (numberp val)
                                 (charp val))
                         (gen "CONST" val)))))
                  (t
                   (gen "GVAR" (unknown-variable name env)))))))

     (gen-set (name env &optional (local (find-var name env)))
       (if (find-special name env)
           (gen "GSET" name)
           (if local
               (gen "LSET" (car local) (cadr local))
               (gen "GSET" (unknown-variable name env)))))

     (mklabel ()
       (gensym "L"))

     (macro (sym env)
       (aif (find-func sym env)
            (getf (cddr it) :macro)
            (%macro sym)))

     (unknown-function (sym)
       (unless (symbol-function sym)
         (unless (%memq sym *unknown-functions*)
           (push sym *unknown-functions*)))
       sym)

     (unknown-variable (sym env)
       (unless (or (%globalp sym)
                   (find-special sym env))
         (unless (%memq sym *unknown-variables*)
           (push sym *unknown-variables*)))
       sym)

     (function-name (sym)
       ;; Hack to handle #'(SETF STUFF). In CL it seems that the name of the
       ;; function is the list itself (SETF STUFF), but we really need to make
       ;; it a symbol.
       (or (maybe-setter sym) sym))

     (comp (x env val? more?)
       (cond
         ((symbolp x)
          (cond
            ((eq x nil) (comp-const nil val? more?))
            ((eq x t) (comp-const t val? more?))
            ((keywordp x) (comp-const x val? more?))
            (t (comp-var x env val? more?))))
         ((atom x)
          (comp-const x val? more?))
         ((case (car x)
            ((quote)
             (arg-count x 1 1)
             (comp-const (cadr x) val? more?))
            ((progn)
             (comp-seq (cdr x) env val? more?))
            ((locally)
             (comp-decl-seq (cdr x) env val? more?))
            ((prog1)
             (arg-count x 1)
             (comp-prog1 (cadr x) (cddr x) env val? more?))
            ((progv)
             (arg-count x 2)
             (comp-progv (cadr x) (caddr x) (cdddr x) env val? more?))
            ((multiple-value-prog1)
             (arg-count x 1)
             (comp-multiple-value-prog1 (cadr x) (cddr x) env val? more?))
            ((%pop)
             (arg-count x 1 1)
             (comp-pop (cadr x) env val? more?))
            ((setq)
             (comp-setq (cdr x) env val? more?))
            ((%psetq)
             (comp-psetq (cdr x) env val? more?))
            ((if)
             (arg-count x 2 3)
             (comp-if (cadr x) (caddr x) (cadddr x) env val? more?))
            ((or)
             (comp-or (flatten 'or (cdr x)) env val? more?))
            ((not null)
             (arg-count x 1 1)
             (if val?
                 (%seq (comp (cadr x) env t t)
                       (gen "NOT")
                       (if more? nil (gen "RET")))
                 (comp (cadr x) env nil more?)))
            ((c/c)
             (arg-count x 0 0)
             (%seq (if val? (gen "CC"))
                   (unless more? (gen "RET"))))
            ((let)
             (comp-let (cadr x) (cddr x) env val? more?))
            ((let*)
             (comp-let* (cadr x) (cddr x) env val? more?))
            ((multiple-value-bind)
             (comp-mvb (cadr x) (caddr x) (cdddr x) env val? more?))
            ((values)
             (comp-values (cdr x) env val? more?))
            ((labels)
             (comp-flets (cadr x) (cddr x) env t val? more?))
            ((flet)
             (comp-flets (cadr x) (cddr x) env nil val? more?))
            ((macrolet)
             (comp-macrolet (cadr x) (cddr x) env val? more?))
            ((symbol-macrolet)
             (comp-symbol-macrolet (cadr x) (cddr x) env val? more?))
            ((lambda λ)
             (when val?
               (%seq (comp-lambda nil (cadr x) (cddr x) env)
                     (unless more? (gen "RET")))))
            ((%fn)
             (when val?
               (%seq (comp-lambda (cadr x) (caddr x) (cdddr x) env)
                     (unless more? (gen "RET")))))
            ((function)
             (arg-count x 1 1)
             (let ((sym (function-name (cadr x))))
               (when val?
                 (cond
                   ((when (and (consp sym)
                               (%memq (car sym) *lambda-syms*))
                      (comp sym env t more?)))
                   (t
                    (assert (symbolp sym) "FUNCTION requires a symbol")
                    (let ((local (find-func sym env)))
                      (%seq (if local
                                (gen "LVAR" (car local) (cadr local))
                                (gen "FGVAR" (unknown-function sym)))
                            (unless more? (gen "RET")))))))))
            ((tagbody)
             (comp-tagbody (cdr x) env val? more?))
            ((go)
             (arg-count x 1 1)
             (comp-go (cadr x) env))
            ((block)
             (arg-count x 1)
             (comp-block (cadr x) (cddr x) env val? more?))
            ((return-from)
             (arg-count x 1 2)
             (comp-return (cadr x) (caddr x) env))
            ((catch)
             (arg-count x 1)
             (comp-catch (cadr x) (cddr x) env val? more?))
            ((throw)
             (arg-count x 2 2)
             (comp-throw (cadr x) (caddr x) env))
            ((unwind-protect)
             (arg-count x 1)
             (comp-unwind-protect (cadr x) (cddr x) env val? more?))
            ((funcall)
             (comp-funcall (cadr x) (cddr x) env val? more?))
            ((apply)
             (comp-apply (cadr x) (cddr x) env val? more?))
            ((%op)
             (comp-op (cadr x) (cddr x) env val? more?))
            (otherwise
             (cond
               ((aif (and (symbolp (car x))
                          (compiler-macro-function (car x)))
                     (let ((form (funcall it x)))
                       (unless (eq form x)
                         (comp form env val? more?)))))
               ((aif (and (symbolp (car x))
                          (macro (car x) env))
                     (comp-macroexpand it x env val? more?)
                     (comp-call t (car x) (cdr x) env val? more?)))))))))

     (comp-op (opname args env val? more?)
       ;; We'll now compute the args (they'll remain on the stack) and generate
       ;; an operator specified by the caller, who hopefully knows what it's
       ;; doing. Such custom OP-s are assumed to be side-effect-free.
       (cond
         ((not val?)
          ;; If the value is not needed, compile arguments for potential side
          ;; effects.
          (comp-seq args env nil t))
         ((%seq (comp-list args env)
                (gen (strcat opname))
                (unless more? (gen "RET"))))))

     (comp-one-setq (name value env val? more?)
       (assert (symbolp name) "Only symbols can be SETQ")
       (let ((local (find-var name env)))
         (cond
           ((and local (eq :smac (caddr local)))
            ;; setq on symbol macro should be treated as setf on expansion
            (comp `(setf ,(cadddr local) ,value) env val? more?))
           (t
            (%seq (comp value env t t)
                  (gen-set name env local)
                  (unless val? (gen "POP"))
                  (unless more? (gen "RET")))))))

     (comp-setq (exps env val? more?)
       (cond
         ((not exps)
          (comp-const nil val? more?))
         ((not (cddr exps))
          (comp-one-setq (car exps) (cadr exps) env val? more?))
         (t (%seq (comp-one-setq (car exps) (cadr exps) env nil t)
                  (comp-setq (cddr exps) env val? more?)))))

     (comp-psetq (exps env val? more?)
       (with-seq-output <<
         (let ((names (let rec ((exps exps)
                                (ret nil))
                        (if (not exps) ret
                            (let ((name (car exps))
                                  (value (cadr exps)))
                              (<< (comp value env t t))
                              (rec (cddr exps) (cons name ret)))))))
           (foreach names (lambda (name)
                            (<< (gen-set name env)
                                (gen "POP")))))
         (<< (comp-const nil val? more?))))

     (comp-const (x val? more?)
       (when val?
         (%seq (gen "CONST" x)
               (unless more? (gen "RET")))))

     (comp-var (x env val? more?)
       (when val?
         (%seq (gen-var x env)
               (unless more? (gen "RET")))))

     (comp-seq (exps env val? more?)
       (cond
         ((not exps) (comp-const nil val? more?))
         ((not (cdr exps)) (comp (car exps) env val? more?))
         (t (%seq (comp (car exps) env nil t)
                  (comp-seq (cdr exps) env val? more?)))))

     (comp-decl-seq (exps env val? more?)
       (with-declarations exps
         (declare-locally-special)
         (with-env (comp-seq exps env val? more?))))

     (comp-multiple-value-prog1 (first rest env val? more?)
       (cond
         ((not val?)
          (comp-seq (list* first rest) env nil more?))
         (rest
          (%seq (comp first env t t)
                (comp-seq rest env nil t)
                (unless more? (gen "RET"))))
         ((comp first env val? more?))))

     (comp-prog1 (first rest env val? more?)
       (cond
         ((not val?)
          (comp-seq (list* first rest) env nil more?))
         (rest
          (%seq (comp first env t t)
                (gen "VALUES" 1)
                (comp-seq rest env nil t)
                (unless more? (gen "RET"))))
         ((%seq (comp first env t t)
                (gen "VALUES" 1)
                (unless more? (gen "RET"))))))

     (comp-progv (names values body env val? more?)
       (cond
         ((and body names)
          (%seq (comp names env t t)
                (comp values env t t)
                (gen "PROGV")
                (comp-seq body env val? more?)
                (if more?
                    (gen "UNFR" 0 1)
                    (gen "RET"))))
         (t
          (comp-seq (append values body) env val? more?))))

     (comp-pop (name env val? more?)
       (assert (symbolp name) (strcat "%POP expects a symbol, got: " name))
       (%seq (if (find-special name env)
                 (gen "GLPOP" name)
                 (aif (find-var name env)
                      (if (eq :smac (caddr it))
                          (error/wp "%POP called on symbol macro: " name)
                          (gen "LPOP" (car it) (cadr it)))
                      (gen "GLPOP" (unknown-variable name env))))
             (unless val? (gen "POP"))
             (unless more? (gen "RET"))))

     (comp-list (exps env)
       (with-seq-output <<
         (let rec ((exps exps))
           (when exps
             (<< (comp (car exps) env t t))
             (rec (cdr exps))))))

     (comp-block (name forms env val? more?)
       (assert (symbolp name) (strcat "BLOCK expects a symbol, got: " name))
       (let ((body (and (not (find-block name env))
                        (catch name
                          (with-env (comp-seq forms env val? more?))))))
         (or body
             (let ((label (gensym "block")))
               (%seq (gen "BLOCK")
                     (with-extenv (:lex (vector (list name :block label val? more?)))
                       (comp-seq forms env val? more?))
                     (vector label)
                     (if more?
                         (gen "UNFR" 1 0)
                         (gen "RET")))))))

     (comp-return (name value env)
       (assert (symbolp name) (strcat "RETURN-FROM expects a symbol, got: " name))
       (let* ((block (or (find-block name env)
                         (throw name nil)))
              (data (cddr block))
              (label (%pop data))
              (val? (%pop data))
              (more? (%pop data)))
         (%seq (comp value env val? t)
               (if val?
                   (gen "LRET" label (car block))
                   (gen "LJUMP" label (car block))))))

     (comp-tagbody (forms env val? more?)
       ;; a TAGBODY introduces a single return point in the lexical
       ;; environment; this is necessary because we can jump to a tag
       ;; from a nested environment, so the runtime will need to save
       ;; the stack length and current environment for each tagbody.
       ;; a GO instruction will fetch the TAGBODY variable that it
       ;; refers to, restore that environment and jump to the
       ;; specified index.
       (with-seq-output <<
         (let ((tags (list nil))
               (tbody (gensym "tagbody")))
           ;; pass 1: fetch tags
           (let rec ((forms forms)
                     (p tags))
             (when forms
               (if (atom (car forms))
                   (let ((cell (list (list (car forms)
                                           :tag tbody (gensym "tag")))))
                     (rec (cdr forms) (%rplacd p cell)))
                   (rec (cdr forms) p))))
           (%pop tags)
           (cond
             ((null tags)
              (<< (comp-seq forms env nil t)
                  (comp-const nil val? more?)))
             ((with-extenv (:tags (as-vector tags) :lex (vector (list tbody :tagbody)))
                (<< (gen "BLOCK"))           ; define the tagbody entry
                (foreach forms (lambda (x)
                                 (if (atom x)
                                     (<< (vector (cadddr (%pop tags)))) ; label
                                     (<< (comp x env nil t)))))
                (when val? (<< (gen "NIL"))) ; tagbody returns NIL
                (<< (gen "UNFR" 1 0))        ; pop the tagbody from the env
                (unless more? (<< (gen "RET")))))))))

     (comp-go (tag env)
       (let ((pos (find-tag tag env)))
         (assert pos (strcat "TAG " tag " not found"))
         (let* ((tbody (find-tagbody (caddr pos) env))
                (i (car tbody)))
           (gen "LJUMP" (cadddr pos) i))))

     (comp-if (pred then else env val? more?)
       (cond
         ((always-true-p pred)
          (comp then env val? more?))
         ((always-false-p pred)
          (comp else env val? more?))
         ((and (consp pred)
               (%memq (car pred) '(not null)))
          (comp-if (cadr pred) else then env val? more?))
         (t
          (let ((pcode (comp pred env t t))
                (tcode (comp then env val? more?))
                (ecode (comp else env val? more?)))
            (cond
              ((equal tcode ecode)
               (comp pred env nil t))
              ((zerop (length tcode))
               (let ((l2 (mklabel)))
                 (%seq pcode
                       (gen "TJUMP" l2)
                       ecode
                       (vector l2)
                       (if more? nil (gen "RET")))))
              ((zerop (length ecode))
               (let ((l1 (mklabel)))
                 (%seq pcode
                       (gen "FJUMP" l1)
                       tcode
                       (vector l1)
                       (if more? nil (gen "RET")))))
              (t
               (let ((l1 (mklabel))
                     (l2 (if more? (mklabel))))
                 (%seq pcode
                       (gen "FJUMP" l1)
                       tcode
                       (if more? (gen "JUMP" l2))
                       (vector l1)
                       ecode
                       (if more? (vector l2))))))))))

     (comp-or (exps env val? more? &optional l1)
       (cond
         ((null exps)
          (comp-const nil val? more?))
         ((and (consp (car exps))
               (eq 'or (caar exps)))
          (comp-or (append (cdar exps) (cdr exps))
                   env val? more? l1))
         ((cdr exps)
          (cond
            ((always-true-p (car exps))
             (comp (car exps) env val? more?))
            ((always-false-p (car exps))
             (comp-or (cdr exps) env val? more? l1))
            (l1
             (%seq (comp (car exps) env t t)
                   (gen (if val? "TJUMPK" "TJUMP") l1)
                   (comp-or (cdr exps) env val? more? l1)))
            (t
             (let ((l1 (mklabel)))
               (%seq (comp (car exps) env t t)
                     (gen (if val? "TJUMPK" "TJUMP") l1)
                     (comp-or (cdr exps) env val? more? l1)
                     (vector l1)
                     (gen "VALUES" 1)
                     (unless more? (gen "RET")))))))
         (t
          (comp (car exps) env val? more?))))

     (comp-funcall (f args env val? more?)
       (if (or (safe-atom-p f)
               (and (consp f)
                    (or (%memq (car f) *lambda-syms*)
                        (eq (car f) 'function)))
               (let rec ((args args))
                 (if (not args)
                     t
                     (and (safe-atom-p (car args))
                          (rec (cdr args))))))
           (comp-call nil f args env val? more?)
           (comp-call t 'funcall (list* f args) env val? more?)))

     (comp-apply (f args env val? more?)
       (if (or (safe-atom-p f)
               (and (consp f)
                    (or (%memq (car f) *lambda-syms*)
                        (eq (car f) 'function)))
               (let rec ((args args))
                 (if (not args)
                     t
                     (and (safe-atom-p (car args))
                          (rec (cdr args))))))
           (comp-call nil f args env val? more? :apply t)
           (comp-call t 'apply (list* f args) env val? more? :apply t)))

     (comp-call (local f args env val? more? &key apply)
       (labels ((mkret (the-function)
                  (cond
                    (more? (let ((k (mklabel)))
                             (%seq (gen "SAVE" k)
                                   (comp-list args env)
                                   the-function
                                   (gen (if apply "APPLY" "CALL") (length args))
                                   (vector k)
                                   (unless val? (gen "POP")))))
                    (t (%seq (comp-list args env)
                             the-function
                             (gen (if apply "APPLY" "CALL") (length args))))))
                (maybe-inline-global (f)
                  (when (and (not apply)
                             (%get-symbol-prop f :inline))
                    (let ((lambda-list (%get-symbol-prop f :lambda-list))
                          (body (%get-symbol-prop f :lambda-body)))
                      (when body
                        (let ((form (inline-call lambda-list args body)))
                          (unless (eq form 'decline)
                            (return-from maybe-inline-global
                              (comp form env val? more?)))))))
                  (mkret (gen "FGVAR" (unknown-function f)))))
         (cond
           ((or (numberp f)
                (stringp f)
                (regexpp f)
                (charp f)
                (vectorp f))
            (error/wp (strcat f " is not a function")))
           ((and local (symbolp f))
            (let ((localfun (find-func f env)))
              (cond
                (localfun
                 (mkret (gen "LVAR" (car localfun) (cadr localfun))))
                ((%primitivep f)
                 (if (and (not val?) (not (%prim-side-effects f)))
                     (comp-seq args env nil more?)
                     (%seq (comp-list args env)
                           (gen "PRIM" f (length args))
                           (unless val? (gen "POP"))
                           (unless more? (gen "RET")))))
                ((or (eq f t) (eq f nil))
                 (error/wp (strcat f " is not a function")))
                (t
                 (maybe-inline-global f)))))
           ((and (not local)
                 (consp f)
                 (eq (car f) 'quote)
                 (symbolp (cadr f)))
            ;; (funcall 'stuff ...)
            (maybe-inline-global (cadr f)))
           ((and (consp f)
                 (%memq (car f) *lambda-syms*))
            (cond
              ((and (not (cadr f)) (not args))
               (comp-decl-seq (cddr f) env val? more?))
              (t
               ;; This is the case for (apply (lambda ...) ...), or
               ;; (funcall (lambda ...) ...), or ((lambda ...) ...).
               ;; We inline the function, instead of consing a closure and
               ;; then call it. Of course, it would be nice if we would also
               ;; do compile-time argument matching and perhaps avoid the
               ;; XARGS overhead, but oh well.. good enough for now.
               (%seq (comp-list args env)
                     ;; this sets the machine n_args value, which is required
                     ;; by the function header (either ARGS, ARG_ or XARGS).
                     (gen "NARGS" (if apply (- (length args)) (length args)))
                     (let ((name (when (eq (%pop f) '%fn)
                                   (%pop f)))
                           (args (%pop f))
                           (body f))
                       (comp-inner-lambda name args body env val? more?))))))
           (t (mkret (comp f env t t))))))

     (gen-simple-args (args n names)
       (cond
         ((null args) (gen "ARGS" n))
         ((symbolp args)
          (when (%memq args names)
            (error/wp (strcat "Duplicate function argument " args)))
          (gen "ARG_" n))
         ((%memq (car args) names)
          (error/wp (strcat "Duplicate function argument " (car args))))
         (t
          (gen-simple-args (cdr args)
                           (1+ n)
                           (cons (car args) names)))))

     (make-true-list (lst)
       (when lst
         (if (atom lst)
             (list lst)
             (cons (car lst) (make-true-list (cdr lst))))))

     (comp-simple-lambda (name args body env val? more?)
       (with-seq-output <<
         (with-declarations body
           (<< (gen-simple-args args 0 nil))
           (let ((args (make-true-list args))
                 (specials 0))
             (foreach-index args
               (lambda (name index)
                 (when (or (%specialp name)
                           (%memq name locally-special))
                   (incf specials)
                   (<< (gen "BIND" name index)))))
             (cond
               (args
                (setq env (extenv env :lex (map1-vector #'maybe-special args)))
                (declare-locally-special :except args)
                (<< (with-env (comp-lambda-body name body env val? more?)))
                (when more?
                  (<< (gen "UNFR" 1 specials))))
               (t
                (declare-locally-special)
                (<< (with-env (comp-lambda-body name body env val? more?)))))))))

     (comp-extended-lambda
         (name body env val? more?
               &key required optional rest key has-key aux aok names
               &aux
               (index 0)
               (envcell (vector))
               (specials 0))
       (with-declarations body
         (with-seq-output <<
           (setq env (extenv env :lex envcell))
           (labels ((newarg (name)
                      (%vector-push envcell (maybe-special name))
                      (when (or (%specialp name)
                                (%memq name locally-special))
                        (incf specials)
                        (<< (gen "BIND" name index)))
                      (incf index))
                    (newdef (name defval supplied-p)
                      (unless supplied-p
                        (setq supplied-p (gensym (strcat name "-SUPPLIED-P"))))
                      (newarg supplied-p)
                      (when defval
                        (let ((l1 (mklabel)))
                          (<< (gen "LVAR" 0 (1- index)) ;; supplied-p
                              (gen "TJUMP" l1)          ;; if T then it's passed
                              (with-env
                                (comp defval env t t))  ;; compile default value
                              (gen "LSET" 0 index)      ;; set arg value in env
                              (gen "POP")               ;; discard from stack
                              (vector l1))))
                      (newarg name)))
             (<< (gen "XARGS"
                      (length required)
                      (length optional)
                      (if rest 1 0)
                      (when has-key (map1-vector #'caar key))
                      aok))
             (foreach required #'newarg)
             (foreach optional
               (lambda (opt)
                 (newdef (car opt) (cadr opt) (caddr opt))))
             (when rest (newarg rest))
             (foreach key
               (lambda (key)
                 (newdef (cadar key) (cadr key) (caddr key))))
             (foreach aux
               (lambda (aux)
                 (let ((name (car aux))
                       (defval (cadr aux)))
                   (when defval
                     (<< (with-env (comp defval env t t))
                         (gen "VAR")))
                   (newarg name))))
             (declare-locally-special :except names)
             (<< (with-env (comp-lambda-body name body env val? more?)))
             (when more?
               (<< (gen "UNFR" 1 specials)))))))

     (comp-inner-lambda (name args body env val? more?)
       (let* ((parsed (parse-lambda-list args))
              (required (getf parsed :required))
              (rest (getf parsed :rest)))
         (cond
           ((or (getf parsed :optional)
                (getf parsed :key)
                (getf parsed :has-key)
                (getf parsed :aok)
                (getf parsed :aux))
            (apply #'comp-extended-lambda name body env val? more? parsed))
           (t
            (comp-simple-lambda name (cond
                                       ((and required rest)
                                        `(,@required . ,rest))
                                       (required)
                                       (rest))
                                body env val? more?)))))

     (comp-lambda (name args body env)
       (gen "FN" (comp-inner-lambda name args body env t nil) name))

     (comp-lambda-body (name body env val? more?)
       (if name
           (comp-block name body env val? more?)
           (comp-seq body env val? more?)))

     (get-bindings (bindings vars?)
       (let (names vals)
         (foreach bindings (lambda (x)
                             (if (consp x)
                                 (progn (push (if vars? (cadr x) (cdr x)) vals)
                                        (setq x (car x)))
                                 (if vars?
                                     (push nil vals)
                                     (error/wp "Malformed LABELS/FLET/MACROLET")))
                             (unless vars?
                               (setq x (function-name x)))
                             (when (and (not vars?)
                                        (%memq x names))
                               (error/wp "Duplicate name in LABELS/FLET/MACROLET"))
                             (push x names)))
         (list (nreverse names) (nreverse vals))))

     (comp-flets (bindings body env labels? val? more?)
       (if bindings
           (with-seq-output <<
             (let* ((bindings (get-bindings bindings nil))
                    (names (car bindings))
                    (funcs (cadr bindings))
                    (len (length names)))
               (flet ((extenv ()
                        (setq env (extenv env :lex (map1-vector (lambda (name)
                                                                  (list name :func))
                                                                names)))
                        (<< (gen "FRAME"))))
                 (when labels? (extenv))
                 (map2 (lambda (name func)
                         (<< (with-env (comp-lambda name (car func) (cdr func) env))))
                       names funcs)
                 (unless labels? (extenv))
                 (<< (if (> len 1) (gen "VARS" len) (gen "VAR")))
                 (cond
                   (more?
                    (<< (with-env (comp-decl-seq body env val? t))
                        (gen "UNFR" 1 0)))
                   (t
                    (<< (with-env (comp-decl-seq body env val? nil))))))))
           (comp-decl-seq body env val? more?)))

     (comp-macrolet-function (def)
       (let ((name (car def))
             (args (cadr def))
             (body (cddr def)))
         (compile (macro-lambda name args body))))

     (comp-macrolet (bindings body env val? more?)
       (when bindings
         (setq env (extenv env :lex
                           (cons '%skip-count
                                 (map1-vector
                                  (lambda (el)
                                    (list (car el) :func
                                          :macro (comp-macrolet-function el)))
                                  bindings)))))
       (with-env (comp-decl-seq body env val? more?)))

     (comp-symbol-macrolet (bindings body env val? more?)
       (when bindings
         (setq env (extenv env :lex
                           (cons '%skip-count
                                 (map1-vector
                                  (lambda (el)
                                    (list (car el) :var
                                          :smac (cadr el)))
                                  bindings)))))
       (with-env (comp-decl-seq body env val? more?)))

     (comp-macroexpand (expander form env val? more?)
       (let ((expansion (gethash form *macroexpand-cache*)))
         (unless expansion
           (setq expansion
                 (%hash-set (funcall expander form)
                            form
                            *macroexpand-cache*)))
         (with-env (comp expansion env val? more?))))

     (comp-mvb (names values-form body env val? more?)
       (cond
         ((null body)
          (comp-seq (list values-form nil) env val? more?))
         (names
          (with-seq-output <<
            (<< (comp values-form env t t)
                (gen "MVB" (length names)))
            (with-declarations body
              (let ((specials 0))
                (foreach-index names
                  (lambda (name index)
                    (when (or (%specialp name)
                              (%memq name locally-special))
                      (incf specials)
                      (<< (gen "BIND" name index)))))
                (setq env (extenv env :lex (map1-vector #'maybe-special names)))
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  (t
                   (<< (with-env (comp-seq body env val? nil)))))))))
         (t
          (comp-decl-seq (list* values-form body) env val? more?))))

     (comp-values (forms env val? more?)
       (cond
         (val?
          (%seq (comp-list forms env)
                (gen "VALUES" (length forms))
                (unless more? (gen "RET"))))
         (t
          (comp-seq forms env nil more?))))

     (comp-named-let (looop bindings body env val? more?)
       (let ((names (map1 (lambda (x)
                            (if (consp x) (car x) x))
                          bindings)))
         (comp `(labels ((,looop ,names
                           ,@body))
                  (,looop ,@(map1 (lambda (x)
                                    (if (consp x) (cadr x)))
                                  bindings)))
               env val? more?)))

     (comp-let (bindings body env val? more?)
       (cond
         ((null bindings)
          (comp-decl-seq body env val? more?))
         ((symbolp bindings)
          (comp-named-let bindings (%pop body) body env val? more?))
         ((null body)
          (comp-seq `(,@(cadr (get-bindings bindings t)) nil) env val? more?))
         (t
          (with-seq-output <<
            (with-declarations body
              (let* ((bindings (get-bindings bindings t))
                     (names (car bindings))
                     (vals (cadr bindings))
                     (specials 0))
                (foreach vals (lambda (val) (<< (comp val env t t))))
                (<< (gen "LET" (length names)))
                (foreach-index names
                  (lambda (name index)
                    (when (or (%specialp name)
                              (%memq name locally-special))
                      (incf specials)
                      (<< (gen "BIND" name index)))))
                (setq env (extenv env :lex (map1-vector #'maybe-special names)))
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  (t
                   (<< (with-env (comp-seq body env val? nil)))))))))))

     (comp-let* (bindings body env val? more?)
       (cond
         ((null bindings)
          (comp-decl-seq body env val? more?))
         ((and (null (cdr body))
               (consp (car body))
               (eq 'let* (caar body)))
          (comp-let* (append bindings (cadar body))
                     (cddar body)
                     env val? more?))
         (t
          (with-seq-output <<
            (with-declarations body
              (let* ((bindings (get-bindings bindings t))
                     (names (car bindings))
                     (vals (cadr bindings))
                     (specials 0)
                     (index 0)
                     (envcell (vector)))
                (setq env (extenv env :lex envcell))
                (<< (gen "FRAME"))
                (map2 (lambda (name x)
                        (<< (with-env (comp x env t t))
                            (gen "VAR")
                            (when (or (%specialp name)
                                      (%memq name locally-special))
                              (incf specials)
                              (gen "BIND" name index)))
                        (incf index)
                        (%vector-push envcell (maybe-special name)))
                      names vals)
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  (t
                   (<< (with-env (comp-seq body env val? nil)))))))))))

     (comp-catch (tag body env val? more?)
       (if body
           (let ((k1 (mklabel)))
             (%seq (comp tag env t t)
                   (gen "CATCH" k1)
                   (comp-seq body env val? more?)
                   (vector k1)
                   (if val?
                       (if more?
                           (gen "UNFR" 0 1)
                           (gen "RET"))
                       (gen "POP"))))
           (comp-const nil val? more?)))

     (comp-throw (tag ret env)
       (%seq (comp tag env t t)
             (comp ret env t t)
             (gen "THROW")))

     (comp-unwind-protect (form cleanup env val? more?)
       (if cleanup
           (let ((k (mklabel)))
             (%seq (gen "UPOPEN" k)
                   (comp form env val? t) ; if val? is T, this leaves it on the stack
                   (gen "UPEXIT")
                   (vector k)
                   (comp-seq cleanup env nil t) ; result of cleanup code not needed
                   (gen "UPCLOSE")
                   (if more? nil (gen "RET"))))
           (comp form env val? more?)))

     (compile (exp)
       (assert (and (consp exp)
                    (%memq (car exp) *lambda-syms*))
               "Expecting (LAMBDA (...) ...) in COMPILE")
       (let ((*macroexpand-cache* (make-hash)))
         (%eval-opcode (comp exp (make-environment) t nil))))

     (compile-string (str &optional (filename *current-file*))
       (let ((*current-file* filename)
             (reader (lisp-reader str 'EOF))
             (all-code (vector))
             (link-addr 0)
             (*xref-info* (vector))
             (env (make-environment))
             (delayed ()))
         (labels ((comp1 (form)
                    (let ((code (with-env (comp form env nil t))))
                      (when code
                        (if *delay-eval*
                            (push (setq code (%assemble-opcode code))
                                  delayed)
                            (setq code (%assemble-and-exec-opcode code)))
                        (setq code (copy-seq code))
                        (%relocate-code code link-addr)
                        (setq link-addr (+ link-addr (length code)))
                        (%seq-cat all-code (list code)))))
                  (rec ()
                    (let* ((token (funcall reader 'next))
                           (form (cdr token)))
                      (unless (eq form 'EOF)
                        (let ((*current-pos* (car token))
                              (*macroexpand-cache* (make-hash))
                              (*delay-eval* nil))
                          (comp1 form))
                        (rec)))))
           (rec)
           (let ((xref *xref-info*)
                 (*xref-info* nil))
             (when (and *current-file* (plusp (length xref)))
               (comp1 `(%grok-xref-info ,*current-file* ,xref))))
           (unwind-protect
               (%serialize-code all-code (make-hash))
             (foreach (nreverse delayed) #'%eval-code))))))

  (set-symbol-function! 'compile #'compile)
  (set-symbol-function! '%compile-string #'compile-string))

;; (if (if *xref-info*
;;         (< (setq *build-count* (1+ *build-count*)) 3))
;;     (%:console.log "Rebuild" *build-count*)
;;     (throw 'compiler 'restart))

(defun compile-string args
  (let rec ()
    (let ((result (catch 'compiler
                    (apply #'%compile-string args))))
      (if (eq result 'restart)
          (rec)
          result))))

(defun read1-from-string (str)
  (let ((reader (lisp-reader str 'EOF)))
    (vector (cdr (funcall reader 'next))
            (funcall reader 'pos))))

(defun %with-undefined-warnings (thunk)
  (let ((*unknown-functions* nil)
        (*unknown-variables* nil))
    (multiple-value-prog1
        (funcall thunk)
      (foreach *unknown-functions*
        (lambda (sym)
          (unless (symbol-function sym)
            (warn (strcat "Unknown function: " sym)))))
      (foreach *unknown-variables*
        (lambda (sym)
          (unless (or (%globalp sym)
                      (%specialp sym))
            (warn (strcat "Undefined variable: " sym))))))))

(defmacro with-undefined-warnings body
  `(%with-undefined-warnings (lambda () ,@body)))

(defun eval (expr)
  (with-undefined-warnings
    ((compile (list 'lambda nil expr)))))

(defun eval-string (str)
  (with-undefined-warnings
    (let ((reader (lisp-reader str 'EOF)))
      (labels ((rec (last expr)
                 (if (eq expr 'EOF) last
                     (rec ((compile (list 'lambda nil expr)))
                          (cdr (funcall reader 'next))))))
        (rec nil (cdr (funcall reader 'next)))))))

(defmacro with-load-timings body
  `(cond
     (*load-timing*
      ,(let ((t-load (gensym))
             (t-comp (gensym)))
         `(flet ((%get-file-contents args
                   (let ((,t-load (get-internal-run-time)))
                     (prog1 (apply #'%get-file-contents args)
                       (%rplaca *load-timing*
                                (+ (car *load-timing*)
                                   (- (get-internal-run-time) ,t-load))))))
                 (compile-string args
                   (let ((,t-comp (get-internal-run-time)))
                     (prog1 (apply #'compile-string args)
                       (%rplaca (cdr *load-timing*)
                                (+ (cadr *load-timing*)
                                   (- (get-internal-run-time) ,t-comp)))))))
            ,@body)))
     (t ,@body)))

(defun %load (url)
  (%stream-put *trace-output* (strcat ";; Loading " url #\Newline))
  (with-load-timings
    (let ((code (%get-file-contents (make-url url))))
      (unless code (error (strcat "Unable to load file: " url)))
      (with-undefined-warnings
        (compile-string code url)))))

(defun make-url (url)
  (if *url-prefix*
      (strcat *url-prefix* url)
      url))

(defun load (url)
  (let ((*package* *package*)
        (*read-table* *read-table*))
    (%load url)
    nil))

;;; multiple values

(defmacro multiple-value-call (fn &rest values)
  `(apply ,fn (nconc ,@(map1 (lambda (form)
                               `(multiple-value-list ,form))
                             values))))

(defun values-list (list)
  (apply #'values list))

(defun macroexpand-1 (form &optional (*compiler-env* *compiler-env*))
  (cond
    ((atom form)
     (or (%:find-symbol-macrolet-in-compiler-env form) form))
    ((and (eq 'progn (car form))
          (null (cdr form)))
     nil)
    (t
     (aif (or (%:find-macrolet-in-compiler-env (car form))
              (%macro (car form)))
          (funcall it form)
          form))))

(defun macroexpand (form &optional (*compiler-env* *compiler-env*))
  (let ((result (macroexpand-1 form)))
    (if (eq result form)
        result
        (macroexpand result))))

(defconstant *core-files*
  '("lisp/init.lisp"
    "lisp/array.lisp"
    "lisp/byte.lisp"
    "lisp/hash.lisp"
    "lisp/type.lisp"
    "lisp/macroexpand.lisp"
    "lisp/format.lisp"
    "lisp/struct.lisp"
    "lisp/loop.lisp"
    "lisp/list.lisp"
    "lisp/seq.lisp"
    "lisp/closette.lisp"
    "lisp/printer.lisp"
    "lisp/conditions.lisp"
    "lisp/stream.lisp"
    "lisp/ffi.lisp"
    "ide/ide.lisp"))

;;;

;; (defconstant *id-test-list* '(foo bar))
;; (defun test-serialization-identity ()
;;   (eq '#.*id-test-list* '#.*id-test-list*))

;; (defun test-serialization-identity2 ()
;;   (eq *id-test-list* *id-test-list*))

;; (defparameter *id-test-circular* (list 'a))
;; (%rplacd *id-test-circular* *id-test-circular*)
;; (defun test-serialization-circular ()
;;   (eq '#.*id-test-circular* '#.*id-test-circular*))

;; (defconstant *id-test-array* #(1 2 3))
;; (defun test-serialization-array ()
;;   (eq '#.*id-test-array* '#.*id-test-array*))

;; (defparameter *id-test-array-circular* (vector 'a 'self))
;; (vector-set *id-test-array-circular* *id-test-array-circular* 1)
;; (defun test-serialization-array-circular ()
;;   (eq '#.*id-test-array-circular* '#.*id-test-array-circular*))

EOF

(let ((*current-file* "compiler.lisp"))
  (console.print (compile-string (%js-eval "window.CURRENT_FILE"))))
