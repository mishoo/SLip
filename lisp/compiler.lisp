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
(%special! '*read-table*
           '*package*
           '*standard-input*
           '*current-file*
           '*current-pos*
           '*url-prefix*
           '*unknown-functions*
           '*unknown-variables*
           '*compiler-env*
           '*xref-info*
           '*compiler-macros*
           '*standard-output*
           '*error-output*
           '*trace-output*
           '*whole-form*
           '*load-timing*)

(setq *read-table* nil)
(setq *url-prefix* nil)
(setq *unknown-functions* nil)
(setq *unknown-variables* nil)
(setq *compiler-env* nil)
(setq *compiler-macros* nil)
(setq *whole-form* nil)
(setq *load-timing* nil)

(setq *standard-output* (%make-output-stream))
(setq *error-output* (%make-output-stream))
(setq *trace-output* (%make-output-stream))

;; (defmacro cond (clauses)
;;   (when clauses
;;     (let ((first (car clauses)))
;;       (if (cdr first)
;;           `(if ,(car first)
;;                (progn ,@(cdr first))
;;                (cond ,@(cdr clauses)))
;;           `(or ,(car first)
;;                (cond ,@(cdr clauses)))))))
;;
;; Since we don't yet have quasiquote, I've macro-expanded it below. It's
;; horrible to write it manually.
(%macro! 'cond
         (%fn cond clauses
              (if clauses
                  (let ((first (car clauses)))
                    (if (cdr first)
                        (list 'if
                              (car first)
                              (cons 'progn
                                    (cdr first))
                              (cons 'cond
                                    (cdr clauses)))
                        (list 'or
                              (car first)
                              (cons 'cond
                                    (cdr clauses))))))))

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
          ((numberp x) x)
          ((stringp x) x)
          ((regexpp x) x)
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

(set-symbol-function!
 'maybe-xref-info
 (%fn maybe-xref-info (name type)
      (if (boundp '*xref-info*)
          (vector-push *xref-info*
                       #(name type *current-pos*)))))

;; better to avoid quasiquote here:
(%macro! 'defmacro
         (%fn defmacro (name args . body)
              (if (%primitivep name)
                  (error (strcat "Cannot DEFMACRO on primitive " name)))
              (maybe-xref-info name 'defmacro)
              (list '%macro!
                    (list 'quote name)
                    (list* '%fn name args body))))

(defmacro quasiquote (thing)
  (qq thing))

;;;; let the show begin

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

(defmacro defun (name args . body)
  (multiple-value-bind (setter name) (%:maybe-setter name)
    (maybe-xref-info name (if setter 'setf 'defun))
    `(set-symbol-function! ',(or setter name)
                           (%fn ,name ,args ,@body))))

(defun error (msg)
  (%error msg))

(defun warn (msg)
  (%warn msg))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defun map1 (func lst)
  (let ((ret nil))
    (tagbody
     next
     (when lst
       (setq ret (cons (funcall func (%pop lst)) ret))
       (go next)))
    (nreverse ret)))

(defun map1-vector (func lst)
  (let ((ret (vector)))
    (tagbody
     next
     (when lst
       (vector-push ret (funcall func (%pop lst)))
       (go next)))
    ret))

(defun map2 (func lst1 lst2)
  (let ((ret nil))
    (tagbody
     next
     (when (and lst1 lst2)
       (setq ret (cons (funcall func (%pop lst1) (%pop lst2)) ret))
       (go next)))
    (nreverse ret)))

(defun map2-vector (func lst1 lst2)
  (let ((ret (vector)))
    (tagbody
     next
     (when (and lst1 lst2)
       (vector-push ret (funcall func (%pop lst1) (%pop lst2)))
       (go next)))
    ret))

(defun foreach (lst func)
  (tagbody
   next
   (when lst
     (funcall func (%pop lst))
     (go next))))

(defmacro %incf (var)
  `(setq ,var (1+ ,var)))

(defmacro %decf (var)
  `(setq ,var (1- ,var)))

(defun foreach-index (lst func)
  (let ((index -1))
    (tagbody
     next
     (when lst
       (funcall func (%pop lst) (%incf index))
       (go next)))))

(defun identity (x) x)

(defun filter (lst &optional (pred #'identity))
  (let rec ((lst lst)
            (ret nil))
    (cond
      ((null lst) (nreverse ret))
      ((funcall pred (car lst))
       (rec (cdr lst) (cons (car lst) ret)))
      ((rec (cdr lst) ret)))))

(defun index-of (el lst)
  (let rec ((lst lst)
            (index 0))
    (when lst
      (if (eq el (car lst))
          index
          (rec (cdr lst) (1+ index))))))

(defmacro prog2 (exp1 exp2 . body)
  `(progn
     ,exp1
     (prog1 ,exp2 ,@body)))

;; OR is implemented in the compiler now.
;;
;; (defmacro %or (x exps)
;;   (cond
;;     ((cdr exps) `(if (setq ,x ,(car exps)) ,x (%or ,x ,(cdr exps))))
;;     (exps (car exps))))
;; (defmacro or exps
;;   (cond
;;     ((cdr exps) (let ((x (gensym "OR")))
;;                   `(let ((,x ,(car exps)))
;;                      (if ,x ,x (%or ,x ,(cdr exps))))))
;;     (exps (car exps))))

(defmacro and exps
  (cond
    ((cdr exps) `(when ,(car exps) (and ,@(cdr exps))))
    (exps (car exps))
    (t t)))

(defmacro member (item lst)
  `(%memq ,item ,lst))

;; will be redefined later to also check compiler env for symbol-macrolet
(defun safe-atom-p (thing)
  (symbolp thing))

(defmacro case (expr . cases)
  (let* ((safe (safe-atom-p expr))
         (vexpr (if safe
                    expr
                    (gensym "CASE")))
         (code (let recur ((cases cases))
                 (cond
                   ((null cases)
                    nil)
                   ((consp (caar cases))
                    (if (cdaar cases)
                        `(if (member ,vexpr ',(caar cases))
                             (progn ,@(cdar cases))
                             ,(recur (cdr cases)))
                        `(if (eq ,vexpr ',(caaar cases))
                             (progn ,@(cdar cases))
                             ,(recur (cdr cases)))))
                   ((and (not (cdr cases))
                         (member (caar cases) '(otherwise t)))
                    `(progn ,@(cdar cases)))
                   ((not (caar cases))
                    (recur (cdr cases)))
                   (t
                    `(if (eq ,vexpr ',(caar cases))
                         (progn ,@(cdar cases))
                         ,(recur (cdr cases))))))))
    (if safe
        code
        `(let ((,vexpr ,expr)) ,code))))

(defmacro aif (cond . rest)
  `(let ((it ,cond))
     (if it ,@rest)))

(defmacro push (obj place)
  `(setq ,place (cons ,obj ,place)))

(%global! '+keyword-package+)
(setq +keyword-package+ (find-package "KEYWORD"))

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
           (let ((out (%make-output-stream)))
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
                                    #\[ #\] #\{ #\} #\/ #\^ #\#)))))))
             (upcase str)))

         (read-symbol ()
           (let ((str (read-symbol-name)))
             (when (zerop (length str))
               (error (strcat "Bad character (or reader bug) in read-symbol: " (peek))))
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
                            (setq sym (intern sym +keyword-package+))
                            (%export sym +keyword-package+)
                            sym)
                           (t
                            (setq pak (find-package pak))
                            (if internal
                                (intern sym pak)
                                (or (%find-exported-symbol sym pak)
                                    (error (strcat "Symbol " sym " not accessible in package " pak)))))))
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
             (#\( (list* 'vector (read-list)))
             (#\' (next) (list 'function (read-token)))
             (#\: (next) (make-symbol (read-symbol-name)))
             (#\. (next) (eval (read-token)))
             (otherwise (croak (strcat "Unsupported sharp syntax #" (peek))))))

         (read-quote ()
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
                          ((nil) (croak "Unterminated list"))
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
             ((#\' #\’) (next) (read-quote))
             ((nil) eof)
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

(defmacro return (val)
  `(return-from nil ,val))

(defun lambda-keyword-p (sym)
  (member sym '(&optional &rest &body &key &aux &allow-other-keys)))

(defun macro-keyword-p (sym)
  (member sym '(&whole &environment)))

(defun ordinary-lambda-list-p (args)
  (let dig ((args args)
            (seen nil))
    (cond
      ((symbolp args))
      ((not (consp args))
       (error "Bad macro lambda list"))
      ((consp (car args))
       (when (and seen (symbolp (caar args)))
         (dig (cdr args) t)))
      ((member (car args) '(&rest &body))
       (when (symbolp (cadr args))
         (dig (cddr args) seen)))
      ((lambda-keyword-p (car args))
       (dig (cdr args) t))
      ((macro-keyword-p (car args))
       nil)
      ((dig (cdr args) seen)))))

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
           (if p p (error msg)))

         (symp (x)
           (and x (symbolp x)
                (not (eq x t))
                (not (lambda-keyword-p x))))

         (add (name)
           (unless (symp name)
             (error (strcat "Invalid name in lambda list " name)))
           (when (member name all)
             (error (strcat "Duplicate name in lambda list " name)))
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
              (error "Bad lambda list"))))

         (rec-rest (args)
           (when (cdr args)
             (case (cadr args)
               (&key (rec-key (cddr args)))
               (&aux (rec-aux (cddr args)))
               (otherwise
                (error "Bad lambda list after &rest"))))
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
                 (error "Bad &optional parameter"))))))

         (key-arg-names (arg)
           (cond
             ((consp arg)
              (add (cadr arg))
              arg)
             ((symp arg)
              (list (intern (symbol-name arg) :keyword) (add arg)))
             (t
              (error "Bad &key argument name"))))

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
                 (error "Bad &key parameter in lambda list"))))))

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
              (error "Bad &aux parameter in lambda list")))))

      (rec args)
      (list :required required
            :optional optional
            :rest rest
            :key key
            :has-key has-key
            :aux aux
            :aok allow-other-keys))))

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
;;               (%decf j)
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

(defun find-in-compiler-env (name type env)
  (%:%find-in-env name type env))

(defun find-macrolet-in-compiler-env (name &optional (env *compiler-env*))
  (when env
    (aif (find-in-compiler-env name :func (hash-get env :lex))
         (getf (cddr it) :macro))))

(defun find-symbol-macrolet-in-compiler-env (name &optional (env *compiler-env*))
  (and env (symbolp name)
       (aif (find-var-in-compiler-env name)
            (getf (cddr it) :smac))))

(defun find-var-in-compiler-env (name &optional (env *compiler-env*))
  (when env
    (find-in-compiler-env name :var (hash-get env :lex))))

(defun safe-atom-p (thing &optional (env *compiler-env*))
  (and (atom thing)
       (not (and (symbolp thing)
                 (aif (find-var-in-compiler-env thing)
                      (eq :smac (caddr it)))))))

(defun %dbind-error-missing-arg (arg)
  (error (strcat "Missing required argument: " arg)))

(defun %fn-destruct (macro? args values body)
  (let (names decls)
    (let ((topv (gensym)) rec)
      (labels
          ((add (name val)
             (cond
               ((symbolp name)
                (push `(,name ,val) decls))
               ((consp name)
                (let ((current (gensym "dstr")))
                  (add current val)
                  (rec nil nil nil nil name current 0)))
               (t
                (error (strcat "Unknown destructuring pattern: " name)))))
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
                            (unless thisarg
                              (error "Missing variable name for &WHOLE"))
                            (add thisarg (if (and macro? (eq values topv))
                                             `(or *whole-form* ,values)
                                             values)))
                          (rec nil nil nil nil (cddr args) values i))

                         (&optional
                          (when (or optional? rest? key? aux?)
                            (error "Invalid &OPTIONAL"))
                          (rec t nil nil nil (cdr args) values i))

                         ((&rest &body)
                          (when (or rest? key? aux?)
                            (error "Invalid &REST/&BODY"))
                          (let ((thisarg (cadr args)))
                            (unless thisarg
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
                             (add thisarg `(%pop ,values)))
                            (aux?
                             (add thisarg nil))
                            (key?
                             (add thisarg `(getf ,values ,(intern (symbol-name thisarg) (find-package "KEYWORD")))))
                            (t
                             (add thisarg `(if ,values
                                               (%pop ,values)
                                               (%dbind-error-missing-arg ',thisarg)))))
                          (rec optional? rest? key? aux? (cdr args) values (+ i 1)))))

                      ((consp thisarg)
                       (cond
                         (optional?
                          (let ((thisarg (car thisarg))
                                (default (cadr thisarg))
                                (thisarg-p (caddr thisarg)))
                            (when thisarg-p
                              (add thisarg-p `(if ,values t nil)))
                            (add thisarg `(if ,values (%pop ,values) ,default))))
                         (key?
                          (let ((thisarg (car thisarg))
                                (default (cadr thisarg))
                                (thisarg-p (caddr thisarg)))
                            (when thisarg-p
                              (add thisarg-p nil))
                            (add thisarg
                                 (let ((val (gensym)))
                                   `(let ((,val (getf ,values ,(intern (symbol-name thisarg)
                                                                       (find-package "KEYWORD"))
                                                      '%not-found)))
                                      (if (eq ,val '%not-found)
                                          ,default
                                          (progn
                                            ,@(when thisarg-p
                                                `((setq ,thisarg-p t)))
                                            ,val)))))))
                         (aux? (let ((thisarg (car thisarg))
                                     (value (cadr thisarg)))
                                 (add thisarg value)))
                         (rest? (error "Invalid argument list following &REST/&BODY"))
                         (t
                          (let ((sublist (gensym)))
                            (add sublist `(if ,values (%pop ,values) (error "Missing sublist")))
                            (rec nil nil nil nil thisarg sublist 0))))
                       (rec optional? rest? key? aux? (cdr args) values (+ i 1))))))
                 (t (error "Invalid lambda-list"))))))
        (rec nil nil nil nil args topv 0))
      `(let* ((,topv ,values) ,@(nreverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct nil args values body))

(defmacro defmacro (name lambda-list . body)
  (when (%primitivep name)
    (error (strcat "We shall not DEFMACRO on " name " (primitive function)")))
  (%::maybe-xref-info name 'defmacro)
  (if (ordinary-lambda-list-p lambda-list)
      `(%macro! ',name (%::%fn ,name ,lambda-list ,@body))
      (let ((args (gensym "ARGS")))
        `(%macro! ',name (%::%fn ,name ,args
                                 ,(%fn-destruct t lambda-list args body))))))

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
             (hash-add h key (cons val (hash-get h key)))
             (rec (cddr stuff)))))
       h))
    (env)))

(defmacro with-env body
  `(let ((*compiler-env* env))
     ,@body))

(defmacro with-extenv (forms . body)
  `(let ((env (extenv env ,@forms)))
     (with-env ,@body)))

(defun always-true-p (form)
  (or (eq form t)
      (numberp form)
      (stringp form)
      (regexpp form)
      (charp form)
      (vectorp form)
      (and (consp form)
           (eq 'quote (car form))
           (cadr form))))

(defun compiler-macro-function (name &optional (env *compiler-env*))
  (unless (and env (find-in-compiler-env name :func (hash-get env :lex)))
    (getf *compiler-macros* name)))

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
      `(:special ,specials))))

(defmacro with-declarations (exps &body body)
  `(multiple-value-bind (,exps declarations) (dig-declarations ,exps)
     (setq declarations (zip-declarations declarations))
     (let ((locally-special (filter (getf declarations :special)
                                    (lambda (sym)
                                      (not (%specialp sym))))))
       (labels ((declare-locally-special (&key except)
                  (when locally-special
                    (setq env (extenv env :lex
                                      (cons '%skip-count
                                            (map1-vector
                                             (lambda (name)
                                               (list name :var :special t))
                                             (if except
                                                 (filter locally-special
                                                         (lambda (name)
                                                           (not (member name except))))
                                                 locally-special))))))))
         ,@body))))

(labels
    ((assert (p msg)
       (if p p (error msg)))

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
       #( (as-vector cmd) ))

     (find-in-env (name type env)
       (find-in-compiler-env name type env))

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
       (find-macrolet-in-compiler-env name env))

     (find-special (name env)
       (or (%specialp name)
           (let ((v (find-var name env)))
             (when v (getf v :special)))))

     (gen-var (name env)
       (if (or (%globalp name)
               (find-special name env))
           (gen "GVAR" name)
           (aif (find-var name env)
                (if (eq :smac (caddr it))
                    (comp (cadddr it) env t t) ;; symbol macro expansion
                    (gen "LVAR" (car it) (cadr it)))
                (gen "GVAR" (unknown-variable name env)))))

     (gen-set (name env local)
       (if (or (%globalp name)
               (find-special name env))
           (gen "GSET" name)
           (if local
               (gen "LSET" (car local) (cadr local))
               (gen "GSET" (unknown-variable name env)))))

     (mklabel ()
       (gensym "label"))

     (macro (sym env)
       (aif (find-func sym env)
            (getf (cddr it) :macro)
            (%macro sym)))

     (unknown-function (sym)
       (unless (member sym *unknown-functions*)
         (push sym *unknown-functions*)))

     (unknown-variable (sym env)
       (unless (or (%globalp sym)
                   (find-special sym env))
         (unless (member sym *unknown-variables*)
           (push sym *unknown-variables*)))
       sym)

     (function-name (sym)
       ;; Hack to handle #'(SETF STUFF). In CL it seems that the name of the
       ;; function is the list itself (SETF STUFF), but we really need to make
       ;; it a symbol.
       (or (maybe-setter sym) sym))

     (comp (x env val? more?)
       (cond
         ((symbolp x) (cond
                        ((eq x nil) (comp-const nil val? more?))
                        ((eq x t) (comp-const t val? more?))
                        ((keywordp x) (comp-const x val? more?))
                        (t (comp-var x env val? more?))))
         ((atom x) (comp-const x val? more?))
         (t (case (car x)
              (quote
               (arg-count x 1 1)
               (comp-const (cadr x) val? more?))
              (progn (comp-seq (cdr x) env val? more?))
              (locally (comp-decl-seq (cdr x) env val? more?))
              ((prog1)
               (arg-count x 1)
               (comp-prog1 (cadr x) (cddr x) env val? more?))
              ((progv)
               (arg-count x 2)
               (comp-progv (cadr x) (caddr x) (cdddr x) env val? more?))
              ((multiple-value-prog1)
               (arg-count x 1)
               (comp-multiple-value-prog1 (cadr x) (cddr x) env val? more?))
              (%pop
               (arg-count x 1 1)
               (comp-pop (cadr x) env val? more?))
              (setq
               (arg-count x 2 2)
               (assert (symbolp (cadr x)) "Only symbols can be SETQ")
               (let* ((name (cadr x))
                      (value (caddr x))
                      (local (when (not (%globalp name))
                               (find-var name env))))
                 (cond
                   ((and local (eq :smac (caddr local)))
                    ;; setq on symbol macro should be treated as setf on expansion
                    (comp `(setf ,(cadddr local) ,value) env val? more?))
                   (t
                    (%seq (comp value env t t)
                          (gen-set name env local)
                          (unless val? (gen "POP"))
                          (unless more? (gen "RET")))))))
              (if (arg-count x 2 3)
                  (comp-if (cadr x) (caddr x) (cadddr x) env val? more?))
              (or (comp-or (cdr x) env val? more?))
              ((not null)
               (arg-count x 1 1)
               (if val?
                   (%seq (comp (cadr x) env t t)
                         (gen "NOT")
                         (if more? nil (gen "RET")))
                   (comp (cadr x) env nil more?)))
              (c/c
               (arg-count x 0 0)
               (if val? (gen "CC")))
              (let (comp-let (cadr x) (cddr x) env val? more?))
              (let* (comp-let* (cadr x) (cddr x) env val? more?))
              (multiple-value-bind (comp-mvb (cadr x) (caddr x) (cdddr x) env val? more?))
              (values (comp-values (cdr x) env val? more?))
              (labels (comp-flets (cadr x) (cddr x) env t val? more?))
              (flet (comp-flets (cadr x) (cddr x) env nil val? more?))
              (macrolet (comp-macrolet (cadr x) (cddr x) env val? more?))
              (symbol-macrolet (comp-symbol-macrolet (cadr x) (cddr x) env val? more?))
              ((lambda λ)
               (when val?
                 (%seq (comp-lambda nil (cadr x) (cddr x) env)
                       (unless more? (gen "RET")))))
              (%fn
               (when val?
                 (%seq (comp-lambda (cadr x) (caddr x) (cdddr x) env)
                       (unless more? (gen "RET")))))
              (function
               (arg-count x 1 1)
               (let ((sym (function-name (cadr x))))
                 (when val?
                   (cond
                     ((when (and (consp sym)
                                 (eq 'lambda (car sym)))
                        (comp sym env t more?)))
                     (t
                      (assert (symbolp sym) "FUNCTION requires a symbol")
                      (let ((local (find-func sym env)))
                        (%seq (if local
                                  (gen "LVAR" (car local) (cadr local))
                                  (progn
                                    (unless (symbol-function sym)
                                      (unknown-function sym))
                                    (gen "FGVAR" sym)))
                              (unless more? (gen "RET")))))))))
              (tagbody (comp-tagbody (cdr x) env val? more?))
              (go
               (arg-count x 1 1)
               (comp-go (cadr x) env))
              (block (comp-block (cadr x) (cddr x) env val? more?))
              (return-from (arg-count x 1 2) (comp-return (cadr x) (caddr x) env))
              (catch (comp-catch (cadr x) (cddr x) env val? more?))
              (throw (arg-count x 2 2) (comp-throw (cadr x) (caddr x) env))
              (unwind-protect (comp-unwind-protect (cadr x) (cddr x) env val? more?))
              (funcall (comp-funcall (cadr x) (cddr x) env val? more?))
              (t (cond
                   ((aif (and (symbolp (car x))
                              (compiler-macro-function (car x)))
                         (let ((form (funcall it x)))
                           (unless (eq form x)
                             (comp form env val? more?)))))
                   ((aif (and (symbolp (car x))
                              (macro (car x) env))
                         (comp-macroexpand it x env val? more?)
                         (comp-call t (car x) (cdr x) env val? more?)))))))))

     (comp-const (x val? more?)
       (when val?
         (%seq (gen "CONST" x)
               (if more? nil (gen "RET")))))

     (comp-var (x env val? more?)
       (when val?
         (%seq (gen-var x env)
               (if more? nil (gen "RET")))))

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
       (%seq (if (%globalp name)
                 (gen "GLPOP" name)
                 (aif (find-var name env)
                      (if (eq :smac (caddr it))
                          (error "%POP called on symbol macro: " name)
                          (gen "LPOP" (car it) (cadr it)))
                      (gen "GLPOP" (unknown-variable name env))))
             (unless val? (gen "POP"))
             (unless more? (gen "RET"))))

     (comp-list (exps env)
       (with-seq-output <<
         (let rec ((exps exps))
           (when exps
             (cond
               ((and (consp (car exps))
                     (eq '%ooo (caar exps)))
                ;; “out-of-order” operator: evaluate this item *after* the rest of
                ;; the list, but leave values on the stack in the proper order.
                (cond
                  ((cdr exps)
                   (<< (gen "NIL"))
                   (rec (cdr exps))
                   (<< (comp (cadar exps) env t t)
                       (gen "POPBACK" (length (cdr exps)))))
                  (t
                   (<< (comp (cadar exps) env t t)))))
               (t
                (<< (comp (car exps) env t t))
                (rec (cdr exps))))))))

     (comp-block (name forms env val? more?)
       (assert (symbolp name) (strcat "BLOCK expects a symbol, got: " name))
       (let ((body (and (not (find-block name env))
                        (catch name
                          (comp-seq forms env val? more?)))))
         (or body
             (let ((label (gensym "block")))
               (%seq (gen "BLOCK")
                     (with-extenv (:lex (vector (list name :block label val?)))
                       (comp-seq forms env val? more?))
                     #( label )
                     (if more?
                         (gen "UNFR" 1 0)
                         (gen "RET")))))))

     (comp-return (name value env)
       (assert (symbolp name) (strcat "RETURN-FROM expects a symbol, got: " name))
       (let* ((block (or (find-block name env)
                         (throw name nil)))
              (data (cddr block))
              (label (%pop data))
              (val? (%pop data)))
         (%seq (comp value env val? t)
               (gen "LVAR" (car block) (cadr block))
               (if val?
                   (gen "LRET" label)
                   (gen "LJUMP" label)))))

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
                     (rec (cdr forms) (rplacd p cell)))
                   (rec (cdr forms) p))))
           (%pop tags)
           (cond
             ((null tags)
              (<< (comp-seq forms env nil t)
                  (when val? (gen "NIL"))
                  (unless more? (gen "RET"))))
             ((with-extenv (:tags (as-vector tags) :lex (vector (list tbody :tagbody)))
                (<< (gen "BLOCK"))           ; define the tagbody entry
                (foreach forms (lambda (x)
                                 (if (atom x)
                                     (<< #( (cadddr (%pop tags)) )) ; label
                                     (<< (comp x env nil t)))))
                (when val? (<< (gen "NIL"))) ; tagbody returns NIL
                (<< (gen "UNFR" 1 0))        ; pop the tagbody from the env
                (unless more? (<< (gen "RET")))))))))

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
         ((always-true-p pred)
          (comp then env val? more?))
         ((and (consp pred)
               (eq (car pred) 'not))
          (comp-if (cadr pred) else then env val? more?))
         (t
          (let ((pcode (comp pred env t t))
                (tcode (comp then env val? more?))
                (ecode (comp else env val? more?)))
            (cond
              ((equal tcode ecode)
               (%seq (comp pred env nil t) ecode))
              ((zerop (length tcode))
               (let ((l2 (mklabel)))
                 (%seq pcode
                       (gen "TJUMP" l2)
                       ecode
                       #( l2 )
                       (if more? nil (gen "RET")))))
              ((zerop (length ecode))
               (let ((l1 (mklabel)))
                 (%seq pcode
                       (gen "FJUMP" l1)
                       tcode
                       #( l1 )
                       (if more? nil (gen "RET")))))
              (t
               (let ((l1 (mklabel))
                     (l2 (if more? (mklabel))))
                 (%seq pcode
                       (gen "FJUMP" l1)
                       tcode
                       (if more? (gen "JUMP" l2))
                       #( l1 )
                       ecode
                       (if more? #( l2 ))))))))))

     (comp-or (exps env val? more? &optional l1)
       (cond
         ((null exps)
          (%seq (when val? (gen "NIL"))
                (unless more? (gen "RET"))))
         ((and (consp (car exps))
               (eq 'or (caar exps)))
          (comp-or (append (cdar exps) (cdr exps))
                   env val? more? l1))
         ((cdr exps)
          (cond
            ((always-true-p (car exps))
             (comp (car exps) env val? more?))
            (l1
             (%seq (comp (car exps) env t t)
                   (if val?
                       (gen "TJUMPK" l1)
                       (gen "TJUMP" l1))
                   (comp-or (cdr exps) env val? more? l1)))
            (t
             (let ((l1 (mklabel)))
               (%seq (comp (car exps) env t t)
                     (if val?
                         (gen "TJUMPK" l1)
                         (gen "TJUMP" l1))
                     (comp-or (cdr exps) env val? more? l1)
                     #( l1 )
                     (gen "VALUES" 1)
                     (unless more? (gen "RET")))))))
         ((comp (car exps) env val? more?))))

     (comp-funcall (f args env val? more?)
       (if (or (safe-atom-p f)
               (let rec ((args args))
                 (if (not args)
                     t
                     (and (safe-atom-p (car args))
                          (rec (cdr args))))))
           (comp-call nil f args env val? more?)
           (comp-call t 'funcall (list* f args) env val? more?)))

     (comp-call (local f args env val? more?)
       (labels ((mkret (the-function)
                  (cond
                    (more? (let ((k (mklabel)))
                             (%seq (gen "SAVE" k)
                                   (comp-list args env)
                                   the-function
                                   (gen "CALL" (length args))
                                   #( k )
                                   (unless val? (gen "POP")))))
                    (t (%seq (comp-list args env)
                             the-function
                             (gen "CALL" (length args)))))))
         (cond
           ((or (numberp f)
                (stringp f)
                (regexpp f)
                (charp f)
                (vectorp f))
            (error (strcat f " is not a function")))
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
                (t
                 (unless (symbol-function f)
                   (unknown-function f))
                 (mkret (gen "FGVAR" f))))))
           ((and (not local)
                 (consp f)
                 (eq (car f) 'quote)
                 (symbolp (cadr f)))
            ;; (funcall 'stuff ...)
            (let ((f (cadr f)))
              (unless (symbol-function f)
                (unknown-function f))
              (mkret (gen "FGVAR" f))))
           ((and (consp f)
                 (eq (car f) 'lambda)
                 (not (cadr f)))
            (assert (not args) "Too many arguments")
            (comp-decl-seq (cddr f) env val? more?))
           (t (mkret (comp f env t t))))))

     (gen-simple-args (args n names)
       (cond
         ((null args) (gen "ARGS" n))
         ((symbolp args)
          (when (member args names)
            (error (strcat "Duplicate function argument " args)))
          (gen "ARG_" n))
         ((lambda-keyword-p (car args))
          (throw '$xargs '$xargs))
         ((member (car args) names)
          (error (strcat "Duplicate function argument " (car args))))
         ((gen-simple-args (cdr args)
                           (1+ n)
                           (cons (car args) names)))))

     (make-true-list (lst)
       (when lst
         (if (atom lst)
             (list lst)
             (cons (car lst) (make-true-list (cdr lst))))))

     (comp-extended-lambda (name args body env)
       (let* ((args (parse-lambda-list args))
              (required (getf args :required))
              (optional (getf args :optional))
              (rest (getf args :rest))
              (key (getf args :key))
              (has-key (getf args :has-key))
              (aux (getf args :aux))
              (allow-other-keys (getf args :aok))
              (names (append required
                             (map1 #'car optional)
                             (when rest (list rest))
                             (map1 #'cadar key)
                             (map1 #'car aux)))
              (index 0)
              (envcell (vector)))
         (with-declarations body
           (with-seq-output <<
             (setq env (extenv env :lex envcell))
             (labels ((newarg (name)
                        (vector-push envcell (if (member name locally-special)
                                                 (list name :var :special t)
                                                 (list name :var)))
                        (when (or (%specialp name)
                                  (member name locally-special))
                          (<< (gen "BIND" name index)))
                        (%incf index))
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
                                #(l1))))
                        (newarg name)))
               (<< (gen "XARGS"
                        (length required)
                        (length optional)
                        (if rest 1 0)
                        (when has-key (map1-vector #'caar key))
                        allow-other-keys))
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
                                  (gen "LSET" 0 index)
                                  (gen "POP")))
                            (newarg name))))
               (declare-locally-special :except names)
               (<< (with-env (comp-lambda-body name body env))))))))

     (comp-lambda (name args body env)
       (gen "FN"
            (let ((code
                   (catch '$xargs
                     (with-seq-output <<
                       (<< (gen-simple-args args 0 nil))
                       (with-declarations body
                         (let ((args (make-true-list args)))
                           (foreach-index args
                                          (lambda (name index)
                                            (when (or (%specialp name)
                                                      (member name locally-special))
                                              (<< (gen "BIND" name index)))))
                           (cond
                             (args
                              (setq env (extenv env :lex (map1-vector
                                                          (lambda (name)
                                                            (if (member name locally-special)
                                                                (list name :var :special t)
                                                                (list name :var)))
                                                          args)))
                              (declare-locally-special :except args)
                              (<< (with-env (comp-lambda-body name body env))))
                             (t
                              (declare-locally-special)
                              (<< (with-env (comp-lambda-body name body env)))))))))))
              (if (eq code '$xargs)
                  (comp-extended-lambda name args body env)
                  code))
            name))

     (comp-lambda-body (name body env)
       (if name
           (comp-block name body env t nil)
           (comp-seq body env t nil)))

     (get-bindings (bindings vars?)
       (let (names vals)
         (foreach bindings (lambda (x)
                             (if (consp x)
                                 (progn (push (if vars? (cadr x) (cdr x)) vals)
                                        (setq x (car x)))
                                 (if vars?
                                     (push nil vals)
                                     (error "Malformed LABELS/FLET/MACROLET")))
                             (unless vars?
                               (setq x (function-name x)))
                             (when (and (not vars?)
                                        (member x names))
                               (error "Duplicate name in LABELS/FLET/MACROLET"))
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
                   ((<< (with-env (comp-decl-seq body env val? nil))))))))
           (comp-decl-seq body env val? more?)))

     (comp-macrolet-function (def)
       (let ((name (car def))
             (args (cadr def))
             (body (cddr def)))
         (cond
           ((ordinary-lambda-list-p args)
            (compile (list* '%fn name args body)))
           (t
            (let ((val (gensym "macrolet")))
              (compile `(%fn ,name ,val
                             ,(%fn-destruct t args val body))))))))

     (comp-macrolet (bindings body env val? more?)
       (when bindings
         (setq env (extenv env :lex
                           (cons '%skip-count
                                 (map1-vector
                                  (lambda (def)
                                    (list (car def) :func
                                          :macro (comp-macrolet-function def)))
                                  bindings)))))
       (with-env (comp-decl-seq body env val? more?)))

     (comp-symbol-macrolet (bindings body env val? more?)
       (let ((ext (map1-vector (lambda (el)
                                 (let ((name (car el))
                                       (expansion (cadr el)))
                                   (list name :var :smac expansion)))
                               bindings)))
         (with-extenv (:lex (cons '%skip-count ext))
           (comp-decl-seq body env val? more?))))

     (comp-macroexpand (expander form env val? more?)
       (with-env (comp (let ((*whole-form* form))
                         (apply expander (cdr form)))
                       env val? more?)))

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
                                           (member name locally-special))
                                   (%incf specials)
                                   (<< (gen "BIND" name index)))))
                (setq env (extenv env :lex (map1-vector
                                            (lambda (name)
                                              (if (member name locally-special)
                                                  (list name :var :special t)
                                                  (list name :var)))
                                            names)))
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  ((<< (with-env (comp-seq body env val? nil)))))))))
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
         ((with-seq-output <<
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
                                           (member name locally-special))
                                   (%incf specials)
                                   (<< (gen "BIND" name index)))))
                (setq env (extenv env :lex (map1-vector
                                            (lambda (name)
                                              (if (member name locally-special)
                                                  (list name :var :special t)
                                                  (list name :var)))
                                            names)))
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  ((<< (with-env (comp-seq body env val? nil)))))))))))

     (comp-let* (bindings body env val? more?)
       (cond
         ((null bindings)
          (comp-decl-seq body env val? more?))
         ((with-seq-output <<
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
                                      (member name locally-special))
                              (%incf specials)
                              (gen "BIND" name index)))
                        (%incf index)
                        (vector-push envcell (if (member name locally-special)
                                                 (list name :var :special t)
                                                 (list name :var))))
                      names vals)
                (declare-locally-special :except names)
                (cond
                  (more?
                   (<< (with-env (comp-seq body env val? t))
                       (gen "UNFR" 1 specials)))
                  ((<< (with-env (comp-seq body env val? nil)))))))))))

     (comp-catch (tag body env val? more?)
       (if body
           (let ((k1 (mklabel)))
             (%seq (comp tag env t t)
                   (gen "CATCH" k1)
                   (comp-seq body env val? more?)
                   #( k1 )
                   (if val?
                       (if more?
                           (gen "UNFR" 0 1)
                           (gen "RET"))
                       (gen "POP"))))
           (%seq (when val? (gen "NIL"))
                 (unless more? (gen "RET")))))

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
                   #( k )
                   (comp-seq cleanup env nil t) ; result of cleanup code not needed
                   (gen "UPCLOSE")
                   (if more? nil (gen "RET"))))
           (comp form env val? more?)))

     (compile (exp)
       (assert (and (consp exp)
                    (member (car exp) '(%fn lambda λ)))
               "Expecting (LAMBDA (...) ...) in COMPILE")
       (%eval-bytecode (comp exp (make-environment) t nil)))

     (compile-string (str . filename)
       (let ((*current-file* (or (car filename)
                                 *current-file*))
             (reader (lisp-reader str 'EOF))
             (cache (make-hash))
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
                        (%stream-put out (%serialize-code code cache) #\Newline))))
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

(defun %with-undefined-warnings (thunk)
  (let ((*unknown-functions* nil)
        (*unknown-variables* nil))
    (unwind-protect
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
                     (prog1 (apply (symbol-function '%get-file-contents) args)
                       (rplaca *load-timing*
                               (+ (car *load-timing*)
                                  (- (get-internal-run-time) ,t-load))))))
                 (compile-string args
                   (let ((,t-comp (get-internal-run-time)))
                     (prog1 (apply (symbol-function 'compile-string) args)
                       (rplaca (cdr *load-timing*)
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
    (%load url)))

(%global! '*core-files*)
(setq *core-files*
      '("lisp/init.lisp"
        "lisp/macroexpand.lisp"
        "lisp/tiny-clos.lisp"
        "lisp/printer.lisp"
        "lisp/format.lisp"
        "lisp/ffi.lisp"
        "lisp/conditions.lisp"
        "lisp/loop.lisp"
        "ide/ide.lisp"))

;;;

EOF

(let ((*current-file* "compiler.lisp"))
  (console.print (compile-string (%js-eval "window.CURRENT_FILE"))))
