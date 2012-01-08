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

(defun mapcar (func list)
  (when list
    (cons (func (car list)) (mapcar func (cdr list)))))

(defun foreach (list func)
  (when list
    (func (car list))
    (foreach (cdr list) func)))

(defmacro let (defs . body)
  `((lambda ,(mapcar (lambda (x)
                       (if (listp x)
                           (car x)
                           x)) defs)
      ,@body)
    ,@(mapcar (lambda (x)
                (if (listp x)
                    (cadr x))) defs)))

(defmacro prog1 (exp . body)
  (let ((ret (gensym)))
    `(let ((,ret ,exp))
       ,@body
       ,ret)))

(defmacro prog2 (exp1 exp2 . body)
  `(progn
     ,exp1
     (prog1 ,exp2 ,@body)))

(defmacro let* (defs . body)
  (if defs
      `(let (,(car defs))
         (let* ,(cdr defs)
           ,@body))
      `(progn ,@body)))

(defmacro labels (defs . body)
  `(let ,(mapcar (lambda (x) (car x)) defs)
     ,@(mapcar (lambda (x)
                 `(set! ,(car x) (%set-function-name
                                  (lambda ,(cadr x) ,@(cddr x))
                                  ',(car x)))) defs)
     ,@body))

(defmacro flet (defs . body)
  `(let ,(mapcar (lambda (x)
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

(defun member (item list)
  (if list
      (if (eq item (car list))
          list
          (member item (cdr list)))))

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
                                       (or (eq (caar cases) 'otherwise)
                                           (eq (caar cases) 't)))
                                  `(progn ,@(cdar cases))
                                  `(if (eq ,vexpr ',(caar cases))
                                       (progn ,@(cdar cases))
                                       ,(recur (cdr cases))))))))
                (recur cases)))))

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

(defmacro incf (var)
  `(set! ,var (+ ,var 1)))

(defmacro decf (var)
  `(set! ,var (- ,var 1)))

(set! *amb-fail* (lambda (arg)
                   (clog "TOTAL FAILURE")))

(defmacro amb alternatives
  (if alternatives
      `(let ((+prev-amb-fail *amb-fail*))
         (with-cc +sk
           ,@(mapcar (lambda (alt)
                       `(with-cc +fk
                          (set! *amb-fail* +fk)
                          (+sk ,alt)))
                     alternatives)
           (set! *amb-fail* +prev-amb-fail)
           (+prev-amb-fail nil)))
      `(*amb-fail* nil)))

(defun macroexpand (form)
  (if (and (consp form)
           (symbolp (car form))
           (%macrop (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(defun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (mapcar macroexpand-all form))
      form))

;;;;

(defmacro while (cond . body)
  (let ((rec (gensym "while")))
    `(labels ((,rec ()
                (when ,cond
                  ,@body
                  (,rec))))
       (,rec))))

(defun input-stream (text eof)
  (let ((i 0)
        (len (length text))
        (line 1)
        (col 0))
    (lambda (what)
      (case what
        (peek (if (< i len)
                  (elt text i)
                  eof))
        (next (if (< i len)
                  (let ((ch (elt text i)))
                    (case ch
                      (#\Newline (set! line (+ line 1))
                                 (set! col 0))
                      (otherwise (set! col (+ col 1))))
                    (set! i (+ i 1))
                    ch)
                  eof))
        (line line)
        (col col)
        (pos i)))))

(defun output-stream ()
  (let ((text ""))
    (lambda (what . rest)
      (case what
        (put (foreach rest (lambda (str)
                             (set! text (strcat text str)))))
        (get text)))))

(defun lisp-reader (text eof)
  (let ((input (input-stream text eof))
        (in-qq 0))
    (labels
        ((peek ()
           (input 'peek))

         (next ()
           (input 'next))

         (read-while (pred)
           (let ((out (output-stream)))
             (while (and (not (eq (peek) eof))
                         (pred (peek)))
               (out 'put (next)))
             (out 'get)))

         (croak (msg)
           (%error (strcat msg ", line: " (input 'line) ", col: " (input 'col))))

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
           (let ((escaped nil)
                 (out (output-stream)))
             (with-cc return            ; XXX: this seems wasteful
               (while (not (eq (peek) eof))
                 (let ((ch (next)))
                   (cond
                     (escaped
                      (out 'put ch)
                      (set! escaped nil))

                     ((eq ch #\\)
                      (if inces (out 'put #\\))
                      (set! escaped t))

                     ((eq ch end)
                      (return (out 'get)))

                     (t (out 'put ch))))))))

         (read-string ()
           (read-escaped #\" #\" nil))

         (read-regexp ()
           (let ((str (read-escaped #\/ #\/ t))
                 (mods (downcase (read-while (lambda (ch)
                                               (case (downcase ch)
                                                 ((#\g #\m #\i #\y) t)))))))
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
           (if (eq (peek) #\()
               (prog2
                   (incf in-qq)
                   (list 'quasiquote (read-token))
                 (decf in-qq))
               `(quote ,(read-token))))

         (read-comma ()
           (when (zerop in-qq) (croak "Comma outside quasiquote"))
           (skip #\,)
           (skip-ws)
           (prog2
               (decf in-qq)
               (if (eq (peek) #\@)
                   (progn (next)
                          (list 'qq-splice (read-token)))
                   (list 'qq-unquote (read-token)))
             (incf in-qq)))

         (read-list ()
           (let ((ret nil)
                 (p nil))
             (labels ((rec ()
                        (skip-ws)
                        (when (eq (peek) eof)
                          (croak "Unterminated list"))
                        (case (peek)
                          (#\) ret)
                          (#\; (skip-comment) (rec))
                          (#\. (next)
                               (rplacd p (read-token))
                               (skip-ws)
                               ret)
                          (otherwise (let ((cell (cons (read-token) nil)))
                                       (if ret
                                           (rplacd p cell)
                                           (set! ret cell))
                                       (set! p cell))
                                     (rec)))))
               (skip #\()
               (prog1
                   (rec)
                 (skip #\))))))

         (read-token ()
           (skip-ws)
           (if (eq (peek) eof)
               eof
               (case (peek)
                 (#\; (skip-comment) (read-token))
                 (#\" (read-string))
                 (#\( (read-list))
                 (#\# (read-sharp))
                 (#\` (read-quasiquote))
                 (#\, (read-comma))
                 (#\' (read-quote))
                 (otherwise (read-symbol))))))

      read-token)))

(let ((reader (lisp-reader
               (%js-eval "window.CURRENT_FILE")
               ;;"(a b c)"
               ;;"#\\Newline mak"
               'EOF)))
  (let ((tok))
    (while (not (eq 'EOF (set! tok (reader))))
      (clog tok)
      )))
