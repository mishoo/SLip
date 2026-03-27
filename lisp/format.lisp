(in-package :sl)

(export '(print-object
          print-unreadable-object
          print-object-to-string
          *print-readably*
          *print-escape*
          *print-base*
          *print-radix*
          *print-pretty*
          format formatter time print))

(defpackage :sl-format
  (:use :sl :%))

(in-package :sl-format)

(defparameter *print-readably* nil)
(defparameter *print-escape* t)
(defparameter *print-base* 10)
(defparameter *print-radix* nil)
(defparameter *print-pretty* t)

(import 'sl::defun-memoize)
(import 'sl::with-collectors)

(defparameter *format-handlers* (make-hash))

(defun print-object-to-string (obj)
  (with-output-to-string (out)
    (print-object obj out)))

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  (let ((_stream (gensym))
        (_object (gensym)))
    `(let ((,_stream ,stream)
           (,_object ,object))
       (%stream-put ,_stream "#<")
       ,@(when type
           `((%stream-put ,_stream (%:%dump (type-of ,object)))
             ,@(when (or body identity)
                 `((%stream-put ,_stream " ")))))
       ,@body
       ,@(when identity
           `(,@(when (or type body)
                 `((%stream-put ,_stream " ")))
             (%stream-put ,_stream (%:%dump ,object))))
       (%stream-put ,_stream ">"))))

(defun print-object (obj stream)
  (%stream-put stream (%dump obj)))

;; check out some fine unhygienic macros

(defmacro with-format-args (cmdargs values &body body)
  `(let* ,(mapcar (lambda (x)
                    (unless (consp x)
                      (setf x (list x)))
                    `(,(car x)
                      (let ((val (pop ,values)))
                        (case val
                          (fetch (pop args))
                          (count (length args))
                          ((nil) ,(cadr x))
                          (t val)))))
                  cmdargs)
     ,@body))

(defmacro def-format (char cmdargs &body body)
  (let ((v (gensym))
        (name (intern (strcat "INTERNAL-FORMAT-" (char-code (upcase char))))))
    `(progn
       (defun ,name (output args colmod? atmod? . ,v)
         (with-format-args ,cmdargs ,v ,@body))
       (setf (gethash ,(upcase char) *format-handlers*)
             ',name))))

(defmacro with-input (instr &body body)
  (let ((stream (gensym)))
    `(let ((,stream (%make-text-memory-input-stream ,instr)))
       (labels ((peek () (%stream-peek ,stream))
                (next () (%stream-next ,stream))
                (croak (msg)
                  (error (strcat msg
                                 ", line: " (%stream-line ,stream)
                                 ", col: " (%stream-col ,stream))))
                (read-while (pred)
                  (let ((out (%make-text-memory-output-stream)) rec)
                    (labels ((rec (ch)
                               (when (and ch (funcall pred ch))
                                 (%stream-put out (next))
                                 (rec (peek)))))
                      (rec (peek)))
                    (%get-output-stream-string out)))
                (skip-ws ()
                  (read-while (lambda (ch)
                                (member ch '(#\Space
                                             #\Newline
                                             #\Tab
                                             #\Page
                                             #\Line_Separator
                                             #\Paragraph_Separator
                                             #\NO-BREAK_SPACE)))))
                (expect (ch)
                  (unless (eq (next) ch)
                    (croak (strcat "Expecting " ch)))))
         ,@body))))

(defun-memoize %parse-format (str)
  (with-input str
    (labels
        ((read-sublist (end)
           (let looop ((ret '()))
             (case (peek)
               (#\~ (next)
                    (let ((tok (read-directive)))
                      (if (consp tok)
                          (if (eq end (car tok))
                              (if (> (length tok) 3)
                                  (error "End constructs ~~} and ~~] don't accept parameters")
                                  (cons (cdr tok) (nreverse ret)))
                              (looop (cons tok ret)))
                          (if tok
                              (looop (cons tok ret))
                              (looop ret)))))
               ((nil) (if end
                          (error (strcat "Expecting " end))
                          (nreverse ret)))
               (t (looop (cons (read-text) ret))))))

         (read-directive ()
           (let* ((params (read-params))
                  (directive (upcase (next))))
             (labels ((slurp (end modlist)
                        (let ((end (read-sublist end)))
                          `(,directive
                            ,(car params)     ;; col-mod?
                            ,(cadr params)    ;; at-mod?
                            ,@(car end)       ;; terminator col,at mods
                            ,(if modlist
                                 (funcall modlist (cdr end))
                                 (cdr end))   ;; sublist
                            ,@(cddr params)   ;; optional args
                            ))))
               (case directive
                 (#\{
                  (slurp #\} nil))
                 (#\(
                  (slurp #\) nil))
                 (#\[
                  (slurp #\] (lambda (sublist)
                               (let ((ret (list)))
                                 (let looop ((list sublist)
                                             (a (list nil)))
                                   (if list
                                       (let ((x (car list)))
                                         (cond ((and (listp x) (eq (car x) #\;))
                                                (push (nreverse a) ret)
                                                (looop (cdr list) (list (cadr x))))
                                               (t
                                                (looop (cdr list) (cons x a)))))
                                       (push (nreverse a) ret)))
                                 (nreverse ret)))))
                 (#\Newline
                  (destructuring-bind (colmod? atmod?) params
                    (cond ((eq colmod? atmod?) (skip-ws) nil)
                          (colmod? nil)
                          (atmod? (skip-ws) #\Newline))))
                 (t (cons directive params))))))

         (read-number ()
           (labels ((read-it ()
                      (or (parse-integer (read-while #'digitp))
                          (croak "Expecting an integer"))))
             (case (peek)
               (#\- (next) (- (read-it)))
               (#\+ (next) (read-it))
               (t (read-it)))))

         (read-params ()
           (let ((ret '())
                 (colmod? nil)
                 (atmod? nil))
             (tagbody
              t0 (case (upcase (peek))
                   ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-)
                    (push (read-number) ret))
                   (#\' (next)
                        (push (next) ret))
                   (#\V (next)
                        (push 'fetch ret))
                   (#\# (next)
                        (push 'count ret))
                   (#\, (push nil ret))
                   ((nil) (croak "Unterminated parameter list")))
              t1 (case (peek)
                   (#\, (next)
                        (if (or colmod? atmod?)
                            (croak "Illegal comma")
                            (go t0)))
                   (#\: (next) (setf colmod? t) (go t1))
                   (#\@ (next) (setf atmod? t) (go t1))))
             (list* colmod? atmod? (nreverse ret))))

         (read-text ()
           (read-while (lambda (ch)
                         (not (char= ch #\~))))))

      (read-sublist nil))))

(defun %exec-format (list args stream)
  (catch 'abort-format-iteration
    (dolist (x list)
      (cond ((listp x)
             (let ((handler (gethash (car x) *format-handlers*))
                   (cmdargs (cdr x)))
               (setf args (apply handler stream args cmdargs))))
            (t
             (%stream-put stream x)))))
  args)

;;; directives

;; basic newline
(def-format #\% (n)
  (if n
      (let looop ((n n))
        (when (> n 0)
          (%stream-put output #\Newline)
          (looop (1- n))))
      (%stream-put output #\Newline))
  args)

;; fresh-line
(defun %fresh-line (stream)
  (when (> (%stream-col stream) 0)
    (%stream-put stream #\Newline)))

(def-format #\& ((n 1))
  (when (> n 0)
    (%fresh-line output)
    (let looop ((n (1- n)))
      (when (> n 0)
        (%stream-put output #\Newline)
        (looop (1- n)))))
  args)

;; tilde
(def-format #\~ ((n 1))
  (let looop ((n n))
    (when (> n 0)
      (%stream-put output "~")
      (looop (1- n))))
  args)

;; general-purpose ~A and ~S
(defun %print-general (output args colmod? atmod? mincol colinc minpad padchar)
  (cond
    ((or (plusp mincol)
         (plusp minpad))
     (let ((str (print-object-to-string (car args))))
       (%stream-put output
                    (%pad-string str mincol padchar atmod? colinc minpad))))
    (t
     (print-object (car args) output)))
  (cdr args))

(def-format #\A ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (let ((*print-readably* nil)
        (*print-escape* nil))
    (%print-general output args colmod? atmod? mincol colinc minpad padchar)))

(def-format #\S ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (let ((*print-escape* t)
        (*print-readably* t))
    (%print-general output args colmod? atmod? mincol colinc minpad padchar)))

;; integers (missing ~R for now)
(defun %print-integer (output args colmod? atmod? mincol padchar commachar comma-interval base)
  (let* ((x (floor (car args)))
         (s (if (and atmod? (plusp x))
                (strcat #\+ (number-string x))
                (number-string x base))))
    (when colmod?
      (setf s (%add-commas s commachar comma-interval)))
    (%stream-put output (%pad-string (upcase s) mincol padchar t))
    (cdr args)))

(def-format #\D ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
  (%print-integer output args colmod? atmod? mincol padchar commachar comma-interval 10))

(def-format #\B ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
  (%print-integer output args colmod? atmod? mincol padchar commachar comma-interval 2))

(def-format #\O ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
  (%print-integer output args colmod? atmod? mincol padchar commachar comma-interval 8))

(def-format #\X ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
  (%print-integer output args colmod? atmod? mincol padchar commachar comma-interval 16))

;; floating-point (incomplete)
(def-format #\f ((mincol 0) declen scale overflowchar padchar)
  (let ((x (car args)))
    (setf x (if declen
                (number-fixed x declen)
                (strcat x)))
    (%stream-put output (%pad-string x mincol padchar))
    (cdr args)))

(def-format #\( (#:ignored #:ignored sublist)
  (let* ((args args)
         (str (with-output-to-string (out)
                (setf args (%exec-format sublist args out)))))
    (%stream-put output
                 (cond
                   ((and colmod? atmod?)
                    (string-upcase str))
                   (colmod?
                    (string-capitalize str))
                   (atmod?
                    (%:string-capitalize-1 str))
                   (t
                    (string-downcase str))))
    args))

(def-format #\) ()
  (error "Unmatched ~~)"))

;;; iteration

(defparameter *iteration-last-sublist* nil)

(def-format #\{ (ensure-once? #:end-at? sublist maxn)
  ;; "If str is empty, then an argument is used as str."
  (unless sublist
    (setf sublist (%parse-format (pop args))))
  ;; "if atmod, use the rest of the arguments as list"
  (let ((myargs (if atmod? args (pop args)))
        (count 0))
    (flet ((iterate (args list set-myargs)
             (cond
               ((eql count maxn)
                (throw 'max-format-iterations nil))
               ((not args)
                (if ensure-once?
                    (setf ensure-once? nil)
                    (throw 'abort-format-iteration nil))))
             (incf count)
             (dolist (x list)
               (cond
                 ((listp x)
                  (let ((handler (gethash (car x) *format-handlers*))
                        (cmdargs (cdr x)))
                    (setf args
                          (apply handler output args cmdargs))
                    (when set-myargs
                      (setf myargs args))))
                 (t
                  (%stream-put output x))))
             args))
      (catch 'max-format-iterations
        (cond
          (colmod?
           ;; iterate once for each argument sublist
           (let ((*iteration-last-sublist* nil))
             (foreach myargs
               (lambda (args)
                 (catch 'abort-format-iteration
                   (iterate args sublist nil)
                   (pop myargs)
                   (unless (cdr myargs)
                     (setf *iteration-last-sublist* t)))))))
          (t
           ;; normal case (no colmod)
           (catch 'abort-format-iteration
             (tagbody
              :loop
                (setf myargs (iterate myargs sublist t))
                (when myargs
                  (go :loop))))))))
    (if atmod? myargs args)))

(def-format #\^ ((a nil) (b nil) (c nil))
  (cond
    (colmod?
     (when *iteration-last-sublist*
       (throw 'max-format-iterations nil)))
    ((if a (if b (if c (<= a b c)
                     (eql a b))
               (zerop a))
         (not args))
     (throw 'abort-format-iteration nil)))
  args)

(def-format #\} ()
  (error "Unmatched ~~}"))

;;; conditional

(def-format #\[ (#:end-col? #:end-at? clauses n)
  (if atmod?
      (if colmod?
          (error "Both @ and : specified in conditional")
          (if (cdr clauses)
              (error "Only one clause can be specified with @")
              (if (car args)
                  (%exec-format (cdar clauses) args output)
                  (cdr args))))
      (if colmod?
          (if (/= 2 (length clauses))
              (error "Exactly two clauses expected for : specifier")
              (%exec-format (cdr (if (pop args)
                                     (cadr clauses)
                                     (car clauses)))
                            args output))
          (let (last selected)
            (let looop ((n (or n (pop args)))
                        (list clauses))
              (when list
                (cond ((zerop n) (setf selected (car list)))
                      (t (setf last (car list))
                         (looop (1- n) (cdr list))))))
            (when (and (not selected) last (car last))
              (setf selected last))
            (if selected
                (%exec-format (cdr selected) args output)
                args)))))

(def-format #\] ()
  (error "Unmatched ~~]"))

(def-format #\; ()
  (error "~~; outside ~~[...~~]"))

;; recursive
(def-format #\? ()
  (when colmod? (error ": not supported"))
  (if atmod?
      (%exec-format (%parse-format (pop args))
                    args output)
      (progn
        (%exec-format (%parse-format (pop args))
                      (pop args) output)
        args)))

(def-format #\C ()
  (cond
    (colmod?
     (%stream-put output (substr (%dump (car args)) 2)))
    (atmod?
     (%stream-put output (%dump (car args))))
    (t
     (%stream-put output (car args))))
  (cdr args))

;;; main entry point

(defun exec-format (parsed args stream)
  (labels ((doit (stream)
             (%exec-format parsed args stream)))
    (cond ((eq stream nil)
           (let ((out (%make-text-memory-output-stream)))
             (doit out)
             (%get-output-stream-string out)))
          ((eq stream t)
           (doit *standard-output*))
          (t (doit stream)))))

(defun format (stream format . args)
  (cond
    ((stringp format)
     (exec-format (%parse-format format) args stream))
    ((functionp format)
     (apply format stream args))
    (t
     (error "Unsupported format control ~A" format))))

(defun formatter (control-string)
  (let ((parsed (%parse-format control-string)))
    (lambda (stream &rest arguments)
      (exec-format parsed arguments stream))))

(defmacro time body
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-run-time)))
       (multiple-value-prog1 (progn ,@body)
         (format t "Evaluation time: ~,2Fms~%" (- (get-internal-run-time) ,t1))))))

;; XXX: temporary
(defun print (&rest args)
  (format t "~{~S~^ ~}~%" args))

(defun error args
  (%error (apply #'format nil args)))

(defun warn args
  (%warn (apply #'format nil args)))

;;; %:EOF ;; compiler macros. uncomment this to disable.

;;; I'm leaving them for now, they seem to help a little in runtime speed,
;;; although compilation time increases significantly (e.g. for the test
;;; suite).

(defun %expand-format (list args stream)
  (with-collectors (forms)
    (dolist (x list)
      (cond
        ((listp x)
         (let ((handler (gethash (car x) *format-handlers*))
               (cmdargs (cdr x)))
           (forms `(setf ,args (,handler ,stream ,args
                                ,@(mapcar #'%:quote-if-you-must cmdargs))))))
        (t
         (forms `(%stream-put ,stream ,x)))))
    forms))

(defun repeater-compiler-macro (decline char output args count)
  (cond
    ((not count)
     `(progn (%stream-put ,output ,char)
             ,args))
    ((numberp count)
     `(progn (%stream-put ,output ,(%pad-string "" count char))
             ,args))
    ((equal count ''FETCH)
     `(progn (%stream-put ,output (%pad-string "" (pop ,args) ,char))
             ,args))
    (t decline)))

(define-compiler-macro internal-format-37 ;; #\%
    (&whole decline
            output args colmod? atmod? &optional count)
  (repeater-compiler-macro decline #\Newline output args count))

(define-compiler-macro internal-format-126 ;; #\~
    (&whole decline
            output args colmod? atmod? &optional count)
  (repeater-compiler-macro decline #\~ output args count))

(define-compiler-macro internal-format-67 ;; #\C
    (output args colmod? atmod?)
  (cond
    (colmod?
     `(progn (%stream-put ,output (substr (%dump (pop ,args)) 2))
             ,args))
    (atmod?
     `(progn (%stream-put ,output (%dump (pop ,args)))
             ,args))
    (t
     `(progn (%stream-put ,output (pop ,args))
             ,args))))

(define-compiler-macro internal-format-123 ;; #\{
    (&whole decline
            output args colmod? atmod? ensure-once? #:end-at?
            &optional sublist maxn)
  (when (or (not sublist) colmod?)
    (return-from internal-format-123 decline))
  `(let (,@(when (equal maxn ''FETCH)
             (setf maxn '$maxn)
             `(($maxn (pop ,args))))
         (myargs ,(if atmod? args `(pop ,args))))
     (catch 'abort-format-iteration
       ,(cond
          (maxn
           (cond
             (ensure-once?
              `(dotimes (#:i ,maxn)
                 ,@(%expand-format (cadr sublist) 'myargs output)
                 (unless myargs
                   (return))))
             (t
              `(dotimes (#:i ,maxn)
                 (unless myargs
                   (return))
                 ,@(%expand-format (cadr sublist) 'myargs output)))))
          (t
           (cond
             (ensure-once?
              `(tagbody
                :loop
                ,@(%expand-format (cadr sublist) 'myargs output)
                  (when myargs
                    (go :loop))))
             (t
              `(tagbody
                :loop
                  (when myargs
                    ,@(%expand-format (cadr sublist) 'myargs output)
                    (go :loop))))))))
     ,(if atmod? 'myargs args)))

(define-compiler-macro internal-format-94 ;; #\^
    (&whole decline
            output args colmod? atmod? &optional a b c)
  (when (or colmod? atmod? a b c)
    (return-from internal-format-94 decline))
  `(or ,args (throw 'abort-format-iteration nil)))

(define-compiler-macro format (&whole form stream format . args)
  (cond
    ((stringp format)
     (cond
       ((eq stream t)
        (let ((vargs (gensym "args")))
          `(let ((,vargs (list ,@args)))
             ,@(%expand-format (%parse-format format) vargs '*standard-output*))))
       ((eq stream nil)
        (cond
          (%:*compiler-macro-val?*
           (let ((vstream (gensym "stream"))
                 (vargs (gensym "args")))
             `(let ((,vstream (%make-text-memory-output-stream))
                    (,vargs (list ,@args)))
                ,@(%expand-format (%parse-format format) vargs vstream)
                (%get-output-stream-string ,vstream))))
          (t
           `(progn ,@args))))
       (t
        (let ((vstream (gensym "stream"))
              (vargs (gensym "args"))
              (result (gensym "result")))
          `(let ((,vstream ,stream)
                 (,vargs (list ,@args))
                 (,result nil))
             (cond
               ((eq ,vstream t)
                (setf ,vstream *standard-output*))
               ((eq ,vstream nil)
                (setf ,vstream (%make-text-memory-output-stream)
                      ,result t)))
             ,@(%expand-format (%parse-format format) vargs vstream)
             ,@(when %:*compiler-macro-val?*
                 `((when ,result
                     (%get-output-stream-string ,vstream)))))))))
    (t form)))
