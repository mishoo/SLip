(in-package :sl)

(export '(format))

(defglobal *format-handlers* (make-hash))

(defparameter *format-current-args* nil)

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
                          (nil ,(cadr x))
                          (t val)))))
                  cmdargs)
     ,@body))

(defmacro def-format (char cmdargs &body body)
  (let ((v (gensym)))
    `(hash-add *format-handlers* ,(upcase char)
               (lambda (output args colmod? atmod? . ,v)
                 (with-format-args ,cmdargs ,v ,@body)))))

(defmacro with-input (instr &body body)
  (let ((stream (gensym)))
    `(let ((,stream (%make-input-stream ,instr)))
       (labels ((peek () (%stream-peek ,stream))
                (next () (%stream-next ,stream))
                (croak (msg)
                  (error (strcat msg
                                 ", line: " (%stream-line ,stream)
                                 ", col: " (%stream-col ,stream))))
                (read-while (pred)
                  (let ((out (%make-output-stream)) rec)
                    (labels ((rec (ch)
                               (when (and ch (funcall pred ch))
                                 (%stream-put out (next))
                                 (rec (peek)))))
                      (rec (peek)))
                    (%stream-get out)))
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

(defun %parse-format (str)
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
               (nil (if end
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
                   (nil (croak "Unterminated parameter list")))
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
  (let looop ((list list))
    (when list
      (let ((x (car list)))
        (cond ((listp x)
               (let ((handler (hash-get *format-handlers* (car x)))
                     (cmdargs (cdr x)))
                 (setf args (apply handler stream args cmdargs))))
              (t
               (%stream-put stream x)))
        (looop (cdr list)))))
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
(labels ((fresh-line (stream)
           (when (> (%stream-col stream) 0)
             (%stream-put stream #\Newline))))
  (def-format #\& ((n 1))
    (when (> n 0)
      (fresh-line output)
      (let looop ((n (1- n)))
        (when (> n 0)
          (%stream-put output #\Newline)
          (looop (1- n)))))
    args))

;; tilde
(def-format #\~ ((n 1))
  (let looop ((n n))
    (when (> n 0)
      (%stream-put output "~")
      (looop (1- n))))
  args)

;; general-purpose ~A and ~S
(flet ((print (output args colmod? atmod? mincol colinc minpad padchar)
         (cond
           ((or (plusp mincol)
                (plusp minpad))
            (let ((str (print-object-to-string (car args))))
              (%stream-put output
                           (%pad-string str mincol padchar atmod? colinc minpad))))
           (t
            (print-object (car args) output)))
         (cdr args)))

  (def-format #\A ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
    (let ((*print-readably* nil)
          (*print-escape* nil))
      (print output args colmod? atmod? mincol colinc minpad padchar)))

  (def-format #\S ((mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
    (let ((*print-escape* t))
      (print output args colmod? atmod? mincol colinc minpad padchar))))

;; integers (missing ~R for now)
(flet ((print (output args colmod? atmod? mincol padchar commachar comma-interval base)
         (let* ((x (floor (car args)))
                (s (if (and atmod? (plusp x))
                       (strcat #\+ (number-string x))
                       (number-string x base))))
           (when colmod?
             (setf s (%add-commas s commachar comma-interval)))
           (%stream-put output (%pad-string (upcase s) mincol padchar t))
           (cdr args))))
  (def-format #\D ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
    (print output args colmod? atmod? mincol padchar commachar comma-interval 10))
  (def-format #\B ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
    (print output args colmod? atmod? mincol padchar commachar comma-interval 2))
  (def-format #\O ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
    (print output args colmod? atmod? mincol padchar commachar comma-interval 8))
  (def-format #\X ((mincol 0) (padchar #\Space) (commachar #\,) (comma-interval 3))
    (print output args colmod? atmod? mincol padchar commachar comma-interval 16)))

;; floating-point (incomplete)
(def-format #\f ((mincol 0) declen scale overflowchar padchar)
  (let ((x (car args)))
    (setf x (if declen
                (number-fixed x declen)
                (strcat x)))
    (%stream-put output (%pad-string x mincol padchar))
    (cdr args)))

;;; iteration

(def-format #\{ (ensure-once? #:end-at? sublist maxn)
  ;; "If str is empty, then an argument is used as str."
  (unless sublist
    (setf sublist (%parse-format (car args))
          args (cdr args)))
  ;; "if atmod, use the rest of the arguments as list"
  (let ((*format-current-args* (if atmod? args (car args))))
    (catch 'abort-format-iteration
      (labels ((iterate (list i)
                 (when (eq i maxn)
                   (throw 'abort-format-iteration))
                 (if list
                     (let ((x (car list)))
                       (if (listp x)
                           (let ((handler (hash-get *format-handlers* (car x)))
                                 (cmdargs (cdr x)))
                             (setf *format-current-args*
                                   (apply handler output *format-current-args* cmdargs)))
                           (%stream-put output x))
                       (iterate (cdr list) i))
                     (unless colmod?
                       (when *format-current-args*
                         (iterate sublist (1+ i)))))))
        (if colmod?
            ;; iterate once for each argument sublist
            (foreach *format-current-args*
              (lambda (*format-current-args*)
                (iterate sublist 0)))
            ;; normal case (no colmod)
            (iterate sublist 0)))))
  (if atmod? nil (cdr args)))

(def-format #\^ ()
  (or *format-current-args*
      (throw 'abort-format-iteration)))

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

;;; main entry point

(defun format (stream format . args)
  (labels ((doit (stream)
             (%exec-format (%parse-format format) args stream)))
    (cond ((eq stream nil)
           (let ((out (%make-output-stream)))
             (doit out)
             (%stream-get out)))
          ((eq stream t)
           (doit *standard-output*))
          (t (doit stream)))))

(def-emac time body
  (let ((t1 (gensym)))
    `(let ((,t1 (%get-time)))
       (prog1 (progn ,@body)
         (format t "Evaluation time: ~Ams~%" (- (%get-time) ,t1))))))
