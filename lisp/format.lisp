(in-package :ss)

(defglobal *format-handlers* (make-hash))

(defmacro set-defaults defs
  `(progn
     ,@(let rec ((defs defs))
         (when defs
           (cons `(case ,(car defs)
                    (fetch (setf ,(car defs) (car args)
                                 args (cdr args)))
                    (count (setf ,(car defs) (length args)))
                    (nil (setf ,(car defs) ,(cadr defs))))
                 (rec (cddr defs)))))))

;; check out some fine unhygienic macros

(defmacro def-format (char cmdargs &body body)
  (let ((v (gensym)))
    `(hash-add *format-handlers* ,(upcase char)
               (lambda (output args colmod? atmod? . ,v)
                 ;; XXX: having a dumber destructuring-bind here
                 ;;      would help improve speed
                 (destructuring-bind (&optional
                                      ,@(mapcar (lambda (x)
                                                  (if (consp x) (car x) x))
                                                cmdargs)) ,v
                   (set-defaults
                    ,@(%apply #'append (collect-if #'consp cmdargs)))
                   ,@body)))))

(defmacro with-input (instr &body body)
  (let ((stream (gensym)))
    `(let ((,stream (%make-input-stream ,instr)))
       (labels ((peek () (%stream-peek ,stream))
                (next () (%stream-next ,stream))
                (croak (msg)
                  (error (strcat msg
                                 ", line: " (%stream-line ,stream)
                                 ", col: " (%stream-col ,stream))))
                (skip (ch)
                  (unless (eq (next) ch)
                    (croak (strcat "Expecting " ch))))
                (read-while (pred)
                  (let ((out (%make-output-stream)) rec)
                    (labels ((rec (ch)
                               (when (and ch (funcall pred ch))
                                 (%stream-put out (next))
                                 (rec (peek)))))
                      (rec (peek)))
                    (%stream-get out))))
         ,@body))))

(defun %wrap-lists (fmt)
  (let looop ((list fmt) end)
    (if list
        (let ((x (car list)))
          (if (listp x)
              (if (eq end (car x))
                  (prog1 (cdr list)
                    (setf (cdr list) nil))
                  (case (car x)
                    (#\{ (looop (setf (cdr (last x)) (cdr list)
                                      (cdr list) (looop (cdr list) #\}))
                                end))
                    (#\[ (looop (setf (cdr (last x)) (cdr list)
                                      (cdr list) (looop (cdr list) #\]))
                                end))))
              (looop (cdr list) end)))
        (if end
            (error (strcat "Expecting " end)))))
  fmt)

(defun %parse-format (str)
  (with-input str
    (labels
        ((read-directive ()
           (skip #\~)
           (let ((params (read-params)))
             (cons (upcase (next)) params)))

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

      (%wrap-lists
       (nreverse
           (let looop ((ret '()))
             (case (peek)
               (#\~ (looop (cons (read-directive) ret)))
               (nil ret)
               (t (looop (cons (read-text) ret))))))))))

(defun %exec-format (list args stream)
  (let looop ((list list))
    (when list
      (let ((x (car list)))
        (cond ((stringp x)
               (%stream-put stream x))
              (t
               (let ((handler (hash-get *format-handlers* (car x)))
                     (cmdargs (cdr x)))
                 (setf args (%apply handler (list* stream args cmdargs))))))
        (looop (cdr list))))))

;;; directives

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

;;; main entry point

(defun format (stream format . args)
  (labels ((doit (stream)
             (%exec-format (%parse-format format) args stream)))
    (cond ((eq stream nil)
           (let ((out (%make-output-stream)))
             (doit out)
             (%stream-get out)))
          ((eq stream t)
           (error "Standard output not yet supported!"))
          (t (doit stream)))))