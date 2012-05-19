(in-package :sl)

(export '(print-object
          *print-readably*
          *print-escape*
          *print-base*
          *print-radix*))

(defgeneric print-object)

(defparameter *print-readably* nil)
(defparameter *print-escape* t)
(defparameter *print-base* 10)
(defparameter *print-radix* nil)

(defmacro def-print ((type) &body body)
  `(defmethod print-object ((,type ,type) (out output-stream))
     (macrolet ((<< args
                  `(%stream-put out ,@args)))
       ,@body)))

(let ((%to-string (%js-eval "function to_string(obj) { return obj + '' }")))
  (def-print (unknown-class)
    (<< "<UNKNOWN-CLASS " (%js-apply %to-string nil #(unknown-class)) ">")))

(def-print (object)
  (<< "<OBJECT")
  (let* ((class (class-of object))
         (name (class-name class)))
    (when name
      (<< " " name)))
  (<< ">"))

(def-print (primitive)
  (<< "<PRIMITIVE")
  (let* ((class (class-of primitive))
         (name (class-name class)))
    (when name
      (<< " " name)))
  (<< ">"))

(def-print (number)
  (<< (number-string number *print-base*)))

(def-print (function)
  (<< "<FUNCTION")
  (when (%function-name function)
    (<< " " (%function-name function)))
  (<< ">"))

(def-print (class)
  (<< "<CLASS")
  (when (class-name class)
    (<< " " (class-name class)))
  (<< ">"))

(def-print (cons)
  (let ((x (car cons))
        (two (and (consp (cdr cons))
                  (not (cddr cons)))))
    (cond ((and two (eq x 'quote))
           (<< "'")
           (print-object (cadr cons) out))
          ((and two (eq x 'quasiquote))
           (<< "`")
           (print-object (cadr cons) out))
          ((and two (eq x '%::qq-unquote))
           (<< ",")
           (print-object (cadr cons) out))
          ((and two (eq x '%::qq-splice))
           (<< ",@")
           (print-object (cadr cons) out))
          ((and two (eq x '%::function))
           (<< "#'")
           (print-object (cadr cons) out))
          (t (<< "(")
             (let print-list ((list cons))
               (when list
                 (cond
                   ((consp list)
                    (print-object (car list) out)
                    (when (cdr list)
                      (<< " ")
                      (print-list (cdr list))))
                   (t
                    (<< ". ")
                    (print-object list out)))))
             (<< ")")))))

(def-print (vector)
  (<< "#(")
  (let looop ((i 0)
              (len (length vector)))
    (unless (zerop len)
      (when (> i 0)
        (<< " "))
      (print-object (vector-ref vector i) out)
      (looop (+ i 1) (- len 1))))
  (<< ")"))

(def-print (string)
  (<< (if *print-escape* (%dump string) string)))

(def-print (char)
  (<< (if *print-escape* (%dump char) char)))

(def-print (regexp)
  (<< "<REGEXP #" (%dump regexp) ">"))

(def-print (package)
  (<< "<PACKAGE " (%package-name package) ">"))

(def-print (null)
  (<< "NIL"))

(let ((pak-keyword (%find-package :keyword)))
  (def-print (symbol)
    (if (eq symbol t)
        (<< "T")
        (let ((pak (%symbol-package symbol)))
          (cond ((not pak)
                 (<< "#:"))
                ((eq pak pak-keyword)
                 (<< ":"))
                ((not (%symbol-accessible symbol *package*))
                 (<< (%package-name pak) ":")))
          (<< (%symbol-name symbol))))))

(def-efun print-object-to-string (obj)
  (let ((out (%make-output-stream)))
    (print-object obj out)
    (%stream-get out)))
