(defpackage :sl-print
  (:use :sl :%))

(in-package :sl-print)

(import '(sl-struct::find-structure
          sl-struct::structure-name
          sl-struct::structure-slots
          sl-struct::structure-print-object
          sl-struct::structure-print-function
          sl-struct::structure-of))

(defmacro def-print ((type &optional (name type)) &body body)
  `(defmethod print-object ((,name ,type) (out output-stream))
     (macrolet ((<< args
                  `(%stream-put out ,@args)))
       ,@body)))

(defconstant %to-string (%js-eval "function to_string(obj) { return obj + '' }"))

(def-print (t obj)
  (<< (%js-apply %to-string nil (vector obj))))

(def-print (number)
  (<< (number-string number *print-base*)))

(def-print (function)
  (cond
    ((%function-name function)
     (<< "#'")
     (print-object (%function-name function) out))
    (t
     (<< "#'#:anonymous-function"))))

(def-print (hash-table)
  (<< "#<HASH[" (length hash-table) "]")
  (let rec ((keys (%:as-list (hash-keys hash-table)))
            (vals (%:as-list (hash-values hash-table))))
    (when keys
      (<< " ")
      (print-object (car keys) out)
      (<< " ")
      (print-object (car vals) out)
      (rec (cdr keys) (cdr vals))))
  (<< ">"))

(def-print (cons)
  (when *print-pretty*
    (return-from print-object (pprint-object cons out)))
  (let ((x (car cons))
        (two (and (consp (cdr cons))
                  (not (cddr cons)))))
    (when two
      (case x
        (quote
         (<< "'")
         (return-from print-object (print-object (cadr cons) out)))
        (quasiquote
         (<< "`")
         (return-from print-object (print-object (cadr cons) out)))
        (%::qq-unquote
         (<< ",")
         (return-from print-object (print-object (cadr cons) out)))
        (%::qq-splice
         (<< ",@")
         (return-from print-object (print-object (cadr cons) out)))
        (%::function
         (<< "#'")
         (return-from print-object (print-object (cadr cons) out)))))
    (<< "(")
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
    (<< ")")))

(defun default-print-structure (obj def out)
  (macrolet ((<< args
               `(%stream-put out ,@args)))
    (let* ((name (structure-name def))
           (slots (structure-slots def)))
      (<< "#S(")
      (print-object name out)
      (foreach-index slots (lambda (slot index)
                             (<< " ")
                             (print-object (intern (strcat (getf slot :name)) "KEYWORD") out)
                             (<< " ")
                             (print-object (%struct-ref obj index) out)))
      (<< ")"))))

(def-print (structure-object structure)
  (let* ((def (structure-of structure))
         (print-object (structure-print-object def))
         (print-function (structure-print-function def)))
    (cond
      (print-object
       (funcall print-object structure out))
      (print-function
       (funcall print-function structure out *indentation*))
      (t
       (default-print-structure structure def out)))))

(def-print (structure-class structure)
  (<< "#<STRUCTURE " (sl-struct::structure-name (structure-of structure)) ">"))

(def-print (vector)
  (when *print-pretty*
    (return-from print-object (pprint-object vector out)))
  (<< "#(")
  (let looop ((i 0)
              (len (length vector)))
    (unless (zerop len)
      (when (> i 0)
        (<< " "))
      (print-object (svref vector i) out)
      (looop (+ i 1) (- len 1))))
  (<< ")"))

(def-print (string)
  (<< (if *print-escape* (%dump string) string)))

(def-print (character)
  (<< (if *print-escape* (%dump character) character)))

(def-print (regexp)
  (<< "#" (%dump regexp)))

(def-print (package)
  (<< "#<PACKAGE " (package-name package) ">"))

(def-print (null)
  (<< "NIL"))

(def-print (symbol)
  (if (eq symbol t)
      (<< "T")
      (let ((pak (symbol-package symbol)))
        (when *print-escape*
          (unless (eq pak *package*)
            (cond ((not pak)
                   (<< "#:"))
                  ((eq pak #.(find-package :keyword))
                   (<< ":"))
                  ((%symbol-accessible symbol *package*))
                  ((%find-exported-symbol symbol pak)
                   (<< (package-local-name pak) ":"))
                  ((%symbol-accessible symbol pak)
                   (<< (package-local-name pak) "::")))))
        (<< (symbol-name symbol)))))

;;;; pretty printing
;;
;; XXX: I don't like this code. But it's better than nothing, and it helps me
;; assess the depth of the rabbit hole. I'll burn it someday.

(defparameter *pretty-printers* nil)
(defparameter *indentation* 0)
(defparameter *pp-stream* nil)
(defparameter *max-col* 100)

(defun indent ()
  (%stream-put *pp-stream* (%pad-string "" *indentation* " " t)))

(defmacro with-indent (indent &body body)
  `(let ((*indentation* ,(if (numberp indent)
                             `(+ ,indent *indentation*)
                             indent)))
     ,@body))

(defun %def-pretty-print (symbol func)
  (cond
    ((symbolp symbol)
     (setf (getf *pretty-printers* symbol) func))
    ((consp symbol)
     (%def-pretty-print (car symbol) func)
     (when (cdr symbol)
       (%def-pretty-print (cdr symbol) func)))))

(defmacro defun<< (name args &body body)
  `(defun ,name ,args
     (macrolet ((<< args
                  `(%stream-put *pp-stream* ,@args))
                (with-parens body
                  `(progn
                     (<< "(")
                     ,@body
                     (<< ")"))))
       ,@body)))

(defmacro def-pretty-print (symbol (&rest args) &rest body)
  (let ((fname (intern (strcat "PRETTY-PRINT-" (if (consp symbol)
                                                   (car symbol)
                                                   symbol)))))
    `(progn
       (defun<< ,fname (symbol ,@args)
         ,@body)
       (%def-pretty-print ',symbol (function ,fname)))))

(defun<< %pp-list (lst &optional funcall?)
  (with-parens
    (with-indent (%stream-col *pp-stream*)
      (%pp-object (car lst))
      (when (cdr lst)
        (<< " ")
        (let ((first t))
          (with-indent (if funcall?
                           (let ((ccol (%stream-col *pp-stream*)))
                             (if (or (< 12 (- ccol *indentation*))
                                     (and (symbolp (car lst))
                                          (regexp-test #/^(?:with-|def)/i (symbol-name (car lst)))))
                                 (progn
                                   (setf first 'newline)
                                   (+ 2 *indentation*))
                                 ccol))
                           *indentation*)
            (let rec ((lst (cdr lst)))
              (when lst
                (cond
                  ((not (consp lst))
                   (unless first (<< " "))
                   (<< ". ")
                   (%pp-object lst))
                  (t (let ((form (car lst)))
                       (cond
                         ((or (eq first 'newline)
                              (>= (%stream-col *pp-stream*) *max-col*)
                              (and (not first) (consp form)
                                   (not (and (member (car form) '(quote quasiquote function))
                                             (symbolp (cadr form))))))
                          (setf first nil)
                          (<< #\Newline)
                          (indent))
                         (t
                          (if first
                              (setf first nil)
                              (<< " "))))
                       (%pp-object form)
                       (rec (cdr lst)))))))))))))

(defun<< %pp-object (thing)
  (cond
    ((consp thing)
     (let ((printer (getf *pretty-printers* (car thing))))
       (if printer
           (apply printer thing)
           (%pp-list thing t))))
    ((symbolp thing)
     (<< (%pp-symbol thing)))
    (t
     (let ((*print-pretty* nil))
       (print-object thing *pp-stream*)))))

(defun<< %pp-body (forms &optional printer)
  (let rec ((forms forms)
            (first t))
    (when forms
      (cond
        ((not (consp forms))
         (<< " . ")
         (%pp-object forms))
        (t (let ((form (car forms)))
             (unless first
               (<< #\Newline)
               (indent))
             (if printer
                 (funcall printer form)
                 (%pp-object form))
             (rec (cdr forms) nil)))))))

(defun<< %pp-body-indent (forms &optional printer)
  (when forms
    (<< #\Newline)
    (with-indent 2
      (indent)
      (%pp-body forms printer))))

(defun<< %pp-funargs (args)
  (cond
    ((consp args)
     (%pp-list args))
    ((null args)
     (<< "()"))
    ((%pp-object args))))

(defun %pp-symbol (sym)
  (if (symbolp sym)
      (print-object-to-string sym)
      (with-output-to-string (out)
        (%pp-object sym))))

(def-pretty-print quote (&rest forms)
  (<< "'")
  (cond
    ((consp (car forms))
     (%pp-list (car forms)))
    (t
     (%pp-object (car forms)))))

(def-pretty-print quasiquote (&rest forms)
  (<< "`")
  (%pp-object (car forms)))

(def-pretty-print %::qq-unquote (&rest forms)
  (<< ",")
  (%pp-object (car forms)))

(def-pretty-print %::qq-splice (&rest forms)
  (<< ",@")
  (%pp-object (car forms)))

(def-pretty-print function (&rest forms)
  (<< "#'")
  (%pp-object (car forms)))

(def-pretty-print progn (&rest forms)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol))
      (%pp-body-indent forms))))

(def-pretty-print (lambda λ) (args &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (%pp-funargs args)
      (%pp-body-indent body))))

(def-pretty-print (defun defmacro defmethod %::%fn) (name args &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (%pp-object name)
      (<< " ")
      (%pp-funargs args)
      (%pp-body-indent body))))

(def-pretty-print (let let*) (bindings &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (when (symbolp bindings)
        (<< (%pp-symbol bindings) " ")
        (setf bindings (pop body)))
      (with-parens
        (with-indent (%stream-col *pp-stream*)
          (%pp-body bindings
                    (lambda (binding)
                      (cond
                        ((symbolp binding)
                         (<< (%pp-symbol binding)))
                        ((and (consp binding)
                              (= 2 (length binding)))
                         (destructuring-bind (name val) binding
                           (case name
                             (%:qq-unquote
                              (<< ",")
                              (%pp-object val))
                             (%:qq-splice
                              (<< ",@")
                              (%pp-object val))
                             (t
                              (with-parens
                                (<< (%pp-symbol name) " ")
                                (%pp-object val))))))
                        (t
                         (%pp-funargs binding)))))))
      (%pp-body-indent body))))

(def-pretty-print (flet labels macrolet symbol-macrolet) (bindings &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (with-parens
        (with-indent (%stream-col *pp-stream*)
          (%pp-body bindings
                    (lambda (binding)
                      (destructuring-bind (name args &rest body) binding
                        (with-parens
                          (<< (%pp-symbol name) " ")
                          (cond
                            ((eq 'symbol-macrolet symbol)
                             (%pp-object args))
                            (t
                             (%pp-funargs args)
                             (%pp-body-indent body)))))))))
      (%pp-body-indent body))))

(def-pretty-print if (condition &optional
                                (then nil then-supplied-p)
                                (else nil else-supplied-p))
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (with-indent (%stream-col *pp-stream*)
        (%pp-object condition)
        (when then-supplied-p
          (<< #\Newline)
          (indent)
          (%pp-object then)
          (when else-supplied-p
            (<< #\Newline)
            (indent)
            (%pp-object else)))))))

(def-pretty-print (cond case ecase typecase etypecase) (&rest cases)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol))
      (%pp-body-indent cases
                       (lambda (cs)
                         (with-parens
                           (with-indent (%stream-col *pp-stream*)
                             (%pp-body cs))))))))

(def-pretty-print tagbody (&rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol))
      (%pp-body-indent body (lambda (form)
                              (when (consp form)
                                (<< "  "))
                              (%pp-object form))))))

(def-pretty-print (unwind-protect prog1) (form &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol))
      (with-indent 2 (%pp-body-indent (list form)))
      (%pp-body-indent body))))

(def-pretty-print (multiple-value-bind destructuring-bind) (names values &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (%pp-funargs names)
      (with-indent 2 (%pp-body-indent (list values)))
      (%pp-body-indent body))))

(def-pretty-print (when unless block catch) (thing &rest body)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (%pp-object thing)
      (%pp-body-indent body))))

(def-pretty-print (setq setf psetq psetf) (&rest exps)
  (with-indent (%stream-col *pp-stream*)
    (with-parens
      (<< (%pp-symbol symbol) " ")
      (with-indent (%stream-col *pp-stream*)
        (let rec ((exps exps)
                  (first t))
          (when exps
            (unless first
              (<< #\Newline)
              (indent))
            (%pp-object (car exps))
            (when (cdr exps)
              (<< " ")
              (%pp-object (cadr exps)))
            (rec (cddr exps) nil)))))))

(defgeneric pprint-object (object stream))

(defmethod pprint-object (object (output output-stream))
  (let ((*print-pretty* nil))
    (print-object object output)))

(defmethod pprint-object ((object cons) (output output-stream))
  (if (eq *pp-stream* output)
      (%pp-object object)
      (let ((*pp-stream* output))
        (%pp-object object))))

(defun<< %pp-array (array)
  (cond
    ((zerop (length array))
     (<< "#()"))
    (t
     (<< "#")
     (%pp-list (%:as-list array)))))

(defmethod pprint-object ((object vector) (output output-stream))
  (if (eq *pp-stream* output)
      (%pp-array object)
      (let ((*pp-stream* output))
        (%pp-array object))))
