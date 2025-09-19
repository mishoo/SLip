(in-package :sl)

(export '(type-of typep deftype null symbol number integer cons function char
          hash-table package structure input-stream output-stream string vector
          satisfies mod))

(defpackage :sl-type
  (:use :sl :%)
  (:export "DEFPREDICATE" "TYPE-OF-STRUCTURE" "TYPE-OF-OBJECT"))

(in-package :sl-type)

;; Map type name -> predicate, which must be funcallable and return T if and
;; only if the type matches. Symbols are preferred because a compiler macro
;; will optimize TYPEP calls into direct calls to the predicate whenever
;; possible.
;;
;; Predicates are called with the object to test followed by the type
;; arguments. For example (typep x '(integer 0 9)) will call (integer x 0 9),
;; where `integer' is the predicate for integer (defined with defpredicate below).
(defglobal *built-in-types*
  (%:make-hash
   'null           'null
   'atom           'atom
   'cons           'consp
   'list           'listp
   'symbol         'symbolp
   'number         'numberp
   'function       'functionp
   'char           'charp
   'hash-table     'hash-table-p
   'package        'packagep
   'input-stream   '%input-stream-p
   'output-stream  '%output-stream-p
   'string         'stringp
   'vector         'vectorp))

(defglobal *ext-types* *built-in-types*)

(defmacro defpredicate (name args &body body)
  ;; This will make life a little harder during development, but we really
  ;; don't want to change built-in type defs. We also don't want to throw an
  ;; error, because calls to DEFPREDICATE will happen while bootstrapping the
  ;; class hierarchy. So if built-in type is already defined, just do nothing.
  (unless (gethash name *built-in-types*)
    (let ((intname (intern (strcat (symbol-name name) "-SL-TYPE-INTERNAL"))))
      `(progn
         (setf (fdefinition ',intname) (%:%fn ,name ,args ,@body))
         (setf (gethash ',name *ext-types*) ',intname)
         ',name))))

;; built-in complex definitions

;; Everything is of type T, and nothing is of type NIL.
(defpredicate t (obj) (declare (ignore obj)) t)
(defpredicate nil (obj) (declare (ignore obj)) nil)

(defpredicate boolean (obj)
  (or (null obj) (eq obj t)))

(defpredicate integer (obj &optional (min '*) (max '*))
  (and (integerp obj)
       (or (eq min '*) (>= obj min))
       (or (eq max '*) (<= obj max))))

(defpredicate or (obj &rest typespecs)
  (do ((ts typespecs (cdr ts)))
      ((null ts) nil)
    (when (typep obj (car ts))
      (return t))))

(defpredicate and (obj &rest typespecs)
  (do ((ts typespecs (cdr ts)))
      ((null ts) t)
    (unless (typep obj (car ts))
      (return nil))))

(defpredicate not (obj typespec)
  (not (typep obj typespec)))

(defpredicate satisfies (obj predicate)
  (funcall predicate obj))

(defpredicate member (obj &rest options)
  (if (member obj options) t))

(defpredicate eql (obj value)
  (eql obj value))

(defpredicate mod (obj n)
  (and (integerp obj)
       (plusp obj)
       (< obj n)))

;; we no longer touch *built-in-types* from this point on
(setq *ext-types* (%:hash-copy *ext-types*))

(defun %typep (object typespec)
  (cond
    ((atom typespec)
     (aif (gethash typespec *ext-types*)
          (funcall it object)
          (error "TYPEP: unknown type ~S" typespec)))
    (t
     (aif (gethash (car typespec) *ext-types*)
          (apply it object (cdr typespec))
          (error "TYPEP: unknown type ~S" typespec)))))

(setf (symbol-function 'typep) #'%typep)

(defun expand (tspec object)
  (symbol-macrolet (($thing (progn
                              (incf count)
                              '$thing)))
    (let* ((count 0)
           (code
            (let expand ((tspec tspec))
              (cond
                ((eq tspec t)
                 t)
                ((not tspec)
                 nil)
                ((symbolp tspec)
                 (let ((pred (gethash tspec *ext-types*)))
                   (if (and pred (symbolp pred))
                       `(,pred ,$thing)
                       `(%typep ,$thing ',tspec))))
                ((atom tspec)
                 (error "Unexpected atom in type spec: ~S" tspec))
                (t
                 (case (car tspec)
                   ((and)
                    `(and ,@(mapcar #'expand (cdr tspec))))
                   ((or)
                    `(or ,@(mapcar #'expand (cdr tspec))))
                   ((not)
                    `(not ,(expand (cadr tspec))))
                   ((eql)
                    `(eql ,$thing ,(cadr tspec)))
                   ((satisfies)
                    `(,(cadr tspec) ,$thing))
                   ((member)
                    `(member ,$thing ',(cdr tspec)))
                   (otherwise
                    (let ((pred (gethash (car tspec) *ext-types*)))
                      (if (and pred (symbolp pred))
                          `(,pred ,$thing ,@(cdr tspec))
                          `(%typep ,$thing ',tspec))))))))))
      (if (and (> count 1)
               (not (%:safe-atom-p object)))
          `(let (($thing ,object))
             ,code)
          `(symbol-macrolet (($thing ,object))
             ,code)))))

(define-compiler-macro typep (&whole form object typespec)
  (if (and (consp typespec)
           (eq (car typespec) 'quote))
      (expand (cadr typespec) object)
      form))

(defmacro deftype (name lambda-list &rest forms)
  (assert (symbolp name)
          "DEFTYPE: type name must be a symbol, but it's ~S" name)
  (when (gethash name *built-in-types*)
    (error "DEFTYPE: we won't change built-in type ~S" name))
  `(progn
     ,(let ((typespec `(block ,name ,@forms)))
        (cond
          ((null lambda-list)
           ;; this is constant and we can evaluate it right away
           (let ((typespec (eval typespec))
                 (obj (make-symbol "OBJ")))
             `(defpredicate ,name (,obj)
                ,(expand typespec obj))))
          (t
           ;; Parametrized type: this case is completely unoptimized and will
           ;; be slow. First, the lambda-list will always be parsed with
           ;; destructuring-bind, thanks to the default value '* :( that's
           ;; much slower than ordinary lambda lists. Second, the typespec
           ;; will be expanded every time TYPEP is called. We could do some
           ;; caching based on argument values (SBCL seems to do that), but
           ;; that would require a hash table with :test EQUAL, which we lack
           ;; at the moment.
           ;;
           ;; TODO: at least use DEFPREDICATE to intern a symbol, rather than
           ;; function object.
           `(setf (gethash ',name *ext-types*)
                  ,(%:macro-lambda name (cons '$obj lambda-list)
                                   `((typep $obj ,typespec))
                                   :default-value '*)))))
     ',name))

(defun type-of (x)
  (let ((type (%:%type-of x)))
    (case type
      (structure (type-of-structure x))
      (standard-object (type-of-object x))
      (t type))))

;; the following two will be redefined when the systems are initialized

(defun type-of-structure (x)
  (error "TYPE-OF-STRUCTURE: Structures not initialized"))

(defun type-of-object (x)
  (error "TYPE-OF-OBJECT: CLOS not initialized"))
