(in-package :sl)

(export '(type-of typep deftype typecase etypecase
          fixnum float input-stream output-stream satisfies mod
          string-designator))

(defpackage :sl-type
  (:use :sl :%)
  (:export #:defpredicate #:def-val-predicate
           #:type-of-structure #:type-of-object))

(in-package :sl-type)

(defun integer-predicate (obj &optional (min '*) (max '*))
  (and (integerp obj)
       (if (eq min '*) t (>= obj min))
       (if (eq max '*) t (<= obj max))))

(define-compiler-macro integer-predicate (&whole form obj &optional (min ''*) (max ''*))
  (cond
    ((and (equal min ''*) (equal max ''*))
     `(integerp ,obj))
    ((%:safe-atom-p obj)
     `(and (integerp ,obj)
           ,@(unless (equal min ''*)
               `((<= ,min ,obj)))
           ,@(unless (equal max ''*)
               `((>= ,max ,obj)))))
    (t form)))

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
   'atom           'atom
   'list           'listp
   'symbol         'symbolp
   'number         'numberp
   'integer        'integer-predicate
   'function       'functionp
   'char           'charp
   'character      'charp
   'hash-table     'hash-table-p
   'package        'packagep
   'input-stream   '%input-stream-p
   'output-stream  '%output-stream-p
   'string         'stringp
   'vector         'vectorp
   'regexp         'regexpp))

(defglobal *ext-types* *built-in-types*)

(defglobal *complex-types* (make-hash-table :test #'equal))
(defglobal *complex-type-preds* (make-hash-table :test #'equal))

(defmacro defpredicate (name args &body body)
  ;; This will make life a little harder during development, but we really
  ;; don't want to change built-in type defs. We also don't want to throw an
  ;; error, because calls to DEFPREDICATE will happen while bootstrapping the
  ;; class hierarchy. So if built-in type is already defined, just do nothing.
  (unless (gethash name *built-in-types*)
    (%:maybe-xref-info name :type)
    (let ((intname (intern (strcat (symbol-name name) "-SL-TYPE-INTERNAL")
                           (symbol-package name))))
      `(progn
         (setf (fdefinition ',intname) (%:%fn ,name ,args ,@body))
         (setf (gethash ',name *ext-types*) ',intname)))))

(defmacro defcomplex (name lambda-list form)
  (let ((args (make-symbol "ARGS"))
        (exp (make-symbol "EXP")))
    (unless (gethash name *built-in-types*)
      `(setf (gethash ',name *complex-types*)
             (lambda (,args)
               (or (gethash (cons ',name ,args) *complex-types*)
                   (setf (gethash (cons ',name ,args) *complex-types*)
                         ,(%:%fn-destruct nil lambda-list args
                                          `(,form)
                                          :default-value ''*))))))))

(defun def-val-predicate (val predicate)
  (setf (gethash val *ext-types*) predicate))

;; built-in complex definitions

;; Everything is of type T, and nothing is of type NIL.
(defpredicate t (obj) (declare (ignore obj)) t)
(defpredicate nil (obj) (declare (ignore obj)) nil)

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

(defpredicate %cons (obj &optional (left '*) (right '*))
  (and (consp obj)
       (if (eq left '*) t (typep (car obj) left))
       (if (eq right '*) t (typep (cdr obj) right))))

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

(defun %expand (tspec object)
  (let expand ((tspec tspec))
    (cond
      ((eq tspec t)
       t)
      ((not tspec)
       nil)
      ((symbolp tspec)
       (aif (gethash tspec *complex-types*)
            (expand (funcall it nil))
            (let ((pred (gethash tspec *ext-types*)))
              (if (and pred (symbolp pred))
                  `(,pred ,object)
                  `(%typep ,object ',tspec)))))
      ((atom tspec)
       (error "Unexpected atom in type spec: ~S" tspec))
      (t
       (case (car tspec)
         ((and)
          (cond
            ((null (cdr tspec)) 'nil)
            ((null (cddr tspec))
             (expand (cadr tspec)))
            (t
             `(when ,(expand (cadr tspec))
                ,(expand `(and ,@(cddr tspec)))))))
         ((or)
          (cond
            ((null (cdr tspec)) 't)
            ((null (cddr tspec))
             (expand (cadr tspec)))
            (t
             `(if ,(expand (cadr tspec)) 't
                  ,(expand `(or ,@(cddr tspec)))))))
         ((not)
          `(not ,(expand (cadr tspec))))
         ((eql)
          `(eql ,object ,(cadr tspec)))
         ((satisfies)
          `(,(cadr tspec) ,object))
         ((member)
          `(member ,object ',(cdr tspec)))
         (t
          (aif (gethash (car tspec) *complex-types*)
               (expand (funcall it (cdr tspec)))
               (let ((pred (gethash (car tspec) *ext-types*)))
                 (if (and pred (symbolp pred))
                     `(,pred ,object ,@(mapcar (lambda (x) `',x)
                                               (cdr tspec)))
                     `(%typep ,object ',tspec))))))))))

(defun expand (tspec object)
  (if (%:safe-atom-p object)
      (%expand tspec object)
      `(let (($thing ,object))
         ,(%expand tspec '$thing))))

(define-compiler-macro typep (&whole form object typespec)
  (cond
    ((eq typespec t) 't)
    ((eq typespec nil) 'nil)
    ((and (consp typespec)
          (eq (car typespec) 'quote))
     (expand (cadr typespec) object))
    (t
     form)))

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
             `(progn
                ;; still, making it available through `defcomplex' makes it
                ;; eligible for further expansion and inlining at compile time,
                ;; which should be a win.
                (defcomplex ,name nil ',typespec)
                (defpredicate ,name (,obj)
                  ,(expand typespec obj)))))
          (t
           ;; Parametrized type; expansion is cached by name and arguments in
           ;; *complex-types*, and the predicate in *complex-type-preds*. The
           ;; predicates should be needed only when the typespec is not fully
           ;; known at compile-time, e.g. (typep foo `(unsigned-byte ,size));
           ;; otherwise the expansion will be inlined via `expand' (above).
           (let ((obj (make-symbol "OBJ"))
                 (args (make-symbol "ARGS"))
                 (exp (make-symbol "EXP")))
             `(progn
                (defcomplex ,name ,lambda-list ,typespec)
                (defpredicate ,name (,obj &rest ,args)
                  (funcall
                   (or (gethash (cons ',name ,args) *complex-type-preds*)
                       (setf (gethash (cons ',name ,args) *complex-type-preds*)
                             (let ((,exp (funcall (gethash ',name *complex-types*) ,args)))
                               (compile `(lambda (,',obj)
                                           ,(expand ,exp ',obj))))))
                   ,obj)))))))
     ',name))

(deftype null ()
  `(eql nil))

(deftype boolean ()
  `(or (eql t) (eql nil)))

(deftype mod (n)
  `(integer 0 ,(1- n)))

(deftype fixnum ()
  `(integer ,most-negative-fixnum ,most-positive-fixnum))

(deftype float ()
  `(and number (not integer)))

(deftype unsigned-byte (&optional size)
  (cond
    ((eq size '*)
     '(integer 0 *))
    ((and (integerp size) (plusp size))
     `(integer 0 ,(1- (expt 2 size))))
    (t (error "UNSIGNED-BYTE: unsupported SIZE argument ~S" size))))

(deftype string-designator ()
  `(or string symbol character))

(deftype cons (&optional left right)
  (cond
    ((and (eq left '*)
          (eq right '*))
     '(satisfies consp))
    (t
     `(%cons ,left ,right))))

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

(defun %typecase-error (expr cases)
  (error "~S fell through ETYPECASE expression. Wanted one of ~S."
         expr cases))

(defun %typecase (expr cases errorp)
  (let* ((safe (%:safe-atom-p expr))
         (vexpr (if safe expr (gensym "TYPECASE")))
         (exps nil)
         (code (let recur ((cases cases))
                 (cond
                   ((null cases)
                    (when errorp
                      `(%typecase-error ,vexpr ',(nreverse exps))))
                   ((and (not (cdr cases))
                         (%memq (caar cases) '(otherwise t)))
                    `(progn ,@(cdar cases)))
                   ((not (caar cases))
                    (recur (cdr cases)))
                   (t
                    (when errorp
                      (push (caar cases) exps))
                    `(if (typep ,vexpr ',(caar cases))
                         (progn ,@(cdar cases))
                         ,(recur (cdr cases))))))))
    (if safe
        code
        `(let ((,vexpr ,expr)) ,code))))

(defmacro typecase (expr &rest clauses)
  (%typecase expr clauses nil))

(defmacro etypecase (expr &rest clauses)
  (%typecase expr clauses t))

;; we no longer touch *built-in-types* from this point on
(setq *ext-types* (%:hash-copy *ext-types*))
