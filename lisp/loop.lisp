;; An implementation for the LOOP macro.
;;
;; (c) Mihai Bazon <mihai.bazon@gmail.com> 2025
;; License: MIT

(in-package :sl)

(export '(loop as in on by across named then else repeat when unless for do doing
          collect collecting append appending nconc nconcing sum summing maximize
          maximizing minimize minimizing with and into finally count counting from
          upfrom downfrom being each the of using hash-key hash-keys hash-value
          hash-values present-symbol present-symbols symbol symbols external-symbol
          external-symbols while until initially always never thereis to downto upto
          below above))

(defpackage :sl-loop
  (:use :sl :%))

(in-package :sl-loop)

(defparameter *clause-parsers* (list))
(defparameter *loop-body* (cons nil nil))
(defparameter *loop-variables* (cons nil nil))
(defparameter *loop-start* (cons nil nil))
(defparameter *loop-iterate* (cons nil nil))
(defparameter *loop-finish* (cons nil nil))
(defparameter *loop-block-name* nil)

(defun parse-clause (args)
  (let ((sym (car args)))
    (unless (symbolp sym)
      (error "Expecting loop clause, got ~A" sym))
    (let ((parser (getf *clause-parsers* sym)))
      (unless parser
        (error "Unknown loop clause ~A" sym))
      (apply parser (cdr args)))))

(defun %list-add (ls thing)
  (let ((cell (cons thing nil)))
    (setf (cdr ls)
          (if (car ls)
              (setf (cdr (cdr ls)) cell)
              (setf (car ls) cell)))))

(defun %list-append (ls elements)
  (setf (cdr ls)
        (last (if (car ls)
                  (setf (cdr (cdr ls)) elements)
                  (setf (car ls) elements)))))

(defun %register-parser (name parser)
  (cond
    ((consp name)
     (%register-parser (car name) parser)
     (%register-parser (cdr name) parser))
    ((symbolp name)
     (%register-parser (symbol-name name) parser))
    ((stringp name)
     (let ((symbol (intern name #.(find-package :sl)))
           (kwsym (intern name #.(find-package :keyword))))
       (setf (getf *clause-parsers* symbol) parser)
       (setf (getf *clause-parsers* kwsym) parser)))))

(defmacro defparser (symbol args &body body)
  `(labels ((parser ,args ,@body))
     (%register-parser ',symbol #'parser)))

(let ((%data (gensym "dsetq")))
  (defun dsetq (var data)
    (cond
      ((not var))
      ((symbolp var)
       (unless (member var (car *loop-variables*))
         (%list-add *loop-variables* var))
       (when data
         (list `(setf ,var ,data))))
      ((consp var)
       (if data
           (list `(let ((,%data ,data))
                    ,@(dsetq (car var) `(car ,%data))
                    ,@(dsetq (cdr var) `(cdr ,%data))))
           (progn
             (dsetq (car var) nil)
             (dsetq (cdr var) nil)))))))

(defun iskw (x name)
  (if (and name (symbolp x))
      (if (listp name)
          (or (iskw x (car name))
              (iskw x (cdr name)))
          (or (eq x name)
              (eq x (intern (symbol-name name)
                            #.(find-package :keyword)))))))

(defun parse-for-in (kind var args)
  (let ((seq (gensym "list"))
        (next nil))
    (%list-add *loop-variables* seq)
    (%list-add *loop-start* `(setf ,seq ,(pop args)))
    (when (iskw (car args) 'by)
      (pop args)
      (setf next (gensym "next"))
      (%list-add *loop-variables* next)
      (%list-add *loop-start* `(setf ,next ,(pop args))))
    (%list-append *loop-body*
                  `((unless ,seq (go %loop-end))
                    ,@(dsetq var (case kind
                                   ((in :in) `(car ,seq))
                                   ((on :on) seq)))
                    (setf ,seq ,(if next
                                    `(funcall ,next ,seq)
                                    `(cdr ,seq))))))
  args)

(defun parse-for-across (var args)
  (let ((seq (gensym "array"))
        (index (gensym "index")))
    (%list-append *loop-variables* (list seq index))
    (%list-add *loop-start* `(setf ,seq ,(pop args)
                                   ,index 0))
    (%list-add *loop-body* `(when (>= ,index (length ,seq))
                              (go %loop-end)))
    (%list-append *loop-body* (dsetq var `(vector-ref ,seq ,index)))
    (%list-add *loop-iterate* `(incf ,index)))
  args)

(defun parse-for-equal (var args)
  (%list-append *loop-start* (dsetq var (pop args)))
  (when (iskw (car args) 'then)
    (pop args)
    (%list-append *loop-iterate* (dsetq var (pop args))))
  args)

(defun check-positive-loop-step (step)
  (assert (> step 0) "LOOP FOR step must be positive"))

(defun parse-for-arithmetic (var args)
  (%list-add *loop-variables* var)
  (let ((step nil))
    (let* ((init-form nil)
           (step-form nil)
           (limit-form nil)
           (limit (gensym "limit"))
           (step (gensym "step"))
           (noteq nil)
           (kind (car args))
           (upwards (iskw kind '(from upfrom below upto to))))

      (when (iskw kind '(from upfrom downfrom))
        (pop args)
        (setf init-form (pop args)))

      (unless init-form
        (if upwards
            (setf init-form 0)
            (error "Downward LOOP requires init form")))

      (%list-add *loop-start* `(setf ,var ,init-form))

      (when (iskw (car args) '(to upto downto below above))
        (cond
          ((iskw (car args) '(downto above))
           (setf upwards nil))
          ((iskw (car args) '(upto below))
           (setf upwards t)))
        (setf noteq (iskw (pop args) '(below above))
              limit-form (pop args))
        (%list-add *loop-variables* limit)
        (%list-add *loop-start* `(setf ,limit ,limit-form)))

      (when (iskw (car args) 'by)
        (pop args)
        (setf step-form (pop args))
        (%list-add *loop-variables* step)
        (%list-add *loop-start* `(setf ,step ,step-form))
        (%list-add *loop-start* `(check-positive-loop-step ,step)))

      (when limit-form
        (%list-add *loop-body*
                   `(when (,(if noteq
                                (if upwards '>= '<=)
                                (if upwards '> '<)) ,var ,limit)
                      (go %loop-end))))

      (%list-add *loop-iterate*
                 `(setf ,var ,(if step-form
                                  `(,(if upwards '+ '-) ,var ,step)
                                  `(,(if upwards '1+ '1-) ,var))))))
  args)

(defun parse-for-being-symbols (var args)
  (let ((index (gensym "index"))
        (length (gensym "length"))
        (symbols (gensym "symbols"))
        (get-symbols nil)
        (next (pop args)))
    (cond
      ((iskw next '(present-symbol present-symbols))
       (setf get-symbols (list '%:%interned-symbols)))
      ((iskw next '(symbol symbols))
       (setf get-symbols (list '%:%accessible-symbols)))
      ((iskw next '(external-symbol external-symbols))
       (setf get-symbols (list '%:%external-symbols))))
    (setf next (pop args))
    (assert (iskw next '(in of)) "Bad LOOP for-being syntax: ~A" next)
    (push `(find-package ,(pop args)) get-symbols)
    (%list-append *loop-variables* `((,index 0) ,length ,symbols ,var))
    (%list-append *loop-start* `((setf ,symbols ,(nreverse get-symbols))
                                 (setf ,length (length ,symbols))))
    (%list-append *loop-body* `((when (>= ,index ,length)
                                  (go %loop-end))
                                (setf ,var (vector-ref ,symbols ,index))
                                (incf ,index))))
  args)

(defun parse-for-being (var args)
  (let ((next (pop args)))
    (assert (iskw next '(each the)) "Bad LOOP for-being syntax: ~A" next)
    (cond
      ((iskw (car args)
             '(present-symbol present-symbols
               symbol symbols
               external-symbol external-symbols))
       (parse-for-being-symbols var args))
      (t
       ;; hash table
       (macrolet ((dig-var ()
                    `(progn
                       (setf next (pop args))
                       (cond
                         ((iskw next '(hash-value hash-values))
                          (when vval
                            (error "LOOP for-being value variable already defined"))
                          (setf vval var))
                         ((iskw next '(hash-key hash-keys))
                          (when vkey
                            (error "LOOP for-being key variable already defined"))
                          (setf vkey var))
                         (t (error "Bad LOOP for-being syntax: ~A" next))))))
         (let ((iter (gensym "hash-iterator"))
               (itval (gensym "hash-current"))
               (hash-form nil)
               (vkey nil)
               (vval nil))
           (dig-var)
           (setf next (pop args))
           (assert (iskw next '(in of)) "Bad LOOP for-being syntax: ~A" next)
           (setf hash-form (pop args))
           (when (iskw (car args) 'using)
             (pop args)
             (let* ((args (pop args))
                    (var (cadr args)))
               (dig-var)))
           (%list-append *loop-variables* (list iter itval))
           (when vkey
             (%list-add *loop-variables* vkey))
           (when vval
             (%list-add *loop-variables* vval))
           (%list-add *loop-start* `(setf ,iter (hash-iterator ,hash-form)))
           (%list-append *loop-body*
                         `((setf ,itval (iterator-next ,iter))
                           (when (cdr ,itval)
                             (go %loop-end))))
           (when vkey
             (%list-add *loop-body* `(setf ,vkey (vector-ref (car ,itval) 0))))
           (when vval
             (%list-add *loop-body* `(setf ,vval (vector-ref (car ,itval) 1))))))
       args))))

(defparser (for as) (var . args)
  (let ((kind (pop args)))
    (cond
      ((iskw kind '(in on))
       (parse-for-in kind var args))
      ((iskw kind '=)
       (parse-for-equal var args))
      ((iskw kind 'across)
       (parse-for-across var args))
      ((iskw kind '(from downfrom upfrom to upto below))
       (parse-for-arithmetic var (cons kind args)))
      ((iskw kind 'being)
       (parse-for-being var args))
      (t (error "Unknown token in LOOP FOR: ~A" kind)))))

(defparser repeat args
  (let ((count (gensym "repeat")))
    (%list-add *loop-variables* count)
    (%list-add *loop-start* `(setf ,count ,(pop args)))
    (%list-add *loop-body* `(when (< (decf ,count) 0) (go %loop-end))))
  args)

(defparser (do doing) args
  (cond
    ((and args (consp (car args)))
     (%list-add *loop-body* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser return args
  (%list-add *loop-body* `(return-from ,*loop-block-name* ,(pop args)))
  args)

(defparser named args
  (setf *loop-block-name* (car args))
  (cdr args))

(defparser finally args
  (cond
    ((and args (consp (car args)))
     (%list-add *loop-finish* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser initially args
  (cond
    ((and args (consp (car args)))
     (%list-add *loop-start* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser with args
  (let ((variable (pop args))
        (value (when (iskw (car args) '=)
                 (pop args)
                 (pop args))))
    (cond
      ((symbolp variable)
       (%list-add *loop-variables* variable)
       (when value
         (%list-add *loop-start* `(setf ,variable ,value))))
      (t
       (%list-append *loop-start* (dsetq variable value))))
    (cond
      ((iskw (car args) 'and)
       ;; XXX: this is incorrect, AND should produce "parallel" bindings, but
       ;; it's kinda tricky to implement; I won't bother, at the moment, so
       ;; just recurse and compile it as WITH.
       (apply #'parser (cdr args)))
      (t
       args))))

(defmacro maybe-into (name)
  `(if (iskw (car args) 'into)
       (progn (pop args) (pop args))
       (gensym ,name)))

(defparser (collect collecting) args
  (let ((form (pop args))
        (name (maybe-into "collect"))
        (tail (gensym "tail")))
    (%list-append *loop-variables* `(,name ,tail))
    (%list-add *loop-body*
               `(let ((cell (cons ,form nil)))
                  (setf ,tail
                        (if ,tail
                            (setf (cdr ,tail) cell)
                            (setf ,name cell)))))
    (unless (symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defun %loop-make-list (append args)
  (let ((form (pop args))
        (name (maybe-into (if append "append" "nconc")))
        (tail (unless append (gensym "tail"))))
    (%list-add *loop-variables* name)
    (when tail (%list-add *loop-variables* tail))
    (%list-add *loop-body*
               (if append
                   `(setf ,name (append ,name ,form))
                   `(let ((ls ,form))
                      (when ls
                        (if ,tail
                            (setf (cdr ,tail) ls)
                            (setf ,name ls))
                        (setf ,tail (last ls))))))
    (unless (symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defparser (append appending) args
  (%loop-make-list t args))

(defparser (nconc nconcing) args
  (%loop-make-list nil args))

(defparser (sum summing) args
  (let ((form (pop args))
        (name (maybe-into "sum")))
    (%list-add *loop-variables* name)
    (%list-add *loop-start* `(setf ,name 0))
    (%list-add *loop-body* `(setf ,name (+ ,name ,form)))
    (unless (symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defparser (count counting) args
  (let ((form (pop args))
        (name (maybe-into "count")))
    (%list-add *loop-variables* `(,name 0))
    (%list-add *loop-body* `(when ,form (incf ,name)))
    (unless (symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defun extremizing (name op args)
  (let ((form (pop args))
        (name (maybe-into name)))
    (%list-add *loop-variables* name)
    (%list-add *loop-body* `(let ((val ,form))
                              (if (or (not ,name)
                                      (,op val ,name))
                                  (setf ,name val))))
    (unless (symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defparser (maximize maximizing) args
  (extremizing "max" '> args))

(defparser (minimize minimizing) args
  (extremizing "min" '< args))

(defparser when args
  (let ((condition (pop args))
        (body (let* ((*loop-body* (cons nil nil)))
                (setf args (parse-clause args))
                (car *loop-body*))))
    (%list-add *loop-body* `(when ,condition ,@body)))
  args)

(defparser unless args
  (let ((condition (pop args))
        (body (let* ((*loop-body* (cons nil nil)))
                (setf args (parse-clause args))
                (car *loop-body*))))
    (%list-add *loop-body* `(unless ,condition ,@body)))
  args)

(defparser if args
  (let ((condition (pop args))
        (then-body (let* ((*loop-body* (cons nil nil)))
                     (setf args (parse-clause args))
                     (car *loop-body*)))
        (else-body (when (iskw (car args) 'else)
                     (let* ((*loop-body* (cons nil nil)))
                       (setf args (parse-clause (cdr args)))
                       (car *loop-body*)))))
    (%list-add *loop-body* `(if ,condition
                                (progn ,@then-body)
                                (progn ,@else-body))))
  args)

(defparser while args
  (let ((condition (pop args)))
    (%list-add *loop-body* `(unless ,condition (go %loop-end))))
  args)

(defparser until args
  (let ((condition (pop args)))
    (%list-add *loop-body* `(when ,condition (go %loop-end))))
  args)

(defparser always args
  (%list-add *loop-body* `(unless ,(pop args)
                            (return-from ,*loop-block-name* nil)))
  (%list-add *loop-finish* t)
  args)

(defparser never args
  (%list-add *loop-body* `(when ,(pop args)
                            (return-from ,*loop-block-name* nil)))
  (%list-add *loop-finish* t)
  args)

(defparser thereis args
  (%list-add *loop-body* `(let ((obj ,(pop args)))
                            (when obj
                              (return-from ,*loop-block-name* obj))))
  args)

(defun expand-loop (args)
  (let ((*loop-body* (cons nil nil))
        (*loop-variables* (cons nil nil))
        (*loop-start* (cons nil nil))
        (*loop-iterate* (cons nil nil))
        (*loop-finish* (cons nil nil))
        (*loop-block-name* nil))
    (let rec ((args args))
      (when args
        (rec (parse-clause args))))
    `(block ,*loop-block-name*
       (let (,@(car *loop-variables*))
         (tagbody
            ,@(car *loop-start*)
          %loop-next
            ,@(car *loop-body*)
            ,@(car *loop-iterate*)
            (go %loop-next)
          %loop-end)
         ,@(car *loop-finish*)))))

(defmacro loop (&body args)
  (if (symbolp (car args))
      (expand-loop args)
      ;; simple loop
      `(block nil
         (let %loop-next ()
           ,@args
           (%loop-next)))))
