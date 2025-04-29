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
          find maximizes minimizes that which below above))

(defpackage :sl-loop
  (:use :sl :%))

(in-package :sl-loop)

(defparameter *clause-parsers* (list))
(defparameter *loop-body* nil)
(defparameter *loop-variables* nil)
(defparameter *loop-collect* nil)
(defparameter *loop-start* nil)
(defparameter *loop-iterate* nil)
(defparameter *loop-finish* nil)
(defparameter *loop-block-name* nil)

(defun parse-clause (args)
  (let ((sym (car args)))
    (unless (symbolp sym)
      (error "Expecting loop clause, got ~A" sym))
    (let ((parser (getf *clause-parsers* sym)))
      (unless parser
        (error "Unknown loop clause ~A" sym))
      (apply parser (cdr args)))))

(defmacro list-add (ls thing)
  `(setf ,ls (setf (cdr ,ls) (cons ,thing nil))))

(defmacro list-nconc (ls elements)
  `(setf ,ls (last (setf (cdr ,ls) ,elements))))

(defun register-parser (name parser)
  (cond
    ((consp name)
     (register-parser (car name) parser)
     (register-parser (cdr name) parser))
    ((symbolp name)
     (register-parser (symbol-name name) parser))
    ((stringp name)
     (let ((symbol (intern name #.(find-package :sl)))
           (kwsym (intern name #.(find-package :keyword))))
       (setf (getf *clause-parsers* symbol) parser)
       (setf (getf *clause-parsers* kwsym) parser)))))

(defmacro defparser (symbol args &body body)
  `(labels ((parser ,args ,@body))
     (register-parser ',symbol #'parser)))

(defun loop-variable-defined (name)
  (some (lambda (x)
          (if (consp x)
              (eq (car x) name)
              (eq x name)))
        *loop-variables*))

(defun dsetq (var data)
  (cond
    ((not var) nil)
    ((symbolp var)
     (unless (loop-variable-defined var)
       (list-add *loop-variables* var))
     (when data
       (list `(setf ,var ,data))))
    ((consp var)
     (if data
         (list `(let (($data ,data))
                  ,@(dsetq (car var) `(car $data))
                  ,@(dsetq (cdr var) `(cdr $data))))
         (progn
           (dsetq (car var) nil)
           (dsetq (cdr var) nil))))))

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
    (list-add *loop-variables* seq)
    (list-add *loop-start* `(unless (setf ,seq ,(pop args))
                              (go $loop-end)))
    (when (iskw (car args) 'by)
      (pop args)
      (list-add *loop-variables*
                (setf next (gensym "next")))
      (list-add *loop-start* `(setf ,next ,(pop args))))
    (list-add *loop-body*
              `(unless (setf ,seq ,(if next
                                       `(funcall ,next ,seq)
                                       `(cdr ,seq)))
                 (go $loop-end)))
    (let ((setvar (dsetq var (case kind
                               ((in :in) `(car ,seq))
                               ((on :on) seq)))))
      (list-nconc *loop-start* (copy-list setvar))
      (list-nconc *loop-body* (copy-list setvar))))
  args)

(defun parse-for-across (var args)
  (let ((seq (gensym "array"))
        (index (gensym "index"))
        (length (gensym "length")))
    (list-nconc *loop-variables* `(,seq (,index 0) ,length))
    (list-nconc *loop-start* `((when (zerop (setf ,seq ,(pop args)
                                                  ,length (length ,seq)))
                                 (go $loop-end))))
    (let ((setvar (dsetq var `(vector-ref ,seq ,index))))
      (list-add *loop-body* `(when (>= (incf ,index) ,length)
                               (go $loop-end)))
      (list-nconc *loop-start* (copy-list setvar))
      (list-nconc *loop-body* (copy-list setvar))))
  args)

(defun parse-for-equal (var args)
  (let ((init (dsetq var (pop args))))
    (list-nconc *loop-start* (copy-list init))
    (cond
      ((iskw (car args) 'then)
       (pop args)
       (list-nconc *loop-body* (dsetq var (pop args))))
      (t
       (list-nconc *loop-body* (copy-list init))))
    args))

(defun %check-positive-loop-step (step)
  (assert (> step 0) "LOOP FOR step must be positive")
  step)

(defun check-positive-loop-step (step)
  (cond
    ((numberp step)
     (%check-positive-loop-step step)
     step)
    (t
     `(%check-positive-loop-step ,step))))

(defun parse-for-arithmetic (var args)
  (list-add *loop-variables* var)
  (let ((step nil))
    (let* ((init-form nil)
           (step-form nil)
           (limit-form nil)
           (limit)
           (step)
           (noteq nil)
           (kind (car args))
           (upwards (iskw kind '(from upfrom below upto to))))

      (flet ((dig ()
               (cond
                 ((iskw (car args) '(from upfrom downfrom))
                  (when init-form
                    (error "LOOP for: more than one init form ~A" args))
                  (pop args)
                  (setf init-form (pop args))
                  (list-add *loop-start* `(setf ,var ,init-form))
                  t)

                 ((iskw (car args) '(to upto downto below above))
                  (when limit-form
                    (error "LOOP for: more than one limit form ~A" args))
                  (cond
                    ((iskw (car args) '(downto above))
                     (setf upwards nil))
                    ((iskw (car args) '(upto below))
                     (setf upwards t)))
                  (setf noteq (iskw (pop args) '(below above))
                        limit-form (pop args))
                  (cond
                    ((numberp limit-form)
                     (setf limit limit-form))
                    (t
                     (setf limit (gensym "limit"))
                     (list-add *loop-variables* limit)
                     (list-add *loop-start* `(setf ,limit ,limit-form))))
                  t)

                 ((iskw (car args) 'by)
                  (when step-form
                    (error "LOOP for: more than one step form ~A" args))
                  (pop args)
                  (setf step-form (pop args))
                  (cond
                    ((numberp step-form)
                     (setf step step-form))
                    (t
                     (setf step (gensym "step"))
                     (list-add *loop-variables* step)
                     (list-add *loop-start*
                               `(setf ,step ,(check-positive-loop-step step-form)))))
                  t))))
        (when (dig) (when (dig) (dig))))

      (unless init-form
        (if upwards
            (list-add *loop-start* `(setf ,var 0))
            (error "Downward LOOP requires init form")))

      (list-add *loop-body*
                `(setf ,var ,(if step-form
                                 `(,(if upwards '+ '-) ,var ,step)
                                 `(,(if upwards '1+ '1-) ,var))))

      (when limit-form
        (let ((end-cond `(when (,(if noteq
                                     (if upwards '>= '<=)
                                     (if upwards '> '<)) ,var ,limit)
                           (go $loop-end))))
          (list-add *loop-start* end-cond)
          (list-add *loop-body* end-cond)))))
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
    (list-nconc *loop-variables* `((,index 0) ,length ,symbols ,var))
    (list-nconc *loop-start* `((setf ,symbols ,(nreverse get-symbols))
                               (when (zerop (setf ,length (length ,symbols)))
                                 (go $loop-end))
                               (setf ,var (vector-ref ,symbols 0))))
    (list-nconc *loop-body* `((when (>= (incf ,index) ,length)
                                (go $loop-end))
                              (setf ,var (vector-ref ,symbols ,index)))))
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
               (hash-var (gensym "hash"))
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
           (list-nconc *loop-variables* (list iter itval hash-var))
           (when vkey (list-add *loop-variables* vkey))
           (when vval (list-add *loop-variables* vval))
           (let ((next-item `((setf ,itval (iterator-next ,iter))
                              (when (cdr ,itval)
                                (go $loop-end))
                              ,@(when vkey
                                  `((setf ,vkey (vector-ref (car ,itval) 0))))
                              ,@(when vval
                                  `((setf ,vval (vector-ref (car ,itval) 1)))))))
             (list-nconc *loop-start*
                         `((setf ,hash-var ,hash-form)
                           (setf ,iter (hash-iterator ,hash-var))
                           ,@(copy-list next-item)))
             (list-nconc *loop-body* (copy-list next-item)))))
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
      ((iskw kind '(from downfrom upfrom to upto below above by))
       (parse-for-arithmetic var (cons kind args)))
      ((iskw kind 'being)
       (parse-for-being var args))
      (t (error "Unknown token in LOOP FOR: ~A" kind)))))

(defparser repeat args
  (let ((count (gensym "repeat")))
    (list-add *loop-variables* count)
    (list-add *loop-start* `(when (< (setf ,count (1- ,(pop args))) 0)
                              (go $loop-end)))
    (list-add *loop-body* `(when (< (decf ,count) 0) (go $loop-end))))
  args)

(defparser (do doing) args
  (cond
    ((and args (consp (car args)))
     (list-add *loop-iterate* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser return args
  (list-add *loop-iterate* `(return-from ,*loop-block-name* ,(pop args)))
  args)

(defparser named args
  (setf *loop-block-name* (car args))
  (cdr args))

(defparser finally args
  (cond
    ((and args (consp (car args)))
     (list-add *loop-finish* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser initially args
  (cond
    ((and args (consp (car args)))
     (list-add *loop-start* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser with args
  (let ((variable (pop args))
        (value (when (iskw (car args) '=)
                 (pop args)
                 (pop args))))
    (list-nconc *loop-start* (dsetq variable value))
    (cond
      ;; ((iskw (car args) 'and)
      ;;  ;; XXX: this is incorrect, AND should produce "parallel" bindings, but
      ;;  ;; it's kinda tricky to implement; I won't bother, at the moment, so
      ;;  ;; just recurse and compile it as WITH.
      ;;  (apply #'parser (cdr args)))
      (t
       args))))

(defmacro maybe-into (name)
  `(if (iskw (car args) 'into)
       (progn (pop args) (pop args))
       (gensym ,name)))

(defun make-list-collect-vars (args &optional append?)
  (let ((name (if (iskw (car args) 'into)
                  (progn (pop args)
                         (pop args))
                  '$collect)))
    (aif (getf *loop-collect* name)
         (list args name it)
         (cond
           (append?
            (list-add *loop-variables* name)
            (when (eq name '$collect)
              (list-add *loop-finish* '$collect))
            (setf (getf *loop-collect* name) t)
            (list args name nil))
           (t
            (let ((tail (gensym (strcat name "-TAIL"))))
              (list-nconc *loop-variables* (list name tail))
              (when (eq name '$collect)
                (list-add *loop-finish* '$collect))
              (setf (getf *loop-collect* name) tail)
              (list args name tail)))))))

(defparser (collect collecting) args
  (let* ((form (pop args))
         (vars (make-list-collect-vars args))
         (name (cadr vars))
         (tail (caddr vars)))
    (setf args (car vars))
    (list-add *loop-iterate*
              `(setf ,tail
                     (if ,tail
                         (setf (cdr ,tail) (list ,form))
                         (setf ,name (list ,form))))))
  args)

(defparser (append appending) args
  (let* ((form (pop args))
         (vars (make-list-collect-vars args t))
         (name (cadr vars)))
    (setf args (car vars))
    (list-add *loop-iterate* `(setf ,name (append ,name ,form))))
  args)

(defparser (nconc nconcing) args
  (let* ((form (pop args))
         (vars (make-list-collect-vars args))
         (name (cadr vars))
         (tail (caddr vars)))
    (setf args (car vars))
    (list-add *loop-iterate*
              `(let (($nconc ,form))
                 (when $nconc
                   (setf ,tail
                         (last
                          (if ,tail
                              (rplacd ,tail $nconc)
                              (setf ,name $nconc))))))))
  args)

(defparser (sum summing) args
  (let ((form (pop args))
        (name (maybe-into "sum")))
    (list-add *loop-variables* `(,name 0))
    (list-add *loop-iterate* `(setf ,name (+ ,name ,form)))
    (unless (symbol-package name)
      (list-add *loop-finish* name)))
  args)

(defparser (count counting) args
  (let ((form (pop args))
        (name (maybe-into "count")))
    (list-add *loop-variables* `(,name 0))
    (list-add *loop-iterate* `(when ,form (incf ,name)))
    (unless (symbol-package name)
      (list-add *loop-finish* name)))
  args)

(defun extremizing (name op args)
  (let ((form (pop args))
        (name (maybe-into name)))
    (list-add *loop-variables* name)
    (list-add *loop-iterate* `(setf ,name (if ,name
                                              (,op ,name ,form)
                                              ,form)))
    (unless (symbol-package name)
      (list-add *loop-finish* name)))
  args)

(defparser (maximize maximizing) args
  (extremizing "max" 'max args))

(defparser (minimize minimizing) args
  (extremizing "min" 'min args))

(defun parse-conditional (args negated)
  (let ((condition (pop args))
        (then-body (let* ((loop-iterate (cons nil nil))
                          (*loop-iterate* loop-iterate))
                     (setf args (parse-clause args))
                     (cdr loop-iterate)))
        (else-body (when (iskw (car args) 'else)
                     (let* ((loop-iterate (cons nil nil))
                            (*loop-iterate* loop-iterate))
                       (setf args (parse-clause (cdr args)))
                       (cdr loop-iterate)))))
    (let ((form `(if ,(if negated
                          `(not ,condition)
                          condition)
                     (progn ,@then-body)
                     (progn ,@else-body))))
      (list-add *loop-iterate* form)))
  args)

(defparser when args
  (parse-conditional args nil))

(defparser unless args
  (parse-conditional args t))

(defparser if args
  (parse-conditional args nil))

(defparser while args
  (let ((condition (pop args)))
    (let ((form `(unless ,condition (go $loop-end))))
      (list-add *loop-iterate* form)))
  args)

(defparser until args
  (let ((condition (pop args)))
    (let ((form `(when ,condition (go $loop-end))))
      (list-add *loop-iterate* form)))
  args)

(defparser always args
  (let ((form `(unless ,(pop args)
                 (return-from ,*loop-block-name* nil))))
    (list-add *loop-iterate* form))
  (list-add *loop-finish* t)
  args)

(defparser never args
  (let ((form `(when ,(pop args)
                 (return-from ,*loop-block-name* nil))))
    (list-add *loop-iterate* form))
  (list-add *loop-finish* t)
  args)

(defparser thereis args
  (let ((form `(let (($obj ,(pop args)))
                 (when $obj
                   (return-from ,*loop-block-name* $obj)))))
    (list-add *loop-iterate* form))
  args)

;; Wish CL LOOP had something like this:
;;
;;   (loop for el in '(1 2 7 4) find el minimizing (/ 1 el))
;;
;;   (loop for el in '(1 2 7 4)
;;         find the el which minimizes (/ 1 el) into (best-el best-val)
;;         finally (return (list best-el best-val)))
;;
;; Let's just do it, it's simple:
(defparser find args
  (when (iskw (car args) 'the)
    (pop args))
  (let ((el (pop args))
        kind form name op)
    (when (iskw (car args) '(that which))
      (pop args))
    (setf kind (pop args))
    (case kind
      ((minimizing minimizes) (setf op '<))
      ((maximizing maximizes) (setf op '>))
      (t (error "Unsupported kind of FIND in LOOP: ~A" kind)))
    (setf form (pop args)
          name (maybe-into "find-val"))
    (let ((best (if (consp name)
                    (prog1 (cadr name)
                      (setf name (car name)))
                    (gensym "best"))))
      (list-nconc *loop-variables* (list best name))
      (list-add *loop-iterate* `(let (($val ,form))
                                  (when (or (not ,best)
                                            (,op $val ,best))
                                    (setf ,best $val
                                          ,name ,el)))))
    (unless (symbol-package name)
      (list-add *loop-finish* name)))
  args)

(defun expand-loop (args)
  (let ((loop-body (cons nil nil))
        (loop-variables (cons nil nil))
        (loop-start (cons nil nil))
        (loop-iterate (cons nil nil))
        (loop-finish (cons nil nil))
        (*loop-block-name* nil)
        (*loop-collect* nil))
    (let ((*loop-body* loop-body)
          (*loop-variables* loop-variables)
          (*loop-start* loop-start)
          (*loop-iterate* loop-iterate)
          (*loop-finish* loop-finish))
      (let rec ((args args))
        (when args
          (rec (parse-clause args)))))
    `(block ,*loop-block-name*
       (let (,@(cdr loop-variables))
         (tagbody
            ,@(cdr loop-start)
          $loop-next
            ,@(cdr loop-iterate)
            ,@(cdr loop-body)
            (go $loop-next)
          $loop-end)
         ,@(cdr loop-finish)))))

(defmacro loop (&body args)
  (if (symbolp (car args))
      (expand-loop args)
      ;; simple loop
      `(block nil
         (let $loop-next ()
           ,@args
           ($loop-next)))))
