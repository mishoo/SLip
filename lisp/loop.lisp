;; make sure to intern/export symbols into the SL package before we use them
;; in SL-LOOP.
(%export (list 'sl::loop
               'sl::in 'sl::on 'sl::by 'sl::across 'sl::named
               'sl::then 'sl::else 'sl::repeat
               'sl::when 'sl::unless
               'sl::for 'sl::do
               'sl::collect 'sl::collecting
               'sl::append 'sl::appending
               'sl::nconc 'sl::nconcing
               'sl::sum 'sl::summing
               'sl::maximize 'sl::maximizing
               'sl::minimize 'sl::minimizing
               'sl::with 'sl::and 'sl::into
               'sl::finally 'sl::count 'sl::counting
               'sl::from 'sl::upfrom 'sl::downfrom
               'sl::being 'sl::each 'sl::the 'sl::of 'sl::using
               'sl::hash-key 'sl::hash-keys
               'sl::hash-value 'sl::hash-values
               'sl::to 'sl::downto 'sl::upto 'sl::below 'sl::above)
         #.(%find-package :sl))

(defpackage :sl-loop
  (:use :sl))

(in-package :sl-loop)

(defparameter *clause-parsers* (make-hash))
(defparameter *loop-body* (cons nil nil))
(defparameter *loop-variables* (cons nil nil))
(defparameter *loop-start* (cons nil nil))
(defparameter *loop-iterate* (cons nil nil))
(defparameter *loop-finish* (cons nil nil))
(defparameter *loop-block-name* nil)

(defun %list-add (ls thing)
  (let ((cell (cons thing nil)))
    (if (car ls)
        (setf (cdr (cdr ls)) cell)
        (setf (car ls) cell))
    (setf (cdr ls) cell)))

(defun %list-append (ls elements)
  (if (car ls)
      (setf (cdr (cdr ls)) elements)
      (setf (car ls) elements))
  (setf (cdr ls) (last elements)))

(defun %register-parser (name parser)
  (cond
    ((consp name)
     (%register-parser (car name) parser)
     (%register-parser (cdr name) parser))
    ((symbolp name)
     (%register-parser (%symbol-name name) parser))
    ((stringp name)
     (let ((symbol (%intern name #.(%find-package :sl)))
           (kwsym (%intern name #.(%find-package :keyword))))
       (hash-add *clause-parsers* symbol parser)
       (hash-add *clause-parsers* kwsym parser)))))

(defmacro defparser (symbol args &body body)
  `(labels ((parser ,args ,@body))
     (%register-parser ',symbol #'parser)))

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
         (list `(let ((%data ,data))
                  ,@(dsetq (car var) '(car %data))
                  ,@(dsetq (cdr var) '(cdr %data))))
         (progn
           (dsetq (car var) nil)
           (dsetq (cdr var) nil))))))

(defun iskw (x name)
  (if (and name (symbolp x))
      (if (listp name)
          (or (iskw x (car name))
              (iskw x (cdr name)))
          (or (eq x name)
              (eq x (%intern (%symbol-name name)
                             #.(%find-package :keyword)))))))

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
        (%list-add *loop-start* `(assert (> ,step 0) "LOOP FOR step must be positive")))

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

(defun parse-for-hash (var args)
  (let ((next (pop args)))
    (assert (iskw next '(each the)) "Bad LOOP FOR-hash syntax: ~A" next)
    (macrolet ((dig-var ()
                 `(progn
                    (setf next (pop args))
                    (cond
                      ((iskw next '(hash-value hash-values))
                       (when vval
                         (error "LOOP for-hash value variable already defined"))
                       (setf vval var))
                      ((iskw next '(hash-key hash-keys))
                       (when vkey
                         (error "LOOP for-hash key variable already defined"))
                       (setf vkey var))
                      (t (error "Bad LOOP FOR-hash syntax: ~A" next))))))
      (let ((iter (gensym "hash-iterator"))
            (itval (gensym "hash-current"))
            (hash-form nil)
            (vkey nil)
            (vval nil))
        (dig-var)
        (setf next (pop args))
        (assert (iskw next '(in of)) "Bad LOOP FOR-hash syntax: ~A" next)
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
          (%list-add *loop-body* `(setf ,vval (vector-ref (car ,itval) 1)))))))
  args)

(defparser for (var . args)
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
       (parse-for-hash var args))
      (t (error "Unknown token in LOOP FOR: ~A" kind)))))

(defparser repeat args
  (let ((count (gensym "repeat")))
    (%list-add *loop-variables* count)
    (%list-add *loop-start* `(setf ,count ,(pop args)))
    (%list-add *loop-body* `(when (< (decf ,count) 0) (go %loop-end))))
  args)

(defparser do args
  (cond
    ((and args (consp (car args)))
     (%list-add *loop-body* (car args))
     (apply #'parser (cdr args)))
    (t args)))

(defparser named args
  (setf *loop-block-name* (car args))
  (cdr args))

(defparser finally args
  (cond
    ((and args (consp (car args)))
     (%list-add *loop-finish* (car args))
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
       (%list-add *loop-start* `(setf ,variable ,value)))
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
    (unless (%symbol-package name)
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
    (unless (%symbol-package name)
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
    (unless (%symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defparser (count counting) args
  (let ((form (pop args))
        (name (maybe-into "count")))
    (%list-add *loop-variables* `(,name 0))
    (%list-add *loop-body* `(when ,form (incf ,name)))
    (unless (%symbol-package name)
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
    (unless (%symbol-package name)
      (%list-add *loop-finish* name)))
  args)

(defparser (maximize maximizing) args
  (extremizing "max" '> args))

(defparser (minimize minimizing) args
  (extremizing "min" '< args))

(defun parse-clause (args)
  (let ((sym (car args)))
    (unless (symbolp sym)
      (error "Expecting loop clause, got ~A" sym))
    (let ((parser (hash-get *clause-parsers* sym)))
      (unless parser
        (error "Unknown loop clause ~A" sym))
      (apply parser (cdr args)))))

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
  (expand-loop args))
