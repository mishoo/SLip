(set! qq-expand-list
      (lambda (x)
        (if (consp x)
            (if (eq (car x) 'qq-unquote)
                (list 'list (cadr x))
                (if (eq (car x) 'qq-splice)
                    (cadr x)
                    (if (eq (car x) 'quasiquote)
                        (qq-expand-list (qq-expand (cadr x)))
                        (list 'list (list 'append
                                          (qq-expand-list (car x))
                                          (qq-expand (cdr x)))))))
            (list 'quote (list x)))))

(set! qq-expand
      (lambda (x)
        (if (consp x)
            (if (eq (car x) 'qq-unquote)
                (cadr x)
                (if (eq (car x) 'quasiquote)
                    (qq-expand (qq-expand (cadr x)))
                    (list 'append
                          (qq-expand-list (car x))
                          (qq-expand (cdr x)))))
            (list 'quote x))))

(defmacro quasiquote (thing)
  (qq-expand thing))

;;;; let the show begin

(defmacro defun (name args . body)
  `(set! ,name (lambda ,args ,@body)))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defun mapcar (func list)
  (when list
    (cons (func (car list)) (mapcar func (cdr list)))))

(defmacro let (defs . body)
  `((lambda ,(mapcar (lambda (x)
                       (if (listp x)
                           (car x)
                           x)) defs)
      ,@body)
    ,@(mapcar (lambda (x)
                (if (listp x)
                    (cadr x))) defs)))

(defmacro let* (defs . body)
  (if defs
      `(let (,(car defs))
         (let* ,(cdr defs)
           ,@body))
      `(progn ,@body)))

(defmacro labels (defs . body)
  `(let ,(mapcar (lambda (x) (car x)) defs)
     ,@(mapcar (lambda (x)
                 `(set! ,(car x) (lambda ,(cadr x) ,@(cddr x)))) defs)
     ,@body))

(defmacro flet (defs . body)
  `(let ,(mapcar (lambda (x)
                   `(,(car x) (lambda ,(cadr x) ,@(cddr x)))) defs)
     ,@body))

(defmacro or exps
  (when exps
    (let ((x (gensym "OR")))
      `(let ((,x ,(car exps)))
         (if ,x ,x (or ,@(cdr exps)))))))

(defmacro and exprs
  (when exprs
    (let ((x (gensym "AND")))
      `(let ((,x ,(car exprs)))
         (when ,x
           ,(if (cdr exprs) `(and ,@(cdr exprs)) x))))
    t))

(defmacro cond (cases)
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,(cdr cases)))))

(defmacro call/cc (func)
  `(,func (c/c)))

(defmacro with-cc (name . body)
  `((lambda (,name) ,@body) (c/c)))

;;;;;

(set! *amb-fail* (lambda (arg)
                   (clog "TOTAL FAILURE")))

(defmacro amb alternatives
  (if alternatives
      `(let ((+prev-amb-fail *amb-fail*))
         (with-cc +sk
           ,@(mapcar (lambda (alt)
                       `(with-cc +fk
                          (set! *amb-fail* +fk)
                          (+sk ,alt)))
                     alternatives)
           (set! *amb-fail* +prev-amb-fail)
           (+prev-amb-fail nil)))
      `(*amb-fail* nil)))

(defun sumis (n sum)
  (let (solutions)
    (with-cc *amb-fail*
      (labels ((add (numbers)
                 (labels ((rec (sum numbers)
                            (if numbers
                                (rec (+ sum (car numbers)) (cdr numbers))
                                sum)))
                   (rec 0 numbers)))
               (required-sum? (numbers)
                 (= sum (add numbers)))
               (rec (numbers next)
                 (if (= next 0)
                     (progn
                       (when (required-sum? numbers)
                         (set! solutions (cons numbers solutions)))
                       (amb))
                     (rec (cons (amb next (- 0 next))
                                numbers)
                          (- next 1)))))
        (rec (list) n)))
    (clog (length solutions))))

(sumis 14 1)




