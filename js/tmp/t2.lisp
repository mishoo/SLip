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
  `(progn
     (set! ,name (lambda ,args ,@body))
     (%set-function-name ,name ',name)))

(defmacro when (pred . body)
  `(if ,pred (progn ,@body)))

(defmacro unless (pred . body)
  `(if ,pred nil (progn ,@body)))

(defun mapcar (func list)
  (when list
    (cons (func (car list)) (mapcar func (cdr list)))))

(defun foreach (list func)
  (when list
    (func (car list))
    (foreach (cdr list) func)))

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
  (if exprs
      (let ((x (gensym "AND")))
        `(let ((,x ,(car exprs)))
           (when ,x
             ,(if (cdr exprs) `(and ,@(cdr exprs)) x))))
      t))

(defmacro cond cases
  (if cases
      `(if ,(caar cases)
           (progn ,@(cdar cases))
           (cond ,@(cdr cases)))))

(defun member (item list)
  (if list
      (if (eq item (car list))
          list
          (member item (cdr list)))))

(defmacro case (expr . cases)
  (let ((vexpr (gensym "CASE")))
    `(let ((,vexpr ,expr))
       ,(labels ((recur (cases)
                        (when cases
                          (if (listp (caar cases))
                              `(if (member ,vexpr ',(caar cases))
                                   (progn ,@(cdar cases))
                                   ,(recur (cdr cases)))
                              (if (and (not (cdr cases))
                                       (or (eq (caar cases) 'otherwise)
                                           (eq (caar cases) t)))
                                  `(progn ,@(cdar cases))
                                  `(if (eq ,vexpr ',(caar cases))
                                       (progn ,@(cdar cases))
                                       ,(recur (cdr cases))))))))
                (recur cases)))))

;;;;;;;;;;;;;;;;;;;;;

(set! qq
      (lambda (x)
        (if (atom x)
            (list 'quote x)
            (if (eq 'qq-unquote (car x))
                (cadr x)
                (if (eq 'quasiquote (car x))
                    (qq (qq (cadr x)))
                    (if (consp (car x))
                        (if (eq 'qq-splice (caar x))
                            (list 'append (cadar x) (qq (cdr x)))
                            (list 'cons (qq (car x)) (qq (cdr x))))
                        (list 'cons (qq (car x)) (qq (cdr x)))))))))

;; (qq '(list 1 (quote 2) ,@(list 'a 'b 'c) 3 ,(+ 10 20 30) 4 5))
;; (qq-expand '(list 1 2 ,@(list 'a 'b 'c) 3 ,(+ 10 20 30) 4 5))

(qq '(let ((,vexpr ,expr))
      ,(labels ((recur (cases)
                       (when cases
                         (if (listp (caar cases))
                             `(if (member ,vexpr ',(caar cases))
                                  (progn ,@(cdar cases))
                                  ,(recur (cdr cases)))
                             (if (and (not (cdr cases))
                                      (or (eq (caar cases) 'otherwise)
                                          (eq (caar cases) t)))
                                 `(progn ,@(cdar cases))
                                 `(if (eq ,vexpr ',(caar cases))
                                      (progn ,@(cdar cases))
                                      ,(recur (cdr cases))))))))
               (recur cases))))
