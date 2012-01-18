;; this is `once-only' from Practical Common Lisp
(defmacro with-rebinds (names . body)
  (let ((gensyms (mapcar (lambda (_) (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                ,@body)))))

(defun macroexpand (form)
  (if (and (consp form)
           (symbolp (car form))
           (%macrop (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(defun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (map macroexpand-all form))
      form))

(defmacro pushnew (obj place)
  (let ((sym (gensym)))
    `(let ((,sym ,obj))
       (unless (member ,sym ,place)
         (push ,sym ,place)))))

(defmacro flet (defs . body)
  `(let ,(map (lambda (x)
                `(,(car x) (%set-function-name
                            (lambda ,(cadr x) ,@(cddr x))
                            ',(car x)))) defs)
     ,@body))

(defmacro awhen (cond . body)
  `(let ((it ,cond))
     (when it ,@body)))

;;;; destructuring-bind

(defmacro %next (lst)
  `(prog1 (car ,lst)
     (set! ,lst (cdr ,lst))))

(defun %fn-destruct (args values body)
  (let (optional? rest? key? aux? names decls)
    (let ((topv (gensym)) rec)
      ((set! rec
             (lambda (args values i)
               (when args
                 (let ((thisarg (car args)))
                   (cond
                     ((symbolp thisarg)
                      (case thisarg
                        (&whole
                         (when (> i 0) (error "Misplaced &WHOLE"))
                         (let ((thisarg (cadr args)))
                           (unless (and thisarg (symbolp thisarg))
                             (error "Missing variable name for &WHOLE"))
                           (push `(,thisarg ,values) decls))
                         (rec (cddr args) values i))

                        (&optional
                         (when (or optional? rest? key? aux?)
                           (error "Invalid &OPTIONAL"))
                         (set! optional? t)
                         (rec (cdr args) values i))

                        ((&rest &body)
                         (when (or rest? key? aux?)
                           (error "Invalid &REST/&BODY"))
                         (set! rest? t)
                         (set! optional? nil)
                         (let ((thisarg (cadr args)))
                           (unless (and thisarg (symbolp thisarg))
                             (error "Missing variable name for &REST"))
                           (push `(,thisarg ,values) decls))
                         (rec (cddr args) values i))

                        (&key
                         (when (or key? aux?)
                           (error "Invalid &KEY"))
                         (set! key? t)
                         (set! optional? nil)
                         (set! rest? nil)
                         (rec (cdr args) values i))

                        (&aux
                         (when aux?
                           (error "Invalid &AUX"))
                         (set! aux? t)
                         (set! optional? nil)
                         (set! rest? nil)
                         (set! key? nil)
                         (rec (cdr args) values i))

                        (t
                         (when (member thisarg names)
                           (error (strcat "Argument seen twice: " thisarg)))
                         (push thisarg names)
                         (cond
                           (optional?
                            (push `(,thisarg (%next ,values)) decls))
                           (aux?
                            (push thisarg decls))
                           (key?
                            (push `(,thisarg (%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")))) decls))
                           (t
                            (push `(,thisarg (if ,values
                                                 (%next ,values)
                                                 (error ,(strcat "Missing required argument: " thisarg))))
                                  decls)))
                         (rec (cdr args) values (+ i 1)))))

                     ((consp thisarg)
                      (cond
                        ((or optional? key?)
                         (let ((thisarg (car thisarg))
                               (default (cadr thisarg))
                               (thisarg-p (caddr thisarg)))
                           (when thisarg-p
                             (push `(,thisarg-p (if ,values t nil)) decls))
                           (push `(,thisarg ,(if key?
                                                 (let ((val (gensym)))
                                                   `(let ((,val (%getf ,values ,(%intern (%symbol-name thisarg) (%find-package "KEYWORD")) 'not-found)))
                                                      (if (eq ,val 'not-found) ,default ,val)))
                                                 `(if ,values (%next ,values) ,default)))
                                 decls)))
                        (aux? (let ((thisarg (car thisarg))
                                    (value (cadr thisarg)))
                                (push `(,thisarg ,value) decls)))
                        (rest? (error "Invalid argument list following &REST/&BODY"))
                        (t
                         (let ((sublist (gensym)))
                           (push `(,sublist (if ,values (%next ,values) (error "Missing sublist"))) decls)
                           (rec thisarg sublist 0))))
                      (rec (cdr args) values (+ i 1))))))))
       args topv 0)
      `(let* ((,topv ,values) ,@(reverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct args values body))
