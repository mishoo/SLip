(let ((main (%make-package "SS"))
      (boot (%find-package "%"))
      (user (%make-package "SS-USER")))
  (foreach '(quasiquote defmacro defun when unless map labels foreach
             prog1 prog2 or and cond member case mapcar with-cc aif push error
             lisp-reader compile compile-string
             quote lambda let let* if progn set! t nil not
             *package* *read-table*
             &key &rest &body &whole &optional &aux)
           (lambda (sym)
             (%export sym boot)))
  (%use-package boot main)
  (%use-package main user)
  (set! *package* main))

(defun load (url)
  (let ((*package* *package*)
        (*read-table* *read-table*))
    (%::load-lisp-file url)))

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
           (%macro (car form)))
      (macroexpand (macroexpand-1 form))
      form))

(defun macroexpand-all (form)
  (if (consp form)
      (let ((form (macroexpand form)))
        (map #'macroexpand-all form))
      form))

(defmacro pushnew (obj place)
  (let ((sym (gensym)))
    `(let ((,sym ,obj))
       (unless (member ,sym ,place)
         (push ,sym ,place)))))

(defmacro awhen (cond . body)
  `(let ((it ,cond))
     (when it ,@body)))

;;;; destructuring-bind

(defmacro %next (lst)
  `(prog1 (car ,lst)
     (set! ,lst (cdr ,lst))))

(defun %fn-destruct (args values body)
  (let (names decls)
    (let ((topv (gensym)) rec)
      (labels
          ((rec (optional? rest? key? aux? args values i)
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
                       (rec nil nil nil nil (cddr args) values i))

                      (&optional
                       (when (or optional? rest? key? aux?)
                         (error "Invalid &OPTIONAL"))
                       (rec t nil nil nil (cdr args) values i))

                      ((&rest &body)
                       (when (or rest? key? aux?)
                         (error "Invalid &REST/&BODY"))
                       (let ((thisarg (cadr args)))
                         (unless (and thisarg (symbolp thisarg))
                           (error "Missing variable name for &REST"))
                         (push `(,thisarg ,values) decls))
                       (rec nil t nil nil (cddr args) values i))

                      (&key
                       (when (or key? aux?)
                         (error "Invalid &KEY"))
                       (rec nil nil t nil (cdr args) values i))

                      (&aux
                       (when aux?
                         (error "Invalid &AUX"))
                       (rec nil nil nil t (cdr args) values i))

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
                       (rec optional? rest? key? aux? (cdr args) values (+ i 1)))))

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
                         (rec nil nil nil nil thisarg sublist 0))))
                    (rec optional? rest? key? aux? (cdr args) values (+ i 1))))))))
        (rec nil nil nil nil args topv 0))
      `(let* ((,topv ,values) ,@(reverse decls))
         ,@body))))

(defmacro destructuring-bind (args values . body)
  (%fn-destruct args values body))

