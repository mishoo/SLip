;;;; Nothing to see here, move along. I'm just testing ideas. It's not
;;;; included in the compilation.

(in-package :sl)

(defpackage :sl-compiler
  (:use :sl :%))

(in-package :sl-compiler)

(defvar *global-env*)
(defconstant *none* (list nil))

(defun make-global-env ()
  (let ((env (make-compiler-env)))
    (setf (gethash :func env) nil)
    env))

;; for top-level forms
(defun ir (form &key (env (make-global-env)) (val? t))
  (let ((*global-env* env)
        (*compiler-env* env))
    (irexp form val?)))

(defun irexp (form val?)
  (labels
      ((arg-count (lst min &optional max)
         (let ((len (length lst)))
           (if max
               (assert (<= min (1- len) max)
                       "~A should take between ~D and ~D arguments"
                       (car lst) min max)
               (assert (<= min (1- len))
                       "~A should take at least ~D arguments"
                       (car lst) min)))))
    (cond
      ((symbolp form)
       (cond
         ((eq form nil) (irexp-const nil val?))
         ((eq form t) (irexp-const t val?))
         ((keywordp form) (irexp-const form val?))
         (t (irexp-var-ref form val? :ref t))))
      ((atom form)
       (irexp-const form val?))
      (t
       (case (car form)
         ((quote)
          (arg-count form 1 1)
          (irexp-const (cadr form) val?))
         ((progn)
          (irexp-progn (cdr form) val?))
         ((locally)
          (irexp-decl-seq (cdr form) val?))
         ((prog1)
          (arg-count form 1)
          (irexp-prog1 (cadr form) (cddr form) val?))
         ((progv)
          (arg-count form 2)
          (irexp-progv (cadr form) (caddr form) (cdddr form) val?))
         ((multiple-value-prog1)
          (arg-count form 1)
          (irexp-multiple-value-prog1 (cadr form) (cddr form) val?))
         ((%pop)
          (arg-count form 1 1)
          (irexp-pop (cadr form) val?))
         ((setq)
          (irexp-setq (cdr form) val?))
         ((if)
          (arg-count form 2 3)
          (irexp-if (cadr form) (caddr form) (caddr form) val?))
         ((or)
          (irexp-or (cdr form) val?))
         ((c/c)
          (arg-count form 0 0)
          (when val? '(c/c)))
         ((let)
          (arg-count form 1)
          (irexp-let (cadr form) (cddr form) val?))
         ((let*)
          (arg-count form 1)
          (irexp-let* (cadr form) (cddr form) val?))
         ((multiple-value-bind)
          (arg-count form 2)
          (irexp-mvb (cadr form) (caddr form) (cdddr form) val?))
         ((values)
          (irexp-values (cdr form) val?))
         ((labels)
          (irexp-labels (cadr form) (cddr form) val?))
         ((flets)
          (irexp-flets (cadr form) (cddr form) val?))

         (t
          `(,(car form) ,@(irexp-arguments (cdr form)))))))))

(defun irexp-arguments (args)
  (mapcar (lambda (arg)
            (irexp arg t))
          args))

(defun irexp-const (const val?)
  (when val?
    `(IR CONSTANT ,const)))

(defun binding-frame (binding)
  (car binding))

(defun binding-index (binding)
  (cadr binding))

(defun binding-data (binding)
  (cddr binding))

(defun binding-prop (binding prop &optional default)
  (getf (binding-data binding) prop default))

(defun (setf binding-prop) (value binding prop &optional default)
  (declare (ignore default))
  (let rec ((lst (cddr binding))
            (prev (cdr binding)))
    (cond
      ((null lst)
       (setf (cdr prev) (list prop value))
       value)
      ((eq (car lst) prop)
       (setf (cadr lst) value))
      (t (rec (cddr lst) (cdr lst))))))

(defun irexp-var-ref (name val? &key ref set)
  (let ((binding (find-var-in-compiler-env name)))
    (unless binding
      (return-from irexp-var-ref
        (when val? `(IR GLOBAL-VARIABLE ,name))))
    (let* ((smac (when binding (binding-prop binding :smac *none*))))
      (cond
        ((eq smac *none*)
         (cond
           ((binding-prop binding :special)
            `(IR SPECIAL-VARIABLE ,name ,(binding-data binding)))
           (val?
            (when ref
              (incf (binding-prop binding :referenced 0)))
            (when set
              (incf (binding-prop binding :assigned 0)))
            `(IR LEXICAL-VARIABLE ,name ,(binding-data binding)))))
        (val?
         (irexp smac t))))))

(defun irexp-progn (forms val?)
  (labels ((do-seq (forms ret)
             (cond
               ((cdr forms)
                (do-seq (cdr forms)
                        (cons (irexp (car forms) nil)
                              ret)))
               (t
                (nreconc ret (list (irexp (car forms) val?)))))))
    (cond
      ((null forms)
       (irexp-const nil val?))
      ((cdr forms)
       `(progn ,@(do-seq forms nil)))
      (t
       (irexp (car forms) val?)))))

(defun irexp-decl-seq (forms val?)
  (let ((env *compiler-env*))
    (with-declarations forms
      (declare-locally-special)
      (irexp-maybe-new-env env nil forms val?))))

(defun irexp-maybe-new-env (env type forms val?)
  (cond
    ((eq env *compiler-env*)
     (irexp-progn forms val?))
    (t
     (let ((*compiler-env* env))
       `(IR ENV ,type ,env ,(irexp-progn forms val?))))))

(defun irexp-prog1 (first rest val?)
  (cond
    ((not val?)
     (irexp-progn (list* first rest) nil))
    (rest
     `(prog1 ,(irexp first t)
        ,(irexp-progn rest nil)))
    (t
     (irexp first t))))

(defun irexp-progv (names values body val?)
  (cond
    ((and body names)
     `(progv ,(irexp names t)
             ,(irexp values t)
             ,(irexp-progn body val?)))
    (t
     (irexp-progn (append values body) val?))))

(defun irexp-multiple-value-prog1 (first rest val?)
  (cond
    ((not val?)
     (irexp-progn (list* first rest) nil))
    (rest
     `(multiple-value-prog1
          ,(irexp first t)
        ,(irexp-progn rest nil)))
    (t
     (irexp first t))))

(defun irexp-pop (name val?)
  (assert (symbolp name) "%POP expects a symbol, got: ~S" name)
  `(%pop ,(irexp-var-ref name t :set t)))

(defun irexp-setq (forms val?)
  (assert (evenp (length forms)) "Odd number of forms in SETQ")
  `(setq ,@(sl::with-collectors (result)
             (let rec ((forms forms))
               (when forms
                 (result (irexp-var-ref (pop forms) t :set t))
                 (result (irexp (pop forms) t))
                 (rec forms)))
             result)))

(defun irexp-let (definitions body val?)
  (cond
    ((null definitions)
     (irexp-decl-seq body val?))
    ((null body)
     (irexp-progn (mapcar #'cadr definitions) val?))
    (t
     (let ((env *compiler-env*))
       (with-declarations body
         (sl::with-collectors (names inits)
           (let rec ((defs definitions))
             (when defs
               (cond
                 ((consp (car defs))
                  (names (caar defs))
                  (inits (irexp (cadar defs) t)))
                 (t
                  (names (car defs))
                  (inits nil)))
               (rec (cdr defs))))
           (setf env (extend-compiler-env
                      (list :lex (map2-vector #'maybe-special names inits))
                      env))
           (declare-locally-special :except names)
           (irexp-maybe-new-env env 'LET body val?)))))))

(defun irexp-let* (definitions body val?)
  (cond
    ((null definitions)
     (irexp-decl-seq body val?))
    (t
     (let ((env *compiler-env*)
           (frame (vector)))
       (with-declarations body
         (sl::with-collectors (names inits)
           (let rec ((defs definitions))
             (when defs
               (cond
                 ((consp (car defs))
                  (names (caar defs))
                  (inits (cadar defs)))
                 (t
                  (names (car defs))
                  (inits nil)))
               (rec (cdr defs))))
           (setq env (extend-compiler-env (list :lex frame) env))
           (map2 (lambda (name init)
                   (vector-push-extend
                    (maybe-special name (with-env (irexp init t)))
                    frame))
                 names inits)
           (declare-locally-special :except names)
           (irexp-maybe-new-env env 'LET* body val?)))))))

(defun irexp-if (pred then else val?)
  (cond
    ((always-true-p pred)
     (irexp then val?))
    ((always-false-p pred)
     (irexp else val?))
    ((and (consp pred)
          ;; XXX BUG: should check if NOT or NULL are local functions
          (%memq (car pred) '(not null)))
     (irexp-if (cadr pred) else then val?))
    (t
     `(if ,(irexp pred t)
          ,(irexp then val?)
          ,(irexp else val?)))))

(defun irexp-or (exps val?)
  (cond
    ((null exps)
     (irexp-const nil val?))
    ((cdr exps)
     (cond
       ((always-true-p (car exps))
        (irexp (car exps) val?))
       ((always-false-p (car exps))
        (irexp-or (cdr exps) val?))
       (t
        `(or ,(irexp (car exps) t)
             ,@(irexp-or (cdr exps) val?)))))
    (t
     (irexp (car exps) val?))))

(defun irexp-mvb (names values-form body val?)
  (cond
    ((null body)
     (irexp-progn (list values-form nil) val?))
    (names
     (let ((env *compiler-env*))
       (with-declarations body
         `(IR MVB ,(irexp values-form t)))))
    (t
     `(progn
        ,(irexp values-form nil)
        ,(irexp-decl-seq body val?)))))

(defun irexp-values (forms val?)
  (cond
    (val?
     `(values ,@(irexp-arguments forms)))
    (t
     (irexp-progn forms nil))))

(defun irexp-labels (definitions body val?)
  (cond
    ((and definitions body)
     (let ((env *compiler-env*)
           (frame (mapcar (lambda (fun)
                            (let ((name (car fun))
                                  (args (cadr fun))
                                  (body (cddr fun)))
                              (list name :func
                                    :lambda-list (parse-lambda-list args)
                                    :lambda-body body)))
                          definitions)))
       (setq env (extend-compiler-env (list :lex (as-vector frame)) env))
       (foreach frame
         (lambda (fun)))))
    (body
     (irexp-decl-seq body val?))))