;;; this file contains helper functions that will be called from Ymacs
;;; to perform certain things, such as evaluating code or getting
;;; symbol completion.

(defpackage :ymacs
  (:use :sl :%))

(in-package :ymacs)

(import '(sl-ffi:defun-js))
(import '(sl::defun-memoize2))
(export '(make-dialog))

(defun grep (list pred)
  (when list
    (if (funcall pred (car list))
        (cons (car list) (grep (cdr list) pred))
        (grep (cdr list) pred))))

(defglobal *handlers* (make-hash))

(defparameter *results* nil)

(defun-js send-ymacs-reply (req-id what value) "
  YMACS.callHooks('onLispResponse', req_id, what, value);
")

(defun-js %send-ymacs-notify (what value) "
  YMACS.callHooks('onLispNotify', what, value);
")

(defun-js make-dialog (width height) "
  return YMACS.makeDialog({ width, height, closable: true, draggable: true });
")

(defun send-ymacs-notify (what value)
  (%send-ymacs-notify (strcat what) value))

(defmacro define-handler (what (&rest args) &body body)
  (let ((name (intern (strcat "EXEC-" what)))
        (pass-args (gensym)))
    `(labels ((,name ,args ,@body))
       (setf (symbol-function ',name) #',name)
       (setf (gethash ,(symbol-name what) *handlers*)
             (lambda (req-id . ,pass-args)
               (make-thread
                (lambda ()
                  (block out
                    (handler-bind
                        ((warning (lambda (x)
                                    (send-ymacs-notify :warning (format nil "~A" x))))
                         (error (lambda (x)
                                  (send-ymacs-notify :error (format nil "~A" x))
                                  (return-from out x))))
                      (let ((ret (apply #',name ,pass-args)))
                        (send-ymacs-reply req-id ,what ret)
                        ret))))))))))

(define-handler :read (pak str)
  (let ((*package* (or (and pak (find-package pak))
                       *package*)))
    (read1-from-string str)))

(define-handler :eval (expr)
  (let ((ret (eval expr)))
    ret))

(defun save-result (val)
  (unless (eq val (car *results*))
    (push val *results*)
    (let ((cell (nthcdr 2 *results*)))
      (when cell (setf (cdr cell) nil)))))

(defun ymacs-print (&rest vals)
  (when vals (save-result (car vals)))
  (cond
    ((null vals)
     "; No value")
    ((cdr vals)
     (format nil "~{  ~A~^~%~}" vals))
    ((stringp (car vals))
     (car vals))
    (t
     (print-object-to-string (car vals)))))

(define-handler :read-eval-print (code)
  (let ((expr (svref (read1-from-string code) 0)))
    (multiple-value-call #'ymacs-print (eval `(let ((,(intern "*") (car *results*))
                                                    (,(intern "**") (cadr *results*))
                                                    (,(intern "***") (caddr *results*)))
                                                ,expr)))))

(define-handler :eval-print (expr)
  (multiple-value-call #'ymacs-print (eval expr)))

(define-handler :compile-file (filename)
  (let ((t1 (get-internal-run-time)))
    (prog1
        (load filename)
      (send-ymacs-notify :message
                         (strcat ";; " filename
                                 " compiled in "
                                 (number-fixed (/ (- (get-internal-run-time) t1) 1000) 3) " s")))))

(define-handler :eval-string (pak str &optional save)
  (let ((*package* (or (and pak (find-package pak))
                       *package*)))
    (let ((val (eval-string str)))
      (when save (save-result val))
      val)))

(define-handler :compile-string (code filename)
  (compile-string code filename))

(defun-memoize2 lev (a b)
  (cond
    ((zerop (length b))
     (length a))
    ((zerop (length a))
     (length b))
    ((eq (elt a 0) (elt b 0))
     (lev (substr a 1)
          (substr b 1)))
    (t
     (1+ (min (lev (substr a 1) b)
              (lev a (substr b 1))
              (lev (substr a 1)
                   (substr b 1)))))))

(labels ((symbol-completion (query all)
           (setf all (mapcar #'symbol-name all))
           (let* ((rx (make-regexp (strcat "^"
                                           (replace-regexp
                                            #/[-_.\/]/g
                                            (quote-regexp (replace-regexp #/\./g query "-"))
                                            "[^-_./]*[-_./]"))
                                   "i"))
                  (matching (grep all (lambda (name)
                                        (regexp-test rx name)))))
             (sort matching
                   (lambda (a b)
                     (< (lev a query)
                        (lev b query)))))))

  (define-handler :list-symbol-completions (query)
    (let (m)
      (cond
        ;; keyword?
        ((setf m (regexp-exec #/^:([^:]*)/ query))
         (let* ((query (elt m 1))
                (comps (symbol-completion query
                                          (as-list
                                           (%interned-symbols
                                            (find-package :keyword))))))
           (mapcar (lambda (x) (strcat ":" x)) comps)))

        ;; fully qualified symbol?
        ((setf m (regexp-exec #/^([^:]*?)(::?)([^:]*)/ query))
         (let* ((pak (find-package (upcase (elt m 1))))
                (sep (elt m 2))
                (external (= 1 (length sep)))
                (query (elt m 3)))
           (when pak
             (mapcar (lambda (x) (strcat (package-name pak) sep x))
                     (symbol-completion query
                                        (as-list
                                         (%accessible-symbols pak external)))))))

        ;; no colon?
        ((regexp-test #/[^:]/ query)
         (symbol-completion query (as-list (%accessible-symbols *package* nil))))

        ;; dunno what to do here, just return empty list
        (t
         nil)))))

(define-handler :list-packages ()
  (apply #'vector (sort (mapcar #'package-name (%list-packages)) #'string<)))

(define-handler :set-package (name)
  (let ((pak (find-package name)))
    (unless pak (error "There's no package named ~S" name))
    (setf *package* (find-package name))))

(defglobal
    *thread*
    (make-thread
     (lambda ()
       (let ((*package* (find-package :sl-user))
             (*read-table* *read-table*))
         (let looop ()
           (%receive *handlers*)
           (looop))))))
