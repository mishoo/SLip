;;; this file contains helper functions that will be called from Ymacs
;;; to perform certain things, such as evaluating code or getting
;;; symbol completion.

(defpackage :ymacs
  (:use :ss))

(in-package :ymacs)

(defun grep (list pred)
  (when list
    (if (funcall pred (car list))
        (cons (car list) (grep (cdr list) pred))
        (grep (cdr list) pred))))

(defglobal *handlers* (make-hash))

(let ((send-reply (%js-eval "
function send_ymacs_reply(req_id, what, value) {
    YMACS.callHooks(\"onLispResponse\", req_id, what, value);
}
")))
  (defun send-ymacs-reply (req-id what value)
    (%js-apply send-reply nil #( req-id what value ))))

(let ((send-notify (%js-eval "
function send_ymacs_notify(what, value) {
    YMACS.callHooks(\"onLispNotify\", what, value);
}
")))
  (defun send-ymacs-notify (what value)
    (%js-apply send-notify nil #( (strcat what) value ))))

(defmacro define-handler (what (&rest args) &body body)
  (let ((name (intern (strcat "EXEC-" what))))
    `(labels ((,name ,args ,@body))
       (set-symbol-function! ',name #',name)
       (hash-set *handlers* ,what
                 (lambda (req-id ,@args)
                   (make-thread
                    (lambda ()
                      (block out
                        (handler-bind
                            ((warning (lambda (x)
                                        (send-ymacs-notify :message (format nil "!WARN: ~A" x))))
                             (error (lambda (x)
                                      (send-ymacs-notify :message (format nil "!ERROR: ~A" x))
                                      (return-from out x))))
                          (let ((ret (,name ,@args)))
                            (send-ymacs-reply req-id ,what ret)
                            ret))))))))))

(define-handler :read (pak str)
  (let ((*package* (or (and pak (%find-package pak t))
                       *package*)))
    (%::read1-from-string str)))

(define-handler :eval (expr)
  (let ((ret (%::eval expr)))
    ret))

(define-handler :eval-print (expr)
  (let ((ret (%::eval expr)))
    (if (stringp ret)
        ret
        (print-object-to-string ret))))

(define-handler :compile-file (filename)
  (let ((t1 (%get-time)))
    (prog1
        (%::load filename)
      (send-ymacs-notify :message
                         (strcat "; " filename
                                 " compiled in "
                                 (number-fixed (/ (- (%get-time) t1) 1000) 3) " s")))))

(define-handler :eval-string (pak str)
  (let ((*package* (or (and pak (%find-package pak t))
                       *package*)))
    (%::eval-string str)))

(labels ((symbol-completion (query all)
           (let* ((rx (make-regexp (strcat "^" (replace-regexp #/[-_.\/]/g
                                                               query
                                                               "[^-_./]*[-_./]"))
                                   "i"))
                  (len (length query))
                  (matching (mapcar #'%symbol-name
                                    (grep all (lambda (sym)
                                                (regexp-test rx (%symbol-name sym)))))))
             (sort matching (lambda (a b)
                              (cond ((string-equal a query) nil)
                                    ((string-equal b query) t)
                                    (t
                                     (< (abs (- (length a) len))
                                        (abs (- (length b) len))))))))))

  (define-handler :list-symbol-completions (query)
    (let (m)
      (cond
        ;; keyword?
        ((setf m (regexp-exec #/^:([^:]*)/ query))
         (let* ((query (elt m 1))
                (comps (symbol-completion query
                                          (as-list
                                           (%interned-symbols
                                            (%find-package :keyword))))))
           (mapcar (lambda (x) (strcat ":" x)) comps)))

        ;; fully qualified symbol?
        ((setf m (regexp-exec #/^([^:]*?)(::?)([^:]*)/ query))
         (let* ((pak (%find-package (upcase (elt m 1)) t))
                (sep (elt m 2))
                (external (= 1 (length sep)))
                (query (elt m 3)))
           (when pak
             (mapcar (lambda (x) (strcat (%package-name pak) sep x))
                     (symbol-completion query
                                        (as-list
                                         (%accessible-symbols pak external)))))))

        ;; no colon?
        ((regexp-test #/[^:]/ query)
         (symbol-completion query (as-list (%accessible-symbols *package* nil))))

        ;; dunno what to do here, just return empty list
        (t
         nil)))))

(defglobal
    *thread*
    (make-thread
     (lambda ()
       (let ((*package* (%find-package :ss-user))
             (*read-table* *read-table*))
         (let looop ()
           (%receive *handlers*)
           (looop))))))
