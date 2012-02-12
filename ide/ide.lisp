(defpackage :ymacs
  (:use :ss))

(in-package :ymacs)

(defglobal *handlers* (make-hash))

(let ((send-reply (%js-eval "
function send_ymacs_reply(req_id, what, value) {
    YMACS.callHooks(\"onLispResponse\", req_id, what, value);
}
")))
  (defun send-ymacs-reply (req-id what value)
    (%js-apply send-reply nil #( req-id what value ))))

(defmacro define-handler (what (&rest args) &body body)
  (let ((name (intern (strcat "EXEC-" what))))
    `(labels ((,name (req-id ,@args)
                (let ((ret (progn ,@body)))
                  (send-ymacs-reply req-id ,what ret)
                  ret)))
       (hash-set *handlers* ,what #',name))))

(define-handler :read (str)
  (%::read1-from-string str))

(define-handler :eval (expr)
  (%::eval expr))

(define-handler :eval-string (str)
  (%::eval-string str))

(defglobal *thread*
    (make-thread
     (lambda ()
       (let ((*package* *package*)
             (*read-table* *read-table*))
         (let looop ()
              (%receive *handlers*)
              (looop))))))
