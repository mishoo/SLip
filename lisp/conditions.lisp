;;;; modeled after conditions.lisp from project Sacla
;;;; http://homepage1.nifty.com/bmonkey/lisp/sacla/html/lisp/condition.lisp.html

(in-package :sl)

(export '(condition
          simple-condition
          simple-warning
          simple-error
          serious-condition
          warning
          signal
          handler-case
          handler-bind
          ignore-errors))

(defun typep (obj type)
  (if (is-a obj type) t nil))

(defclass condition () (datum))
(defclass simple-condition (condition) (format-control format-args))
(defclass serious-condition (condition) ())
(defclass error (serious-condition) ())
(defclass simple-error (simple-condition error) ())
(defclass primitive-error (simple-error) ())
(defclass warning (condition) ())
(defclass simple-warning (simple-condition warning) ())

(defglobal <condition> (find-class 'condition))

(defmethod print-object ((c simple-condition) (out output-stream))
  (apply #'format out
         (strcat "<~A: "
                 (slot-ref c 'format-control)
                 ">")
         (class-name (class-of c))
         (slot-ref c 'format-args)))

(defparameter *handler-clusters* '())
(defparameter *restart-clusters* '())

(defmacro handler-bind (bindings &body body)
  `(let ((*handler-clusters*
          (cons (list ,@(mapcar
                         (lambda (binding)
                           (destructuring-bind (type handler) binding
                             `(cons ',type ,handler)))
                         bindings))
                *handler-clusters*)))
     ,@body))

(defun handler-case-bindings (block-tag condition-variable clauses)
  (with-append-list (body body-add)
    (let ((bindings '()))
      (let looop ((clauses clauses))
        (when clauses
          (let ((clause (car clauses))
                (clause-tag (gensym)))
            (destructuring-bind (typespec (&optional var) &body rest) clause
              (push `(,typespec (lambda (temp)
                                  ,(when var
                                     `(setq ,condition-variable temp))
                                  (go ,clause-tag)))
                    bindings)
              (body-add `(,clause-tag
                          (return-from
                              ,block-tag
                            (let ,(when var `((,var ,condition-variable)))
                              ,@rest))))))
          (looop (cdr clauses))))
      (cons (nreverse bindings) body))))

(defmacro handler-case (form &rest clauses)
  (let ((has-no-error (assq :no-error clauses)))
    (if has-no-error
        (let ((normal-return (gensym "NORMAL-RETURN-"))
              (error-return (gensym "ERROR-RETURN-")))
          `(block ,error-return
             (block ,normal-return
               (return-from ,error-return
                 (handler-case (return-from ,normal-return ,form)
                   ,@(remove has-no-error clauses))))))
        (let ((block-tag (gensym))
              (condition (gensym)))
          (destructuring-bind
              (bindings . body)
              (handler-case-bindings block-tag condition clauses)
            `(block ,block-tag
               (let ((,condition nil))
                 (tagbody
                  (handler-bind ,bindings
                    (return-from ,block-tag ,form))
                  ,@body))))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition))))

(defun existing-condition-name? (name)
  (let ((class (find-class name)))
    (and class (%memq <condition> (class-cpl class)))))

(defun %condition (datum arguments default-class)
  (cond ((typep datum 'condition) datum)
        ((typep datum 'string) (make-instance default-class
                                              'format-control datum
                                              'format-args arguments))
        ((existing-condition-name? datum)
         (apply #'make-instance datum arguments))))

(defun signal (datum . arguments)
  (let ((condition (%condition datum arguments 'simple-condition))
        (*handler-clusters* *handler-clusters*))
    (tagbody
     t0
     (when *handler-clusters*
       (let looop ((cluster (pop *handler-clusters*)))
         (when cluster
           (let ((handler (car cluster)))
             (when (typep condition (car handler))
               (funcall (cdr handler) condition)))
           (looop (cdr cluster))))
       (go t0)))))

(defun error (datum . arguments)
  (let ((condition (%condition datum arguments 'simple-error)))
    (signal condition)
    ;; XXX: no debugger
    (%error condition)))

(defun primitive-error (fmt . arguments)
  (error 'primitive-error 'format-control fmt 'format-args arguments))

(defun warn (datum . args)
  (let ((warning (%condition datum args 'simple-warning)))
    (signal warning)))
