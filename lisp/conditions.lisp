;;;; modeled after conditions.lisp from project Sacla
;;;; http://homepage1.nifty.com/bmonkey/lisp/sacla/html/lisp/condition.lisp.html

(in-package :sl)

(export '(condition
          simple-condition
          simple-condition-format-control
          simple-condition-format-arguments
          simple-warning
          simple-error
          type-error
          type-error-datum
          type-error-expected-type
          primitive-error
          serious-condition
          warning
          signal
          handler-case
          handler-bind
          ignore-errors
          make-condition
          check-type))

(defpackage :sl-cond
  (:use :sl :%))

(in-package :sl-cond)

(defclass condition () (datum))
(defclass simple-condition (condition)
  ((format-control :initarg :format-control
                   :accessor simple-condition-format-control)
   (format-arguments :initarg :format-arguments
                     :initform nil
                     :accessor simple-condition-format-arguments)))
(defclass serious-condition (condition) ())
(defclass error (serious-condition) ())
(defclass type-error (error)
  ((datum :initarg :datum :accessor type-error-datum)
   (symbol :initarg :symbol :initform nil :accessor type-error-symbol)
   (expected-type :initarg :expected-type :accessor type-error-expected-type)))
(defclass simple-error (simple-condition error) ())
(defclass primitive-error (simple-error) ())
(defclass warning (condition) ())
(defclass simple-warning (simple-condition warning) ())

(defmethod print-object ((c simple-condition) (out output-stream))
  (let ((format-control (slot-value c 'format-control))
        (format-arguments (slot-value c 'format-arguments)))
    (strcat
     "<" (class-name (class-of c)) ": "
     (apply #'format out format-control format-arguments)
     ">")))

(defmethod print-object ((c type-error) (out output-stream))
  (let ((sym (type-error-symbol c)))
    (cond
      (sym
       (format out "TYPE-ERROR: the value of ~S (~S) is not ~A"
               (type-error-symbol c)
               (type-error-datum c)
               (type-error-expected-type c)))
      (t
       (format out "TYPE-ERROR: the value ~S is not ~A"
               (type-error-datum c)
               (type-error-expected-type c))))))

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

(defmacro with-append-list ((var append &key (tail (gensym))) &body body)
  `(let (,var ,tail)
     (flet ((,append (x)
              (setf ,tail
                    (last (if ,tail
                              (setf (cdr ,tail) x)
                              (setf ,var x))))))
       ,@body)))

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
  (let ((has-no-error (%assq :no-error clauses)))
    (if has-no-error
        (let ((normal-return (gensym "NORMAL-RETURN-"))
              (error-return (gensym "ERROR-RETURN-")))
          `(block ,error-return
             (block ,normal-return
               (return-from ,error-return
                 (handler-case (return-from ,normal-return
                                 (multiple-value-call
                                  (lambda ,(cadr has-no-error)
                                    (pop *handler-clusters*)
                                    (locally ,@(cddr has-no-error)))
                                  ,form))
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
     (error (condition)
       (values nil condition))))

(defun %condition (datum arguments default-class)
  (cond ((typep datum 'condition) datum)
        ((or (stringp datum)
             (functionp datum))
         (make-instance default-class
                        :format-control datum
                        :format-arguments arguments))
        (t
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
  (error 'primitive-error :format-control fmt :format-arguments arguments))

(defun warn (datum . args)
  (let ((warning (%condition datum args 'simple-warning)))
    (signal warning)))

(defun make-condition (datum &rest args)
  (%condition datum args 'simple-condition))

(defmacro check-type (place typespec &optional string)
  (cond
    ((and (symbolp place)
          (%:safe-atom-p place))
     `(assert (typep ,place ',typespec)
              'type-error
              :datum ,place
              :symbol ',place
              :expected-type ,(or string `',typespec)))
    (t
     (let ((_place (gensym)))
       `(let ((,_place ,place))
          (assert (typep ,_place ',typespec)
                  'type-error
                  :datum ,_place
                  :expected-type ,(or string `',typespec)))))))