(in-package :sl)

;; Implementation based on TinyCLOS.  Original license:
;;
;; **********************************************************************
;; Copyright (c) 1992 Xerox Corporation.
;; All Rights Reserved.
;;
;; Use, reproduction, and preparation of derivative works are permitted.
;; Any copy of this software or of any derivative work must include the
;; above copyright notice of Xerox Corporation, this paragraph and the
;; one after it.  Any distribution of this software or derivative works
;; must comply with all applicable United States export control laws.
;;
;; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
;; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
;; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGES.
;; **********************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(call-next-method
          initialize
          object))

(def-efun find-class (sym)
  (%get-symbol-prop sym :class))

(defsetf find-class (sym) (class)
  `(%set-symbol-prop ,sym :class ,class))

(def-efun find-generic (sym)
  (%get-symbol-prop sym :generic))

(defsetf find-generic (sym) (generic)
  `(%set-symbol-prop ,sym :generic ,generic))

(def-emac defclass (name direct-supers direct-slots)
  (unless direct-supers
    (setf direct-supers '(object)))
  `(setf (find-class ',name)
         (make-class ',name (list ,@(mapcar (lambda (name)
                                              `(find-class ',name))
                                            direct-supers)) ',direct-slots)))

(def-emac defgeneric (name)
  (let ((generic (gensym "GENERIC")))
    `(unless (find-generic ',name)
       (let ((,generic (make-generic)))
         (setf (find-generic ',name) ,generic)
         (defun ,name args
           (%apply (%get-entity-proc ,generic) args))))))

(def-efun make-instance (class . initargs)
  (apply #'make (if (symbolp class)
                    (find-class class)
                    class)
         initargs))

(def-emac defmethod (name args &rest body)
  (let ((c-n-m (gensym "CALL-NEXT-METHOD")))
    `(add-method ',name
                 (make-method
                  ;; specializers
                  (list ,@(mapcar (lambda (x)
                                    (if (consp x) `(find-class ',(cadr x)) <top>))
                                  args))
                  (lambda (,c-n-m ,@(mapcar (lambda (x)
                                              (if (consp x) (car x) x))
                                            args))
                    (macrolet ((call-next-method args `(funcall ,',c-n-m ,@args)))
                      ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TinyCLOS

(defun %allocate-instance (class nfields)
  (%allocate-instance-internal class nfields nil))

(defun %allocate-entity (class nfields)
  (%allocate-instance-internal
   class nfields
   (lambda () (error "Entity procedure not set!"))))

;; internal API
(progn

  (defun %allocate-instance-internal (class nfields proc)
    (let* ((obj (%make-object (+ nfields 2)))
           (vec (%object-vector obj)))
      (vector-set vec 0 class)
      (vector-set vec 1 proc)
      obj))

  (defun %instance-class (obj)
    (vector-ref (%object-vector obj) 0))

  (defun %set-instance-class-to-self (obj)
    (vector-set (%object-vector obj) 0 obj))

  (defun %get-entity-proc (obj)
    (vector-ref (%object-vector obj) 1))

  (defun %set-entity-proc (obj proc)
    (vector-set (%object-vector obj) 1 proc))

  (defun %instance-ref (obj index)
    (vector-ref (%object-vector obj) (+ index 2)))

  (defun %instance-set (obj index new-value)
    (vector-set (%object-vector obj) (+ index 2) new-value)))

(defun class-of (x)
  (cond ((%objectp x) (%instance-class x))
        (t
         (console.error x)
         (error "Classes for built-ins later!"))))

;; this version of make is designed only for the basic internal
;; classes; a new version is defined later in terms of the generic
;; functions allocate-instance and initialize.
(defun make (class . initargs)
  (cond ((or (eq class <class>)
             (eq class <entity-class>))
         (let* ((new (%allocate-instance
                      class
                      (length *the-slots-of-a-class*)))
                (dsupers (%getf initargs 'direct-supers '()))
                (dslots (map #'list (%getf initargs 'direct-slots '())))
                (cpl (labels ((rec (sups so-far)
                                (if (not sups)
                                    (nreverse so-far)
                                    (rec (class-direct-supers (car sups))
                                         (cons (car sups) so-far)))))
                       (rec dsupers (list new))))
                (slots (%apply #'append
                               (cons dslots
                                     (map #'class-direct-slots (cdr cpl)))))
                (nfields 0)
                (field-initializers '())
                (allocator (lambda (init)
                             (let ((f nfields))
                               (incf nfields)
                               (setf field-initializers
                                     (cons init field-initializers))
                               (list (lambda (o) (%instance-ref o f))
                                     (lambda (o n) (%instance-set o f n))))))
                (getters-n-setters (map (lambda (s)
                                          (cons (car s) (funcall allocator (lambda ()))))
                                        slots)))

           (slot-set new 'name (%getf initargs 'name))
           (slot-set new 'direct-supers dsupers)
           (slot-set new 'direct-slots dslots)
           (slot-set new 'cpl cpl)
           (slot-set new 'slots slots)
           (slot-set new 'nfields nfields)
           (slot-set new 'field-initializers (nreverse field-initializers))
           (slot-set new 'getters-n-setters getters-n-setters)
           new))

        ((eq class <generic>)
         (let ((new (%allocate-entity class
                                      (length (class-slots class)))))
           (%set-entity-proc new (lambda () (error "No methods defined")))
           (slot-set new 'methods '())
           new))

        ((eq class <method>)
         (let ((new (%allocate-instance class
                                        (length (class-slots class)))))
           (slot-set new 'specializers (%getf initargs 'specializers '()))
           (slot-set new 'procedure (%getf initargs 'procedure '()))
           new))))

(def-efun slot-ref (object slot-name)
  (let* ((info (lookup-slot-info (class-of object) slot-name))
         (getter (car info)))
    (funcall getter object)))

(def-efun slot-set (object slot-name new-value)
  (let* ((info (lookup-slot-info (class-of object) slot-name))
         (setter (cadr info)))
    (funcall setter object new-value)))

(defun lookup-slot-info (class slot-name)
  (let* ((getters-n-setters (if (eq class <class>)
                                *getters-n-setters-for-class*
                                (slot-ref class 'getters-n-setters)))
         (entry (assq slot-name getters-n-setters)))
    (if entry
        (cdr entry)
        (error (strcat "No slot " slot-name)))))

(def-efun class-name (class) (slot-ref class 'name))
(def-efun class-direct-slots (class) (slot-ref class 'direct-slots))
(def-efun class-direct-supers (class) (slot-ref class 'direct-supers))
(def-efun class-slots (class) (slot-ref class 'slots))
(def-efun class-cpl (class) (slot-ref class 'cpl))

(def-efun generic-methods (generic) (slot-ref generic 'methods))

(def-efun method-specializers (method) (slot-ref method 'specializers))
(def-efun method-procedure (method) (slot-ref method 'procedure))

(defglobal *the-slots-of-a-class* '(name
                                    direct-supers
                                    direct-slots
                                    cpl
                                    slots
                                    nfields
                                    field-initializers
                                    getters-n-setters))

(defglobal *getters-n-setters-for-class*
    (labels ((make-em (slot-name index)
               (list slot-name
                     (lambda (obj) (%instance-ref obj index))
                     (lambda (obj new-value) (%instance-set obj index new-value))))
             (rec (rest index)
               (when rest
                 (cons (make-em (car rest) index)
                       (rec (cdr rest) (+ index 1))))))
      (rec *the-slots-of-a-class* 0)))

(defglobal <class> (%allocate-instance nil (length *the-slots-of-a-class*)))
(setf (find-class 'class) <class>)
(%set-instance-class-to-self <class>)

(defglobal <top> (make <class>
                       'direct-supers nil
                       'direct-slots nil))
(setf (find-class 'top) <top>)

(defglobal <object> (make <class>
                          'direct-supers (list <top>)
                          'direct-slots nil))
(setf (find-class 'object) <object>)

(slot-set <class> 'name 'class)
(slot-set <class> 'direct-supers (list <object>))
(slot-set <class> 'direct-slots (map #'list *the-slots-of-a-class*))
(slot-set <class> 'cpl (list <class> <object> <top>))
(slot-set <class> 'slots (map #'list *the-slots-of-a-class*))
(slot-set <class> 'nfields (length *the-slots-of-a-class*))
(slot-set <class> 'field-initializers (map (lambda (s)
                                             (lambda ()))
                                           *the-slots-of-a-class*))
(slot-set <class> 'getters-n-setters '())

(defglobal <procedure-class> (make <class>
                                   'direct-supers (list <class>)
                                   'direct-slots nil))
(setf (find-class 'procedure-class) <procedure-class>)
(defglobal <entity-class> (make <class>
                                'direct-supers (list <procedure-class>)
                                'direct-slots nil))
(setf (find-class 'entity-class) <entity-class>)
(defglobal <generic> (make <entity-class>
                           'direct-supers (list <object>)
                           'direct-slots '(methods)))
(setf (find-class 'generic) <generic>)
(defglobal <method> (make <class>
                          'direct-supers (list <object>)
                          'direct-slots '(specializers procedure)))
(setf (find-class 'method) <method>)

;;;
;;; Compute class precedence list
;;;
(defun compute-simple-cpl (class)
  (labels ((chase (supers)
             (append supers (chase-1 supers)))
           (chase-1 (supers)
             (%apply #'append
                     (mapcar #'chase
                             (mapcar #'class-direct-supers supers)))))
    (remove-duplicates (cons class (chase (class-direct-supers class)))
                       :from-end t)))

;;;
;;; API
;;;

(def-efun make-class (name direct-supers direct-slots)
  (make <class>
        'name name
        'direct-supers direct-supers
        'direct-slots direct-slots))
(def-efun make-generic () (make <generic>))
(def-efun make-method (specializers procedure)
  (make <method>
        'specializers specializers
        'procedure procedure))

(def-efun is-a (obj class)
  (when (symbolp class)
    (setf class (find-class class)))
  (%memq class (class-cpl (class-of obj))))

;;
;; Initialization protocol
;;
(defgeneric initialize)

;;
;; Instance structure protocol
;;
(defgeneric allocate-instance)
(defgeneric compute-getter-and-setter)

;;
;; Class initialization protocol
;;
(defgeneric compute-cpl)
(defgeneric compute-slots)

;;
;; Generic invocation protocol
;;
(defgeneric compute-apply-generic)
(defgeneric compute-methods)
(defgeneric compute-method-more-specific?)
(defgeneric compute-apply-methods)

;;
;; Bootstrap generic functions
;;
(defglobal *generic-invocation-generics* (mapcar #'find-generic
                                                 (list 'compute-apply-generic
                                                       'compute-methods
                                                       'compute-method-more-specific?
                                                       'compute-apply-methods)))

(def-efun add-method (generic method)
  (setq generic (find-generic generic))
  (slot-set generic
            'methods
            (cons method
                  (collect-if (lambda (m)
                                (not (every #'eq
                                            (method-specializers m)
                                            (method-specializers method))))
                              (slot-ref generic 'methods))))
  (%set-entity-proc generic (compute-apply-generic generic)))

(%set-entity-proc (find-generic 'compute-apply-generic)
                  (lambda (generic)
                    (let ((method (car (generic-methods generic))))
                      ((method-procedure method) nil generic))))

(add-method
 'compute-apply-generic
 (make-method (list <generic>)
              (lambda (call-next-method generic)
                (lambda args
                  (if (and (%memq generic *generic-invocation-generics*)
                           (%memq (car args) *generic-invocation-generics*))
                      (%apply (method-procedure
                               (car (last (generic-methods generic))))
                              (cons nil args))
                      ((compute-apply-methods generic)
                       ((compute-methods generic) args)
                       args))))))

(add-method
 'compute-methods
 (make-method (list <generic>)
              (lambda (call-next-method generic)
                (lambda (args)
                  (let ((applicable
                         (collect-if (lambda (method)
                                       (every #'applicable?
                                              (method-specializers method)
                                              args))
                                     (generic-methods generic))))
                    (sort applicable
                          (lambda (m1 m2)
                            ((compute-method-more-specific? generic)
                             m1
                             m2
                             args))))))))

(add-method
 'compute-method-more-specific?
 (make-method (list <generic>)
              (lambda (call-next-method generic)
                (lambda (m1 m2 args)
                  (labels ((looop (specls1 specls2 args)
                             (cond ((and (not specls1) (not specls2))
                                    (error "Two methods are equally specific."))
                                   ((or (not specls1) (not specls2))
                                    (error "Two methods have a different number of specializers."))
                                   ((not args)
                                    (error "Fewer arguments than specializers"))
                                   (t (let ((c1 (car specls1))
                                            (c2 (car specls2))
                                            (arg (car args)))
                                        (if (eq c1 c2)
                                            (looop (cdr specls1)
                                                   (cdr specls2)
                                                   (cdr args))
                                            (more-specific? c1 c2 arg)))))))
                    (looop (method-specializers m1)
                           (method-specializers m2)
                           args))))))

(add-method
 'compute-apply-methods
 (make-method (list <generic>)
              (lambda (call-next-method generic)
                (lambda (methods args)
                  (let (one-step)
                    (setf one-step (lambda (tail)
                                     (lambda ()
                                       (if (not tail)
                                           (error "No applicable methods/next methods")
                                           (%apply (method-procedure (car tail))
                                                   (cons (funcall one-step (cdr tail)) args))))))
                    ((funcall one-step methods)))))))

(defun applicable? (c arg)
  (%memq c (class-cpl (class-of arg))))

(defun more-specific? (c1 c2 arg)
  (%memq c2 (%memq c1 (class-cpl (class-of arg)))))

(add-method
 'initialize
 (make-method (list <object>)
              (lambda (call-next-method object initargs)
                (let looop ((arg initargs))
                     (when arg
                       (slot-set object (car arg) (cadr arg))
                       (looop (cddr arg))))
                object)))

(add-method
 'initialize
 (make-method (list <class>)
              (lambda (call-next-method class initargs)
                (slot-set class 'direct-supers
                          (%getf initargs 'direct-supers '()))
                (slot-set class 'direct-slots
                          (map (lambda (s)
                                 (if (consp s) s (list s)))
                               (%getf initargs 'direct-slots '())))
                (slot-set class 'cpl (compute-cpl class))
                (slot-set class 'slots (compute-slots class))
                (slot-set class 'name (%getf initargs 'name))
                (let* ((nfields 0)
                       (field-initializers '())
                       (allocator
                        (lambda (init)
                          (let ((f nfields))
                            (incf nfields)
                            (setf field-initializers (cons init field-initializers))
                            (list (lambda (o) (%instance-ref o f))
                                  (lambda (o n) (%instance-set o f n))))))
                       (getters-n-setters
                        (map (lambda (slot)
                               (cons (car slot)
                                     (compute-getter-and-setter class slot allocator)))
                             (slot-ref class 'slots))))
                  (slot-set class 'nfields nfields)
                  (slot-set class 'field-initializers field-initializers)
                  (slot-set class 'getters-n-setters getters-n-setters)))))

(add-method
 'initialize
 (make-method (list <generic>)
              (lambda (call-next-method generic initargs)
                (slot-set generic 'methods '()))))

(add-method
 'initialize
 (make-method (list <method>)
              (lambda (call-next-method method initargs)
                (slot-set method 'specializers (%getf initargs 'specializers '()))
                (slot-set method 'procedure (%getf initargs 'procedure '())))))

(add-method
 'allocate-instance
 (make-method (list <class>)
              (lambda (call-next-method class)
                (let* ((field-initializers (slot-ref class 'field-initializers))
                       (new (%allocate-instance class (length field-initializers))))
                  (labels ((looop (n inits)
                             (if (consp inits)
                                 (progn
                                   (%instance-set new n ((car inits)))
                                   (looop (+ n 1) (cdr inits)))
                                 new)))
                    (looop 0 field-initializers))))))

(add-method
 'allocate-instance
 (make-method (list <entity-class>)
              (lambda (call-next-method class)
                (let* ((field-initializers (slot-ref class 'field-initializers))
                       (new (%allocate-entity class (length field-initializers))))
                  (labels ((looop (n inits)
                             (if (consp inits)
                                 (progn
                                   (%instance-set new n ((car inits)))
                                   (looop (+ n 1) (cdr inits)))
                                 new)))
                    (looop 0 field-initializers))))))

(add-method
 'compute-cpl
 (make-method (list <class>)
              (lambda (call-next-method class)
                (compute-simple-cpl class))))

(add-method
 'compute-slots
 (make-method (list <class>)
              (lambda (call-next-method class)
                (labels ((collect (to-process result)
                           (if (not to-process)
                               (nreverse result)
                               (let* ((current (car to-process))
                                      (name (car current))
                                      (others '())
                                      (remaining-to-process
                                       (collect-if (lambda (o)
                                                     (if (eq (car o) name)
                                                         (progn (setf others (cons o others))
                                                                nil)
                                                         t))
                                                   (cdr to-process))))
                                 (collect remaining-to-process
                                          (cons (append current (%apply #'append (map #'cdr others)))
                                                result))))))
                  (collect (%apply #'append (map #'class-direct-slots
                                                 (class-cpl class)))
                           '())))))

(add-method
 'compute-getter-and-setter
 (make-method (list <class>)
              (lambda (call-next-method class slot allocator)
                (funcall allocator (lambda ())))))



;; redefine make to call initialize method
(defun make (class . initargs)
  (let ((instance (allocate-instance class)))
    (initialize instance initargs)
    instance))

(defglobal <primitive> (defclass primitive (class) ()))

(defglobal <cons> (defclass cons (primitive) ()))
(defglobal <null> (defclass null (primitive) ()))
(defglobal <symbol> (defclass symbol (primitive) ()))
(defglobal <package> (defclass package (primitive) ()))
(defglobal <regexp> (defclass regexp (primitive) ()))
(defglobal <hash> (defclass hash (primitive) ()))
(defglobal <char> (defclass char (primitive) ()))
(defglobal <function> (defclass function (procedure-class) ()))
(defglobal <number> (defclass number (primitive) ()))
(defglobal <vector> (defclass vector (primitive) ()))
(defglobal <string> (defclass string (primitive) ()))
(defglobal <thread> (defclass thread (primitive) ()))
(defglobal <stream> (defclass stream (primitive) ()))
(defglobal <input-stream> (defclass input-stream (stream) ()))
(defglobal <output-stream> (defclass output-stream (stream) ()))
(defglobal <unknown> (defclass unknown-class (primitive) ()))

(def-efun class-of (x)
  (cond ((%objectp x) (%instance-class x))
        ((consp x) <cons>)
        ((not x) <null>)
        ((symbolp x) <symbol>)
        ((hashp x) <hash>)
        ((charp x) <char>)
        ((packagep x) <package>)
        ((functionp x) <function>)
        ((numberp x) <number>)
        ((vectorp x) <vector>)
        ((stringp x) <string>)
        ((regexpp x) <regexp>)
        ((threadp x) <thread>)
        ((%input-stream-p x) <input-stream>)
        ((%output-stream-p x) <output-stream>)
        (t <unknown>)))

(export '(class
          char
          object
          hash
          generic
          method
          cons
          null
          symbol
          package
          regexp
          function
          number
          vector
          string
          thread
          stream
          input-stream
          output-stream
          unknown-class))
