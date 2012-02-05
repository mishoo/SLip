(in-package :ss)

;; implementation based on TinyCLOS:
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

(defun find-class (sym)
  (%get-symbol-prop sym :class))

(defsetf find-class (sym) (class)
  `(%set-symbol-prop ,sym :class ,class))

(defmacro defclass (name direct-supers direct-slots)
  `(setf (find-class ',name)
         (make-class (list ,@(mapcar (lambda (name)
                                       `(find-class ',name))
                                     direct-supers)) ',direct-slots)))

(defmacro defgeneric (name)
  `(set-symbol-function! ',name (make-generic)))

(defmacro defmethod (name args &rest body)
  (let ((c-n-m (gensym "CALL-NEXT-METHOD")))
    `(add-method #',name
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

(defun getl (list item . default)
  (let scan ((tail list))
       (if tail
           (if (eq (car tail) item)
               (cadr tail)
               (scan (cddr tail)))
           (if default (car default)))))

(defun %allocate-instance (class nfields)
  (%allocate-instance-internal
   class
   t
   (lambda args
     (error "An instance isn't a procedure -- can't apply it."))
   nfields))

(defun %allocate-entity (class nfields)
  (%allocate-instance-internal
   class
   nil
   (lambda args
     (error "Tried to call an entity before its proc is set."))
   nfields))

;; internal API
(labels ((get-vector (closure)
           (%instance-vector closure)))

  (defun %allocate-instance-internal (class lock proc nfields)
    (let* ((vector (make-vector (+ nfields 3) nil))
           (closure (lambda args
                      (%apply (vector-ref vector 0) args))))
      (%set-instance-vector closure vector)
      (vector-set vector 0 proc)
      (vector-set vector 1 lock)
      (vector-set vector 2 class)
      closure))

  (defun %instance? (x)
    (if (get-vector x) t nil))

  (defun %instance-class (closure)
    (let ((vector (get-vector closure)))
      (vector-ref vector 2)))

  (defun %set-instance-class-to-self (closure)
    (let ((vector (get-vector closure)))
      (vector-set vector 2 closure)))

  (defun %set-instance-proc (closure proc)
    (let ((vector (get-vector closure)))
      (if (vector-ref vector 1)
          (error "Can't set procedure of instance.")
          (vector-set vector 0 proc))))

  (defun %instance-ref (closure index)
    (let ((vector (get-vector closure)))
      (vector-ref vector (+ index 3))))

  (defun %instance-set (closure index new-value)
    (let ((vector (get-vector closure)))
      (vector-set vector (+ index 3) new-value))))

(defun class-of (x)
  (cond ((%instance? x) (%instance-class x))
        (t
         (console.error x)
         (error "Classes for built-ins later!"))))

(defun make (class . initargs)
  (cond ((or (eq class <class>)
             (eq class <entity-class>))
         (let* ((new (%allocate-instance
                      class
                      (length *the-slots-of-a-class*)))
                (dsupers (getl initargs 'direct-supers '()))
                (dslots (map #'list (getl initargs 'direct-slots '())))
                (cpl (labels ((rec (sups so-far)
                                (if (not sups)
                                    (reverse so-far)
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
                                          (cons (car s) (funcall allocator (lambda () '()))))
                                        slots)))

           (slot-set new 'direct-supers dsupers)
           (slot-set new 'direct-slots dslots)
           (slot-set new 'cpl cpl)
           (slot-set new 'slots slots)
           (slot-set new 'nfields nfields)
           (slot-set new 'field-initializers (reverse field-initializers))
           (slot-set new 'getters-n-setters getters-n-setters)
           new))

        ((eq class <generic>)
         (let ((new (%allocate-entity class
                                      (length (class-slots class)))))
           (slot-set new 'methods '())
           new))

        ((eq class <method>)
         (let ((new (%allocate-instance class
                                        (length (class-slots class)))))
           (slot-set new 'specializers (getl initargs 'specializers '()))
           (slot-set new 'procedure (getl initargs 'procedure '()))
           new))))

(defun slot-ref (object slot-name)
  (let* ((info (lookup-slot-info (class-of object) slot-name))
         (getter (car info)))
    (funcall getter object)))

(defun slot-set (object slot-name new-value)
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

(defun class-direct-slots (class) (slot-ref class 'direct-slots))
(defun class-direct-supers (class) (slot-ref class 'direct-supers))
(defun class-slots (class) (slot-ref class 'slots))
(defun class-cpl (class) (slot-ref class 'cpl))

(defun generic-methods (generic) (slot-ref generic 'methods))

(defun method-specializers (method) (slot-ref method 'specializers))
(defun method-procedure (method) (slot-ref method 'procedure))

(defparameter *the-slots-of-a-class* '(direct-supers
                                       direct-slots
                                       cpl
                                       slots
                                       nfields
                                       field-initializers
                                       getters-n-setters))

(labels ((make-em (slot-name index)
           (list slot-name
                 (lambda (obj) (%instance-ref obj index))
                 (lambda (obj new-value) (%instance-set obj index new-value)))))
  (defparameter *getters-n-setters-for-class*
    (labels ((rec (rest index)
               (when rest
                 (cons (make-em (car rest) index)
                       (rec (cdr rest) (+ index 1))))))
      (rec *the-slots-of-a-class* 0))))

(defparameter <class> (%allocate-instance nil (length *the-slots-of-a-class*)))
(%set-instance-class-to-self <class>)

(defparameter <top> (make <class>
                          'direct-supers nil
                          'direct-slots nil))

(defparameter <object> (make <class>
                             'direct-supers (list <top>)
                             'direct-slots nil))

(slot-set <class> 'direct-supers (list <object>))
(slot-set <class> 'direct-slots (map #'list *the-slots-of-a-class*))
(slot-set <class> 'cpl (list <class> <object> <top>))
(slot-set <class> 'slots (map #'list *the-slots-of-a-class*))
(slot-set <class> 'nfields (length *the-slots-of-a-class*))
(slot-set <class> 'field-initializers (map (lambda (s)
                                             (lambda () '()))
                                           *the-slots-of-a-class*))
(slot-set <class> 'getters-n-setters '())

(defparameter <procedure-class> (make <class>
                                      'direct-supers (list <class>)
                                      'direct-slots nil))
(defparameter <entity-class> (make <class>
                                   'direct-supers (list <procedure-class>)
                                   'direct-slots nil))
(defparameter <generic> (make <entity-class>
                              'direct-supers (list <object>)
                              'direct-slots '(methods)))
(defparameter <method> (make <class>
                             'direct-supers (list <object>)
                             'direct-slots '(specializers procedure)))

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

(defun make-class (direct-supers direct-slots)
  (make <class>
        'direct-supers direct-supers
        'direct-slots direct-slots))
(defun make-generic () (make <generic>))
(defun make-method (specializers procedure)
  (make <method>
        'specializers specializers
        'procedure procedure))

(defun is-a (obj class)
  (let ((obj (%instance-class obj)))
    (some (lambda (x)
            (eq x class))
          (slot-ref obj 'cpl))))

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
(defparameter *generic-invocation-generics* (list #'compute-apply-generic
                                                  #'compute-methods
                                                  #'compute-method-more-specific?
                                                  #'compute-apply-methods))

(defun add-method (generic method)
  (slot-set generic
            'methods
            (cons method
                  (collect-if (lambda (m)
                                (not (every #'eq
                                            (method-specializers m)
                                            (method-specializers method))))
                              (slot-ref generic 'methods))))
  (%set-instance-proc generic (compute-apply-generic generic)))

(%set-instance-proc
 #'compute-apply-generic
 (lambda (generic)
   (let ((method (car (generic-methods generic))))
     ((method-procedure method) nil generic))))

;;
;; TODO: understand the following stuff :-\
;;

(add-method
 #'compute-apply-generic
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
 #'compute-methods
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
 #'compute-method-more-specific?
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
 #'compute-apply-methods
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
 #'initialize
 (make-method (list <object>)
              (lambda (call-next-method object initargs)
                (let looop ((arg initargs))
                     (when arg
                       (slot-set object (car arg) (cadr arg))
                       (looop (cddr arg))))
                object)))

(add-method
 #'initialize
 (make-method (list <class>)
              (lambda (call-next-method class initargs)
                (slot-set class 'direct-supers
                          (getl initargs 'direct-supers '()))
                (slot-set class 'direct-slots
                          (map (lambda (s)
                                 (if (consp s) s (list s)))
                               (getl initargs 'direct-slots '())))
                (slot-set class 'cpl (compute-cpl class))
                (slot-set class 'slots (compute-slots class))
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
 #'initialize
 (make-method (list <generic>)
              (lambda (call-next-method generic initargs)
                (slot-set generic 'methods '())
                (%set-instance-proc generic (lambda args (error "Has no methods."))))))

(add-method
 #'initialize
 (make-method (list <method>)
              (lambda (call-next-method method initargs)
                (slot-set method 'specializers (getl initargs 'specializers '()))
                (slot-set method 'procedure (getl initargs 'procedure '())))))

(add-method
 #'allocate-instance
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
 #'allocate-instance
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
 #'compute-cpl
 (make-method (list <class>)
              (lambda (call-next-method class)
                (compute-simple-cpl class))))

(add-method
 #'compute-slots
 (make-method (list <class>)
              (lambda (call-next-method class)
                (labels ((collect (to-process result)
                           (if (not to-process)
                               (reverse result)
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
 #'compute-getter-and-setter
 (make-method (list <class>)
              (lambda (call-next-method class slot allocator)
                (funcall allocator (lambda () '())))))



;; redefine make to call initialize method
(defun make (class . initargs)
  (let ((instance (allocate-instance class)))
    (initialize instance initargs)
    instance))

(defparameter <primitive-class> (make <class>
                                      'direct-supers (list <class>)
                                      'direct-slots nil))

(defun make-primitive-class class
  (make (if (not class) <primitive-class> (car class))
        'direct-supers (list <top>)
        'direct-slots nil))

(defparameter <cons> (make-primitive-class))
(defparameter <null> (make-primitive-class))
(defparameter <symbol> (make-primitive-class))
(defparameter <regexp> (make-primitive-class))
(defparameter <function> (make-primitive-class <procedure-class>))
(defparameter <number> (make-primitive-class))
(defparameter <vector> (make-primitive-class))
(defparameter <string> (make-primitive-class))
(defparameter <input-stream> (make-primitive-class))
(defparameter <output-stream> (make-primitive-class))

(defun class-of (x)
  (cond ((%instance? x) (%instance-class x))
        ((consp x) <cons>)
        ((not x) <null>)
        ((symbolp x) <symbol>)
        ((functionp x) <function>)
        ((numberp x) <number>)
        ((vectorp x) <vector>)
        ((stringp x) <string>)
        ((%input-stream-p x) <input-stream>)
        ((%output-stream-p x) <output-stream>)))

(setf (find-class 'object) <object>)
(setf (find-class 'class) <class>)
(setf (find-class 'cons) <cons>)
(setf (find-class 'symbol) <symbol>)
(setf (find-class 'function) <function>)
(setf (find-class 'number) <number>)
(setf (find-class 'vector) <vector>)
(setf (find-class 'input-stream) <input-stream>)
(setf (find-class 'output-stream) <output-stream>)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; test

(defclass person (object) (first-name last-name))
(defclass student (person) (university code))

(defmethod initialize ((p person) initargs)
  (console.log "Initializing PERSON" initargs)
  (call-next-method)
  (console.log "Done PERSON"))

(defmethod initialize ((p student) initargs)
  (console.log "Initializing STUDENT" initargs)
  (call-next-method)
  (console.log "Done STUDENT"))

(defgeneric full-name)
(defmethod full-name ((p person))
  (strcat (slot-ref p 'first-name)
          " "
          (slot-ref p 'last-name)))

(let ((p (make (find-class 'student)
               'first-name "John"
               'last-name "Doe"
               'university "Şmenozenia"
               'code 12)))
  (console.log (full-name p))
  (console.log (class-slots (%instance-class p)))
  (console.log (is-a p (find-class 'number)))
  (console.log (is-a p (find-class 'person)))
  (console.dir (%instance-vector p)))
