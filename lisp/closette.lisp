;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Minor revisions of September 27, 1991 by desRivieres@parc.xerox.com:
;;;   - remove spurious "&key" in header of initialize-instance method
;;;     for standard-class (bottom of pg.310 of AMOP)
;;;   - add recommendation about not compiling this file
;;;   - change comment to reflect PARC ftp server name change
;;;   - add a BOA constructor to std-instance to please AKCL
;;;   - by default, efunctuate methods rather than compile them
;;;   - also make minor changes to newcl.lisp
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.
;;;
;;; This program is available by anonymous FTP, from the /pub/pcl/mop
;;; directory on parcftp.xerox.com.

;;; Modified for SLip by mihai.bazon@gmail.com (2025)
;;; Original source: https://github.com/binghe/closette

(in-package :sl)

(export '(array sequence defclass defgeneric defmethod
          find-class class-of
          call-next-method next-method-p
          slot-value slot-boundp slot-exists-p slot-makunbound
          make-instance change-class
          initialize-instance reinitialize-instance shared-initialize
          update-instance-for-different-class
          print-object describe-object
          standard-object
          standard-class standard-generic-function standard-method
          class-name
          structure-class structure-object

          class-direct-superclasses class-direct-slots
          class-precedence-list class-slots class-direct-subclasses
          class-direct-methods
          generic-function-name generic-function-lambda-list
          generic-function-methods generic-function-discriminating-function
          generic-function-method-class
          method-lambda-list method-qualifiers method-specializers
          method-generic-function method-function
          slot-definition-name slot-definition-initfunction
          slot-definition-initform slot-definition-initargs
          slot-definition-readers slot-definition-writers
          ;;
          ;; Class-related metaobject protocol
          ;;

          compute-class-precedence-list compute-slots
          compute-effective-slot-definition
          finalize-inheritance allocate-instance
          slot-value-using-class slot-boundp-using-class
          slot-exists-p-using-class slot-makunbound-using-class
          ;;
          ;; Generic function related metaobject protocol
          ;;

          compute-discriminating-function
          compute-applicable-methods-using-classes method-more-specific-p
          ensure-generic-function
          add-method remove-method find-method
          compute-effective-method-function
          apply-methods apply-method
          find-generic-function ; Necessary artifact of this implementation
          ))

(defpackage :closette
  (:use :sl))

(in-package :closette)

(import '(sl::defun-memoize2
          sl::clear-memoize-cache))

;;; -------------------- utils.lisp

;;; push-on-end is like push except it uses the other end:

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
        ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

;;; mapappend is like mapcar except that the results are appended together:

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

;;; mapplist is mapcar for property lists:

(defun mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x) (cadr x))
            (mapplist fun (cddr x)))))

;;; -------------------- closette.lisp

;;;
;;; Standard instances
;;;

(defun allocate-std-instance (class slots)
  (%:%make-std-instance class slots))

(define-compiler-macro allocate-std-instance (class slots)
  `(%:%make-std-instance ,class ,slots))

(defun std-instance-p (x)
  (%:%std-instance-p x))

(define-compiler-macro std-instance-p (x)
  `(%:%std-instance-p ,x))

(defun std-instance-class (x)
  (%:%std-instance-class x))

(define-compiler-macro std-instance-class (x)
  `(%:%std-instance-class ,x))

(defun (setf std-instance-class) (class x)
  (%:%set-std-instance-class class x))

(defun std-instance-slots (x)
  (%:%std-instance-slots x))

(define-compiler-macro std-instance-slots (x)
  `(%:%std-instance-slots ,x))

(defun (setf std-instance-slots) (slots x)
  (%:%set-std-instance-slots slots x))

;;; Standard instance allocation

(defconstant secret-unbound-value (list "slot unbound"))

(defun std-allocate-instance (class)
  (allocate-std-instance
   class
   (allocate-slot-storage (length (class-slots class))
                          secret-unbound-value)))

;;; Simple vectors are used for slot storage.

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(defvar the-slots-of-standard-class) ;standard-class's class-slots
(defvar the-class-standard-class)    ;standard-class's class metaobject

;; XXX: memoizing SLOT-LOCATION improves speed dramatically, so let's leave
;; this on for the moment. In the event we'll ever support class redefinition
;; (which Closette doesn't) we'll have to invalidate the cache.
(defun-memoize2 slot-location (class slot-name)
  (if (and (eq slot-name 'effective-slots)
           (eq class the-class-standard-class))
      (position 'effective-slots the-slots-of-standard-class
                :key #'slot-definition-name)
      (let ((slot (find slot-name
                        (class-slots class)
                        :key #'slot-definition-name)))
        (if (null slot)
            (error "The slot ~S is missing from the class ~S."
                   slot-name class)
            (let ((pos (position slot (class-slots class))))
              (if (null pos)
                  (error "The slot ~S is not an instance ~
                           slot in the class ~S."
                         slot-name class)
                  pos))))))

(defun slot-contents (slots location)
  (svref slots location))

(defun (setf slot-contents) (new-value slots location)
  (setf (svref slots location) new-value))

(defun std-slot-value (instance slot-name)
  (let* ((location (slot-location (class-of instance) slot-name))
         (slots (std-instance-slots instance))
         (val (slot-contents slots location)))
    (if (eq secret-unbound-value val)
        (error "The slot ~S is unbound in the object ~S."
               slot-name instance)
        val)))
(defun slot-value (object slot-name)
  (if (eq (class-of (class-of object)) the-class-standard-class)
      (std-slot-value object slot-name)
      (slot-value-using-class (class-of object) object slot-name)))

(defun (setf std-slot-value) (new-value instance slot-name)
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (std-instance-slots instance)))
    (setf (slot-contents slots location) new-value)))
(defun (setf slot-value) (new-value object slot-name)
  (if (eq (class-of (class-of object)) the-class-standard-class)
      (setf (std-slot-value object slot-name) new-value)
      (setf-slot-value-using-class
       new-value (class-of object) object slot-name)))

(defun std-slot-boundp (instance slot-name)
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (std-instance-slots instance)))
    (not (eq secret-unbound-value (slot-contents slots location)))))
(defun slot-boundp (object slot-name)
  (if (eq (class-of (class-of object)) the-class-standard-class)
      (std-slot-boundp object slot-name)
      (slot-boundp-using-class (class-of object) object slot-name)))

(defun std-slot-makunbound (instance slot-name)
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (std-instance-slots instance)))
    (setf (slot-contents slots location) secret-unbound-value))
  instance)
(defun slot-makunbound (object slot-name)
  (if (eq (class-of (class-of object)) the-class-standard-class)
      (std-slot-makunbound object slot-name)
      (slot-makunbound-using-class (class-of object) object slot-name)))

(defun std-slot-exists-p (instance slot-name)
  (not (null (member slot-name (class-slots (class-of instance))
                     :key #'slot-definition-name))))
(defun slot-exists-p (object slot-name)
  (if (eq (class-of (class-of object)) the-class-standard-class)
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class (class-of object) object slot-name)))

;;; class-of

(defun class-of (x)
  (if (std-instance-p x)
      (std-instance-class x)
      (built-in-class-of x)))

;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
  (not (null (member c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
  (let ((cpl (class-precedence-list c-arg)))
    (not (null (member c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

(defconstant the-defclass-standard-class  ;standard-class's defclass form
  '(defclass standard-class ()
    (; :accessor class-name
     (name :initarg :name)
     ; :accessor class-direct-superclasses
     (direct-superclasses :initarg :direct-superclasses)
     ; :accessor class-direct-slots
     (direct-slots)
     ; :accessor class-precedence-list
     (class-precedence-list)
     ; :accessor class-slots
     (effective-slots)
     ; :accessor class-direct-subclasses
     (direct-subclasses :initform ())
     ; :accessor class-direct-methods
     (direct-methods :initform ()))))

;;; Defining the metaobject slot accessor function as regular functions
;;; greatly simplifies the implementation without removing functionality.

(defun class-name (class) (std-slot-value class 'name))
(defun (setf class-name) (new-value class)
  (setf (slot-value class 'name) new-value))

(defun class-direct-superclasses (class)
  (slot-value class 'direct-superclasses))
(defun (setf class-direct-superclasses) (new-value class)
  (setf (slot-value class 'direct-superclasses) new-value))

(defun class-direct-slots (class)
  (slot-value class 'direct-slots))
(defun (setf class-direct-slots) (new-value class)
  (setf (slot-value class 'direct-slots) new-value))

(defun class-precedence-list (class)
  (slot-value class 'class-precedence-list))
(defun (setf class-precedence-list) (new-value class)
  (setf (slot-value class 'class-precedence-list) new-value))

(defun class-slots (class)
  (slot-value class 'effective-slots))
(defun (setf class-slots) (new-value class)
  (setf (slot-value class 'effective-slots) new-value))

(defun class-direct-subclasses (class)
  (slot-value class 'direct-subclasses))
(defun (setf class-direct-subclasses) (new-value class)
  (setf (slot-value class 'direct-subclasses) new-value))

(defun class-direct-methods (class)
  (slot-value class 'direct-methods))
(defun (setf class-direct-methods) (new-value class)
  (setf (slot-value class 'direct-methods) new-value))

;;; defclass

(defun sl-type:type-of-object (obj)
  (class-name (class-of obj)))

(defmacro defclass (name direct-superclasses direct-slots
                         &rest options)
  (%:maybe-xref-info name :defclass)
  `(progn
     (sl-type:defpredicate ,name (obj)
       (subclassp (class-of obj) (find-class ',name)))
     (ensure-class ',name
                   :direct-superclasses
                   ,(canonicalize-direct-superclasses direct-superclasses)
                   :direct-slots
                   ,(canonicalize-direct-slots direct-slots)
                   ,@(canonicalize-defclass-options options))))

(defun canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))

(defun canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun canonicalize-defclass-options (options)
  (mapappend #'canonicalize-defclass-option options))

(defun canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initargs ())
            (readers ())
            (writers ())
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (setq initfunction
                   `(function (lambda () ,(cadr olist))))
             (setq initform `',(cadr olist)))
            (:initarg
             (push-on-end (cadr olist) initargs))
            (:reader
             (push-on-end (cadr olist) readers))
            (:writer
             (push-on-end (cadr olist) writers))
            (:accessor
             (push-on-end (cadr olist) readers)
             (push-on-end `(setf ,(cadr olist)) writers))
            (otherwise
             (push-on-end `',(car olist) other-options)
             (push-on-end `',(cadr olist) other-options))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ,initform :initfunction ,initfunction))
          ,@(when initargs `(:initargs ',initargs))
          ,@(when readers `(:readers ',readers))
          ,@(when writers `(:writers ',writers))
          ,@other-options))))

(defun canonicalize-defclass-option (option)
  (case (car option)
    ((:metaclass)
     (list :metaclass
           `(find-class ',(cadr option))))
    ((:default-initargs)
     (list
      :direct-default-initargs
      `(list ,@(mapappend
                (lambda (x) x)
                (mapplist
                 (lambda (key value)
                   `(',key ,value))
                 (cdr option))))))
    (t (list `',(car option) `',(cadr option)))))

;;; find-class

;;; XXX: use symbol properties
(let ((class-table (make-hash-table)))

  (defun find-class (symbol &optional (errorp t))
    (let ((class (gethash symbol class-table)))
      (if (and (null class) errorp)
          (error "No class named ~S." symbol)
          class)))

  (defun (setf find-class) (new-value symbol)
    (setf (gethash symbol class-table) new-value))

  (defun forget-all-classes ()
    (clrhash class-table)
    (clear-memoize-cache 'slot-location)
    (values))
  ) ;end let class-table

;;; Ensure class

;; (defun ensure-class (name &rest all-keys
;;                           &key (metaclass the-class-standard-class)
;;                           &allow-other-keys)
;;   (if (find-class name nil)
;;       (error "Can't redefine the class named ~S." name)
;;       (let ((class (apply (if (eq metaclass the-class-standard-class)
;;                               #'make-instance-standard-class
;;                               #'make-instance)
;;                           metaclass :name name all-keys)))
;;         (setf (find-class name) class)
;;         class)))

(defun ensure-class (name &rest all-keys
                          &key (metaclass the-class-standard-class)
                          &allow-other-keys)
  (let ((class (apply (if (eq metaclass the-class-standard-class)
                          #'make-instance-standard-class
                          #'make-instance)
                      metaclass :name name all-keys)))
    (setf (find-class name) class)
    class))

;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun make-instance-standard-class
       (metaclass &key name direct-superclasses direct-slots
                  &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (std-allocate-instance the-class-standard-class)))
    (setf (class-name class) name)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-methods class) ())
    (std-after-initialization-for-classes class
                                          :direct-slots direct-slots
                                          :direct-superclasses direct-superclasses)
    class))

(defun std-after-initialization-for-classes
       (class &key direct-superclasses direct-slots &allow-other-keys)
  (let ((supers
         (or direct-superclasses
             (list (find-class 'standard-object)))))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (class-direct-subclasses superclass))))
  (let ((slots
         (mapcar (lambda (slot-properties)
                   (apply #'make-direct-slot-definition
                          slot-properties))
                 direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method
         class reader (slot-definition-name direct-slot)))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method
         class writer (slot-definition-name direct-slot)))))
  (funcall (if (eq (class-of class) the-class-standard-class)
               #'std-finalize-inheritance
               #'finalize-inheritance)
           class)
  (values))

;;; Slot definition metaobjects

;;; N.B. Quietly retain all unknown slot options (rather than signaling an
;;; error), so that it's easy to add new ones.

(defun make-direct-slot-definition
       (&rest properties
              &key name (initargs ()) (initform nil) (initfunction nil)
              (readers ()) (writers ())
              &allow-other-keys)
  (let ((slot (copy-list properties))) ; Don't want to side effect &rest list
    (setf (getf* slot :name) name)
    (setf (getf* slot :initargs) initargs)
    (setf (getf* slot :initform) initform)
    (setf (getf* slot :initfunction) initfunction)
    (setf (getf* slot :readers) readers)
    (setf (getf* slot :writers) writers)
    slot))

(defun make-effective-slot-definition
       (&rest properties
              &key name (initargs ()) (initform nil) (initfunction nil)
              &allow-other-keys)
  (let ((slot (copy-list properties)))  ; Don't want to side effect &rest list
    (setf (getf* slot :name) name)
    (setf (getf* slot :initargs) initargs)
    (setf (getf* slot :initform) initform)
    (setf (getf* slot :initfunction) initfunction)
    slot))

(defun slot-definition-name (slot)
  (getf slot :name))
(defun (setf slot-definition-name) (new-value slot)
  (setf (getf* slot :name) new-value))

(defun slot-definition-initfunction (slot)
  (getf slot :initfunction))
(defun (setf slot-definition-initfunction) (new-value slot)
  (setf (getf* slot :initfunction) new-value))

(defun slot-definition-initform (slot)
  (getf slot :initform))
(defun (setf slot-definition-initform) (new-value slot)
  (setf (getf* slot :initform) new-value))

(defun slot-definition-initargs (slot)
  (getf slot :initargs))
(defun (setf slot-definition-initargs) (new-value slot)
  (setf (getf* slot :initargs) new-value))

(defun slot-definition-readers (slot)
  (getf slot :readers))
(defun (setf slot-definition-readers) (new-value slot)
  (setf (getf* slot :readers) new-value))

(defun slot-definition-writers (slot)
  (getf slot :writers))
(defun (setf slot-definition-writers) (new-value slot)
  (setf (getf* slot :writers) new-value))

;;; finalize-inheritance

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
        (funcall (if (eq (class-of class) the-class-standard-class)
                     #'std-compute-class-precedence-list
                     #'compute-class-precedence-list)
                 class))
  (setf (class-slots class)
        (funcall (if (eq (class-of class) the-class-standard-class)
                     #'std-compute-slots
                     #'compute-slots)
                 class))
  (values))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                       (mapappend #'local-precedence-ordering
                                  classes-to-order))
                      #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
     (let ((minimal-elements
            (remove-if
             (lambda (class)
               (member class remaining-constraints
                       :key #'cadr))
             remaining-elements)))
       (when (null minimal-elements)
         (if (null remaining-elements)
             (return-from topological-sort result)
             (error "Inconsistent precedence graph.")))
       (let ((choice (if (null (cdr minimal-elements))
                         (car minimal-elements)
                         (funcall tie-breaker
                                  minimal-elements
                                  result))))
         (setq result (append result (list choice)))
         (setq remaining-elements
               (remove choice remaining-elements))
         (setq remaining-constraints
               (remove choice
                       remaining-constraints
                       :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
             (let ((to-be-processed
                    (set-difference superclasses seen)))
               (if (null to-be-processed)
                   superclasses
                   (let ((class-to-process
                          (car to-be-processed)))
                     (all-superclasses-loop
                      (cons class-to-process seen)
                      (union (class-direct-superclasses
                              class-to-process)
                             superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (class-direct-superclasses class)))
          (class-direct-superclasses class)))

;;; Slot inheritance

(defun std-compute-slots (class)
  (let* ((all-slots (mapappend #'class-direct-slots
                               (class-precedence-list class)))
         (all-names (remove-duplicates
                     (mapcar #'slot-definition-name all-slots))))
    (mapcar (lambda (name)
              (funcall
               (if (eq (class-of class) the-class-standard-class)
                   #'std-compute-effective-slot-definition
                   #'compute-effective-slot-definition)
               class
               (remove name all-slots
                       :key #'slot-definition-name
                       :test-not #'eq)))
            all-names)))

(defun std-compute-effective-slot-definition (class direct-slots)
  (declare (ignore class))
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
     :name (slot-definition-name (car direct-slots))
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend #'slot-definition-initargs
                           direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defconstant the-defclass-standard-generic-function
  '(defclass standard-generic-function ()
    (; :accessor generic-function-name
     (name :initarg :name)
     ; :accessor generic-function-lambda-list
     (lambda-list :initarg :lambda-list)
     ; :accessor generic-function-parsed-lambda-list
     (parsed-lambda-list :initarg :parsed-lambda-list)
     ; :accessor generic-function-required-args
     (required-args :initarg :required-args)
     ; :accessor generic-function-methods
     (methods :initform ())
     ; :accessor generic-function-method-class
     (method-class :initarg :method-class)
     ; :accessor generic-function-discriminating-function
     (discriminating-function)
     ; :accessor classes-to-emf-table
     (classes-to-emf-table :initform (make-hash-table)))))

(defvar the-class-standard-gf) ;standard-generic-function's class metaobject

(defun generic-function-name (gf)
  (slot-value gf 'name))
(defun (setf generic-function-name) (new-value gf)
  (setf (slot-value gf 'name) new-value))

(defun generic-function-lambda-list (gf)
  (slot-value gf 'lambda-list))
(defun (setf generic-function-lambda-list) (new-value gf)
  (setf (slot-value gf 'lambda-list) new-value))

(defun generic-function-parsed-lambda-list (gf)
  (slot-value gf 'parsed-lambda-list))
(defun (setf generic-function-parsed-lambda-list) (new-value gf)
  (setf (slot-value gf 'parsed-lambda-list) new-value))

(defun generic-function-required-args (gf)
  (slot-value gf 'required-args))
(defun (setf generic-function-required-args) (new-value gf)
  (setf (slot-value gf 'required-args) new-value))

(defun generic-function-methods (gf)
  (slot-value gf 'methods))
(defun (setf generic-function-methods) (new-value gf)
  (setf (slot-value gf 'methods) new-value))

(defun generic-function-discriminating-function (gf)
  (slot-value gf 'discriminating-function))
(defun (setf generic-function-discriminating-function) (new-value gf)
  (setf (slot-value gf 'discriminating-function) new-value))

(defun generic-function-method-class (gf)
  (slot-value gf 'method-class))
(defun (setf generic-function-method-class) (new-value gf)
  (setf (slot-value gf 'method-class) new-value))

;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
  (slot-value gf 'classes-to-emf-table))
(defun (setf classes-to-emf-table) (new-value gf)
  (setf (slot-value gf 'classes-to-emf-table) new-value))

;;;
;;; Method metaobjects and standard-method
;;;

(defconstant the-defclass-standard-method
  '(defclass standard-method ()
    ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
     (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
     (specializers :initarg :specializers)   ; :accessor method-specializers
     (generic-function :initform nil)        ; :accessor method-generic-function
     (function :initarg :function))))        ; :accessor method-function

(defvar the-class-standard-method)    ;standard-method's class metaobject

(defun method-lambda-list (method) (slot-value method 'lambda-list))
(defun (setf method-lambda-list) (new-value method)
  (setf (slot-value method 'lambda-list) new-value))

(defun method-qualifiers (method) (slot-value method 'qualifiers))
(defun (setf method-qualifiers) (new-value method)
  (setf (slot-value method 'qualifiers) new-value))

(defun method-specializers (method) (slot-value method 'specializers))
(defun (setf method-specializers) (new-value method)
  (setf (slot-value method 'specializers) new-value))

(defun method-generic-function (method)
  (slot-value method 'generic-function))
(defun (setf method-generic-function) (new-value method)
  (setf (slot-value method 'generic-function) new-value))

(defun method-function (method) (slot-value method 'function))
(defun (setf method-function) (new-value method)
  (setf (slot-value method 'function) new-value))

;;; defgeneric

(defmacro defgeneric (function-name lambda-list &rest options)
  `(ensure-generic-function
    ',function-name
    :lambda-list ',lambda-list
    ,@(canonicalize-defgeneric-options options)))

(defun canonicalize-defgeneric-options (options)
  (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class
     (list :generic-function-class
           `(find-class ',(cadr option))))
    (:method-class
     (list :method-class
           `(find-class ',(cadr option))))
    (t (list `',(car option) `',(cadr option)))))

;;; find-generic-function looks up a generic function by name.  It's an
;;; artifact of the fact that our generic function metaobjects can't legally
;;; be stored a symbol's function value.

;;; XXX: use symbol properties.
(let ((generic-function-table (make-hash-table)))

  (defun find-generic-function (symbol &optional (errorp t))
    (let ((gf (gethash symbol generic-function-table)))
      (if (and (null gf) errorp)
          (error "No generic function named ~S." symbol)
          gf)))

  (defun (setf find-generic-function) (new-value symbol)
    (setf (gethash symbol generic-function-table) new-value))

  (defun forget-all-generic-functions ()
    (clrhash generic-function-table)
    (values))
  ) ;end let generic-function-table

;;; ensure-generic-function

(defun ensure-generic-function
       (function-name
        &rest all-keys
        &key (generic-function-class the-class-standard-gf)
        (method-class the-class-standard-method)
        &allow-other-keys)
  (multiple-value-bind (setter sym) (%:maybe-setter function-name)
    (%:maybe-xref-info sym (if setter :generic-setf :generic))
    (when setter
      (setf function-name setter)))
  (if (find-generic-function function-name nil)
      (find-generic-function function-name)
      (let ((gf (apply (if (eq generic-function-class the-class-standard-gf)
                           #'make-instance-standard-generic-function
                           #'make-instance)
                       generic-function-class
                       :name function-name
                       :method-class method-class
                       all-keys)))
        (setf (find-generic-function function-name) gf)
        gf)))

;;; finalize-generic-function

;;; N.B. Same basic idea as finalize-inheritance.  Takes care of recomputing
;;; and storing the discriminating function, and clearing the effective method
;;; function table.

(defun finalize-generic-function (gf)
  (setf (generic-function-discriminating-function gf)
        (funcall (if (eq (class-of gf) the-class-standard-gf)
                     #'std-compute-discriminating-function
                     #'compute-discriminating-function)
                 gf))
  (setf (fdefinition (generic-function-name gf))
        (generic-function-discriminating-function gf))
  (clrhash (classes-to-emf-table gf))
  (values))

;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function
       (generic-function-class &key name lambda-list method-class)
  (declare (ignore generic-function-class))
  (let ((gf (std-allocate-instance the-class-standard-gf))
        (parsed-lambda-list (analyze-lambda-list lambda-list)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-parsed-lambda-list gf) parsed-lambda-list)
    (setf (generic-function-required-args gf) (length (getf parsed-lambda-list :required-args)))
    (setf (generic-function-methods gf) ())
    (setf (generic-function-method-class gf) method-class)
    (setf (classes-to-emf-table gf) (make-hash-table))
    (finalize-generic-function gf)
    gf))

;;; defmethod

(defun %no-next-method (generic)
  (error "No next method for the generic function ~S." generic))

(defmacro defmethod (&rest args)
  (multiple-value-bind (function-name qualifiers lambda-list specializers body)
                       (parse-defmethod args)
    (multiple-value-bind (setter) (%:maybe-setter function-name)
      (when setter (setf function-name setter)))
    (%:maybe-xref-info function-name :method)
    `(let (($generic (find-generic-function ',function-name)))
       (ensure-method $generic
                      :lambda-list ',lambda-list
                      :qualifiers ',qualifiers
                      :specializers ,(canonicalize-specializers specializers)
                      :function (lambda ($args $next-emfun)
                                  (flet ((call-next-method (&rest cnm-args)
                                           (if $next-emfun
                                               (funcall $next-emfun (or cnm-args $args))
                                               (%no-next-method $generic)))
                                         (next-method-p ()
                                           (not (null $next-emfun))))
                                    (apply (%:%fn ,function-name
                                                  ,(kludge-arglist lambda-list)
                                                  ,body)
                                           $args)))))))

(defun canonicalize-specializers (specializers)
  `(list ,@(mapcar #'canonicalize-specializer specializers)))

(defun canonicalize-specializer (specializer)
  `(find-class ',specializer))

(defun parse-defmethod (args)
  (let ((fn-spec (car args))
        (qualifiers ())
        (specialized-lambda-list nil)
        (body ())
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
        (:qualifiers
         (if (and (atom arg) (not (null arg)))
             (push-on-end arg qualifiers)
             (progn (setq specialized-lambda-list arg)
                    (setq parse-state :body))))
        (:body (push-on-end arg body))))
    (values fn-spec
            qualifiers
            (extract-lambda-list specialized-lambda-list)
            (extract-specializers specialized-lambda-list)
            (list* 'progn body))))

;;; Several tedious functions for analyzing lambda lists

(defun required-portion (gf args)
  (let ((number-required (generic-function-required-args gf)))
    (when (< (length args) number-required)
      (error "Too few arguments to generic function ~S." gf))
    (subseq args 0 number-required)))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist :required-names))
         (rv (getf plist :rest-var))
         (ks (getf plist :key-args))
         (aok (getf plist :allow-other-keys))
         (opts (getf plist :optional-args))
         (auxs (getf plist :auxiliary-args)))
    `(,@requireds
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist :specializers)))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
             (intern (symbol-name symbol)
                     #.(find-package 'keyword)))
           (get-keyword-from-arg (arg)
             (if (listp arg)
                 (if (listp (car arg))
                     (caar arg)
                     (make-keyword (car arg)))
                 (make-keyword arg))))
    (let ((keys ())           ; Just the keywords
          (key-args ())       ; Keywords argument specs
          (required-names ()) ; Just the variable names
          (required-args ())  ; Variable names & specializers
          (specializers ())   ; Just the specializers
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
            (ecase arg
              (&optional
               (setq state :parsing-optional))
              (&rest
               (setq state :parsing-rest))
              (&key
               (setq state :parsing-key))
              (&allow-other-keys
               (setq allow-other-keys 't))
              (&aux
               (setq state :parsing-aux)))
            (case state
              (:parsing-required
               (push-on-end arg required-args)
               (if (listp arg)
                   (progn (push-on-end (car arg) required-names)
                          (push-on-end (cadr arg) specializers))
                   (progn (push-on-end arg required-names)
                          (push-on-end 't specializers))))
              (:parsing-optional (push-on-end arg optionals))
              (:parsing-rest (setq rest-var arg))
              (:parsing-key
               (push-on-end (get-keyword-from-arg arg) keys)
               (push-on-end arg key-args))
              (:parsing-aux (push-on-end arg auxs)))))
      (list :required-names required-names
            :required-args required-args
            :specializers specializers
            :rest-var rest-var
            :keywords keys
            :key-args key-args
            :auxiliary-args auxs
            :optional-args optionals
            :allow-other-keys allow-other-keys))))

;;; ensure method

(defun ensure-method (gf &rest all-keys)
  (let ((new-method
         (apply
          (if (eq (generic-function-method-class gf)
                  the-class-standard-method)
              #'make-instance-standard-method
              #'make-instance)
          (generic-function-method-class gf)
          all-keys)))
    (add-method gf new-method)
    new-method))

;;; make-instance-standard-method creates and initializes an instance of
;;; standard-method without falling into method lookup.  However, it cannot
;;; be called until standard-method exists.

(defun make-instance-standard-method (method-class
                                      &key lambda-list qualifiers
                                      specializers function)
  (declare (ignore method-class))
  (let ((method (std-allocate-instance the-class-standard-method)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) specializers)
    (setf (method-function method) function)
    (setf (method-generic-function method) nil)
    method))

;;; add-method

;;; N.B. This version first removes any existing method on the generic function
;;; with the same qualifiers and specializers.  It's a pain to develop
;;; programs without this feature of full CLOS.

(defun add-method (gf method)
  (let ((old-method
         (find-method gf (method-qualifiers method)
                      (method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (method-generic-function method) gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (method-specializers method))
    (pushnew method (class-direct-methods specializer)))
  (finalize-generic-function gf)
  method)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (setf (method-generic-function method) nil)
  (dolist (class (method-specializers method))
    (setf (class-direct-methods class)
          (remove method (class-direct-methods class))))
  (finalize-generic-function gf)
  method)

(defun find-method (gf qualifiers specializers
                       &optional (errorp t))
  (let ((method
         (find-if (lambda (method)
                    (and (equal qualifiers
                                (method-qualifiers method))
                         (equal specializers
                                (method-specializers method))))
                  (generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (generic-function-name gf))
        method)))

;;; Reader and write methods

(defun %mk-reader (slot-name)
  (lambda (args next-emfun)
    (declare (ignore next-emfun))
    (slot-value (car args) slot-name)))

(defun add-reader-method (class fn-name slot-name)
  (ensure-method
   (ensure-generic-function fn-name :lambda-list '(object))
   :lambda-list '(object)
   :qualifiers ()
   :specializers (list class)
   :function (%mk-reader slot-name))
  (values))

(defun %mk-writer (slot-name)
  (lambda (args next-emfun)
    (declare (ignore next-emfun))
    (setf (slot-value (car args) slot-name)
          (cadr args))))

(defun add-writer-method (class fn-name slot-name)
  (ensure-method
   (ensure-generic-function fn-name :lambda-list '(new-value object))
   :lambda-list '(new-value object)
   :qualifiers ()
   :specializers (list (find-class 't) class)
   :function (%mk-writer slot-name))
  (values))

;;;
;;; Generic function invocation
;;;

;;; compute-discriminating-function

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (let* ((classes (mapcar #'class-of (required-portion gf args)))
           (key (%:hash-equal-key classes))
           (emfun (gethash key (classes-to-emf-table gf))))
      (if emfun
          (funcall emfun args)
          (slow-method-lookup gf args classes key)))))

(defun slow-method-lookup (gf args classes key)
  (let* ((applicable-methods
          (compute-applicable-methods-using-classes gf classes))
         (emfun
          (funcall
           (if (eq (class-of gf) the-class-standard-gf)
               #'std-compute-effective-method-function
               #'compute-effective-method-function)
           gf applicable-methods)))
    (setf (gethash key (classes-to-emf-table gf)) emfun)
    (funcall emfun args)))

;;; compute-applicable-methods-using-classes

(defun compute-applicable-methods-using-classes
       (gf required-classes)
  (sort
   (copy-list
    (remove-if-not (lambda (method)
                     (every #'subclassp
                            required-classes
                            (method-specializers method)))
                   (generic-function-methods gf)))
   (lambda (m1 m2)
     (funcall
      (if (eq (class-of gf) the-class-standard-gf)
          #'std-method-more-specific-p
          #'method-more-specific-p)
      gf m1 m2 required-classes))))

;;; method-more-specific-p

(defun std-method-more-specific-p (gf method1 method2 required-classes)
  (declare (ignore gf))
  (mapc (lambda (spec1 spec2 arg-class)
          (unless (eq spec1 spec2)
            (return-from std-method-more-specific-p
              (sub-specializer-p spec1 spec2 arg-class))))
        (method-specializers method1)
        (method-specializers method2)
        required-classes)
  nil)

;;; apply-methods and compute-effective-method-function

(defun apply-methods (gf args methods)
  (funcall (compute-effective-method-function gf methods)
           args))

(defun primary-method-p (method)
  (null (method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

(defun std-compute-effective-method-function (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No primary methods for the ~
             generic function ~S." gf))
    (if around
        (let ((next-emfun
               (funcall
                (if (eq (class-of gf) the-class-standard-gf)
                    #'std-compute-effective-method-function
                    #'compute-effective-method-function)
                gf (remove around methods))))
          (lambda (args)
            (funcall (method-function around) args next-emfun)))
        (let ((next-emfun (compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'before-method-p methods))
              (reverse-afters
               (nreverse (remove-if-not #'after-method-p methods))))
          (lambda (args)
            (dolist (before befores)
              (funcall (method-function before) args nil))
            (multiple-value-prog1
                (funcall (method-function (car primaries)) args next-emfun)
              (dolist (after reverse-afters)
                (funcall (method-function after) args nil))))))))

;;; compute an effective method function from a list of primary methods:

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        (lambda (args)
          (funcall (method-function (car methods)) args next-emfun)))))

;;; apply-method

(defun apply-method (method args next-methods)
  (funcall (method-function method)
           args
           (if (null next-methods)
               nil
               (compute-effective-method-function
                (method-generic-function method) next-methods))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.

(defun kludge-arglist (lambda-list)
  (if (and (member '&key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (append lambda-list '(&allow-other-keys))
      (if (and (not (member '&rest lambda-list))
               (not (member '&key lambda-list)))
          (append lambda-list '(&key &allow-other-keys))
          lambda-list)))

;;; -------------------- bootstrap.lisp

;;;
;;; Bootstrap
;;;

;; (format t "Beginning to bootstrap Closette...~%")
(forget-all-classes)
(forget-all-generic-functions)

;; How to create the class hierarchy in 10 easy steps:
;; 1. Figure out standard-class's slots.
(setq the-slots-of-standard-class
      (mapcar (lambda (slotd)
                (make-effective-slot-definition
                 :name (car slotd)
                 :initargs
                 (let ((a (getf (cdr slotd) :initarg)))
                   (if a (list a) ()))
                 :initform (getf (cdr slotd) :initform)
                 :initfunction
                 (let ((a (getf (cdr slotd) :initform)))
                   (if a (lambda () (eval a)) nil))))
              (nth 3 the-defclass-standard-class)))

;; 2. Create the standard-class metaobject by hand.
(setq the-class-standard-class
      (allocate-std-instance
       'tba
       (make-array (length the-slots-of-standard-class)
                   :initial-element secret-unbound-value)))

;; 3. Install standard-class's (circular) class-of link.
(setf (std-instance-class the-class-standard-class)
      the-class-standard-class)

;; (It's now okay to use class-... accessor).
;; 4. Fill in standard-class's class-slots.
(setf (class-slots the-class-standard-class) the-slots-of-standard-class)

;; (Skeleton built; it's now okay to call make-instance-standard-class.)
;; 5. Hand build the class t so that it has no direct superclasses.
(setf (find-class 't)
      (let ((class (std-allocate-instance the-class-standard-class)))
        (setf (class-name class) 't)
        (setf (class-direct-subclasses class) ())
        (setf (class-direct-superclasses class) ())
        (setf (class-direct-methods class) ())
        (setf (class-direct-slots class) ())
        (setf (class-precedence-list class) (list class))
        (setf (class-slots class) ())
        class))

;; (It's now okay to define subclasses of t.)
;; 6. Create the other superclass of standard-class (i.e., standard-object).
(defclass standard-object (t) ())

;; 7. Define the full-blown version of standard-class.
(setq the-class-standard-class (eval the-defclass-standard-class))

;; 7.1 forget slot-location cache.
(clear-memoize-cache 'slot-location)

;; 8. Replace all (3) existing pointers to the skeleton with real one.
(setf (std-instance-class (find-class 't))
      the-class-standard-class)
(setf (std-instance-class (find-class 'standard-object))
      the-class-standard-class)
(setf (std-instance-class the-class-standard-class)
      the-class-standard-class)

;; (Clear sailing from here on in).
;; 9. Define the other built-in classes.
(defconstant <top>               (find-class 't))
(defconstant <symbol>            (defclass symbol (t) ()))
(defconstant <sequence>          (defclass sequence (t) ()))
(defconstant <array>             (defclass array (t) ()))
(defconstant <number>            (defclass number (t) ()))
(defconstant <integer>           (defclass integer (number) ()))
(defconstant <character>         (defclass character (t) ()))
(defconstant <regexp>            (defclass regexp (t) ()))
(defconstant <function>          (defclass function (t) ()))
(defconstant <hash-table>        (defclass hash-table (t) ()))
(defconstant <package>           (defclass package (t) ()))
(defconstant <thread>            (defclass thread (t) ()))
(defconstant <stream>            (defclass stream (t) ()))
(defconstant <input-stream>      (defclass input-stream (stream) ()))
(defconstant <output-stream>     (defclass output-stream (stream) ()))
(defconstant <list>              (defclass list (sequence) ()))
(defconstant <null>              (defclass null (symbol list) ()))
(defconstant <cons>              (defclass cons (list) ()))
(defconstant <vector>            (defclass vector (array sequence) ()))
(defconstant <string>            (defclass string (t) ()))
(defconstant <structure-object>  (defclass structure-object (t) ()))
(defconstant <structure-class>   (defclass structure-class (standard-class) ()))

;; N.B. This version of built-in-class-of is straightforward but very slow.
(defun built-in-class-of (x)
  (cond
    ((null x)                                         <null>)
    ((symbolp x)                                      <symbol>)
    ((integerp x)                                     <integer>)
    ((numberp x)                                      <number>)
    ((consp x)                                        <cons>)
    ((charp x)                                        <character>)
    ((regexpp x)                                      <regexp>)
    ((hash-table-p x)                                 <hash-table>)
    ((packagep x)                                     <package>)
    ((threadp x)                                      <thread>)
    ((%:%input-stream-p x)                            <input-stream>)
    ((%:%output-stream-p x)                           <output-stream>)
    ((stringp x)                                      <string>)
    ((sl-struct::structurep x)                        <structure-class>)
    ((vectorp x)                                      <vector>)
    ((functionp x)                                    <function>)
    (t                                                <top>)))

;; 10. Define the other standard metaobject classes.
(setq the-class-standard-gf (eval the-defclass-standard-generic-function))
(setq the-class-standard-method (eval the-defclass-standard-method))

;; Voila! The class hierarchy is in place.
;; (format t "Class hierarchy created.~%")

;; (It's now okay to define generic functions and methods.)

;;; -------------------- closette-final.lisp

(defgeneric print-object (instance stream))
(defmethod print-object ((instance standard-object) stream)
  (print-unreadable-object (instance stream)
     (format stream "~:(~S~)"
                    (class-name (class-of instance))))
  instance)

;;; Slot access

(defgeneric slot-value-using-class (class instance slot-name))
(defmethod slot-value-using-class
           ((class standard-class) instance slot-name)
  (std-slot-value instance slot-name))

(defgeneric (setf slot-value-using-class) (new-value class instance slot-name))
(defmethod (setf slot-value-using-class)
           (new-value (class standard-class) instance slot-name)
  (setf (std-slot-value instance slot-name) new-value))
;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
(defun setf-slot-value-using-class (new-value class object slot-name)
  (setf (slot-value-using-class class object slot-name) new-value))

(defgeneric slot-exists-p-using-class (class instance slot-name))
(defmethod slot-exists-p-using-class
           ((class standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class
           ((class standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
  (std-slot-makunbound instance slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class))
(defmethod allocate-instance ((class standard-class))
  (std-allocate-instance class))

(defgeneric make-instance (class &key))
(defmethod make-instance ((class standard-class) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))
(defmethod reinitialize-instance
           ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &key))
(defmethod shared-initialize ((instance standard-object)
                              slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
                           (get-properties
                            all-keys (slot-definition-initargs slot))
        (declare (ignore init-key))
        (if foundp
            (setf (slot-value instance slot-name) init-value)
            (when (and (not (slot-boundp instance slot-name))
                       (not (null (slot-definition-initfunction slot)))
                       (or (eq slot-names t)
                           (member slot-name slot-names)))
              (setf (slot-value instance slot-name)
                    (funcall (slot-definition-initfunction slot))))))))
  instance)

;;; change-class

(defgeneric change-class (instance new-class &key))
(defmethod change-class
           ((old-instance standard-object)
            (new-class standard-class)
            &rest initargs)
  (let ((new-instance (allocate-instance new-class)))
    (dolist (slot-name (mapcar #'slot-definition-name
                               (class-slots new-class)))
      (when (and (slot-exists-p old-instance slot-name)
                 (slot-boundp old-instance slot-name))
        (setf (slot-value new-instance slot-name)
              (slot-value old-instance slot-name))))
    (rotatef (std-instance-slots new-instance)
             (std-instance-slots old-instance))
    (rotatef (std-instance-class new-instance)
             (std-instance-class old-instance))
    (apply #'update-instance-for-different-class
           new-instance old-instance initargs)
    old-instance))

(defmethod change-class
           ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new &key))
(defmethod update-instance-for-different-class
           ((old standard-object) (new standard-object) &rest initargs)
  (let ((added-slots
         (remove-if (lambda (slot-name)
                      (slot-exists-p old slot-name))
                    (mapcar #'slot-definition-name
                            (class-slots (class-of new))))))
    (apply #'shared-initialize new added-slots initargs)))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod print-object ((class standard-class) stream)
  (print-unreadable-object (class stream)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class standard-class))
  (std-finalize-inheritance class)
  (values))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class direct-slots))
(defmethod compute-effective-slot-definition
           ((class standard-class) direct-slots)
  (std-compute-effective-slot-definition class direct-slots))

;;;
;;; Methods having to do with generic function metaobjects.
;;;

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream)
     (format stream "~:(~S~) ~S"
             (class-name (class-of gf))
             (generic-function-name gf)))
  gf)

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (finalize-generic-function gf))

;;;
;;; Methods having to do with method metaobjects.
;;;

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream)
     (format stream "~:(~S~) ~S~{ ~S~} ~S"
                    (class-name (class-of method))
                    (generic-function-name
                      (method-generic-function method))
                    (method-qualifiers method)
                    (mapcar #'class-name
                            (method-specializers method))))
  method)

;;;
;;; Methods having to do with generic function invocation.
;;;

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))
(defmethod method-more-specific-p
           ((gf standard-generic-function) method1 method2 required-classes)
  (std-method-more-specific-p gf method1 method2 required-classes))

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function
           ((gf standard-generic-function) methods)
  (std-compute-effective-method-function gf methods))

;;; describe-object is a handy tool for enquiring minds:

(defgeneric describe-object (object stream))
(defmethod describe-object ((object standard-object) stream)
  (format stream "A Closette object~
             ~%Printed representation: ~S~
             ~%Class: ~S~
             ~%Structure "
          object
          (class-of object))
  (dolist (sn (mapcar #'slot-definition-name
                      (class-slots (class-of object))))
    (format stream "~%    ~S <- ~:[not bound~;~S~]"
            sn
            (slot-boundp object sn)
            (and (slot-boundp object sn)
                 (slot-value object sn))))
  (format stream "~%")
  (values))
;; (defmethod describe-object ((object t) stream)
;;   (common-lisp:describe object)
;;   (values))

;; (format t "~%Closette is a Knights of the Lambda Calculus production.~%")
