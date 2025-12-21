(in-package :sl)

(export '(copy-structure defstruct))

(defpackage :sl-struct
  (:use :sl :%))

(in-package :sl-struct)

;; (setf %:*enable-inline* t)

(defconstant *structures* (make-hash-table))

(declaim (inline substructp))
(defun substructp (tgt struct)
  (if (member tgt (structure-include struct)) t nil))

(declaim (inline structurep))
(defun structurep (thing &optional name)
  (cond
    ((not name) (%structp thing))
    ((%structp thing)
     (let ((ctor (%struct-struct thing))
           (tgt (find-structure name)))
       (or (eq ctor tgt)
           (substructp tgt ctor))))))

(declaim (inline assert-struct))
(defun assert-struct (thing &optional name)
  (unless (structurep thing name)
    (if name
        (error "Expected structure ~S." name)
        (error "Expected structure."))))

(declaim (inline find-structure))
(defun find-structure (name &optional (errorp t))
  (or (gethash name *structures*)
      (when errorp
        (error "No such structure ~S." name))))

(define-compiler-macro find-structure (&whole form name &rest _)
  (cond
    ((and (consp name)
          (eq 'quote (car name))
          (symbolp (cadr name))
          (gethash (cadr name) *structures*))
     `(gethash ,name *structures*))
    (t form)))

(defun structure-of (x)
  (assert-struct x)
  (%struct-struct x))

(defun (setf find-structure) (struct name)
  (setf (gethash name *structures*) struct))

(defglobal *structure* nil)

(declaim (inline make-structure))
(defun make-structure (name &key slots include print-object print-function)
  (when (find-structure name nil)
    (warn "Redefining structure ~S." name))
  (when include
    (setf include (find-structure include))
    (setf include (cons include (structure-include include))))
  (setf (find-structure name)
        (%struct *structure* name slots include print-object print-function)))

(setf *structure*
      (make-structure
       'structure
       :slots '((:name name :read-only t :type symbol)
                (:name slots :read-only t :type list)
                (:name include :read-only t :type symbol)
                (:name print-object :read-only t :type function)
                (:name print-function :read-only t :type function))))

(declaim (inline structure-name))
(defun structure-name (struct)
  (%struct-ref struct 0))

(declaim (inline structure-slots))
(defun structure-slots (struct)
  (%struct-ref struct 1))

(declaim (inline structure-include))
(defun structure-include (struct)
  (%struct-ref struct 2))

(declaim (inline structure-print-object))
(defun structure-print-object (struct)
  (%struct-ref struct 3))

(declaim (inline structure-print-function))
(defun structure-print-function (struct)
  (%struct-ref struct 4))

(defun parse-slot (args)
  (when (atom args)
    (setf args (list args)))
  (list :name (pop args)
        :initform (pop args)
        :read-only (getf args :read-only)
        :type (getf args :type)))

(defun parse-name-and-options (args)
  (when (atom args)
    (setf args (list args)))
  (let* ((name (car args))
         (constructor (intern (strcat "MAKE-" name)))
         (constructor-arglist nil)
         (conc-name (strcat name "-"))
         (copier (intern (strcat "COPY-" name)))
         (print-object nil)
         (print-function nil)
         (predicate (intern (strcat name "-P")))
         (include nil))
    (dolist (opt (cdr args)
                 (values name
                         constructor
                         constructor-arglist
                         conc-name
                         copier
                         print-object
                         print-function
                         predicate
                         include))
      (when (symbolp opt)
        (setf opt (list opt)))
      (case (car opt)
        ((:constructor)
         (setf constructor (cadr opt))
         (setf constructor-arglist (caddr opt)))
        ((:conc-name)
         (setf conc-name (strcat (cadr opt))))
        ((:copier)
         (setf copier (cadr opt)))
        ((:predicate)
         (setf predicate (cadr opt)))
        ((:print-object)
         (setf print-object (cadr opt)))
        ((:print-function)
         (setf print-function (cadr opt)))
        ((:include)
         (setf include (cadr opt)))
        (otherwise
         (error "Unsupported struct option ~S." (car opt)))))))

(defun insert-defaults (parsed slots)
  (labels ((find-slot (name)
             (let rec ((slots slots))
               (when slots
                 (if (eq name (getf (car slots) :name))
                     (car slots)
                     (rec (cdr slots))))))
           (do-one (arg)
             (unless (cdr arg)
               (let* ((name (if (consp (car arg)) (cadar arg) (car arg)))
                      (slot (find-slot name)))
                 (when slot
                   (setf (cdr arg) (list (getf slot :initform)))))))
           (do-all (args)
             (mapc #'do-one args)))
    (do-all (getf parsed :optional))
    (do-all (getf parsed :key))
    (do-all (getf parsed :aux))
    `(,@(getf parsed :required)
      ,@(aif (getf parsed :optional)
             `(&optional ,@it))
      ,@(aif (getf parsed :rest)
             `(&rest ,it))
      ,@(aif (getf parsed :key)
             `(&key ,@it))
      ,@(aif (getf parsed :aok)
             `(&allow-other-keys))
      ,@(aif (getf parsed :aux)
             `(&aux ,@it)))))

(defmacro defstruct (name-and-options &rest slot-description)
  (multiple-value-bind (struct-name
                        constructor
                        constructor-arglist
                        conc-name
                        copier
                        print-object
                        print-function
                        predicate
                        include)
                       (parse-name-and-options name-and-options)
    (when include
      (setf include (find-structure include))
      (unless (or print-object print-function)
        (setf print-object (structure-print-object include)
              print-function (structure-print-function include))))
    (when (and print-object print-function)
      (error "DEFSTRUCT ~S: both PRINT-OBJECT and PRINT-FUNCTION are specified"
             struct-name))
    (let* ((documentation (when (stringp (car slot-description))
                            (pop slot-description)))
           (slots (append (when include
                            (structure-slots include))
                          (mapcar #'parse-slot slot-description)))
           (index 0))
      (labels
          ((accessor (slot-name)
             (intern (strcat conc-name slot-name)))
           (make-slot (slot)
             (let ((idx (prog1 index (incf index)))
                   (name (accessor (getf slot :name))))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (obj)
                    (assert-struct obj ',struct-name)
                    (%struct-ref obj ,idx))
                  ,@(unless (getf slot :read-only)
                      `((declaim (inline (setf ,name)))
                        (defun (setf ,name) (value obj)
                          (assert-struct obj ',struct-name)
                          (%struct-set obj ,idx value))))))))
        (unless predicate
          (setf predicate (make-symbol (strcat struct-name "-P"))))
        `(progn
           (make-structure ',struct-name
                           :slots ',slots
                           ,@(when include
                               `(:include ',(structure-name include)))
                           ,@(when print-object
                               `(:print-object ',print-object))
                           ,@(when print-function
                               `(:print-function ',print-function)))
           (declaim (inline ,predicate))
           (defun ,predicate (obj)
             (structurep obj ',struct-name))
           (deftype ,struct-name ()
             '(satisfies ,predicate))
           ,@(mapcar #'make-slot slots)
           ,@(when copier
               `((declaim (inline ,copier))
                 (defun ,copier (obj)
                   (assert-struct obj ',struct-name)
                   (copy-structure obj))))
           ,@(when constructor
               (cond
                 (constructor-arglist
                  (let* ((parsed (parse-lambda-list constructor-arglist))
                         (ctor-args (getf parsed :names))
                         (constructor-arglist (insert-defaults parsed slots)))
                    `((declaim (inline ,constructor))
                      (defun ,constructor ,constructor-arglist
                        (%struct (find-structure ',struct-name)
                                 ,@(mapcar (lambda (slot)
                                             (let ((name (getf slot :name)))
                                               (cond
                                                 ((member name ctor-args)
                                                  name)
                                                 (t
                                                  (getf slot :initform)))))
                                           slots))))))
                 (t
                  `((declaim (inline ,constructor))
                    (defun ,constructor
                           (&key ,@(mapcar (lambda (slot)
                                             (let ((name (getf slot :name))
                                                   (initform (getf slot :initform)))
                                               (cond
                                                 (initform
                                                  `(,name ,initform))
                                                 (t name))))
                                           slots))
                      (%struct (find-structure ',struct-name)
                               ,@(mapcar (lambda (slot) (getf slot :name)) slots)))))))
           ',struct-name)))))

(defun sl-type:type-of-structure (x)
  ;; we already know it's a struct at this point.
  (structure-name (%struct-struct x)))
