(in-package :sl)

(export '(copy-structure defstruct))

(defpackage :sl-struct
  (:use :sl :%))

(in-package :sl-struct)

(defconstant *structures* (make-hash-table))
(defconstant *stag* '%struct)

(defun substructp (name struct)
  (let ((sub (structure-include struct)))
    (when sub
      (if (eq name (structure-name sub))
          t
          (substructp name sub)))))

(defun structurep (thing &optional name)
  (and (vectorp thing)
       (eq *stag* (svref thing 0))
       (if name
           (let ((x (svref thing 1)))
             (or (eq x name)
                 (substructp name (find-structure x))))
           t)))

(defun assert-struct (thing &optional name)
  (unless (structurep thing name)
    (if name
        (error "Expected structure ~S." name)
        (error "Expected structure."))))

(defun find-structure (name &optional (errorp t))
  (or (gethash *structures* name)
      (when errorp
        (error "No such structure ~S." name))))

(defun (setf find-structure) (struct name)
  (setf (gethash *structures* name) struct))

(defun copy-structure (structure)
  (assert-struct structure)
  (copy-seq structure))

(defun make-structure (name &key slots include)
  (when (find-structure name nil)
    (warn "Redefining structure ~S." name))
  (when include
    (setf include (find-structure include)))
  (setf (find-structure name)
        (vector *stag* 'structure name slots include)))

(make-structure 'structure
                :slots '((:name name :read-only t :type symbol)
                         (:name slots :read-only t :type list)
                         (:name include :read-only t :type symbol)))

(defun structure-name (struct)
  (assert-struct struct 'structure)
  (svref struct 2))

(defun structure-slots (struct)
  (assert-struct struct 'structure)
  (svref struct 3))

(defun structure-include (struct)
  (assert-struct struct 'structure)
  (svref struct 4))

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
         (print-function nil)
         (print-object nil)
         (predicate (intern (strcat name "-P")))
         (include nil))
    (dolist (opt (cdr args)
                 (values name
                         constructor
                         constructor-arglist
                         conc-name
                         copier
                         print-function
                         print-object
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
        ((:print-function)
         (setf print-function (cadr opt)))
        ((:print-object)
         (setf print-object (cadr opt)))
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
                        print-function
                        print-object
                        predicate
                        include)
                       (parse-name-and-options name-and-options)
    (when include
      (setf include (find-structure include)))
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
             (let ((idx (1+ (incf index)))
                   (name (accessor (getf slot :name))))
               `(progn
                  (defun ,name (obj)
                    (assert-struct obj ',struct-name)
                    (svref obj ,idx))
                  ,@(unless (getf slot :read-only)
                      `((defun (setf ,name) (value obj)
                          (assert-struct obj ',struct-name)
                          (setf (svref obj ,idx) value))))))))
        `(progn
           (make-structure ',struct-name
                           :slots ',slots
                           ,@(when include `(:include ',(structure-name include))))
           ,@(mapcar #'make-slot slots)
           ,@(when predicate
               `((defun ,predicate (obj)
                   (structurep obj ',struct-name))))
           ,@(when copier
               `((defun ,copier (obj)
                   (assert-struct obj ',struct-name)
                   (copy-seq obj))))
           ,@(when constructor
               (cond
                 (constructor-arglist
                  (let* ((parsed (parse-lambda-list constructor-arglist))
                         (ctor-args (getf parsed :names))
                         (constructor-arglist (insert-defaults parsed slots)))
                    `((defun ,constructor ,constructor-arglist
                        (vector *stag* ',struct-name
                                ,@(mapcar (lambda (slot)
                                            (let ((name (getf slot :name)))
                                              (cond
                                                ((%memq name ctor-args)
                                                 name)
                                                (t
                                                 (getf slot :initform)))))
                                          slots))))))
                 (t
                  `((defun ,constructor
                           (&key ,@(mapcar (lambda (slot)
                                             (let ((name (getf slot :name))
                                                   (initform (getf slot :initform)))
                                               (cond
                                                 (initform
                                                  `(,name ,initform))
                                                 (t name))))
                                           slots))
                      (vector *stag* ',struct-name
                              ,@(mapcar (lambda (slot) (getf slot :name)) slots)))))))
           ',struct-name)))))