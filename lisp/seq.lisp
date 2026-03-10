(in-package :sl)

(export '(remove remove-if remove-if-not
          find find-if find-if-not
          position position-if position-if-not
          count count-if count-if-not
          substitute substitute-if substitute-if-not
          nsubstitute nsubstitute-if nsubstitute-if-not
          remove-duplicates delete-duplicates
          subseq concatenate map))

(defpackage :sl-seq
  (:use :sl :%))

(in-package :sl-seq)

(import '(%:filter
          sl::with-collectors
          sl-list::make-subject-test
          sl-list::update-for-key))

(defun remove-if (predicate list &key key (start 0) end count from-end)
  (update-for-key predicate key)
  (cond
    (count
     (cond
       (from-end
        (let* ((index (length list))
               (end (or end index)))
          (nreverse
           (filter (reverse list)
                   (lambda (x)
                     (or (< (decf index) start)
                         (and end (>= index end))
                         (not (funcall predicate x))
                         (<= count 0)
                         (progn (decf count) nil)))))))
       (t
        (let ((index -1))
          (filter list
                  (lambda (x)
                    (or (< (incf index) start)
                        (and end (>= index end))
                        (not (funcall predicate x))
                        (<= count 0)
                        (progn (decf count) nil))))))))
    (t
     (let ((index -1))
       (filter list
               (lambda (x)
                 (or (< (incf index) start)
                     (and end (>= index end))
                     (not (funcall predicate x)))))))))

(defun remove-if-not (predicate list &rest args)
  (apply #'remove-if (complement predicate) list args))

(defun remove (item list &key key test test-not (start 0) end count from-end)
  (setf test (make-subject-test test test-not key nil))
  (remove-if (lambda (el) (funcall test item el)) list
             :start start :end end :count count :from-end from-end))

(defun %list-delete-if (predicate list)
  (let* ((ret (cons nil list))
         (p ret))
    (tagbody
     :loop
       (when (cdr p)
         (if (funcall predicate (cadr p))
             (setf (cdr p) (cddr p))
             (setf p (cdr p)))
         (go :loop)))
    (cdr ret)))

(defun %list-delete-duplicates (list test)
  (flet ((same-as-first (x)
           (funcall test (car list) x)))
    (tagbody
     :loop
       (when list
         (setf list (setf (cdr list)
                          (%list-delete-if #'same-as-first (cdr list))))
         (go :loop)))))

(defun remove-duplicates (list &key key test test-not from-end)
  (setf list (if from-end (copy-seq list) (reverse list)))
  (%list-delete-duplicates list (make-subject-test test test-not key t))
  (if from-end list (nreverse list)))

(defun delete-duplicates (list &key key test test-not from-end)
  (unless from-end (setf list (nreverse list)))
  (%list-delete-duplicates list (make-subject-test test test-not key t))
  (if from-end list (nreverse list)))

(defconstant +no-value+ '(done))

(defun seq-iterator (seq)
  (cond
    ((null seq)
     (constantly +no-value+))
    ((listp seq)
     (lambda ()
       (if seq (pop seq) +no-value+)))
    ((or (vectorp seq)
         (stringp seq))
     (let ((i -1)
           (len (length seq)))
       (lambda ()
         (if (< (incf i) len)
             (svref seq i)
             +no-value+))))
    (t
     (error "SEQ-ITERATOR: unknown sequence"))))

(defmacro with-list-frobnicator ((&key replace (from-end t) alt) &body body)
  (let ((tail (when replace (gensym "tail")))
        (reverse (if replace 'nreverse 'reverse))
        (alt-it (when alt (intern (strcat alt "-ITER"))))
        (alt-el (when alt (intern (strcat alt "-EL")))))
    `(macrolet (,@(when replace
                    `((replace-with (val)
                                    `(setf (car ,',tail) ,val))))
                (return-sequence ()
                  `(return (if from-end (nreverse list) list))))
       (cond
         ,(when from-end
            `(from-end
              (cond
                (end
                 (loop with len = (length list)
                       with list = (,reverse list)
                       with froblist = (nthcdr (- len end) list)
                       ,@(if tail
                             `(for ,tail on froblist for el = (car ,tail))
                             `(for el in froblist))
                       for index downfrom (1- end) to start
                       ,@body))
                (t
                 (loop with len = (length list)
                       with list = (,reverse list)
                       ,@(if tail
                             `(for ,tail on list for el = (car ,tail))
                             `(for el in list))
                       for index downfrom (1- len) to start
                       ,@body)))))
         (t
          (cond
            (end
             (loop with froblist = (nthcdr start list)
                   ,@(when alt
                       `(with ,alt-it = (seq-iterator ,alt)))
                   ,@(if tail
                         `(for ,tail on froblist for el = (car ,tail))
                         `(for el in froblist))
                   ,@(when alt
                       `(for ,alt-el = (funcall ,alt-it) until (eq ,alt-el +no-value+)))
                   for index from start below end
                   ,@body))
            (t
             (loop with froblist = (nthcdr start list)
                   ,@(when alt
                       `(with ,alt-it = (seq-iterator ,alt)))
                   ,@(if tail
                         `(for ,tail on froblist for el = (car ,tail))
                         `(for el in froblist))
                   ,@(when alt
                       `(for ,alt-el = (funcall ,alt-it) until (eq ,alt-el +no-value+)))
                   for index from start
                   ,@body))))))))

(defmacro with-vector-frobnicator ((&key replace (from-end t) alt) &body body)
  (let ((alt-it (when alt (intern (strcat alt "-ITER"))))
        (alt-el (when alt (intern (strcat alt "-EL")))))
    `(macrolet (,@(when replace
                    `((replace-with (val)
                                    `(setf (svref list index) ,val))))
                (return-sequence ()
                  `(return list)))
       (unless end
         (setf end (length list)))
       (cond
         ,(when from-end
            `(from-end
              (loop for index downfrom (1- end) to start
                    for el = (svref list index)
                    ,@body)))
         (t
          (loop ,@(when alt
                    `(with ,alt-it = (seq-iterator ,alt)))
                for index from start below end
                for el = (svref list index)
                ,@(when alt
                    `(for ,alt-el = (funcall ,alt-it) until (eq ,alt-el +no-value+)))
                ,@body))))))

(defmacro with-seq-frobnicator (args &body body)
  `(cond
     ((listp list)
      (with-list-frobnicator ,args ,@body))
     ((or (vectorp list)
          (stringp list))
      (with-vector-frobnicator ,args ,@body))
     (t
      (error (error "WITH-SEQ-FROBNICATOR: unknown sequence")))))

(defun find-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-seq-frobnicator ()
    :when (funcall predicate el) :do (return el)))

(defun find-if-not (predicate list &rest args)
  (apply #'find-if (complement predicate) list args))

(defun find (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-seq-frobnicator ()
    :when (funcall test item el) :do (return el)))

(defun position-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-seq-frobnicator ()
    :when (funcall predicate el) :do (return index)))

(defun position-if-not (predicate list &rest args)
  (apply #'position-if (complement predicate) list args))

(defun position (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-seq-frobnicator ()
    :when (funcall test item el) :do (return index)))

(defun count-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-seq-frobnicator ()
    :count (funcall predicate el)))

(defun count-if-not (predicate list &rest args)
  (apply #'count-if (complement predicate) list args))

(defun count (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-seq-frobnicator ()
    :count (funcall test item el)))

(defun substitute-if (newitem predicate list &key key (start 0) end from-end count destructive)
  (update-for-key predicate key)
  (unless destructive
    ;; the frobnicator is destructive
    (setf list (copy-seq list)))
  (with-seq-frobnicator (:replace t)
    :when (and (or (not count)
                   (plusp count))
               (funcall predicate el))
    :do (progn
          (replace-with newitem)
          (when count
            (unless (plusp (decf count))
              (return-sequence))))
    :finally (return-sequence)))

(defun substitute-if-not (newitem predicate list &rest args)
  (apply #'substitute-if newitem (complement predicate) list args))

(defun substitute (newitem item list &key key test test-not (start 0) end from-end count destructive)
  (setf test (make-subject-test test test-not key nil))
  (unless destructive
    ;; the frobnicator is destructive
    (setf list (copy-seq list)))
  (with-seq-frobnicator (:replace t)
    :when (and (or (not count)
                   (plusp count))
               (funcall test item el))
    :do (progn
          (replace-with newitem)
          (when count
            (unless (plusp (decf count))
              (return-sequence))))
    :finally (return-sequence)))

(defun nsubstitute-if (newitem predicate list &rest args)
  (apply #'substitute-if newitem predicate list :destructive t args))

(defun nsubstitute-if-not (newitem predicate list &rest args)
  (apply #'substitute-if-not newitem predicate list :destructive t args))

(defun nsubstitute (newitem item list &rest args)
  (apply #'substitute newitem item list :destructive t args))

(defun subseq (list start &optional end)
  (with-seq-frobnicator (:from-end nil)
    :collect el))

(defun (setf subseq) (newseq list start &optional end)
  (with-seq-frobnicator (:from-end nil :alt newseq :replace t)
    :do (replace-with newseq-el)
    :finally (return newseq)))

;; this macro simply serves the role of copy/paste. indeed, it is horrible.
(defmacro concafrob ()
  `(progn
     (when (consp result-type)
       (setf result-type (car result-type)))
     (ecase result-type
       ((string simple-string)
        (with-output-to-string (out)
          (doit (%stream-put out val))))
       ((array vector simple-vector)
        (let ((out (make-array 0 :fill-pointer 0 :adjustable t)))
          (doit (vector-push-extend val out))
          out))
       ((list cons)
        (with-collectors (out)
          (doit (out val))
          out))
       (null
        (loop for seq in sequences
              do (assert (zerop (length seq))
                         "CONCATENATE: non-empty sequence with NULL output type"))
        nil))))

(defun concatenate (result-type &rest sequences)
  (macrolet ((doit (add)
               `(loop for seq in sequences
                      for it = (seq-iterator seq)
                      do (loop for val = (funcall it)
                               until (eq val +no-value+)
                               do ,add))))
    (concafrob)))

;; XXX: in terms of performance, this is, of course, horrendous.
(defun map (result-type function &rest sequences)
  (let ((iterators (mapcar #'seq-iterator sequences)))
    (macrolet ((doit (add)
                 `(loop named outer
                        for args = (loop for it in iterators
                                         for arg = (funcall it)
                                         if (eq arg +no-value+)
                                         do (return-from outer nil)
                                         else collect arg)
                        for val = (apply function args)
                        do ,add)))
      (concafrob))))
