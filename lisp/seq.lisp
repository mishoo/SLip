(in-package :sl)

(export '(sort stable-sort merge
          collect-if
          remove remove-if remove-if-not
          find find-if find-if-not
          position position-if position-if-not
          count count-if count-if-not
          substitute substitute-if substitute-if-not
          nsubstitute nsubstitute-if nsubstitute-if-not
          remove-duplicates delete-duplicates
          subseq))

(defpackage :sl-seq
  (:use :sl :%))

(in-package :sl-seq)

(import '(sl::with-collectors
          sl-list::make-subject-test
          sl-list::update-for-key))

(defun merge (a b predicate)
  (let* ((ret (list nil))
         (p ret))
    (macrolet ((add (cell)
                 `(setf p (setf (cdr p) ,cell))))
      (tagbody
       :loop
       (cond ((and a b)
              (if (funcall predicate (car b) (car a))
                  (setf b (cdr (add b)))
                  (setf a (cdr (add a))))
              (go :loop))
             (a (add a))
             (b (add b))))
      (cdr ret))))

(defun stable-sort (list predicate)
  (let sort ((list list))
    (cond ((not list) nil)
          ((not (cdr list)) list)
          (t (let ((a list)
                   (b (%nhalf-list list)))
               (merge (sort a) (sort b) predicate))))))

(setf (symbol-function 'sort) #'stable-sort)

(defun collect-if (test list)
  (with-collectors (elements)
    (dolist (el list elements)
      (when (funcall test el)
        (elements el)))))

(defun remove-if (predicate list &key key (start 0) end count from-end)
  (update-for-key predicate key)
  (cond
    (count
     (cond
       (from-end
        (let* ((index (length list))
               (end (or end index)))
          (nreverse
           (collect-if (lambda (x)
                         (or (< (decf index) start)
                             (and end (>= index end))
                             (not (funcall predicate x))
                             (<= count 0)
                             (progn (decf count) nil)))
                       (reverse list)))))
       (t
        (let ((index -1))
          (collect-if (lambda (x)
                        (or (< (incf index) start)
                            (and end (>= index end))
                            (not (funcall predicate x))
                            (<= count 0)
                            (progn (decf count) nil)))
                      list)))))
    (t
     (let ((index -1))
       (collect-if (lambda (x)
                     (or (< (incf index) start)
                         (and end (>= index end))
                         (not (funcall predicate x)))) list)))))

(defun remove-if-not (predicate list &rest args)
  (apply #'remove-if (complement predicate) list args))

(defun remove (item list &key key test test-not (start 0) end count from-end)
  (setf test (make-subject-test test test-not key nil))
  (remove-if (lambda (el) (funcall test item el)) list
             :start start :end end :count count :from-end from-end))

(defun %erase-if (list predicate)
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

(defun %delete-duplicates (list test)
  (tagbody
   :loop
   (when list
     (let ((current (car list)))
       (setf list
             (setf (cdr list)
                   (%erase-if (cdr list)
                              (lambda (x)
                                (funcall test current x)))))
       (go :loop)))))

(defun remove-duplicates (list &key key test test-not from-end)
  (setf list (if from-end (copy-list list) (reverse list)))
  (%delete-duplicates list (make-subject-test test test-not key t))
  (if from-end list (nreverse list)))

(defun delete-duplicates (list &key key test test-not from-end)
  (unless from-end (setf list (nreverse list)))
  (%delete-duplicates list (make-subject-test test test-not key t))
  (if from-end list (nreverse list)))

(defmacro with-list-frobnicator ((&key replace (from-end t) alt) &body body)
  (let* ((tail (when replace (gensym "tail")))
         (reverse (if tail 'nreverse 'reverse))
         (alt-el (when alt (intern (strcat alt "-EL")))))
    `(macrolet (,@(when tail
                    `((replace-with (val)
                                    `(setf (car ,',tail) ,val)))))
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
                   ,@(if tail
                         `(for ,tail on froblist for el = (car ,tail))
                         `(for el in froblist))
                   ,@(when alt
                       `(for ,alt-el in ,alt))
                   for index from start below end
                   ,@body))
            (t
             (loop with froblist = (nthcdr start list)
                   ,@(if tail
                         `(for ,tail on froblist for el = (car ,tail))
                         `(for el in froblist))
                   ,@(when alt
                       `(for ,alt-el in ,alt))
                   for index from start
                   ,@body))))))))

(defun find-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-list-frobnicator ()
    :when (funcall predicate el) :do (return el)))

(defun find-if-not (predicate list &rest args)
  (apply #'find-if (complement predicate) list args))

(defun find (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-list-frobnicator ()
    :when (funcall test item el) :do (return el)))

(defun position-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-list-frobnicator ()
    :when (funcall predicate el) :do (return index)))

(defun position-if-not (predicate list &rest args)
  (apply #'position-if (complement predicate) list args))

(defun position (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-list-frobnicator ()
    :when (funcall test item el) :do (return index)))

(defun count-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (with-list-frobnicator ()
    :count (funcall predicate el)))

(defun count-if-not (predicate list &rest args)
  (apply #'count-if (complement predicate) list args))

(defun count (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (with-list-frobnicator ()
    :count (funcall test item el)))

(defun substitute-if (newitem predicate list &key key (start 0) end from-end count destructive)
  (update-for-key predicate key)
  (unless destructive
    ;; the frobnicator is destructive
    (setf list (copy-list list)))
  (with-list-frobnicator (:replace t)
    :when (and (or (not count)
                   (plusp count))
               (funcall predicate el))
    :do (progn
          (replace-with newitem)
          (when count
            (unless (plusp (decf count))
              (return (if from-end (nreverse list) list)))))
    :finally (return (if from-end (nreverse list) list))))

(defun substitute-if-not (newitem predicate list &rest args)
  (apply #'substitute-if newitem (complement predicate) list args))

(defun substitute (newitem item list &key key test test-not (start 0) end from-end count destructive)
  (setf test (make-subject-test test test-not key nil))
  (unless destructive
    ;; the frobnicator is destructive
    (setf list (copy-list list)))
  (with-list-frobnicator (:replace t)
    :when (and (or (not count)
                   (plusp count))
               (funcall test item el))
    :do (progn
          (replace-with newitem)
          (when count
            (unless (plusp (decf count))
              (return (if from-end (nreverse list) list)))))
    :finally (return (if from-end (nreverse list) list))))

(defun nsubstitute-if (newitem predicate list &rest args)
  (apply #'substitute-if newitem predicate list :destructive t args))

(defun nsubstitute-if-not (newitem predicate list &rest args)
  (apply #'substitute-if-not newitem predicate list :destructive t args))

(defun nsubstitute (newitem item list &rest args)
  (apply #'substitute newitem item list :destructive t args))

(defun subseq (list start &optional end)
  (with-list-frobnicator (:from-end nil)
    :collect el))

(defun (setf subseq) (newseq list start &optional end)
  (with-list-frobnicator (:from-end nil :alt newseq :replace t)
    :do (replace-with newseq-el)
    :finally (return newseq)))

;; (defun sublis (alist tree &key key test test-not)
;;   (setf test (make-subject-test test test-not key nil))
;;   (let rec ((tree tree))
;;     (when tree
;;       (let* ((el (car tree))
;;              (match (some (lambda (cell)
;;                             (funcall test el (car cell)))
;;                           alist)))))))
