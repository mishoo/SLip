(in-package :sl)

(export '(sort stable-sort merge
          collect-if
          remove remove-if remove-if-not
          find find-if find-if-not
          position position-if position-if-not
          count count-if count-if-not
          remove-duplicates delete-duplicates
          intersection set-difference))

(defpackage :sl-seq
  (:use :sl :%))

(in-package :sl-seq)

(import '(sl::with-collectors))

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

(defconstant *default-test* (list #'eql #'eq 'eql 'eq))
(defconstant *default-key* (list #'identity 'identity))

(defun make-subject-test (test test-not key key-subject)
  (cond
    (test-not
     (when test
       (error "Both TEST and TEST-NOT are supplied"))
     (setf test (complement test-not)))
    ((member test *default-test*)
     (setf test nil)))
  (when (member key *default-key*)
    (setf key nil))
  (if key
      (if test
          (if key-subject
              (lambda (subject el)
                (funcall test
                         (funcall key subject)
                         (funcall key el)))
              (lambda (subject el)
                (funcall test
                         subject
                         (funcall key el))))
          (if key-subject
              (lambda (subject el)
                (eq (funcall key subject)
                    (funcall key el)))
              (lambda (subject el)
                (eq subject
                    (funcall key el)))))
      (if test
          (lambda (subject el)
            (funcall test subject el))
          (lambda (subject el)
            (eq subject el)))))

(defmacro update-for-key (predicate key)
  `(when (and ,key (not (member ,key *default-key*)))
     (setf ,predicate (let ((p ,predicate))
                        (lambda (item)
                          (funcall p (funcall ,key item)))))))

(defun remove-if (predicate list &key key count from-end)
  (update-for-key predicate key)
  (cond
    (count
     (cond
       (from-end
        (nreverse (remove-if predicate (reverse list) :key key :count count)))
       (t
        (collect-if (lambda (x)
                      (or (not (funcall predicate x))
                          (<= count 0)
                          (progn (decf count) nil)))
                    list))))
    (t
     (collect-if (lambda (x)
                   (not (funcall predicate x))) list))))

(defun remove-if-not (predicate list &key key count from-end)
  (update-for-key predicate key)
  (cond
    (count
     (cond
       (from-end
        (nreverse (remove-if predicate (reverse list) :key key :count count)))
       (t
        (collect-if (lambda (x)
                      (or (funcall predicate x)
                          (<= count 0)
                          (progn (decf count) nil)))
                    list))))
    (t
     (collect-if (lambda (x)
                   (funcall predicate x)) list))))

(defun remove (item list &key key test test-not count from-end)
  (cond
    (count
     (cond
       (from-end
        (nreverse (remove item (reverse list)
                          :test test :test-not test-not :key key :count count)))
       (t
        (setf test (make-subject-test test test-not key nil))
        (collect-if (lambda (x)
                      (or (not (funcall test item x))
                          (<= count 0)
                          (progn (decf count) nil)))
                    list))))
    (t
     (setf test (make-subject-test test test-not key nil))
     (collect-if (lambda (x)
                   (not (funcall test item x))) list))))

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

(defun intersection (list1 list2 &key key test test-not)
  (let ((test (make-subject-test test test-not key t))
        (res nil))
    (dolist (item list1 res)
      (when (some (lambda (el) (funcall test item el)) list2)
        (push item res)))))

(defun set-difference (list1 list2 &key key test test-not)
  (let ((test (make-subject-test test test-not key t))
        (res nil))
    (dolist (item list1 res)
      (unless (some (lambda (el) (funcall test item el)) list2)
        (push item res)))))



(defmacro frobnicate-list-utility (&body body)
  `((cond
      ((not from-end)
       (loop for el in (nthcdr start list)
             for i from start
             while (or (not end) (< i end))
             ,@body))
      (t
       (loop with len = (length list)
             for el in (if end
                           (nthcdr (- len end) (reverse list))
                           (reverse list))
             for i downfrom (or end len)
             while (> i start)
             ,@body)))))




(defun find-if (predicate list &key key from-end)
  (update-for-key predicate key)
  (dolist (el (if from-end (reverse list) list))
    (when (funcall predicate el)
      (return el))))

(defun find-if-not (predicate list &key key from-end)
  (update-for-key predicate key)
  (dolist (el (if from-end (reverse list) list))
    (unless (funcall predicate el)
      (return el))))

(defun find (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (cond
    ((not from-end)
     (loop for el in (nthcdr start list)
           for i from start
           while (or (not end) (< i end))
           when (funcall test item el) do (return el)))
    (t
     (loop with len = (length list)
           for el in (if end
                         (nthcdr (- len end) (reverse list))
                         (reverse list))
           for i downfrom (or end len)
           while (> i start)
           when (funcall test item el) do (return el)))))

(defun position-if (predicate list &key key from-end)
  (update-for-key predicate key)
  (loop for el in (if from-end (reverse list) list)
        for index from 0
        do (when (funcall predicate el)
             (return (if from-end
                         (- (length list) (1+ index))
                         index)))))

(defun position-if-not (predicate list &rest args)
  (apply #'position-if (complement predicate) list args))

(defun position (item list &key key test test-not from-end)
  (setf test (make-subject-test test test-not key nil))
  (loop for el in (if from-end (reverse list) list)
        for index from 0
        do (when (funcall test item el)
             (return (if from-end
                         (- (length list) (1+ index))
                         index)))))

(defun count-if (predicate list &key key (start 0) end from-end)
  (update-for-key predicate key)
  (cond
    ((not from-end)
     (loop for el in (nthcdr start list)
           for i from start
           while (or (not end) (< i end))
           count (funcall predicate el)))
    (t
     (loop with len = (length list)
           for el in (if end
                         (nthcdr (- len end) (reverse list))
                         (reverse list))
           for i downfrom (or end len)
           while (> i start)
           count (funcall predicate el)))))

(defun count-if-not (predicate list &rest args)
  (apply #'count-if (complement predicate) list args))

(defun count (item list &key key test test-not (start 0) end from-end)
  (setf test (make-subject-test test test-not key nil))
  (cond
    ((not from-end)
     (loop for el in (nthcdr start list)
           for i from start
           while (or (not end) (< i end))
           count (funcall test item el)))
    (t
     (loop with len = (length list)
           for el in (if end
                         (nthcdr (- len end) (reverse list))
                         (reverse list))
           for i downfrom (or end len)
           while (> i start)
           count (funcall test item el)))))