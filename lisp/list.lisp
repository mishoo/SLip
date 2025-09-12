(in-package :sl)

(export '(acons pairlis
          assoc assoc-if assoc-if-not
          rassoc rassoc-if rassoc-if-not
          intersection union set-difference
          subst-if subst-if-not subst
          sublis butlast))

(defpackage :sl-list
  (:use :sl :%))

(in-package :sl-list)

(import '(sl::with-collectors))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun pairlis (keys data &optional alist)
  (do ((keys keys (cdr keys))
       (data data (cdr data))
       (result alist (acons (car keys) (car data) result)))
      ((or (null keys) (null data)) result)))

(defconstant *default-test* (list #'eql #'eq 'eql 'eq))
(defconstant *default-key* (list #'identity 'identity))

(defmacro update-for-key (predicate key)
  `(when (and ,key (not (member ,key *default-key*)))
     (setf ,predicate (let ((p ,predicate))
                        (lambda (item)
                          (funcall p (funcall ,key item)))))))

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
          (cond
            ((eq key-subject :only)
             (lambda (subject el)
               (funcall test
                        (funcall key subject)
                        el)))
            (key-subject
             (lambda (subject el)
               (funcall test
                        (funcall key subject)
                        (funcall key el))))
            ((lambda (subject el)
               (funcall test
                        subject
                        (funcall key el)))))
          (cond
            ((eq key-subject :only)
             (lambda (subject el)
               (eq (funcall key subject) el)))
            (key-subject
             (lambda (subject el)
               (eq (funcall key subject)
                   (funcall key el))))
            ((lambda (subject el)
               (eq subject
                   (funcall key el))))))
      (or test #'eq)))

(define-compiler-macro assoc (&whole form item lst &key test test-not key)
  (cond
    ((and (not test-not)
          (or (not test) (member test sl::+member-safe-test+ :test #'equal))
          (or (not key) (member key sl::+member-safe-key+ :test #'equal)))
     `(%:%assq ,item ,lst))
    (t form)))

(defun assoc (item list &key key test test-not)
  (setf test (make-subject-test test test-not key nil))
  (cond
    ((eq test #'eq)
     (%:%assq item list))
    ((dolist (pair list)
       (when (and pair (funcall test item (car pair)))
         (return pair))))))

(defun assoc-if (predicate list &key key)
  (update-for-key predicate key)
  (dolist (pair list)
    (when (and pair (funcall predicate (car pair)))
      (return pair))))

(defun assoc-if-not (predicate list &key key)
  (update-for-key predicate key)
  (dolist (pair list)
    (when pair
      (unless (funcall predicate (car pair))
        (return pair)))))

(defun rassoc (item list &key key test test-not)
  (setf test (make-subject-test test test-not key nil))
  (dolist (pair list)
    (when (and pair (funcall test item (cdr pair)))
      (return pair))))

(defun rassoc-if (predicate list &key key)
  (update-for-key predicate key)
  (dolist (pair list)
    (when (and pair (funcall predicate (cdr pair)))
      (return pair))))

(defun rassoc-if-not (predicate list &key key)
  (update-for-key predicate key)
  (dolist (pair list)
    (when pair
      (unless (funcall predicate (cdr pair))
        (return pair)))))

(defun intersection (list1 list2 &key key test test-not)
  (let ((test (make-subject-test test test-not key t))
        (res nil))
    (dolist (item list1 res)
      (when (some (lambda (el) (funcall test item el)) list2)
        (push item res)))))

(defun union (list1 list2 &key key test test-not)
  (multiple-value-bind (list1 list2)
                       (if (< (length list1) (length list2))
                           (values list2 list1)
                           (values list1 list2))
    (let ((test (make-subject-test test test-not key t))
          (res list2))
      (dolist (item list1 res)
        (when (notany (lambda (el) (funcall test item el)) list2)
          (push item res))))))

(defun set-difference (list1 list2 &key key test test-not)
  (let ((test (make-subject-test test test-not key t))
        (res nil))
    (dolist (item list1 res)
      (unless (some (lambda (el) (funcall test item el)) list2)
        (push item res)))))

(defun subst-if (new predicate tree &key key)
  (update-for-key predicate key)
  (let rec ((tree tree))
    (cond
      ((funcall predicate tree)
       new)
      ((atom tree)
       tree)
      ((let ((car (rec (car tree)))
             (cdr (rec (cdr tree))))
         (if (and (eql car (car tree))
                  (eql cdr (cdr tree)))
             tree
             (cons car cdr)))))))

(defun subst-if-not (new predicate tree &rest args)
  (apply #'subst-if new (complement predicate) tree args))

(defun subst (new item tree &key key test test-not)
  (setf test (make-subject-test test test-not key nil))
  (subst-if new (lambda (el) (funcall test item el)) tree))

(defun sublis (alist tree &key key test test-not)
  (setf test (make-subject-test test test-not key :only))
  (let rec ((tree tree))
    (cond
      ((aif (assoc-if (lambda (el) (funcall test tree el)) alist)
            (cdr it)))
      ((atom tree)
       tree)
      ((let ((car (rec (car tree)))
             (cdr (rec (cdr tree))))
         (if (and (eql car (car tree))
                  (eql cdr (cdr tree)))
             tree
             (cons car cdr)))))))

;;; the following two are (slightly modified) from SBCL (list.lisp)

;; like NTHCDR but doesn't fail on dotted lists
(defun dotted-nthcdr (n list)
  (do ((i n (1- i))
       (result list (cdr result)))
      ((not (plusp i)) result)
    (when (atom result)
      (return nil))))

(defun butlast (list &optional (n 1))
  (cond
    ((zerop n)
     (copy-list list))
    (t
     (let ((head (dotted-nthcdr n list)))
       (and (consp head)                      ; there are at least n
            (with-collectors (copy)           ; conses; copy!
              (do ((trail list (cdr trail))
                   (head head (cdr head)))
                  ;; HEAD is n conses ahead of TRAIL;
                  ;; when HEAD is NIL (or not a cons), return
                  ;; the data copied so far.
                  ((atom head)
                   copy)
                (copy (car trail)))))))))
