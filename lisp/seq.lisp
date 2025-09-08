(in-package :sl)

(export '(collect-if remove remove-duplicates
          sort stable-sort merge))

(defpackage :sl-seq
  (:use :sl :%))

(in-package :sl-seq)

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

(defun make-subject-test (test key key-subject)
  (when (member test *default-test*)
    (setf test nil))
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

(defun remove (item list &key test key count from-end)
  (cond
    (count
     (cond
       (from-end
        (nreverse (remove item (reverse list)
                          :test test :key key :count count)))
       (t
        (setf test (make-subject-test test key nil))
        (collect-if (lambda (x)
                      (or (not (funcall test item x))
                          (<= count 0)
                          (progn (decf count) nil)))
                    list))))
    (t
     (setf test (make-subject-test test key nil))
     (collect-if (lambda (x)
                   (not (funcall test item x))) list))))

(defun remove-duplicates (list &key test key from-end)
  (setf test (make-subject-test test key t))
  (labels ((rmv (list ret)
             (if list
                 (let ((current (car list)))
                   (rmv (collect-if (lambda (x)
                                      (not (funcall test current x)))
                                    (cdr list))
                        (cons current ret)))
                 ret)))
    (if from-end
        (nreverse (rmv list nil))
        (rmv (reverse list) nil))))