(in-package :sl-user)

(defparameter *tests* (list))
(defparameter *compile-time* 0)
(defparameter *run-time* 0)

(defmacro time-it (dest form)
  (let ((t1 (gensym)))
    `(let ((,t1 (get-internal-run-time)))
       (multiple-value-prog1
           ,form
         (setf ,dest (- (get-internal-run-time) ,t1))))))

(defmacro deftest (name &rest args)
  (%:maybe-xref-info name 'deftest)
  (let ((notes
         (loop while (keywordp (car args))
               collect (cons (pop args) (pop args)))))
    (destructuring-bind (form &rest expected) args
      (let* ((val (gensym))
             (exp (gensym))
             (comp (gensym))
             (global (intern (format nil "TEST.~A" name)))
             (ok (gensym)))
        `(flet ((,name ()
                  (let (,val ,exp ,comp ,ok)
                    (format t "#'~S ..." ',name)
                    (setf ,exp ',expected)
                    (handler-bind
                        ((error (lambda (condition)
                                  (format t " FAIL.~%!ERROR: ~A~%!ERROR: ~S~%"
                                          condition (%:%backtrace))
                                  (throw 'test-error 'test-error))))
                      (setf ,comp
                            (catch 'test-error
                              (time-it *compile-time*
                                       (compile (list '%:%fn ',name nil ',form)))))
                      (cond
                        ((eq ,comp 'test-error)
                         (setf ,ok nil))
                        (t
                         (defparameter ,global ,comp)
                         (setf ,val (time-it *run-time*
                                             (multiple-value-list (catch 'test-error
                                                                    (funcall ,comp)))))
                         (setf ,ok (equalp ,val ,exp))
                         (if ,ok
                             (format t " OK~%")
                             (format t " FAIL - ~A~%" ,val)))))
                    ,(when notes
                       `(format t "    ~A~%" ',notes))
                    ,ok)))
           (setf (getf *tests* ',name) #',name))))))

(defun get-test (name)
  (getf *tests* name))

(defun test (name)
  (funcall (get-test name)))

(defun run-tests (&optional match-name)
  (let ((*compile-time* 0)
        (*run-time* 0))
    (let ((tests (if match-name
                     (loop for (name func) on *tests* by #'cddr
                           when (regexp-test match-name (symbol-name name))
                           nconc (list name func))
                     *tests*)))
      (loop for (func name) on (reverse tests) by #'cddr
            for test from 1
            for ok = (funcall func)
            counting ok into success
            summing *compile-time* into compile-time
            summing *run-time* into run-time
            finally (format t "~A tests, ~A OK~%Compile time: ~,2Fms~%Run time: ~,2Fms~%"
                            test success
                            compile-time run-time))
      'done)))

;;;; utils from ansi-test

(defun notnot (x) (not (not x)))

(defmacro notnot-mv (form)
  `(notnot-mv-fn (multiple-value-list ,form)))

(defun notnot-mv-fn (results)
  (if (null results)
      (values)
      (apply #'values
             (not (not (first results)))
             (cdr results))))

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eq x y)))))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))

(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equal x y)))))

(defun equalpt (x y)
  "Like EQUALP, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equalp x y)))))

(defun is-eq-p (x) #'(lambda (y) (eqt x y)))
(defun is-not-eq-p (x) #'(lambda (y) (not (eqt x y))))

(defun is-eql-p (x) #'(lambda (y) (eqlt x y)))
(defun is-not-eql-p (x) #'(lambda (y) (not (eqlt x y))))

(defmacro expand-in-current-env (form)
  (macroexpand form))

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Try out the format control and format arguments of a simple-condition C,
   but make no assumptions about what they print as, only that they
   do print."
  (and (typep c 'simple-condition)
       (let ((fc (slot-value c :format-control))
             (args (slot-value c :format-arguments)))
         (and
          (stringp (apply #'format nil fc args))
          t))))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defmacro defharmless (name form)
  `(deftest ,name
     (block done
       (let ((*debugger-hook* (lambda (&rest args)
                                (declare (ignore args))
                                (return-from done :good))))
         (handler-case
             (unwind-protect (eval ',form) (return-from done :good))
           (condition () :good))))
     :good))

;; XXX: changed the following two, as I don't yet have defstruct.
(defun make-scaffold-copy (x)
  "Make a tree that will be used to check if a tree has been changed."
  (if (consp x)
      (list x
            (make-scaffold-copy (car x))
            (make-scaffold-copy (cdr x)))
      (list x nil nil)))

(defun check-scaffold-copy (x xcopy)
  "Return t if xcopy were produced from x by make-scaffold-copy,
   and none of the cons cells in the tree rooted at x have been
   changed."
  (and (eq x (pop xcopy))
       (or (not (consp x))
           (and (check-scaffold-copy (car x) (pop xcopy))
                (check-scaffold-copy (cdr x) (pop xcopy))))))

(defmacro def-fold-test (name form)
  "Create a test that FORM, which should produce a fresh value,
   does not improperly introduce sharing during constant folding."
  `(deftest ,name
     (flet ((%f () (declare (optimize (speed 3) (safety 0) (space 0)
                                      (compilation-speed 0) (debug 0)))
              ,form))
       (eq (%f) (%f)))
     nil))
