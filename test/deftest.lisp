(in-package :sl-user)

(defparameter *tests* (list))

(defmacro deftest (name &rest args)
  (%:maybe-xref-info name 'defun)
  (let ((notes
         (loop while (keywordp (car args))
               collect (cons (pop args) (pop args)))))
    (destructuring-bind (form &rest expected) args
      (let ((val (gensym))
            (exp (gensym))
            (ok (gensym)))
        `(flet ((,name ()
                  (let (,val ,exp ,ok)
                    (format t "Testing ~A ..." ',name)
                    (setf ,exp ',expected)
                    (handler-bind
                        ((error (lambda (condition)
                                  (format t " FAIL.~%!ERROR: ~A~%!ERROR: ~S~%"
                                          condition (%:%backtrace))
                                  (throw 'test-error '#:test-error))))
                      (setf ,val (catch 'test-error
                                   (multiple-value-list (eval ',form))))
                      (setf ,ok (equal ,val ,exp))
                      (if ,ok
                          (format t " OK~%")
                          (format t " FAIL - ~A~%" ,val)))
                    ,(when notes
                       `(format t "    ~A~%" ',notes))
                    ,ok)))
           (setf (getf *tests* ',name) #',name))))))

(defun get-test (name)
  (getf *tests* name))

(defun test (name)
  (funcall (get-test name)))

(defun run-tests (&optional match-name)
  (let ((tests (if match-name
                   (loop for (name func) on *tests* by #'cddr
                         when (regexp-test match-name (symbol-name name))
                         nconc (list name func))
                   *tests*)))
    (loop for (func name) on (reverse tests) by #'cddr
          for test from 1
          for ok = (funcall func)
          counting ok into success
          finally (format t "~A tests, ~A OK~%" test success))))

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

(defmacro expand-in-current-env (form)
  (macroexpand form))

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Try out the format control and format arguments of a simple-condition C,
   but make no assumptions about what they print as, only that they
   do print."
  (and (typep c 'simple-condition)
       (let ((fc (slot-ref c :format-control))
             (args (slot-ref c :format-arguments)))
         (and
          (stringp (apply #'format nil fc args))
          t))))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (apply #'frob-simple-condition c expected-fmt expected-args)))
