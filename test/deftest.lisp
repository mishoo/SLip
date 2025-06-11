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
  (%:maybe-xref-info name 'defun)
  (let ((notes
         (loop while (keywordp (car args))
               collect (cons (pop args) (pop args)))))
    (destructuring-bind (form &rest expected) args
      (let ((val (gensym))
            (exp (gensym))
            (comp (gensym))
            (ok (gensym)))
        `(flet ((,name ()
                  (let (,val ,exp ,comp ,ok)
                    (format t "#'~S ..." ',name)
                    (setf ,exp ',expected)
                    (handler-bind
                        ((error (lambda (condition)
                                  (format t " FAIL.~%!ERROR: ~A~%!ERROR: ~S~%"
                                          condition (%:%backtrace))
                                  (throw 'test-error '#:test-error))))
                      (setf ,comp (time-it *compile-time*
                                           (compile (list 'lambda nil ',form))))
                      (setf ,val (time-it *run-time*
                                          (catch 'test-error
                                            (multiple-value-list (funcall ,comp)))))
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
            finally (format t "~A tests, ~A OK~%Compile time: ~Ams~%Run time: ~Ams~%"
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
