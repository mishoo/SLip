(in-package :sl-user)

(defparameter *tests* (list))
(defparameter *compile-time* 0)
(defparameter *run-time* 0)
(defparameter *log* t)

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
               nconc (list (pop args) (pop args)))))
    (destructuring-bind (form &rest expected) args
      (let* ((val (gensym))
             (exp (gensym))
             (comp (gensym))
             (global (intern (format nil "TEST.~A" name)))
             (ok (gensym)))
        `(flet ((,name ()
                  (let (,val ,exp ,comp ,ok)
                    (when *log*
                      (format t "#'~S ..." ',name))
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
                         (when *log*
                           (if ,ok
                               (format t " OK~%")
                               (format t " FAIL - ~A~%" ,val))))))
                    ,(when notes
                       `(when *log*
                          (format t "    ~A~%" ',notes)))
                    ,ok)))
           (setf (getf *tests* ',name) (%:make-hash :func #',name :notes ',notes))
           ',name)))))

(defun get-test (name)
  (getf *tests* name))

(defun test (name)
  (funcall (gethash :func (get-test name))))

(defun run-tests (&rest args)
  (let ((match-name (when (or (regexpp (car args))
                              (stringp (car args))
                              (null (and args (car args))))
                      (pop args))))
    (destructuring-bind (&key (all match-name allp)
                              (log t logp)
                              (match match-name))
                        args
      (let ((*compile-time* 0)
            (*run-time* 0)
            (*log* log))
        (loop with ok
              for (test name) on (reverse *tests*) by #'cddr
              for index from 1
              for notes = (gethash :notes test)
              for this-slow = (getf notes :slow)
              when (or (and all allp (not match))
                       (and match
                            (cond
                              ((stringp match)
                               (string-equal match (symbol-name name)))
                              ((regexp-test match (symbol-name name))))
                            (not (and (not all) allp this-slow)))
                       (and (not match)
                            (not this-slow)))
              do (setf ok (funcall (gethash :func test)))
              and count ok into success
              and count (not ok) into failed
              and sum *compile-time* into compile-time
              and sum *run-time* into run-time
              else count test into skipped
              finally (format t "~A tests, ~A skipped, ~A OK, ~A FAILED~%~
                                 Compile time: ~,2Fms~%~
                                 Run time: ~,2Fms~%"
                              index skipped success failed
                              compile-time run-time))
        'done))))

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

(defun union-with-check (x y &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
        (ycopy (make-scaffold-copy y)))
    (let ((result (cond
                   (test (union x y :test test))
                   (test-not (union x y :test-not test-not))
                   (t (union x y)))))
      (if (and (check-scaffold-copy x xcopy)
               (check-scaffold-copy y ycopy))
          result
        'failed))))

(defun union-with-check-and-key (x y key &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
        (ycopy (make-scaffold-copy y)))
    (let ((result  (cond
                   (test (union x y :key key :test test))
                   (test-not (union x y :key key :test-not test-not))
                   (t (union x y :key key)))))
      (if (and (check-scaffold-copy x xcopy)
               (check-scaffold-copy y ycopy))
          result
        'failed))))

(defun check-union (x y z)
  (and (listp x)
       (listp y)
       (listp z)
       (loop for e in z always (or (member e x) (member e y)))
       (loop for e in x always (member e z))
       (loop for e in y always (member e z))
       t))

(defun split-list (x)
  (cond
    ((null x) (values nil nil))
    ((null (cdr x)) (values x nil))
    (t
     (multiple-value-bind
         (y z)
         (split-list (cddr x))
       (values (cons (car x) y) (cons (cadr x) z))))))

(defun shuffle (x)
  (cond
   ((null x) nil)
   ((null (cdr x)) x)
   (t
    (multiple-value-bind
        (y z)
        (split-list x)
      (append (shuffle y) (shuffle z))))))

(defun do-random-unions (size niters &optional (maxelem (* 2 size)))
  (let ()
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
                             (random maxelem))))
           (y (shuffle (loop for j from 1 to size collect
                             (random maxelem)))))
       (let ((z (union x y)))
         (let ((is-good (check-union x y z)))
           (unless is-good (return (values x y z)))))))
    nil))

(defun set-difference-with-check (x y &key (key 'no-key)
                                    test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
        (ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-difference
                         x y
                         `(,@(unless (eqt key 'no-key) `(:key ,key))
                           ,@(when test `(:test ,test))
                           ,@(when test-not `(:test-not ,test-not))))))
      (cond
        ((and (check-scaffold-copy x xcopy)
              (check-scaffold-copy y ycopy))
         result)
        (t
         'failed)))))

(defun check-set-difference (x y z &key (key #'identity)
                               (test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
                               (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-set-differences (size niters &optional (maxelem (* 2 size)))
  (let (;; (state (make-random-state))
        )
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
                             (random maxelem))))
           (y (shuffle (loop for j from 1 to size collect
                             (random maxelem)))))
       (let ((z (set-difference-with-check x y)))
         (let ((is-good (check-set-difference x y z)))
           (unless is-good (return (values x y z)))))))
    nil))

(defun rev-assoc-list (x)
  (cond
    ((null x) nil)
    ((null (car x))
     (cons nil (rev-assoc-list (cdr x))))
    (t
     (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))

(defun check-sublis (a al &key (key 'no-key) test test-not)
  "Apply sublis al a with various keys.  Check that
   the arguments are not themselves changed.  Return nil
   if the arguments do get changed."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((acopy (make-scaffold-copy a))
        (alcopy (make-scaffold-copy al)))
    (let ((as
           (apply #'sublis al a
                  `(,@(when test `(:test ,test))
                    ,@(when test-not `(:test-not ,test-not))
                    ,@(unless (eqt key 'no-key) `(:key ,key))))))
      (and
       (check-scaffold-copy a acopy)
       (check-scaffold-copy al alcopy)
       as))))

(defun check-subst (new old tree &key (key 'no-key) test test-not)
  "Call subst new old tree, with keyword arguments if present.
   Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
        (oldcopy (make-scaffold-copy old))
        (treecopy (make-scaffold-copy tree)))
    (let ((result
           (apply #'subst new old tree
                  `(,@(unless (eqt key 'no-key) `(:key ,key))
                    ,@(when test `(:test ,test))
                    ,@(when test-not `(:test-not ,test-not))))))
      (and (check-scaffold-copy new newcopy)
           (check-scaffold-copy old oldcopy)
           (check-scaffold-copy tree treecopy)
           result))))

(defun set-exclusive-or-with-check (x y &key (key 'no-key)
                                      test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
        (ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-exclusive-or
                         x y
                         `(,@(unless (eqt key 'no-key) `(:key ,key))
                             ,@(when test `(:test ,test))
                             ,@(when test-not `(:test-not ,test-not))))))
      (cond
       ((and (check-scaffold-copy x xcopy)
             (check-scaffold-copy y ycopy))
        result)
       (t
        'failed)))))

(defun check-set-exclusive-or (x y z &key (key #'identity)
                                 (test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (or (member e x :key key :test test)
                               (member e y :key key :test test)))
   (loop for e in x always (if (member e y :key key :test test)
                               (not (member e z :key key :test test))
                             (member e z :key key :test test)))
   (loop for e in y always (if (member e x :key key :test test)
                               (not (member e z :key key :test test))
                             (member e z :key key :test test)))
   t))