(defpackage :test
  (:use :ss))

(in-package :test)

;; (let* ((t1 (make-thread (lambda ()
;;                           (let ((mak 0)
;;                                 (prev-time (%get-time)))
;;                             (labels ((stuff ()
;;                                        (let* ((time (%get-time))
;;                                               (diff (- time prev-time)))
;;                                          (setq prev-time time)
;;                                          (console.print "************************" (%::%incf mak) diff))
;;                                        (when (< mak 20)
;;                                          (set-timeout 10 #'stuff))))
;;                               (set-timeout 10 #'stuff)))
;;                           (let ((receivers (make-hash :foo (lambda (a b)
;;                                                              (console.log "Got :FOO" a b)))))
;;                             (labels ((rec ()
;;                                        (%receive receivers)
;;                                        (rec)))
;;                               (rec)))))))

;;   (labels ((rec (i)
;;              (when (> i 0)
;;                (make-thread (lambda ()
;;                               (labels ((rec (n)
;;                                          (when (> n 0)
;;                                            (%sendmsg t1 :foo i n)
;;                                            (rec (- n 1)))))
;;                                 (rec 50))))
;;                (rec (- i 1)))))
;;     (rec 10)))

(console.log (dom.sizzle "#foo"))

(make-thread (lambda ()
               (dom.subscribe (dom.get-element-by-id "foo")
                              '(:click :mouse-down :mouse-up :mouse-over :mouse-out)
                              (make-thread (lambda ()
                                             (labels ((rec ()
                                                        (%receive (make-hash
                                                                   :click (lambda (ev)
                                                                            (console.log "Got clicked" ev))
                                                                   :mouse-down (lambda (ev)
                                                                                 (console.log "Got mouse-down" ev))
                                                                   :mouse-up (lambda (ev)
                                                                               (console.log "Got mouse-up" ev))
                                                                   :mouse-over (lambda (ev)
                                                                                 (console.log "Got mouse-over" ev))
                                                                   :mouse-out (lambda (ev)
                                                                                (console.log "Got mouse-out" ev))))
                                                        (rec)))
                                               (rec)))))))

(defsetf gethash (hash key) (val)
  `(hash-set ,hash ,key ,val))

(make-thread
 (lambda ()

   (console.log (macroexpand-all '(setf (gethash x :foo) (maka (bar)))))
   (console.log (macroexpand-all '(setf (gethash x :foo) (maka (bar)) crap mak (gethash x :foo) (maka (bar)))))


   (defparameter foo #(1 2 3 4 5))
   (progn
     (console.log (vector-splice foo 1 3 #('a 'b 'c 'd)))
     (console.log foo)
     (vector-push foo 'd 'e 'f)
     (console.log foo))

   (console.log (%apply #'append (list '(1 2 3 4)
                                       '(a b c d)
                                       '(:foo :bar :baz))))

   (let ((foo 1))
     (incf foo)
     (incf foo)
     (decf foo)
     (console.log foo))

   (console.log (map #'list '(:foo :bar :baz)))

   (console.log (every (lambda (x)
                         (= x 2))
                       '(2 2 2 2 2)))
   (console.log (collect-if (lambda (x) (< x 5))
                            '(2 10 6 3 5 1 6 0)))
   (console.log (last '(1 2 3 4 5)))

   (console.log (merge '(1 5 3 9) '(2 3 6 10) #'<))

   (console.log (floor 5 2))

   (let ((foo 10))
     (while (> foo 0)
       (console.log foo)
       (decf foo)))

   (let ((numbers (labels ((rec (n)
                             (when (> n 0)
                               (cons (random 100) (rec (- n 1))))))
                    (time (rec 1000)))))
     (console.log (time (sort numbers #'<))))

   (let ((foo (lambda a1
                (lambda a2
                  (console.log "FOOOOOOOOOO got" a1 a2)))))
     ((funcall foo 'foo 'bar) 'baz 'mak))

   (let loop ((i 10))
        (when (>= i 0)
          (console.log i)
          (loop (- i 1))))

   (console.log (remove-duplicates '(1 2 3 4 5 6 2 3 1) :from-end t))

   (progn
     (defclass person (object) (first-name last-name))
     (defclass student (person) (university code))

     (defmethod initialize ((p person) initargs)
       (console.log "Initializing PERSON" initargs)
       (call-next-method)
       (console.log "Done PERSON"))

     (defmethod initialize ((p student) initargs)
       (console.log "Initializing STUDENT" initargs)
       (call-next-method)
       (console.log "Done STUDENT"))

     (defgeneric full-name)
     (defmethod full-name ((p person))
       (strcat (slot-ref p 'first-name)
               " "
               (slot-ref p 'last-name)))

     (let ((p (make-instance 'student
                             'first-name "John"
                             'last-name "Doe"
                             'university "Şmenozenia"
                             'code 12)))
       (console.log (full-name p))
       (console.log (class-slots (class-of p)))
       (console.log (is-a p (find-class 'number)))
       (console.log (is-a p (find-class 'person)))
       (console.dir (%instance-vector p))
       (console.dir (class-cpl (class-of p))))
     )

   ))
