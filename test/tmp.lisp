(defpackage :test
  (:use :sl))

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

(make-thread
 (lambda ()

   (progn
     (defclass person (object) (first-name last-name))
     (defclass student (person) (university code))

     ;; (defmethod initialize ((p person) initargs)
     ;;   (call-next-method))

     ;; (defmethod initialize ((p student) initargs)
     ;;   (call-next-method))

     (defgeneric full-name)
     (defmethod full-name ((p person))
       (strcat (slot-ref p 'first-name)
               " "
               (slot-ref p 'last-name)))

     (defmethod full-name ((p student))
       (strcat (call-next-method) " (" (slot-ref p 'university) ", " (slot-ref p 'code) ")"))

     (let ((p (make-instance 'student
                             'first-name "John"
                             'last-name "Doe"
                             'university "Şmenozenia"
                             'code 12)))
       (console.log (full-name p))
       (console.log (class-slots (class-of p)))
       (console.log (is-a p (find-class 'number)))
       (console.log (is-a p (find-class 'person)))
       (console.dir (class-cpl (class-of p)))
       (console.log (class-name (class-of p))))

     (console.print "Instantiating 1000 STUDENT objects...")
     (time (let rec ((n 1000))
                (make-instance 'student
                               'first-name "John"
                               'last-name "Doe"
                               'university "Şmenozenia"
                               'code 12)
                (when (> n 0)
                  (rec (- n 1)))))

     )

   ))
