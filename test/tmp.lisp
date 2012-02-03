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



(defsetf car (x) (val)
  `(rplaca ,x ,val))

(defsetf cdr (x) (val)
  `(rplacd ,x ,val))

(defsetf gethash (hash key) (val)
  `(hash-set ,hash ,key ,val))

(console.log (macroexpand-all '(setf (gethash x :foo) (maka (bar)))))
(console.log (macroexpand-all '(setf (gethash x :foo) (maka (bar)) crap mak (gethash x :foo) (maka (bar)))))


(defparameter foo #(1 2 3 4 5))
(progn
  (console.log (vector-splice foo 1 3 #('a 'b 'c 'd)))
  (console.log foo)
  (vector-push foo 'd 'e 'f)
  (console.log foo))
