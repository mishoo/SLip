(in-package :sl-user)

;; by beach
(defun my-remove (item list test)
  (loop with prefix = '()
        with remaining = list
        for position = (position item remaining :test test)
        until (null position)
        do (loop repeat position
                 do (push (pop remaining) prefix))
        (pop remaining)
        finally (loop until (null prefix)
                      do (let ((temp (cdr prefix)))
                           (setf (cdr prefix) remaining
                                 remaining prefix
                                 prefix temp)))
        (return remaining)))

;; by gilberth
(defun my-remove-rec (item list)
  (labels ((rec (list)
             (cond ((endp list)
                    nil)
                   ((eql item (car list))
                    (rec (cdr list)))
                   (t
                    (let ((new-cdr (rec (cdr list))))
                      (if (eq new-cdr (cdr list))
                          list
                          (cons (car list) new-cdr)))))))
    (rec list)))

