(in-package :sl-user)

(use-package :sl-thread)

(defun test (count time)
  (let ((name (format nil "COUNT~D" count)))
    (make-thread
     (lambda ()
       (loop for i from 1 to count
             collect i into ret
             do (sleep time)
             finally
             (format t "Thread ~A finished~%" name)
             (return (values ret :aye))))
     :name name)))