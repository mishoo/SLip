(in-package :sl-user)

(defun test-block ()
  (let ((foo (block blk
               (let ((a 10))
                 (format t "Inside~%")
                 (return-from blk a)
                 (format t "Unreachable~%"))))
        (bar 5))
    (format t "~A~%" (list foo bar))))