(in-package :sl-user)

(defparameter t1
  (make-thread
   (lambda ()
     (%:%catch-all-errors)
     (let* ((rec2 (make-hash
                   :signal3 (lambda ()
                              (format t "S3~%")
                              'result-from-s3)))
            (receivers (make-hash
                        :signal1 (lambda ()
                                   (format t "S1~%")
                                   (%:%receive rec2))
                        :signal2 (lambda ()
                                   (format t "S2~%")
                                   (/ 22 7)))))
       (loop
        (handler-case
            (format t "RECEIVE: ~A~%"
              (%:%receive receivers))
          (error (ex)
            (format t "!ERROR: ~A~%" ex))))))))