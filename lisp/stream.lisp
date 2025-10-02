(in-package :sl)

(export '(peek-char read-char read-line
          peek-byte read-byte
          read-sequence write-sequence
          with-input-from-string file-position
          stream-error stream-error-stream end-of-file))

(defpackage :sl-stream
  (:use :sl :%)
  (:export #:open-url))

(in-package :sl-stream)

(defclass stream-error (error)
  ((stream :initarg :stream :reader stream-error-stream)))

(defclass end-of-file (stream-error) ())

(defmacro with-input-from-string ((var str) &body body)
  `(let ((,var (%make-text-memory-input-stream ,str)))
     ,@body))

(defun read-char (&optional (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (declare (ignore recursive-p))
  (let ((val (%stream-next input-stream)))
    (or val
        (if eof-error-p
            (error 'end-of-file :stream input-stream)
            eof-value))))

(defun peek-char (&optional peek-type
                            (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (declare (ignore recursive-p))
  (let ((val (if peek-type
                 (loop with test = (if (eq t peek-type) #'%:whitespacep
                                       (lambda (ch)
                                         (not (eq ch peek-type))))
                       for ch = (%stream-peek input-stream)
                       while (and ch (funcall test ch))
                       do (%stream-next input-stream)
                       finally (return ch))
                 (%stream-peek input-stream))))
    (or val
        (if eof-error-p
            (error 'end-of-file :stream input-stream)
            eof-value))))

(defun read-line (&optional (input-stream *standard-input*)
                            (eof-error-p t)
                            eof-value
                            recursive-p)
  (declare (ignore recursive-p))
  (let* ((eof nil)
         (line (with-output-to-string (out)
                 (loop for ch = (%stream-next input-stream)
                       unless ch do (progn
                                      (setf eof t)
                                      (return nil))
                       until (eq #\Newline ch)
                       do (%stream-put out ch)))))
    (if (and eof (zerop (length line)))
        (if eof-error-p
            (error 'end-of-file :stream input-stream)
            (values eof-value t))
        (values line eof))))

(defun file-position (stream &optional position)
  (%stream-pos stream position))

(defun open-url (url &optional binary for-output)
  (%async-fetch url binary for-output))