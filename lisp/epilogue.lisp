(in-package :sl)

;;; Prevent further redefinition of global functions/macros from our base
;;; packages. Could be annoying for development, but let's see.
(let ((sealed %:*sealed-packages*))
  (when sealed
    (setf (gethash (find-package :sl) sealed) t
          (gethash (find-package :%) sealed) t)))
