(in-package :sl-user)

(defun test (n)
  (format t "starting~%")
  (tagbody
   start
   (when (zerop n)
     (go end))
   (format t "~A~%" n)
   (decf n)
   (go start)
   end))