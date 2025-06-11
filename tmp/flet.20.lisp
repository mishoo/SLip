(in-package :sl-user)

(defmacro def-large-fun (&optional name)
  (let* ((n 1024)
         (names (loop repeat n collect (gensym))))
    `(flet ((,name (,@names)
              (+ ,@names)))
       #',name)))