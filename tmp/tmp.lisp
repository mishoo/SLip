(defun rect-spiral (max)
  (save-excursion
   (let rec ((n 0))
     (when (< n max)
       (forward n)
       (left 89.7)
       (rec (+ 2 n))))))

(defmacro animate ((&key (speed 50)) &body body)
  (let ((sym (gensym)))
    `(labels ((,sym ()
                ,@body
                (set-timeout ,speed #',sym)))
       (,sym))))