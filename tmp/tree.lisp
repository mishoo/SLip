(in-package :turtle)

(defun tree (n)
  (when (> n 10)
    (forward (/ n 4))
    (left 10)
    (save-excursion
     (right 45)
     (tree (* n 0.6)))
    (forward (/ n 4))
    (right 25)
    (save-excursion
     (left 40)
     (tree (* n 0.7)))
    (left 10)
    (tree (- n 15))))

(defun leaf (n)
  (when (>= n 5)
    (let ((half (/ n 2)))
      (forward half)
      (save-excursion
       (right 60)
       (leaf half))
      (save-excursion
       (left 60)
       (leaf half))
      (right 3)
      (leaf (- n 5)))))