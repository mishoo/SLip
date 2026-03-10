(in-package :sl-user)

(loop for i = 1 then (* i 2)
      for j = i then (- i 1)
      repeat 5
      do (print i j))

;; (loop for (p q) = a then b)
