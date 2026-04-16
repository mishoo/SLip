(defparameter stuff 1)

(let ((stuff 2))
  (catch 'foo
    (throw 'foo nil))
  (print stuff))