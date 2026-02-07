(defparameter quine
  (let ((let '`(let ((let ',let))
                 ,let)))
    `(let ((let ',let))
       ,let)))