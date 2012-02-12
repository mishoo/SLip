(defpackage :js
  (:use :ss))

(in-package :js)

(defmacro js-function (name args code)
  (let ((out (%make-output-stream)))
    (%stream-put out "function ")
    (when name (%stream-put out name))
    (%stream-put out #\()
    (foreach args (lambda (var)
                    (%stream-put out var)))
    (%stream-put out ") {" code "}")
    `(%js-eval ,(%stream-get out))))

;; (let ((fun (js-function "fibonacci" ("n") "
;; if (n < 2) return 1;
;; return fibonacci(n - 2) + fibonacci(n - 1);
;; ")))

;;   (console.log (%js-apply fun nil '(10))))
