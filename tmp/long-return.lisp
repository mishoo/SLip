(in-package :sl-user)

(defun test-block ()
  (let ((foo (block blk
               (let ((a 10))
                 (format t "Inside~%")
                 (return-from blk a)
                 (format t "Unreachable~%"))))
        (bar 5))
    (format t "~A~%" (list foo bar))))

(defun test-block2 ()
  (lambda (x)
    (when x
      (return-from test-block2 'stuff))))

(defun stuff ()
  (format t "In stuff~%")
  (throw 'foo "result")
  (format t "Unreachable~%"))

(defun test-catch ()
  (catch 'foo (stuff)))

(defun problematic-catch ()
  (catch 'foo
    (format t "The inner catch returns ~s.~%"
            (catch 'foo
              (unwind-protect (throw 'foo :first-throw)
                (throw 'foo :second-throw))))
    ;; :outer-catch
    ))

(defun problematic-catch2 ()
  (catch 'a
    (catch 'b
      (unwind-protect (throw 'a 1)
        (throw 'b 2)))))