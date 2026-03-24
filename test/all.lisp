(defpackage :sl-test
  (:use :sl)
  (:export #:run-tests))

(in-package :sl-test)

(cond
  ((and (boundp '*has-slip-tests*)
        (symbol-value '*has-slip-tests*))
   (format t ";; Tests already loaded - skipping~%~
              ;; Evaluate this to override:~%~%    ~
                  (DELETE-PACKAGE :SL-TEST)~%~%"))
  (t
   (defparameter *has-slip-tests* t)

   (format t "Loading tests takes a few seconds, please wait...~%")

   (let* ((files (list "test/deftest.lisp"
                       "test/apply.lisp"
                       "test/funcall.lisp"
                       "test/prog1.lisp"
                       "test/prog2.lisp"
                       "test/progv.lisp"
                       "test/prog.lisp"
                       "test/or.lisp"
                       "test/and.lisp"
                       "test/let.lisp"
                       "test/letstar.lisp"
                       "test/defun.lisp"
                       "test/flet.lisp"
                       "test/labels.lisp"
                       "test/places.lisp"
                       "test/rotatef.lisp"
                       "test/shiftf.lisp"
                       "test/psetf.lisp"
                       "test/values.lisp"
                       "test/values-list.lisp"
                       "test/multiple-value-bind.lisp"
                       "test/multiple-value-call.lisp"
                       "test/multiple-value-list.lisp"
                       "test/multiple-value-prog1.lisp"
                       "test/multiple-value-setq.lisp"
                       "test/if.lisp"
                       "test/cond.lisp"
                       "test/case.lisp"
                       "test/destructuring-bind.lisp"
                       "test/macrolet.lisp"
                       "test/block.lisp"
                       "test/catch.lisp"
                       "test/unwind-protect.lisp"
                       "test/tagbody.lisp"
                       "test/do.lisp"
                       "test/dolist.lisp"
                       "test/dostar.lisp"
                       "test/dotimes.lisp"
                       "test/loop1.lisp"
                       "test/loop2.lisp"
                       "test/loop3.lisp"
                       "test/loop4.lisp"
                       "test/handler-bind.lisp"
                       "test/handler-case.lisp"
                       "test/ignore-errors.lisp"
                       "test/error.lisp"
                       "test/hash/clrhash.lisp"
                       "test/hash/gethash.lisp"
                       "test/hash/hash-table.lisp"
                       "test/hash/hash-table-count.lisp"
                       "test/hash/hash-table-p.lisp"
                       "test/hash/make-hash-table.lisp"
                       "test/hash/maphash.lisp"
                       "test/hash/remhash.lisp"
                       "test/hash/with-hash-table-iterator.lisp"
                       "test/cons/make-list.lisp"
                       "test/cons/mapc.lisp"
                       "test/cons/mapcar.lisp"
                       "test/cons/mapcan.lisp"
                       "test/cons/maplist.lisp"
                       "test/cons/append.lisp"
                       "test/cons/member.lisp"
                       "test/cons/adjoin.lisp"
                       "test/cons/getf.lisp"
                       "test/cons/get-properties.lisp"
                       "test/cons/pop.lisp"
                       "test/cons/push.lisp"
                       "test/cons/pushnew.lisp"
                       "test/cons/acons.lisp"
                       "test/cons/pairlis.lisp"
                       "test/cons/butlast.lisp"
                       "test/cons/subst.lisp"
                       "test/cons/sublis.lisp"
                       "test/cons/assoc.lisp"
                       "test/cons/assoc-if.lisp"
                       "test/cons/assoc-if-not.lisp"
                       "test/cons/rassoc.lisp"
                       "test/cons/rassoc-if.lisp"
                       "test/cons/rassoc-if-not.lisp"
                       "test/cons/intersection.lisp"
                       "test/cons/union.lisp"
                       "test/cons/set-difference.lisp"
                       "test/cons/set-exclusive-or.lisp"
                       "test/seq/count.lisp"
                       "test/seq/count-if.lisp"
                       "test/seq/count-if-not.lisp"
                       "test/seq/find.lisp"
                       "test/seq/find-if.lisp"
                       "test/seq/find-if-not.lisp"
                       "test/seq/position.lisp"
                       "test/seq/position-if.lisp"
                       "test/seq/position-if-not.lisp"
                       "test/seq/substitute.lisp"
                       "test/seq/substitute-if.lisp"
                       "test/seq/substitute-if-not.lisp"
                       "test/seq/nsubstitute.lisp"
                       "test/seq/nsubstitute-if.lisp"
                       "test/seq/nsubstitute-if-not.lisp"
                       "test/seq/subseq.lisp"
                       "test/seq/remove.lisp"
                       "test/seq/concatenate.lisp"
                       "test/struct/structures-01.lisp")))

     (loop with count = (length files)
           for file in files
           for i from 1
           do (progn
                (format t ";; ~D/~D Loading ~A~%" i count file)
                (with-output-to-string (*trace-output*)
                  (load file)))))))
