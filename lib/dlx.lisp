;;;; This package implements Knuth's Algorithm X using the Dancing
;;;; Links technique, which he suggests we call the DLX algorithm.
;;;;
;;;; https://www-cs-faculty.stanford.edu/~uno/papers/dancing-color.ps.gz
;;;;
;;;; You should read the paper (at least the first 6 pages) to get
;;;; familiar with the ideas, in order for this package to be of any
;;;; use.

;;;; https://github.com/mishoo/dlx.lisp/

(defpackage #:dlx
  (:use #:sl)
  (:export #:make-dlx-matrix
           #:print-dlx-solution
           #:search-dlx
           #:dlx-cover-col
           #:dlx-uncover-col
           #:dlx-node
           #:make-dlx-node
           #:dlx-node-size
           #:dlx-node-data
           #:dlx-node-up
           #:dlx-node-down
           #:dlx-node-left
           #:dlx-node-right
           #:dlx-node-col))

(in-package #:dlx)

(setf %:*enable-inline* t)

(defparameter *optimize* '(optimize (speed 3) (safety 1) (space 0) (debug 0)))

(defstruct (dlx-node
            (:print-object print-dlx-node))
  "A node in Knuth's fabulous data structure. For simplicity we use the
same structure for all nodes (including headers), even though,
depending on the node type, some fields might be unused."
  (size 0 :type fixnum)
  (data nil)
  (up nil :type (or null dlx-node))
  (down nil :type (or null dlx-node))
  (left nil :type (or null dlx-node))
  (right nil :type (or null dlx-node))
  (col nil :type (or null dlx-node)))

(defun print-dlx-node (node stream)
  (cond
    ((and (not (dlx-node-up node))
          (not (dlx-node-down node)))
     (format stream "#S(DLX / ~D)" (dlx-node-size node)))
    ((not (dlx-node-col node))
     (format stream "#S(DLX-COL ~A / ~D)" (dlx-node-data node) (dlx-node-size node)))
    (t
     (format stream "#S(DLX-NOD ~A ↑ ~A)"
             (dlx-node-data node) (dlx-node-data (dlx-node-col node))))))

(defun insert-horiz (anchor new)
  "Inserts `new' node horizontally in the circular doubly-linked list,
before `anchor'. Returns the new node."
  (incf (dlx-node-size anchor))
  (setf (dlx-node-right new) anchor
        (dlx-node-left new) (dlx-node-left anchor)
        (dlx-node-right (dlx-node-left anchor)) new
        (dlx-node-left anchor) new))

(defun insert-vert (anchor new)
  "Inserts `new' node vertically in the circular doubly-linked list,
before `anchor'. Returns the new node."
  (incf (dlx-node-size anchor))
  (setf (dlx-node-down new) anchor
        (dlx-node-up new) (dlx-node-up anchor)
        (dlx-node-down (dlx-node-up anchor)) new
        (dlx-node-up anchor) new))

(defun make-dlx-matrix (matrix &optional coldata)
  "Creates Knuth's toroidal data structure, given `matrix' of 1s and 0s,
and optional `coldata' (must be a list if present) for the column
headers `data' slots. `matrix' must be a vector of rows, where rows
are vectors of 1 and 0 (each row could be a bit vector). All rows
should be of the same length. Example #(#*101 #*010).

Returns `head', the matrix entry point (the root object, as Knuth
calls it, which is noted `h' in his paper)."
  (let ((ncols (length (aref matrix 0)))
        (head (make-dlx-node)))
    (setf (dlx-node-left head) head
          (dlx-node-right head) head)
    (loop repeat ncols
          for data = coldata then (cdr data)
          for node = (make-dlx-node :data (car data))
          do (insert-horiz head node)
          (setf (dlx-node-up node) node
                (dlx-node-down node) node))
    (loop for i from 0
          for row across matrix
          do (loop with new
                   with first = nil
                   for col = (dlx-node-right head) then (dlx-node-right col)
                   for bit across row
                   unless (zerop bit)
                   do (setf new (make-dlx-node :col col :data i))
                   (if first
                       (insert-horiz first new)
                       (setf (dlx-node-left new) new
                             (dlx-node-right new) new
                             first new))
                   (insert-vert col new)))
    head))

(defmacro find-best (data predicate exp &body init)
  (setf predicate (case predicate
                    (minimizing '<)
                    (maximizing '>)
                    (otherwise predicate)))
  (let ((best-data (gensym "best-data"))
        (best-val (gensym "best-val"))
        (current-data (gensym "current-data"))
        (current-val (gensym "current-val")))
    `(loop with ,best-data = nil
           with ,best-val = nil
           ,@init
           for ,current-data = ,data
           for ,current-val = ,exp
           when (or (not ,best-val)
                    (,predicate ,current-val ,best-val))
           do (setf ,best-val ,current-val
                    ,best-data ,current-data)
           finally (return (values ,best-data ,best-val)))))

(defun select-col (head)
  (declare (type dlx-node head)
           #.*optimize*)
  ;; wish `loop' could do this..
  (find-best col minimizing (dlx-node-size col)
             for col = (dlx-node-right head) then (dlx-node-right col)
             until (eq col head)))

(defun print-dlx-solution (sol)
  "The default solution printer. Returns the solution selected by the
algorithm as a list of indexes of the rows that fit the constraints."
  (map 'list #'dlx-node-data sol))

(defun is-solution (head sol length)
  "Check if the current partial solution is a solution. By default we
try to satisfy all constraints (complete cover) so we just check if
there are any columns left to cover."
  (declare (ignore sol length))
  (eq (dlx-node-right head) head))

(defun search-dlx (head &key
                        (solbuffer nil)
                        (solcount nil)
                        (printer #'print-dlx-solution)
                        (select-col #'select-col)
                        (is-solution #'is-solution))
  "Search for solutions. `head' is the root object. Pass
`solcount' (integer) if you want to limit the number of solutions (by
default it searches for all solutions, which is impractical for many
problems). `printer' should be `NIL', or a function that converts the
solution. The default printer will convert to row indexes. The print
function receives a list of nodes selected by the algorithm. Check
Knuth's paper to see how he suggests printing solution (page 5 at the
bottom).

If you pass `NIL' for the `printer', this function will only count the
solutions, which is faster and should not do any consing. Otherwise it
returns a list of solutions.

Unless interrupted, the matrix will not be altered; even when the
search is limited by `solcount', links will keep dancing until they
get back to the original shape.

Knuth's algorithm searches for a complete cover (select 1 in every
column), but for various problems it might be useful to search for a
partial cover. If that's the case, pass `is-solution' — must be a
function of two arguments, the root node of the matrix and the current
partial solution as a list of nodes. Return `T' if the solution is
valid (in which case it collects it and backtracks for other
solutions). Note that `printer' must be non-NIL in order to receive
the partial solution.

One final option to tune the algorithm is to pass your own
implementation for `select-col'. The default selects the column with
the minimum number of 1s, which Knuts suggests is best in most
cases. It must be a function of one argument (the root of the matrix)
and return a column object. Return `NIL' to abort the current branch
and backtrack."
  (declare (type dlx-node head)
           (type (or null fixnum) solcount)
           (type (or null (simple-array dlx-node (*))) solbuffer)
           (type (function (dlx-node t fixnum)) is-solution)
           (type (or null (function (t))) printer)
           (type (function (dlx-node)) select-col)
           #.*optimize*)
  (let ((solutions nil)
        (found 0))
    (declare (type fixnum found))
    (labels ((run (level sol)
               (declare (type fixnum level))
               (when (funcall is-solution head (or solbuffer sol) level)
                 (when printer
                   (if solbuffer
                       (push (funcall printer (make-array level :displaced-to solbuffer)) solutions)
                       (push (funcall printer sol) solutions)))
                 (incf found)
                 (return-from run))
               (let ((c (funcall select-col head)))
                 (when c
                   (dlx-cover-col c)
                   (loop for r = (dlx-node-down c) then (dlx-node-down r)
                         until (eq r c)
                         until (and solcount (= found solcount))
                         do (loop for j = (dlx-node-right r) then (dlx-node-right j)
                                  until (eq j r)
                                  do (dlx-cover-col (dlx-node-col j)))
                         (if solbuffer
                             (progn
                               (setf (aref solbuffer level) r)
                               (run (1+ level) nil))
                             (run (1+ level) (when printer (cons r sol))))
                         (loop for j = (dlx-node-left r) then (dlx-node-left j)
                               until (eq j r)
                               do (dlx-uncover-col (dlx-node-col j))))
                   (dlx-uncover-col c)))))
      (run 0 nil))
    (if printer solutions found)))

(defun dlx-cover-col (c)
  "Covers the given column."
  (declare (type dlx-node c)
           #.*optimize*)
  (setf (dlx-node-left (dlx-node-right c)) (dlx-node-left c)
        (dlx-node-right (dlx-node-left c)) (dlx-node-right c))
  (loop for i = (dlx-node-down c) then (dlx-node-down i)
        until (eq i c)
        do (loop for j = (dlx-node-right i) then (dlx-node-right j)
                 until (eq j i)
                 do (setf (dlx-node-up (dlx-node-down j)) (dlx-node-up j)
                          (dlx-node-down (dlx-node-up j)) (dlx-node-down j))
                 (decf (dlx-node-size (dlx-node-col j))))))

(defun dlx-uncover-col (c)
  "Uncovers a column that was previously covered."
  (declare (type dlx-node c)
           #.*optimize*)
  (loop for i = (dlx-node-up c) then (dlx-node-up i)
        until (eq i c)
        do (loop for j = (dlx-node-left i) then (dlx-node-left j)
                 until (eq j i)
                 do (incf (dlx-node-size (dlx-node-col j)))
                 (setf (dlx-node-up (dlx-node-down j)) j
                       (dlx-node-down (dlx-node-up j)) j)))
  (setf (dlx-node-left (dlx-node-right c)) c
        (dlx-node-right (dlx-node-left c)) c))
