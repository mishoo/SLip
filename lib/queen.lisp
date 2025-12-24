;;;; FILE: package.lisp

(defpackage #:queen
  (:use #:sl)
  (:export #:+QUEEN+
           #:+ROOK+
           #:+KNIGHT+
           #:+BISHOP+
           #:+PAWN+
           #:+KING+

           #:+WQUEEN+
           #:+WROOK+
           #:+WKNIGHT+
           #:+WBISHOP+
           #:+WPAWN+
           #:+WKING+

           #:+WHITE+
           #:+FEN-START+

           #:is-pawn?
           #:is-knight?
           #:is-bishop?
           #:is-rook?
           #:is-queen?
           #:is-king?
           #:is-white?
           #:is-black?
           #:same-side?
           #:opp-side?

           #:board-get
           #:board-set
           #:with-piece
           #:board-foreach
           #:print-board
           #:*unicode*

           #:board-index
           #:index-valid?
           #:field-index
           #:index-field
           #:index-row
           #:index-col
           #:with-row-col
           #:piece-char
           #:char-piece

           #:move
           #:move-from
           #:move-to
           #:move-piece
           #:move-black?
           #:move-white?
           #:move-side
           #:move-capture?
           #:move-captured-piece
           #:move-promote?
           #:move-promoted-piece
           #:move-set-promoted-piece
           #:move-enpa?
           #:move-captured-index
           #:move-oo?
           #:move-ooo?
           #:move-check?

           #:game
           #:make-game
           #:reset-from-fen
           #:reset-game
           #:game-fen
           #:game-move
           #:game-undo-move
           #:with-move
           #:king-index
           #:attacked?
           #:game-compute-moves
           #:game-parse-san
           #:game-san
           #:game-board
           #:game-state
           #:game-side
           #:game-enpa
           #:game-fullmove
           #:game-halfmove
           #:draw-by-material?

           #:parse-pgn
           #:game-search
           #:dump-line
           #:play
           ))

(in-package #:queen)

(defparameter *queen-read-table* (make-hash-table))
(setf *read-table* *queen-read-table*)

(setf %:*enable-inline* t)

(defmacro once-only (names . body)
  (let ((gensyms (mapcar (lambda (_) (gensym)) names)))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (g n) ``(,,g ,,n)) gensyms names))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
             ,@body)))))

(defmacro with-gensyms (names &body forms)
  "Binds a set of variables to gensyms and evaluates the implicit progn FORMS.

Each element within NAMES is either a symbol SYMBOL or a pair (SYMBOL
STRING-DESIGNATOR). Bare symbols are equivalent to the pair (SYMBOL SYMBOL).

Each pair (SYMBOL STRING-DESIGNATOR) specifies that the variable named by SYMBOL
should be bound to a symbol constructed using GENSYM with the string designated
by STRING-DESIGNATOR being its first argument."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

(defmacro awhen (cond . rest)
  `(let ((it ,cond))
     (when it ,@rest)))

;;;; FILE: stream.lisp

(defmacro with-parse-stream (input &body body)
  (let ((pos (gensym "pos"))
        (line (gensym "line"))
        (col (gensym "col")))
    `(let ((,pos 0) (,line 1) (,col 0))
       (labels
           ((peek ()
              (peek-char nil ,input nil nil))

            (next ()
              (let ((ch (read-char ,input nil 'EOF)))
                (when (and (eql ch #\Return)
                           (eql (peek) #\Newline))
                  (incf ,pos)
                  (setf ch (read-char ,input nil 'EOF)))
                (case ch
                  (EOF
                   nil)
                  (#\Newline
                   (setf ,col 0)
                   (incf ,line)
                   (incf ,pos)
                   ch)
                  (otherwise
                   (incf ,col)
                   (incf ,pos)
                   ch))))

            (eof? ()
              (not (peek)))

            (croak (msg &rest args)
              (when args
                (setf msg (apply #'format nil msg args)))
              (error "ERROR: ~A (~A:~A)" msg ,line ,col))

            (read-while (pred)
              (with-output-to-string (out)
                (loop for ch = (peek)
                      while (and ch (funcall pred ch))
                      do (write-char (next) out))))

            (whitespace? (ch)
              (case (char-code ch)
                ((32 10 13 9 #xA0 #xFEFF) t)))

            (non-whitespace? (ch)
              (not (whitespace? ch)))

            (digit? (ch)
              (digit-char-p ch))

            (letter? (ch)
              (letterp ch))

            (alnum? (ch)
              (or (digit? ch)
                  (letter? ch)))

            (skip (ch &optional no-error)
              (cond
                ((characterp ch)
                 (let ((curr (next)))
                   (if (eql ch curr)
                       curr
                       (unless no-error
                         (croak "Expected ~A but found ~A" ch curr)))))
                ((stringp ch)
                 (let* ((i -1)
                        (n (length ch))
                        (val (read-while (lambda (curr)
                                           (and (< (incf i) n)
                                                (eql curr (char ch i)))))))
                   (if (= i n) val
                       (unless no-error
                         (croak "Expected ~A but found ~A" ch val)))))
                (t
                 (error "Unknown token in `skip'"))))

            (read-integer ()
              (let ((str (read-while #'digit?)))
                (unless (zerop (length str))
                  (parse-integer str))))

            (read-string (&optional (quote #\") (esc #\\))
              (skip quote)
              (read-while (lambda (ch)
                            (cond
                              ((eql ch esc)
                               (next)
                               (or (peek)
                                   (error "Unexpected EOF reading string")))
                              ((eql ch quote)
                               (next)
                               nil)
                              (t t)))))

            (skip-whitespace ()
              (read-while #'whitespace?)))

         (declare (ignorable #'peek
                             #'next
                             #'eof?
                             #'croak
                             #'read-while
                             #'whitespace?
                             #'non-whitespace?
                             #'digit?
                             #'letter?
                             #'alnum?
                             #'skip
                             #'read-string
                             #'skip-whitespace
                             #'read-integer))

         ,@body))))

;;;; FILE: board.lisp

(defconstant +QUEEN+      #x01)
(defconstant +ROOK+       #x02)
(defconstant +KNIGHT+     #x04)
(defconstant +BISHOP+     #x08)
(defconstant +PAWN+       #x10)
(defconstant +KING+       #x20)

(defconstant +WQUEEN+     (logior #x01 #x40))
(defconstant +WROOK+      (logior #x02 #x40))
(defconstant +WKNIGHT+    (logior #x04 #x40))
(defconstant +WBISHOP+    (logior #x08 #x40))
(defconstant +WPAWN+      (logior #x10 #x40))
(defconstant +WKING+      (logior #x20 #x40))

(defconstant +PROMOTABLE+ #x0f)
(defconstant +CAPTURABLE+ #x3f)
(defconstant +PIECE+      #x3f)

(defconstant +WHITE+      #x40)
(defconstant +BLACK+      #x00)

(defconstant +FEN-START+ "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defparameter *unicode* nil)

(declaim (inline piece
                 piece-side
                 index-valid?
                 white
                 board-index
                 field-index
                 index-field
                 index-row
                 index-col
                 is-pawn?
                 is-knight?
                 is-bishop?
                 is-rook?
                 is-queen?
                 is-king?
                 is-black?
                 is-white?
                 same-side?
                 opp-side?
                 board-get
                 board-get-rc
                 board-set
                 board-set-rc
                 make-move
                 move-from
                 move-to
                 move-piece
                 move-white?
                 move-black?
                 move-side
                 move-capture?
                 move-captured-piece
                 move-promote?
                 move-promoted-piece
                 move-set-promoted-piece
                 move-check?
                 move-set-check
                 move-enpa?
                 move-captured-index
                 move-oo?
                 move-ooo?
                 move-castle?))

(deftype piece ()
  '(unsigned-byte 8))

(deftype board-index ()
  '(integer 0 119))

(deftype board ()
  '(simple-array piece (120)))

(defun piece (p)
  (declare (type piece p))
  (logand p +PIECE+))

(defun piece-side (p)
  (declare (type piece p))
  (logand p +WHITE+))

(defun index-valid? (index)
  (declare (optimize speed)
           (type fixnum index))
  (and (typep index 'board-index)
       (not (logtest index #x88))))

(defun board-index (row col)
  (declare (optimize speed)
           (type (integer 0 7) row col))
  (dpb row (byte 3 4) col))

(defun field-index (field)
  (declare (type (simple-string 2) field))
  (let ((col (aref field 0))
        (row (aref field 1)))
    (board-index (- (char-code row) 49)
                 (- (logior 32 (char-code col)) 97))))

(setf (gethash #\$ *read-table*)
      (lambda (stream ch lisp-reader)
        (declare (ignore ch lisp-reader))
        (field-index (string (list (read-char stream)
                                   (read-char stream))))))

(defun index-row (index)
  (declare (type board-index index))
  (ldb (byte 3 4) index))

(defun index-col (index)
  (declare (type board-index index))
  (ldb (byte 3 0) index))

(defmacro with-row-col ((index row col) &body body)
  (once-only (index)
    `(let ((,row (index-row ,index))
           (,col (index-col ,index)))
       (declare (ignorable ,row ,col))
       ,@body)))

(defun index-field (index)
  (declare (type board-index index))
  (with-row-col (index row col)
    (format nil "~C~C" (code-char (+ col 97)) (code-char (+ row 49)))))

(defun piece-char (p)
  (declare (type piece p))
  (if *unicode*
      (piece-unicode p)
      (ecase p
        ;; empty
        (0 #\-)
        ;; black
        (#.+PAWN+ #\p)
        (#.+KNIGHT+ #\n)
        (#.+KING+ #\k)
        (#.+BISHOP+ #\b)
        (#.+ROOK+ #\r)
        (#.+QUEEN+ #\q)
        ;; white
        (#.+WPAWN+ #\P)
        (#.+WKNIGHT+ #\N)
        (#.+WKING+ #\K)
        (#.+WBISHOP+ #\B)
        (#.+WROOK+ #\R)
        (#.+WQUEEN+ #\Q))))

(defun piece-unicode (p)
  (declare (type piece p))
  (ecase p
    ;; empty
    (0 #\□)
    ;; black
    (#.+PAWN+ #\♟)
    (#.+KNIGHT+ #\♞)
    (#.+KING+ #\♚)
    (#.+BISHOP+ #\♝)
    (#.+ROOK+ #\♜)
    (#.+QUEEN+ #\♛)
    ;; white
    (#.+WPAWN+ #\♙)
    (#.+WKNIGHT+ #\♘)
    (#.+WKING+ #\♔)
    (#.+WBISHOP+ #\♗)
    (#.+WROOK+ #\♖)
    (#.+WQUEEN+ #\♕)))

(defun char-piece (p)
  (declare (type character p))
  (case p
    (#\- 0)
    ;; black
    ((#\p #\♟) +PAWN+)
    ((#\n #\♞) +KNIGHT+)
    ((#\k #\♚) +KING+)
    ((#\b #\♝) +BISHOP+)
    ((#\r #\♜) +ROOK+)
    ((#\q #\♛) +QUEEN+)
    ;; white
    ((#\P #\♙) #.+WPAWN+)
    ((#\N #\♘) #.+WKNIGHT+)
    ((#\K #\♔) #.+WKING+)
    ((#\B #\♗) #.+WBISHOP+)
    ((#\R #\♖) #.+WROOK+)
    ((#\Q #\♕) #.+WQUEEN+)))

(defun is-pawn? (p)
  (declare (type piece p))
  (logtest p +PAWN+))

(defun is-knight? (p)
  (declare (type piece p))
  (logtest p +KNIGHT+))

(defun is-bishop? (p)
  (declare (type piece p))
  (logtest p +BISHOP+))

(defun is-rook? (p)
  (declare (type piece p))
  (logtest p +ROOK+))

(defun is-queen? (p)
  (declare (type piece p))
  (logtest p +QUEEN+))

(defun is-king? (p)
  (declare (type piece p))
  (logtest p +KING+))

(defun is-white? (p)
  (declare (type piece p))
  (logtest p +WHITE+))

(defun is-black? (p)
  (declare (type piece p))
  (not (is-white? p)))

(defun same-side? (p1 p2)
  (declare (type piece p1 p2))
  (= (logand p1 +WHITE+) (logand p2 +WHITE+)))

(defun opp-side? (p1 p2)
  (declare (type piece p1 p2))
  (not (same-side? p1 p2)))

(defun board-get-rc (board row col)
  (declare (optimize speed)
           (type board board)
           (type (integer 0 7) row col))
  (aref board (board-index row col)))

(defun board-set-rc (board row col piece)
  (declare (optimize speed)
           (type board board)
           (type (integer 0 7) row col)
           (type piece piece))
  (setf (aref board (board-index row col)) piece))

(defun board-get (board index)
  (declare (optimize speed)
           (type board board)
           (type board-index index))
  (aref board index))

(defun board-set (board index val)
  (declare (optimize speed)
           (type board board)
           (type board-index index)
           (type piece val))
  (setf (aref board index) val))

(defsetf board-get board-set)

(defmacro with-piece ((board pos p &optional allow-empty) &body body)
  (once-only (board pos)
    `(let ((,p (board-get ,board ,pos)))
       ,(if allow-empty
            `(progn ,@body)
            `(unless (zerop ,p)
               ,@body)))))

(defun make-board ()
  (make-array 120 ;; :element-type 'piece
              :initial-element 0))

(defun board-foreach (board fn)
  (declare (optimize speed)
           (type board board)
           (type (function (piece (integer 0 7) (integer 0 7) board-index) t) fn))
  (loop for row from 0 to 7 do
        (loop for col from 0 to 7
              for index = (board-index row col)
              for piece = (board-get board index)
              when (not (zerop piece))
              do (funcall fn piece row col index))))

(defun print-board (board &key (output t))
  (loop for row from 7 downto 0
        for line = (loop for col from 0 to 7
                         collect (piece-char (board-get-rc board row col)))
        do (format output "~D │ ~{~A~^ ~}~%" (+ row 1) line))
  (format output "  └─────────────────~%")
  (format output "    a b c d e f g h~%"))

(defconstant +WHITE-OO+     1)
(defconstant +WHITE-OOO+    2)
(defconstant +WHITE-CASTLE+ 3)
(defconstant +BLACK-OO+     4)
(defconstant +BLACK-OOO+    8)
(defconstant +BLACK-CASTLE+ 12)
(defconstant +ALL-CASTLE+   15)

(defstruct game
  (board (make-board) :type board)
  (state 0 :type (unsigned-byte 32))
  (side +WHITE+ :type piece)
  (enpa nil :type (or board-index null))
  (fullmove 0 :type (unsigned-byte 32))
  (halfmove 0 :type (unsigned-byte 32)))

(defgeneric reset-from-fen (game input))

(defmethod reset-from-fen (game (in stream))
  (let ((board (game-board game))
        (state 0))
    (with-parse-stream in
      (labels ((read-row (row)
                 (loop for ch = (peek)
                       for col upfrom 0
                       do (cond
                            ((member ch '(#\p #\n #\k #\b #\r #\q #\P #\N #\K #\B #\R #\Q))
                             (next)
                             (board-set-rc board row col (char-piece ch)))
                            ((member ch '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8))
                             (next)
                             (dotimes (i (- (char-code ch) 48))
                               (board-set-rc board row col 0)
                               (incf col))
                             (decf col))
                            (t (return)))))

               (read-position ()
                 (loop for row from 7 downto 0
                       do (read-row row)
                       (unless (zerop row)
                         (skip #\/))))

               (read-side ()
                 (case (next)
                   (#\w (setf (game-side game) +WHITE+))
                   (#\b (setf (game-side game) +BLACK+))
                   (otherwise (error "Cannot read playing side"))))

               (read-castling ()
                 (if (eql (peek) #\-)
                     (next)
                     (loop while (member (peek) '(#\k #\q #\K #\Q))
                           do (case (next)
                                (#\k (setf state (logior state +BLACK-OO+)))
                                (#\q (setf state (logior state +BLACK-OOO+)))
                                (#\K (setf state (logior state +WHITE-OO+)))
                                (#\Q (setf state (logior state +WHITE-OOO+))))))
                 (setf (game-state game) state))

               (read-en-passant ()
                 (if (eql (peek) #\-)
                     (progn
                       (next)
                       (setf (game-enpa game) nil))
                     (let ((col (next))
                           (row (next)))
                       (unless (and (member col '(#\a #\b #\c #\d #\e #\f #\g #\h #\A #\B #\C #\D #\E #\F #\G #\H))
                                    (member row '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8)))
                         (error "Invalid en-passant field"))
                       (setf (game-enpa game)
                             (board-index (- (char-code row) 49)
                                          (- (char-code (char-downcase col)) 97))))))

               (read-halfmove ()
                 (setf (game-halfmove game) (read-integer)))

               (read-fullmove ()
                 (setf (game-fullmove game) (read-integer))))

        ;; now do it
        (read-position)
        (skip #\SPACE)
        (read-side)
        (skip #\SPACE)
        (read-castling)
        (skip #\SPACE)
        (read-en-passant)
        (skip #\SPACE)
        (read-halfmove)
        (skip #\SPACE)
        (read-fullmove)
        game))))

(defmethod reset-from-fen (game (fen string))
  (with-input-from-string (in fen)
    (reset-from-fen game in)))

(defgeneric reset-game (game))

(defmethod reset-game (game)
  (reset-from-fen game +FEN-START+))

(defgeneric game-fen (game))

(defmethod game-fen (game)
  (let ((board (game-board game))
        (state (game-state game))
        (enpa (game-enpa game))
        (*unicode* nil))
    (with-output-to-string (*standard-output*)
      (loop for row from 7 downto 0
            do (loop with empty = 0
                     for col from 0 to 7
                     for p = (board-get-rc board row col)
                     do (cond
                          ((zerop p) (incf empty))
                          (t
                           (unless (zerop empty)
                             (format t "~D" empty))
                           (setf empty 0)
                           (write-char (piece-char p))))
                     finally (unless (zerop empty)
                               (format t "~D" empty)))
            (unless (zerop row)
              (write-char #\/)))
      (write-char #\SPACE)
      (write-char (if (= 0 (game-side game)) #\b #\w))
      (write-char #\SPACE)
      (when (logtest state +WHITE-OO+) (write-char #\K))
      (when (logtest state +WHITE-OOO+) (write-char #\Q))
      (when (logtest state +BLACK-OO+) (write-char #\k))
      (when (logtest state +BLACK-OOO+) (write-char #\q))
      (unless (logtest state +ALL-CASTLE+) (write-char #\-))
      (write-char #\SPACE)
      (if enpa
          (write-string (index-field enpa))
          (write-char #\-))
      (write-char #\SPACE)
      (format t "~D" (game-halfmove game))
      (write-char #\SPACE)
      (format t "~D" (game-fullmove game)))))

;;; moves

(deftype move ()
  '(unsigned-byte 32))

(defmacro pipe (init &rest forms)
  (loop for result = init then (append form (list result))
        for form in forms
        finally (return result)))

(defun make-move (from to piece capture enpa)
  (declare (optimize speed)
           (type board-index from to)
           (type piece piece capture)
           (type (unsigned-byte 1) enpa))
  (pipe (index-col from)
        (dpb (index-row from) (byte 3 3))
        (dpb (index-col to) (byte 3 6))
        (dpb (index-row to) (byte 3 9))
        (dpb piece (byte 7 12))
        (dpb capture (byte 6 23))
        (dpb enpa (byte 1 29))))

(defun move-from (move)
  (declare (type move move))
  (board-index (ldb (byte 3 3) move)
               (ldb (byte 3 0) move)))

(defun move-to (move)
  (declare (type move move))
  (board-index (ldb (byte 3 9) move)
               (ldb (byte 3 6) move)))

(defun move-piece (move)
  (declare (type move move))
  (ldb (byte 7 12) move))

(defun move-white? (move)
  (declare (type move move))
  (ldb-test (byte 1 18) move))

(defun move-black? (move)
  (declare (type move move))
  (not (move-white? move)))

(defun move-side (move)
  (declare (type move move))
  (ash (ldb (byte 1 18) move) 6))

(defun move-capture? (move)
  (declare (type move move))
  (ldb-test (byte 6 23) move))

(defun move-captured-piece (move)
  (declare (type move move))
  (let ((p (ldb (byte 6 23) move)))
    (cond
      ((zerop p) nil)
      ((move-black? move) (logior p +WHITE+))
      (t p))))

(defun move-promote? (move)
  (declare (type move move))
  (ldb-test (byte 4 19) move))

(defun move-promoted-piece (move)
  (declare (type move move))
  (let ((p (ldb (byte 4 19) move)))
    (cond
      ((zerop p) nil)
      ((move-black? move) p)
      (t (logior p +WHITE+)))))

(defun move-set-promoted-piece (move promo)
  (declare (type move move)
           (type piece promo))
  (dpb promo (byte 4 19) move))

(defun move-set-check (move)
  (declare (type move move))
  (dpb 1 (byte 1 30) move))

(defun move-check? (move)
  (declare (type move move))
  (ldb-test (byte 1 30) move))

(defun move-enpa? (move)
  (declare (type move move))
  (ldb-test (byte 1 29) move))

(defun move-captured-index (move)
  (declare (type move move))
  (cond
    ((move-enpa? move)
     (board-index (ldb (byte 3 3) move)
                  (ldb (byte 3 6) move)))
    ((move-capture? move)
     (move-to move))
    (t
     (error "Not a capturing move"))))

(defun move-oo? (move)
  (declare (type move move))
  (= (logand move #b0111111000111000111)
     #b0100000000110000100))

(defun move-ooo? (move)
  (declare (type move move))
  (= (logand move #b0111111000111000111)
     #b0100000000010000100))

(defun move-castle? (move)
  (declare (type move move))
  (= (logand move #b0111111000011000111)
     #b0100000000010000100))

;;; move execution

(defun game-move (game move &optional quick)
  (declare (optimize speed)
           (type game game)
           (type move move))
  (let ((board (game-board game))
        (from (move-from move))
        (to (move-to move))
        (piece (move-piece move))
        (white (move-white? move))
        (promo (move-promoted-piece move)))
    ;; update board
    (board-set board to (or promo piece))
    (board-set board from 0)
    ;; handle special moves (castle and en-passant)
    (cond
      ((move-oo? move)
       (cond
         (white
          (board-set board $H1 0)
          (board-set board $F1 +WROOK+))
         (t
          (board-set board $H8 0)
          (board-set board $F8 +ROOK+))))
      ((move-ooo? move)
       (cond
         (white
          (board-set board $A1 0)
          (board-set board $D1 +WROOK+))
         (t
          (board-set board $A8 0)
          (board-set board $D8 +ROOK+))))
      ((move-enpa? move)
       (board-set board (move-captured-index move) 0)))

    ;; update side to move and en-passant target
    (setf (game-side game) (if white +BLACK+ +WHITE+)
          (game-enpa game) (when (and (is-pawn? piece)
                                      (= (abs (- from to)) 32))
                             (ash (+ from to) -1)))

    ;; update castling state
    (symbol-macrolet ((state (game-state game)))
      (when (and (logtest state +WHITE-OO+)
                 (or (= from $E1) (= from $H1) (= to $H1)))
        (setf state (logxor state +WHITE-OO+)))
      (when (and (logtest state +WHITE-OOO+)
                 (or (= from $E1) (= from $A1) (= to $A1)))
        (setf state (logxor state +WHITE-OOO+)))
      (when (and (logtest state +BLACK-OO+)
                 (or (= from $E8) (= from $H8) (= to $H8)))
        (setf state (logxor state +BLACK-OO+)))
      (when (and (logtest state +BLACK-OOO+)
                 (or (= from $E8) (= from $A8) (= to $A8)))
        (setf state (logxor state +BLACK-OOO+))))

    ;; fullmove and halfmove counters
    (unless quick
      (unless white
        (incf (game-fullmove game)))
      (if (or (is-pawn? piece) (move-capture? move))
          (setf (game-halfmove game) 0)
          (incf (game-halfmove game))))))

(defun board-undo-move (board move)
  (declare (optimize speed)
           (type board board)
           (type move move))
  (let ((from (move-from move))
        (to (move-to move))
        (captured (move-captured-piece move))
        (piece (move-piece move)))
    ;; restore board
    (board-set board from piece)
    (cond
      (captured
       (board-set board (move-captured-index move) captured)
       (when (move-enpa? move)
         (board-set board to 0)))
      (t
       (board-set board to 0)))
    ;; special moves
    (cond
      ((move-oo? move)
       (cond
         ((move-white? move)
          (board-set board $H1 +WROOK+)
          (board-set board $F1 0))
         (t
          (board-set board $H8 +ROOK+)
          (board-set board $F8 0))))
      ((move-ooo? move)
       (cond
         ((move-white? move)
          (board-set board $A1 +WROOK+)
          (board-set board $D1 0))
         (t
          (board-set board $A8 +ROOK+)
          (board-set board $D8 0)))))))

(defmacro with-move ((game move &optional quick) &body body)
  (once-only (game move)
    (with-gensyms (state enpa halfmove fullmove side)
      `(let ((,state (game-state ,game))
             (,enpa (game-enpa ,game))
             (,side (game-side ,game))
             ,@(unless quick
                 `((,halfmove (game-halfmove ,game))
                   (,fullmove (game-fullmove ,game)))))
         (game-move ,game ,move ,quick)
         (prog1
             (progn ,@body)
           (board-undo-move (game-board ,game) ,move)
           (setf (game-side ,game) ,side
                 (game-state ,game) ,state
                 (game-enpa ,game) ,enpa
                 ,@(unless quick
                     `((game-halfmove ,game) ,halfmove
                       (game-fullmove ,game) ,fullmove))))))))

;;; move generation

(defconstant +MOVES-KNIGHT+ '(31 33 14 18 -18 -14 -33 -31))
(defconstant +MOVES-BISHOP+ '(15 17 -15 -17))
(defconstant +MOVES-ROOK+   '(1 16 -16 -1))
(defconstant +MOVES-QING+   `(,@+MOVES-BISHOP+ ,@+MOVES-ROOK+))

(defun king-index (game &optional (side (game-side game)))
  (declare (optimize speed)
           (type game game)
           (type piece side))
  (loop with king = (logior +KING+ side)
        with board = (game-board game)
        for row from 0 to 7 do
        (loop for col from 0 to 7
              for index = (board-index row col)
              when (= (board-get board index) king)
              do (return-from king-index index))))

(defun attacked? (game &optional
                       (side (game-side game))
                       (index (king-index game side)))
  (declare (optimize speed)
           (type piece side)
           (type game game)
           (type board-index index))
  (let* ((board (game-board game))
         (opp (logxor side +WHITE+)))
    (labels ((test (p piece)
               (when (and (same-side? p opp)
                          (= p (logand p piece)))
                 (return-from attacked? t)))
             (check (piece delta)
               (declare (type piece piece)
                        (type fixnum delta))
               (let ((pos (+ index delta)))
                 (when (index-valid? pos)
                   (with-piece (board pos p)
                     (test p piece)))))
             (repeat (piece delta)
               (declare (type fixnum delta))
               (loop for pos = (+ index delta) then (+ pos delta)
                     while (index-valid? pos)
                     do (with-piece (board pos p)
                          (test p piece)
                          (return)))))
      (declare (inline test check repeat))
      (cond ((is-white? opp)
             (check +WPAWN+ -15)
             (check +WPAWN+ -17))
            (t
             (check +PAWN+ +15)
             (check +PAWN+ +17)))
      (loop with piece = (logior opp +KNIGHT+)
            for delta in +MOVES-KNIGHT+
            do (check piece delta))
      (loop with piece = (logior opp +BISHOP+ +QUEEN+)
            for delta in +MOVES-BISHOP+
            do (repeat piece delta))
      (loop with piece = (logior opp +ROOK+ +QUEEN+)
            for delta in +MOVES-ROOK+
            do (repeat piece delta))
      (loop with piece = (logior opp +KING+)
            for delta in +MOVES-QING+
            do (check piece delta))
      nil)))

(defun game-compute-moves (game)
  (declare (optimize speed)
           (type game game))
  (let* ((side (game-side game))
         (opp (logxor side +WHITE+))
         (board (game-board game))
         (moves '())
         (enpa (game-enpa game))
         (my-king (king-index game side))
         (opp-king (king-index game opp))
         flag in-check)
    (loop for row from 0 to 7 do
          (loop for col from 0 to 7
                for from = (board-index row col)
                for piece = (board-get board from)
                when (same-side? piece side) do
                (labels
                    ((move-pawn (c1 c2 a1 a2 on-start on-end)
                       (labels ((try-enpa (delta)
                                  (when enpa
                                    (let ((to (+ from delta)))
                                      (when (= to enpa)
                                        (add (make-move from to piece (logior +PAWN+ opp) 1))))))
                                (try-capture (delta)
                                  (let ((to (+ from delta)))
                                    (when (index-valid? to)
                                      (with-piece (board to p)
                                        (when (opp-side? piece p)
                                          (maybe-promote (make-move from to piece p 0)))))))
                                (try-advance (delta)
                                  (let ((to (+ from delta)))
                                    ;; `to' index should be always valid
                                    (with-piece (board to p t)
                                      (when (zerop p)
                                        (maybe-promote (make-move from to piece 0 0))
                                        ;; we want to return true if the field is empty, so that
                                        ;; i.e. if we're in check and D3 doesn't get us out (in which
                                        ;; case `add' will return nil), we still want to try D4.
                                        t))))
                                (maybe-promote (move)
                                  (cond
                                    (on-end
                                     (when (add (move-set-promoted-piece move +KNIGHT+))
                                       (%add (move-set-promoted-piece move +BISHOP+))
                                       (%add (move-set-promoted-piece move +ROOK+))
                                       (%add (move-set-promoted-piece move +QUEEN+))))
                                    (t
                                     (add move)))))

                         (declare (inline try-enpa try-capture try-advance maybe-promote))

                         (unless (try-enpa c1) (try-capture c1))
                         (unless (try-enpa c2) (try-capture c2))
                         (when (and (try-advance a1) on-start)
                           (try-advance a2))))

                     (move-knight ()
                       (mapc #'move +MOVES-KNIGHT+))

                     (move-bishop ()
                       (mapc #'repeat +MOVES-BISHOP+))

                     (move-rook ()
                       (mapc #'repeat +MOVES-ROOK+))

                     (move-queen ()
                       (mapc #'repeat +MOVES-QING+))

                     (in-check? ()
                       (if flag
                           in-check
                           (setf flag t
                                 in-check (attacked? game side my-king))))

                     (move-king (oo ooo oo1 oo2 ooo1 ooo2 ooo3)
                       (mapc #'move +MOVES-QING+)
                       ;; note: `add' discards all moves that leave our king in check, so it's not
                       ;; necessary to test here whether the target field is attacked; we only
                       ;; have to check the middle field (i.e. F1 for white's O-O).
                       (when (and (logtest (game-state game) oo)
                                  (zerop (board-get board oo1))
                                  (zerop (board-get board oo2))
                                  (not (in-check?))
                                  (not (attacked? game side oo1)))
                         (add (make-move from oo2 piece 0 0)))
                       (when (and (logtest (game-state game) ooo)
                                  (zerop (board-get board ooo1))
                                  (zerop (board-get board ooo2))
                                  (zerop (board-get board ooo3))
                                  (not (in-check?))
                                  (not (attacked? game side ooo1)))
                         (add (make-move from ooo2 piece 0 0))))

                     (move (delta)
                       (declare (type fixnum delta))
                       (let ((to (+ from delta)))
                         (when (index-valid? to)
                           (with-piece (board to p t)
                             (when (or (zerop p) (opp-side? p side))
                               (add (make-move from to piece p 0)))))))

                     (repeat (delta)
                       (declare (type fixnum delta))
                       (loop for to = (+ from delta) then (+ to delta)
                             while (index-valid? to) do
                             (let ((p (board-get board to)))
                               (cond
                                 ((zerop p)
                                  (add (make-move from to piece 0 0)))
                                 ((opp-side? p side)
                                  (add (make-move from to piece p 0))
                                  (return))
                                 (t
                                  (return))))))

                     (add (m)
                       (with-move (game m t)
                         (let ((index (if (is-king? piece) (move-to m) my-king)))
                           (unless (attacked? game side index)
                             (car (push (if (attacked? game opp opp-king)
                                            (move-set-check m)
                                            m)
                                        moves))))))

                     (%add (m)
                       (with-move (game m t)
                         (car (push (if (attacked? game opp opp-king)
                                        (move-set-check m)
                                        m)
                                    moves)))))

                  (declare (inline move-pawn move-king in-check?))

                  (case piece
                    (#.+PAWN+                  (move-pawn -15 -17 -16 -32 (= row 6) (= row 1)))
                    (#.+WPAWN+                 (move-pawn +15 +17 +16 +32 (= row 1) (= row 6)))
                    ((#.+KNIGHT+ #.+WKNIGHT+)  (move-knight))
                    ((#.+BISHOP+ #.+WBISHOP+)  (move-bishop))
                    ((#.+ROOK+ #.+WROOK+)      (move-rook))
                    ((#.+QUEEN+ #.+WQUEEN+)    (move-queen))
                    (#.+KING+                  (move-king +BLACK-OO+ +BLACK-OOO+ $F8 $G8 $D8 $C8 $B8))
                    (#.+WKING+                 (move-king +WHITE-OO+ +WHITE-OOO+ $F1 $G1 $D1 $C1 $B1))))))

    moves))

(defun %game-parse-san (game in moves)
  (let* ((side (game-side game))
         (white (is-white? side))
         (promo nil)
         (capture nil)
         piece from to from-file from-rank to-file to-rank)
    (flet ((matches (m)
             (let ((mfrom (move-from m))
                   (mto (move-to m)))
               (when (and from to)
                 (return-from matches
                   (when (and (= from mfrom) (= to mto)
                              (eql promo (move-promoted-piece m)))
                     m)))
               (when (and (eql piece (move-piece m))
                          (eql promo (move-promoted-piece m)))
                 (with-row-col (mfrom mfromrow mfromcol)
                   (with-row-col (mto mtorow mtocol)
                     (when (and from-file (/= from-file mfromcol))
                       (return-from matches nil))
                     (when (and from-rank (/= from-rank mfromrow))
                       (return-from matches nil))
                     (when (and to-file (/= to-file mtocol))
                       (return-from matches nil))
                     (when (and to-rank (/= to-rank mtorow))
                       (return-from matches nil))
                     (when (eql capture t)
                       (return-from matches (and (move-capture? m) m)))
                     (when capture
                       (return-from matches (and (eql (move-captured-piece m) capture) m)))
                     (return-from matches m)))))))

      (with-parse-stream in
        (labels ((read-piece ()
                   (let* ((ch (peek))
                          (p (and ch (char-piece ch))))
                     (when (and p (or (> (char-code ch) 255)
                                      (is-white? p)))
                       (next)
                       (logior side (piece p)))))

                 (read-field ()
                   (let (file rank)
                     (setf file (peek))
                     (if (and file (char<= #\a file #\h))
                         (progn
                           (setf file (- (char-code file) 97))
                           (next))
                         (setf file nil))
                     (setf rank (peek))
                     (if (and rank (char<= #\1 rank #\8))
                         (progn
                           (setf rank (- (char-code rank) 49))
                           (next))
                         (setf rank nil))
                     (values file rank)))

                 (read-from ()
                   (multiple-value-bind (file rank) (read-field)
                     (setf from-file file
                           from-rank rank)
                     (when (and file rank)
                       (setf from (board-index rank file)))))

                 (maybe-skip (chars)
                   (when (member (peek) chars :test #'eql)
                     (next)))

                 (skip-sep ()
                   (awhen (maybe-skip '(#\x #\: #\-))
                     (when (or (eql it #\x) (eql it #\:))
                       (setf capture t))))

                 (read-to ()
                   (multiple-value-bind (file rank) (read-field)
                     (setf to-file file
                           to-rank rank)
                     (when (and file rank)
                       (setf to (board-index rank file)))))

                 (read-promo ()
                   (when (eql (peek) #\=)
                     (next))
                   (read-piece))

                 (read-castle ()
                   (when (eql (peek) #\O)
                     (next)
                     (unless (eql (next) #\-)
                       (return-from %game-parse-san nil))
                     (unless (eql (next) #\O)
                       (return-from %game-parse-san nil))
                     (cond
                       ((eql (peek) #\-)
                        (next)
                        (unless (eql (next) #\O)
                          (return-from %game-parse-san nil))
                        (setf piece (logior +KING+ side)
                              from  (if white $E1 $E8)
                              to    (if white $C1 $C8)))
                       (t
                        (setf piece (logior +KING+ side)
                              from  (if white $E1 $E8)
                              to    (if white $G1 $G8))))
                     t)))

          (unless (read-castle)
            (setf piece (read-piece))
            (read-from)
            (skip-sep)
            (when capture
              (awhen (read-piece)
                (setf capture (logxor it +WHITE+))))
            (read-to)
            (setf promo (read-promo))
            (loop while (maybe-skip '(#\# #\+ #\! #\?)))

            (when (and (not piece) (or from-file from-rank to-file to-rank))
              (setf piece (logior side +PAWN+)))

            (when (and (or from-file from-rank)
                       (not to-file) (not to-rank)) ; only destination is specified
              (setf to from
                    from nil
                    to-file from-file
                    to-rank from-rank
                    from-file nil
                    from-rank nil)))))

      (loop for m in moves when (matches m) collect m))))

(defgeneric game-parse-san (game input &optional moves))

(defmethod game-parse-san (game (in stream)
                                &optional (moves (game-compute-moves game)))
  (%game-parse-san game in moves))

(defmethod game-parse-san (game (san string)
                                &optional (moves (game-compute-moves game)))
  (with-input-from-string (in san)
    (%game-parse-san game in moves)))

(defgeneric game-san (game move &optional computed-moves))

(defmethod game-san (game move &optional (moves (game-compute-moves game)))
  (declare (type move move))
  (let ((piece (move-piece move))
        (from (move-from move))
        (to (move-to move)))
    (with-output-to-string (out)
      (with-row-col (from row col)
        (cond
          ((move-oo? move) (write-string "O-O" out))
          ((move-ooo? move) (write-string "O-O-O" out))
          ((is-pawn? piece)
           (when (move-capture? move)
             (format out "~Cx" (code-char (+ col 97))))
           (write-string (index-field to) out)
           (awhen (move-promoted-piece move)
             (format out "=~C" (char-upcase (piece-char it)))))
          (t
           (let (same-row same-col ambiguous)
             (loop for m in moves until (and same-row same-col) do
                   (when (and (= (move-piece m) piece)
                              (/= (move-from m) from)
                              (= (move-to m) to))
                     (setf ambiguous t)
                     (when (= (index-row (move-from m)) row)
                       (setf same-row t))
                     (when (= (index-col (move-from m)) col)
                       (setf same-col t))))
             (write-char (char-upcase (piece-char piece)) out)
             (when ambiguous
               (cond
                 ((and same-col same-row)
                  (write-string (index-field from) out))
                 (same-col
                  (write-char (code-char (+ row 49)) out))
                 (t
                  (write-char (code-char (+ col 97)) out))))
             (when (move-capture? move)
               (write-string "x" out))
             (write-string (index-field to) out)))))
      (with-move (game move t)
        (if (attacked? game)
            (write-string (if (null (game-compute-moves game))
                              "#"
                              "+")
                          out))))))

(defun draw-by-material? (game)
  (declare (optimize speed)
           (type game game))
  (let ((has-knights nil)
        (has-bishops nil))
    (board-foreach
     (game-board game)
     (lambda (p row col index)
       (declare (type (integer 0 7) row col)
                (type board-index index)
                (ignore index))
       (cond
         ((logtest p #.(logior +QUEEN+ +ROOK+ +PAWN+))
          (return-from draw-by-material? nil))
         ((is-bishop? p)
          (when has-knights
            (return-from draw-by-material? nil))
          (let ((color (logand (+ row col) 1)))
            (when (and has-bishops (/= has-bishops color))
              (return-from draw-by-material? nil))
            (setf has-bishops color)))
         ((is-knight? p)
          (when has-bishops
            (return-from draw-by-material? nil))
          (setf has-knights t)))))
    t))

;; EOF - TEST STUFF

(defun perft (game depth &optional count-mates)
  (let ((captures 0)
        (enpa 0)
        (castles 0)
        (promotions 0)
        (checks 0)
        (count 0)
        (checkmates 0))
    (labels ((rec (depth)
               (declare (type (unsigned-byte 8) depth))
               (let ((moves (game-compute-moves game)))
                 (if (> depth 1)
                     (loop for m in moves do
                           (with-move (game m t)
                             (rec (1- depth))))
                     (loop for m in moves
                           when (move-capture? m)
                           do (incf captures)
                           when (move-enpa? m)
                           do (incf enpa)
                           when (move-castle? m)
                           do (incf castles)
                           when (move-promote? m)
                           do (incf promotions)
                           when (move-check? m)
                           do (incf checks)
                           do (incf count)
                           (when (and count-mates (move-check? m))
                             (with-move (game m t)
                               (if (and (attacked? game)
                                        (null (game-compute-moves game)))
                                   (incf checkmates)))))))))
      (rec depth)
      (format t "Depth: ~D, Count: ~D, Captures: ~D, Enpa: ~D, Checks: ~D, Promo: ~D, Castle: ~D~%"
              depth count captures enpa checks promotions castles)
      (values count captures enpa castles promotions checks checkmates))))

(defun divide (game depth)
  (loop with moves = (game-compute-moves game)
        with count with captures with enpa with castles with promotions
        for m in moves
        for san = (game-san game m moves)
        do (with-move (game m t)
             (with-output-to-string (*standard-output*)
               (multiple-value-setq (count captures enpa castles promotions) (perft game (1- depth))))
             (format t "~A~A ~A~%"
                     (index-field (move-from m))
                     (index-field (move-to m))
                     count))))

;;;; FILE: pgn.lisp

(defgeneric parse-pgn (input &key ext-moves &allow-other-keys))

(defmethod parse-pgn ((in stream) &key ext-moves)
  (with-parse-stream in
    (labels
        ((read-sym ()
           (read-while #'alnum?))

         (read-header ()
           (let (name value)
             (skip #\[)
             (setf name (read-sym))
             (skip-whitespace)
             (setf value (read-string))
             (skip #\])
             (cons name value)))

         (read-moves (game)
           (let ((data '()))
             (flet ((move ()
                      (let* ((comp-moves (game-compute-moves game))
                             (valid (%game-parse-san game in comp-moves)))
                        (skip-whitespace)
                        (cond
                          ((null valid)
                           (croak "Invalid move"))
                          ((< 1 (length valid))
                           (croak "Ambiguous move")))
                        (cond
                          (ext-moves
                           (push (list :move (car valid)
                                       :san (game-san game (car valid) comp-moves)
                                       :fen-before (game-fen game)
                                       :fen-after (progn
                                                    (game-move game (car valid))
                                                    (game-fen game)))
                                 data))
                          (t
                           (push (cons :move (car valid)) data)
                           (game-move game (car valid))))))
                    (comment1 ()
                      (read-while (lambda (ch)
                                    (not (eql #\Newline ch)))))
                    (comment2 ()
                      (prog1
                          (read-while (lambda (ch)
                                        (not (eql #\} ch))))
                        (skip #\}))))
               (loop for ch = (peek) while ch do
                     (skip-whitespace)
                     (cond
                       ((eql ch #\;)
                        (next)
                        (push (cons :comment (comment1)) data))
                       ((eql ch #\{)
                        (next)
                        (push (cons :comment (comment2)) data))
                       (t
                        (awhen (read-integer)
                          (cond
                            ((and (= it 0) (eql (peek) #\-)) ; 0-1
                             (skip "-1")
                             (return (nreverse data)))
                            ((and (= it 1) (eql (peek) #\-)) ; 1-0
                             (skip "-0")
                             (return (nreverse data)))
                            ((and (= it 1) (eql (peek) #\/)) ; 1-2/1-2
                             (skip "/2-1/2")
                             (return (nreverse data)))
                            (t
                             (skip #\.)
                             (skip-whitespace)
                             (when (eql #\. (peek))
                               (skip ".."))
                             (skip-whitespace))))
                        (move)))
                     finally (return (nreverse data)))))))

      (let* ((headers (loop do (skip-whitespace)
                            while (eql #\[ (peek))
                            collect (read-header)))
             (game (make-game))
             (start-fen (assoc "fen" headers :test #'string-equal)))
        (reset-from-fen game (if start-fen
                                 (cdr start-fen)
                                 +FEN-START+))
        `(:headers ,headers
                   :moves ,(read-moves game)
                   :game ,game)))))

(defmethod parse-pgn ((pgn string) &rest args &key &allow-other-keys)
  (with-input-from-string (in pgn)
    (apply #'parse-pgn in args)))

;;;; FILE: eval.lisp

(defconstant +MATK+ 10000)
(defconstant +MATQ+ 900)
(defconstant +MATR+ 500)
(defconstant +MATB+ 330)
(defconstant +MATN+ 320)
(defconstant +MATP+ 100)

(defparameter +MAX-DEPTH+ 5)

(deftype score ()
  `(integer -32000 32000))

(defmacro defscore (name &body value)
  `(progn
     ;; (declaim (type (simple-array (integer -100 100) (8 8)) ,name))
     (defparameter ,name
       (make-array 64 :initial-contents (apply #'append ',(reverse value))))
     nil))

(defscore *p-scores*
    ( 0   0   0   0   0   0   0   0)
  (50  50  50  50  50  50  50  50)
  (10  10  20  30  30  20  10  10)
  ( 5   5  10  25  25  10   5   5)
  ( 0   0   0  20  20   0   0   0)
  ( 5  -5 -10   0   0 -10  -5   5)
  ( 5  10  10 -20 -20  10  10   5)
  ( 0   0   0   0   0   0   0   0))

(defscore *n-scores*
    (-50 -40 -30 -30 -30 -30 -40 -50)
  (-40 -20   0   0   0   0 -20 -40)
  (-30   0  10  15  15  10   0 -30)
  (-30   5  15  20  20  15   5 -30)
  (-30   0  15  20  20  15   0 -30)
  (-30   5  10  15  15  10   5 -30)
  (-40 -20   0   5   5   0 -20 -40)
  (-50 -40 -30 -30 -30 -30 -40 -50))

(defscore *b-scores*
    (-20 -10 -10 -10 -10 -10 -10 -20)
  (-10   0   0   0   0   0   0 -10)
  (-10   0   5  10  10   5   0 -10)
  (-10   5   5  10  10   5   5 -10)
  (-10   0  10  10  10  10   0 -10)
  (-10  10  10  10  10  10  10 -10)
  (-10   5   0   0   0   0   5 -10)
  (-20 -10 -10 -10 -10 -10 -10 -20))

(defscore *r-scores*
    ( 0   0   0   0   0   0   0   0)
  ( 5  10  10  10  10  10  10   5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  (-5   0   0   0   0   0   0  -5)
  ( 0   0   0   5   5   0   0   0))

(defscore *q-scores*
    (-20 -10 -10  -5  -5 -10 -10 -20)
  (-10   0   0   0   0   0   0 -10)
  (-10   0   5   5   5   5   0 -10)
  ( -5   0   5   5   5   5   0  -5)
  (  0   0   5   5   5   5   0  -5)
  (-10   5   5   5   5   5   0 -10)
  (-10   0   5   0   0   0   0 -10)
  (-20 -10 -10  -5  -5 -10 -10 -20))

(defscore *k-scores-opening*
    (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-30 -40 -40 -50 -50 -40 -40 -30)
  (-20 -30 -30 -40 -40 -30 -30 -20)
  (-10 -20 -20 -20 -20 -20 -20 -10)
  ( 20  20   0   0   0   0  20  20)
  ( 20  30  10   0   0  10  30  20))

(defscore *k-scores-ending*
    (-50 -40 -30 -20 -20 -30 -40 -50)
  (-30 -20 -10   0   0 -10 -20 -30)
  (-30 -10  20  30  30  20 -10 -30)
  (-30 -10  30  40  40  30 -10 -30)
  (-30 -10  30  40  40  30 -10 -30)
  (-30 -10  20  30  30  20 -10 -30)
  (-30 -30   0   0   0   0 -30 -30)
  (-50 -30 -30 -30 -30 -30 -30 -50))

(defun piece-value (piece)
  (declare (optimize speed)
           (type piece piece))
  (ecase (piece piece)
    (#.+PAWN+   +MATP+)
    (#.+KNIGHT+ +MATN+)
    (#.+BISHOP+ +MATB+)
    (#.+ROOK+   +MATR+)
    (#.+QUEEN+  +MATQ+)
    (#.+KING+   +MATK+)))

;; (declaim (type (function () score) get-score))
(defun get-score (piece row col &optional ending)
  (declare (optimize speed)
           (type piece piece)
           (type (integer 0 7) row col))
  (unless (is-white? piece)
    (setf row (- 7 row)
          col (- 7 col)))
  (ecase (piece piece)
    (#.+PAWN+   (+ +MATP+ (aref *p-scores* (+ (ash row 3) col))))
    (#.+KNIGHT+ (+ +MATN+ (aref *n-scores* (+ (ash row 3) col))))
    (#.+BISHOP+ (+ +MATB+ (aref *b-scores* (+ (ash row 3) col))))
    (#.+ROOK+   (+ +MATR+ (aref *r-scores* (+ (ash row 3) col))))
    (#.+QUEEN+  (+ +MATQ+ (aref *q-scores* (+ (ash row 3) col))))
    (#.+KING+   (+ +MATK+ (aref (if ending
                                    *k-scores-ending*
                                    *k-scores-opening*)
                                (+ (ash row 3) col))))))

;; (declaim (type (function () score) static-value))
(defun static-value (game)
  (declare (optimize speed)
           (type game game))
  (let ((total 0)
        (kr1 0) (kc1 0) (kr2 0) (kc2 0)
        (m1 0) (m2 0)
        (our-side (game-side game)))
    (declare (type score total m1 m2)
             (type (integer 0 7) kr1 kc1 kr2 kc2))
    (board-foreach
     (game-board game)
     (lambda (piece row col index)
       (declare (ignore index))
       (cond
         ((is-king? piece)
          (cond
            ((same-side? piece our-side)
             (setf kr1 row kc1 col))
            (t
             (setf kr2 row kc2 col))))
         (t
          (let ((score (get-score piece row col)))
            (if (same-side? piece our-side)
                (progn
                  (incf total score)
                  (unless (logtest piece +PAWN+)
                    (incf m1 (piece-value piece))))
                (progn
                  (decf total score)
                  (unless (logtest piece +PAWN+)
                    (incf m2 (piece-value piece))))))))))
    (let ((end-game (and (< m1 #.(+ +MATQ+ +MATR+))
                         (< m2 #.(+ +MATQ+ +MATR+)))))
      (incf total (get-score (logior +KING+ our-side)
                             kr1 kc1 end-game))
      (decf total (get-score (logxor (logior +KING+ our-side) our-side)
                             kr2 kc2 end-game)))
    total))

(defun move-value (move)
  (declare (optimize speed)
           (type move move))
  (let ((score (piece-value (move-piece move))))
    (declare (type fixnum score))
    (awhen (move-captured-piece move)
      (setf score (+ 10000 (piece-value it))))
    (awhen (move-promoted-piece move)
      (incf score (+ 15000 (piece-value it))))
    (when (move-check? move)
      (incf score 20000))
    score))

(defun sort-moves (moves)
  (stable-sort moves
               (lambda (m1 m2)
                 (> (move-value m1)
                    (move-value m2)))))

(defun quies-moves (moves)
  (sort-moves (remove-if-not (lambda (m)
                               (or (move-capture? m)
                                   (move-promote? m)
                                   ;; (move-check? m)
                                   ))
                             moves)))

;; (declaim (type (function () score) quies))
(defun quies (game α β moves pline)
  (declare (optimize speed)
           (type game game)
           (type score α β)
           (type list moves)
           (type cons pline))
  (let ((score (static-value game)))
    (when (>= score β)
      (return-from quies β))
    (when (> score α)
      (setf α score))
    (if (null moves)
        (if (attacked? game)
            -15000
            (- α))
        (loop for move in (quies-moves moves)
              for line = (cons nil nil)
              do (with-move (game move t)
                   (setf score (- (quies game (- β) (- α)
                                         (game-compute-moves game)
                                         line))))
              (when (>= score β)
                (return β))
              (when (> score α)
                (setf α score)
                (setf (car pline) (cons move (car line))))
              finally (return α)))))

;; (declaim (type (function () score) pvs))
(defun pvs (game start-depth α β pline)
  (declare (optimize speed)
           (type game game)
           (type (unsigned-byte 8) start-depth)
           (type score α β)
           (type cons pline))
  (labels
      ((rec (depth α β pline)
         (declare (type (unsigned-byte 8) depth)
                  (type score α β)
                  (type cons pline))
         (let ((moves (if (= depth start-depth)
                          (init-moves game)
                          (sort-moves (game-compute-moves game)))))
           (cond
             ((null moves)
              (if (attacked? game)
                  ;; for a checkmate, subtract depth so that shallow
                  ;; checkmates score better
                  (- -15000 depth)
                  ;; for a stalemate, negate the static board value -- we'd
                  ;; like to go for draw if we score lower than the opponent.
                  (- (static-value game))))
             ((zerop depth)
              ;; somehow SBCL doesn't figure out that QUIES returns a SCORE
              (- (quies game α β moves pline) depth))
             (t
              (let ((score 0))
                (declare (type score score))
                (loop for first = t then nil
                      for line = (cons nil nil)
                      for move in moves do
                      ;; (when (= depth 5)
                      ;;   (format t "Researching: ~A (~A..~A ~A)~%"
                      ;;           (dump-line game (list move))
                      ;;           α β score))
                      (with-move (game move t)
                        (cond
                          (first
                           (setf score (- (rec (1- depth) (- β) (- α) line))))
                          (t
                           (setf score (- (rec (1- depth) (- 0 α 1) (- α) line)))
                           (when (< α score β)
                             (setf score (- (rec (1- depth) (- β) (- score) line)))))))
                      (when (> score α)
                        (setf α score)
                        (setf (car pline) (cons move (car line))))
                      (when (>= α β)
                        (return-from rec α))))
              α)))))
    (rec start-depth α β pline)))

(defun init-moves (game &optional (depth 2))
  (labels ((score (depth α β)
             (let ((moves (sort-moves (game-compute-moves game))))
               (cond
                 ((null moves)
                  (if (attacked? game)
                      (- -15000 depth)
                      (- (static-value game))))
                 ((zerop depth)
                  (static-value game))
                 (t
                  (loop for m in moves
                        for score = (with-move (game m)
                                      (- (score (1- depth) (- β) (- α))))
                        finally (return α)
                        when (> score α)
                        do (setf α score)
                        when (>= α β)
                        do (return α)))))))
    (let ((scores (loop for m in (sort-moves (game-compute-moves game))
                        collect (cons m (with-move (game m)
                                          (score depth -32000 32000))))))
      (mapcar #'car
              (stable-sort scores
                           (lambda (c1 c2)
                             (< (cdr c1) (cdr c2))))))))

(defun game-search (game &optional (depth +MAX-DEPTH+))
  (let* ((line (cons nil nil))
         (score (pvs game depth -32000 +32000 line)))
    (values (car line) score)))

(defun dump-line (game moves &optional newlines)
  (with-output-to-string (out)
    (labels ((rec (moves first)
               (when moves
                 (let ((move (car moves)))
                   (when (or first (move-white? move))
                     (unless (or first newlines)
                       (write-char #\SPACE out))
                     (format out
                             (if newlines "~3D." "~D.")
                             (game-fullmove game)))
                   (when (and first (move-black? move))
                     (format out ".."))
                   (format out " ~A" (game-san game move))
                   (when (and newlines (move-black? move))
                     (write-char #\Newline out))
                   (with-move (game move)
                     (rec (cdr moves) nil))))))
      (rec moves t))))

(defun play (&key
             (fen +FEN-START+)
             (depth +MAX-DEPTH+))
  (let ((game (make-game))
        (history (list)))
    (reset-from-fen game fen)
    (flet ((computer-move ()
             (format t "...thinking...~%")
             (multiple-value-bind (line score)
                 (game-search game depth)
               (cond
                 ((null line)
                  (format t "No moves found~%"))
                 (t
                  (format t "Computer: ~A (score ~A)~%"
                          (dump-line game line) score)
                  (push (car line) history)
                  (game-move game (car line))))))
           (finished? ()
             (let ((moves (game-compute-moves game)))
               (cond
                 ((null moves)
                  (if (attacked? game) :checkmate :stalemate))
                 ((draw-by-material? game)
                  :draw)))))

      (loop
       (awhen (finished?)
         (format t "Game ended: ~A~%" it))
       (print-board (game-board game))
       (format t "~A: " (if (is-white? (game-side game))
                            "White" "Black"))
       (finish-output)
       (let ((line (read-line *standard-input* nil)))
         (cond
           ((null line)
            (return))

           ((string= line ""))

           ((or (string= line "exit")
                (string= line "end"))
            (return))

           ((string= line "go")
            (computer-move))

           ((string= line "restart")
            (reset-from-fen game fen)
            (setf history (list)))

           ((string= line "reset")
            (reset-game game)
            (setf fen +FEN-START+
                  history (list)))

           ((string= line "undo")
            (pop history)
            (reset-from-fen game fen)
            (mapc (lambda (move)
                    (game-move game move))
                  (reverse history)))

           ((string= line "pgn")
            (let ((game (make-game)))
              (reset-from-fen game fen)
              (let ((*unicode* nil))
                (format t "~A~%" (dump-line game (reverse history))))))

           ((string= line "fen")
            (let ((*unicode* nil))
              (format t "~A~%" (game-fen game))))

           (t
            (let ((moves (game-parse-san game line)))
              (cond
                ((null moves)
                 (format t "Invalid move: ~A~%" line))
                ((> (length moves) 1)
                 (format t "Ambiguous move: ~{~A~^, ~}~%"
                         (mapcar (lambda (m)
                                   (game-san game m))
                                 moves)))
                (t
                 (push (car moves) history)
                 (game-move game (car moves))
                 (print-board (game-board game))
                 (computer-move)))))))

       (format t "~%")))))

;;;; FILE: tests.lisp

(defvar *perft-pathname* "examples/perftsuite.epd")

(defun run-perft-tests (&optional (depth 3))
  (let ((input (sl-stream:open-url *perft-pathname*)))
    (with-parse-stream input
      (let ((failed 0)
            (total 0))
        (loop until (progn
                      (skip-whitespace)
                      (eof?))
              for game = (make-game)
              do (reset-from-fen game input)
              (format t "~A~%" (game-fen game))
              (print-board (game-board game))
              (incf total)
              (let ((results
                     (loop until (char= (peek) #\Newline)
                           collect
                           (cons (progn
                                   (skip-whitespace)
                                   (skip #\;)
                                   (skip #\D)
                                   (read-integer))
                                 (progn
                                   (skip-whitespace)
                                   (read-integer))))))
                (let ((count (perft game depth))
                      (result (cdr (assoc depth results))))
                  (cond
                    ((= count result)
                     (format t "PASS ~A~%" count))
                    (t
                     (format t "FAIL ~A, should be ~A~%~A~%"
                             count result results)
                     (incf failed))))
                (format t "~%~%")))
        (format t "TOTAL: ~A, FAILED: ~A~%" total failed)))))


%:EOF

(defparameter pgn (parse-pgn (sl-stream:open-url "https://lichess.org/api/games/user/vlbz?max=1")))
(defparameter moves (loop for (kind . data) in (getf pgn :moves)
                          when (eq kind :move) collect data))
(defparameter g (make-game))
(reset-from-fen g +fen-start+)
(time (dump-line g moves t))
