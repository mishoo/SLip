(load "examples/queen.lisp")
(load "lib/dom.lisp")

(defpackage :chess-board
  (:use :sl :queen :ffi))

(in-package :chess-board)

(dom:load-css "./examples/chess-board.css")

(defun display-game (pgn)
  (let* ((pgn (parse-pgn pgn :ext-moves t))
         (content (dom:from-html (make-layout pgn)))
         (dlg (dom:make-dialog 700 500
                               :content content
                               :class-name "pgn-viewer"))
         (running t)
         (current-fen (game-fen (getf pgn :game)))
         (el-pieces (dom:query "._pieces" dlg))
         (transitions 0))
    (labels
        ((on-close (target event)
           (setf running nil))

         (on-reverse (target event)
           (dom:toggle-class (dom:query "._board" dlg) "reverse"))

         (piece-selector (row col)
           (format nil ".piece[data-row='~D'][data-col='~D']:not(.fade-out)" row col))

         (piece-element (row &optional col)
           (unless col
             (setf col (index-col row)
                   row (index-row row)))
           (dom:query (piece-selector row col) el-pieces))

         (goto-move (move fen-before fen-after &optional (forward t))
           (let* ((from (move-from move))
                  (to (move-to move))
                  (r1 (index-row from))
                  (c1 (index-col from))
                  (r2 (index-row to))
                  (c2 (index-col to))
                  (white (move-white? move))
                  (capture (when (move-capture? move)
                             (move-captured-index move))))
             (cond
               (forward
                ;; direction forward, should reset to fen-after
                (cond
                  ((string= current-fen fen-before)
                   (without-interrupts
                     (let ((piece (piece-element r1 c1)))
                       (incf transitions)
                       (cond
                         (capture
                          (let ((capture (piece-element capture)))
                            (dom:add-class capture "fade-out")))
                         ((move-oo? move)
                          (let ((rook (piece-element (if white
                                                         #.(field-index "H1")
                                                         #.(field-index "H8")))))
                            (setf (dom:dataset rook :col) 5)))
                         ((move-ooo? move)
                          (let ((rook (piece-element (if white
                                                         #.(field-index "A1")
                                                         #.(field-index "A8")))))
                            (setf (dom:dataset rook :col) 3))))
                       (setf (dom:dataset piece :row) r2
                             (dom:dataset piece :col) c2
                             (dom:style piece :z-index) transitions)
                       (dom:on-event piece "transitionend" :signal :animation-end))))
                  (t
                   (let ((g (make-game)))
                     (reset-from-fen g fen-after)
                     (setf (dom:inner-html el-pieces)
                           (pieces-html (game-board g))))))
                (setf current-fen fen-after))
               (t
                ;; direction backward, should reset to fen-before
                ))))

         (on-move (target event)
           (setf (dom:checked target) t)
           (dom:scroll-into-view (dom:parent-element target))
           (goto-move (parse-integer (dom:dataset target :move))
                      (dom:dataset target :fen-before)
                      (dom:dataset target :fen-after)))

         (on-animation-end (target event)
           (without-interrupts
             (unless (dom:has-animations el-pieces)
               (let ((g (make-game)))
                 (reset-from-fen g current-fen)
                 (setf (dom:inner-html el-pieces)
                       (pieces-html (game-board g)))))))

         (reset ()
           (let ((g (make-game)))
             (setf current-fen +fen-start+)
             (reset-from-fen g +fen-start+)
             (setf (dom:inner-html el-pieces)
                   (pieces-html (game-board g)))
             (dom:do-query (el "input[name='move']" dlg)
               (setf (dom:checked el) nil))))

         (next-move (move-elements)
           (let* ((index (position-if #'dom:checked move-elements)))
             (cond
               ((not index)
                (on-move (car move-elements) nil))
               ((< (incf index) (length move-elements))
                (let ((next (elt move-elements index)))
                  (on-move next nil))))))

         (on-start (target event)
           (reset))

         (on-prev (target event)
           (next-move (reverse (dom:query-all "input[name='move']" dlg))))

         (on-next (target event)
           (next-move (dom:query-all "input[name='move']" dlg))))

      (reset)

      (let ((receivers (make-hash :close #'on-close
                                  :reverse #'on-reverse
                                  :move #'on-move
                                  :animation-end #'on-animation-end
                                  :prev #'on-prev
                                  :next #'on-next
                                  :start #'on-start)))
        (make-thread
         (lambda ()
           (dom:on-event dlg "close" :signal :close)
           (dom:on-event dlg "click" :selector "._reverse" :signal :reverse)
           (dom:on-event dlg "click" :selector "._start" :signal :start)
           (dom:on-event dlg "click" :selector "._prev" :signal :prev)
           (dom:on-event dlg "click" :selector "._next" :signal :next)
           (dom:on-event dlg "input" :selector "input[name='move']" :signal :move)
           (loop while running do (%:%receive receivers))
           (format *error-output*
                   "Thread exit ~A~%"
                   (current-thread))))))))

(defconstant +svg-chessboard+
  "<svg fill='currentColor' viewBox='0 0 512 512' xmlns='http://www.w3.org/2000/svg'><path d='M255.9.2h-64v64h64zM0 64.17v64h64v-64zM128 .2H64v64h64zm64 255.9v64h64v-64zM0 192.12v64h64v-64zM383.85.2h-64v64h64zm128 0h-64v64h64zM128 256.1H64v64h64zM511.8 448v-64h-64v64zm0-128v-64h-64v64zM383.85 512h64v-64h-64zm128-319.88v-64h-64v64zM128 512h64v-64h-64zM0 512h64v-64H0zm255.9 0h64v-64h-64zM0 320.07v64h64v-64zm319.88-191.92v-64h-64v64zm-64 128h64v-64h-64zm-64 128v64h64v-64zm128-64h64v-64h-64zm0-127.95h64v-64h-64zm0 191.93v64h64v-64zM64 384.05v64h64v-64zm128-255.9v-64h-64v64zm191.92 255.9h64v-64h-64zm-128-191.93v-64h-64v64zm128-127.95v64h64v-64zm-128 255.9v64h64v-64zm-64-127.95H128v64h64zm191.92 64h64v-64h-64zM128 128.15H64v64h64zm0 191.92v64h64v-64z'/></svg>")

(defun make-layout (pgn)
  (let* ((headers (getf pgn :headers))
         (game (getf pgn :game))
         (title (format nil "<div><b>~A</b> [~A~A] - <b>~A</b> [~A~A] (~A)</div>"
                        (or (cdr (assoc "White" headers :test #'string-equal))
                            "White")
                        (or (cdr (assoc "WhiteElo" headers :test #'string-equal))
                            "?")
                        (or (cdr (assoc "WhiteRatingDiff" headers :test #'string-equal))
                            "")
                        (or (cdr (assoc "Black" headers :test #'string-equal))
                            "Black")
                        (or (cdr (assoc "BlackElo" headers :test #'string-equal))
                            "?")
                        (or (cdr (assoc "BlackRatingDiff" headers :test #'string-equal))
                            "")
                        (or (cdr (assoc "Result" headers :test #'string-equal))
                            "*"))))
    (format nil "
<div class='layout'>
  <div class='cont-board'>
    <div class='board _board'>
      ~A
      <div class='_pieces'></div>
    </div>
  </div>
  <div class='cont-ctrl'>
    <button class='_reverse'>↺</button>
    <button class='_start'>⯬</button>
    <button class='_prev'>🞀</button>
    <button class='_next'>🞂</button>
    <button class='_end'>⯮</button>
  </div>
  <div class='cont-list'><form>~A</form></div>
  <div class='cont-title _drag-dialog'>~A</div>
</div>
"
            +svg-chessboard+
            (moves-html pgn)
            title)))

(defun pieces-html (board)
  (with-output-to-string (output)
    (let ((*unicode* nil))
      (board-foreach
       board
       (lambda (piece row col index)
         (declare (ignore index))
         (format output "<div class='piece' ~
                              data-row='~D' data-col='~D' ~
                              data-piece='~C'></div>"
                 row col
                 (piece-char piece)))))))

(defun moves-html (pgn)
  (with-output-to-string (output)
    (write-string "<div class='moves-list'>" output)
    (loop with index = 0
          for (tag . data) in (getf pgn :moves)
          when (eq tag :move)
          do (progn
               (let ((move (pop data))
                     (san (getf data :san))
                     (fen-before (getf data :fen-before))
                     (fen-after (getf data :fen-after)))
                 (when (move-white? move)
                   (format output "<div class='index'>~D</div>" (incf index)))
                 (format output "<div class='~A'>
                   <label class='move'>
                     <input name='move' type='radio' data-fen-before='~A' data-fen-after='~A' data-move='~A' />
                     ~A
                   </label></div>"
                         (if (move-white? move) "white" "black")
                         fen-before fen-after move san))))
    (write-string "</div>" output)))

(defparameter g (make-game))
(reset-from-fen g +fen-start+)

(defun test ()
  (display-game (sl-stream:open-url "examples/test.pgn")))
