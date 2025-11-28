(load "lib/queen.lisp")
(load "lib/dom.lisp")

(defpackage :pgn-viewer
  (:use :sl :queen))

(in-package :pgn-viewer)

(dom:load-css "./examples/pgn-viewer.css")

(defun display-game (pgn)
  (let* ((pgn (parse-pgn pgn :ext-moves t))
         (content (dom:from-html (make-layout pgn)))
         (dlg (dom:make-dialog 800 620
                               :content content
                               :class-name "pgn-viewer"))
         (running t)
         (current-fen (game-fen (getf pgn :game)))
         (el-pieces (dom:query "._pieces" dlg))
         (transitions 0))
    (dom:focus dlg)
    (labels
        ((on-close (target event)
           (setf running nil))

         (on-reverse (target event)
           (dom:toggle-class (dom:query "._board" dlg) "reverse"))

         (piece-selector (index)
           (format nil ".piece[data-index='~D']:not(.fade-out)" index))

         (piece-element (index)
           (dom:query (piece-selector index) el-pieces))

         (goto-move (move fen-before fen-after &optional (forward t))
           (let* ((from (move-from move))
                  (to (move-to move))
                  (white (move-white? move))
                  (capture (when (move-capture? move)
                             (move-captured-index move))))
             (cond
               (forward
                ;; direction forward, should reset to fen-after
                (cond
                  ((string= current-fen fen-before)
                   (without-interrupts
                     (let ((piece (piece-element from)))
                       (incf transitions)
                       (cond
                         (capture
                          (let ((capture (piece-element capture)))
                            (dom:add-class capture "fade-out")))
                         ((move-oo? move)
                          (let ((rook (piece-element (if white
                                                         #.(field-index "H1")
                                                         #.(field-index "H8")))))
                            (setf (dom:dataset rook :index)
                                  (if white
                                      #.(field-index "F1")
                                      #.(field-index "F8")))))
                         ((move-ooo? move)
                          (let ((rook (piece-element (if white
                                                         #.(field-index "A1")
                                                         #.(field-index "A8")))))
                            (setf (dom:dataset rook :index)
                                  (if white
                                      #.(field-index "D1")
                                      #.(field-index "D8"))))))
                       (setf (dom:dataset piece :index) to
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

         (on-end (target event)
           (let ((moves (reverse (dom:query-all "input[name='move']" dlg))))
             (on-move (car moves) nil)))

         (on-prev (target event)
           (next-move (reverse (dom:query-all "input[name='move']" dlg))))

         (on-next (target event)
           (next-move (dom:query-all "input[name='move']" dlg)))

         (on-keydown (target event)
           (case (dom:key event)
             ("ArrowLeft" (on-prev target event))
             ("ArrowRight" (on-next target event))
             ("Home" (on-start target event))
             ("End" (on-end target event))
             ("Escape" (dom:close-dialog dlg)))))

      (reset)

      (let ((receivers (make-hash :close          #'on-close
                                  :reverse        #'on-reverse
                                  :move           #'on-move
                                  :animation-end  #'on-animation-end
                                  :prev           #'on-prev
                                  :next           #'on-next
                                  :start          #'on-start
                                  :end            #'on-end
                                  :keydown        #'on-keydown)))
        (make-thread
         (lambda ()
           (dom:on-event dlg "close" :signal :close)
           (dom:on-event dlg "click" :selector "._reverse" :signal :reverse)
           (dom:on-event dlg "click" :selector "._start" :signal :start)
           (dom:on-event dlg "click" :selector "._prev" :signal :prev)
           (dom:on-event dlg "click" :selector "._next" :signal :next)
           (dom:on-event dlg "click" :selector "._end" :signal :end)
           (dom:on-event dlg "input" :selector "input[name='move']" :signal :move)
           (dom:on-event dlg "keydown" :signal :keydown)
           (loop while running do (%:%receive receivers))
           (format *error-output*
                   "Thread exit ~A~%"
                   (current-thread))))))))

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
      <div class='_pieces'></div>
    </div>
  </div>
  <div class='cont-ctrl'>
    <button class='_reverse'>↺</button>
    <button class='_start'>⏮</button>
    <button class='_prev'>❮</button>
    <button class='_next'>❯</button>
    <button class='_end'>⏭</button>
  </div>
  <div class='cont-list'><form>~A</form></div>
  <div class='cont-title _drag-dialog'>~A</div>
</div>
"
            (moves-html pgn)
            title)))

(defun pieces-html (board)
  (with-output-to-string (output)
    (let ((*unicode* nil))
      (board-foreach
       board
       (lambda (piece row col index)
         (declare (ignore row col))
         (format output "<div class='piece' ~
                              data-index='~D' ~
                              data-piece='~C'></div>"
                 index
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

(defun test ()
  (display-game (sl-stream:open-url "examples/test.pgn")))

(defun lichess (&optional (user "vlbz"))
  (display-game (sl-stream:open-url (format nil "https://lichess.org/api/games/user/~A?max=1" user))))

(defun make-pieces-css ()
  (with-output-to-string (out)
    (loop for row from 0 to 7 do
          (loop for col from 0 to 7
                for index = (board-index row col)
                do (format out "&[data-index='~D'] { translate: ~F% ~F% }~%"
                           index
                           (* col 100)
                           (* (- 7 row) 100))))))
