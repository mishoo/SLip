(load "lib/queen.lisp")
(load "lib/dom.lisp")

(defpackage :pgn-viewer
  (:use :sl :queen))

(in-package :pgn-viewer)

(dom:load-css "./examples/pgn-viewer.css")

(defparameter *autoplay-timeout* 1000)

(defgeneric display-game (pgn))

(defmethod display-game (pgn)
  (display-game (parse-pgn pgn :ext-moves t)))

(defmethod display-game ((pgn cons))
  (let* ((dlg (dom:make-dialog 700 530
                               :content (dom:from-html (make-layout pgn))
                               :class-name "pgn-viewer"))
         (thread nil)
         (start-fen (get-header pgn "FEN" +fen-start+))
         (current-fen start-fen)
         (el-board (dom:query dlg "._board"))
         (el-pieces (dom:query dlg "._pieces"))
         (transitions 0))
    (setf (dom:style dlg "left") "40px"
          (dom:style dlg "bottom") "60px")
    (dom:focus dlg)
    (labels
        ((on-close (target event)
           (throw 'exit-thread nil))

         (on-reverse (button)
           (dom:toggle-class el-board "reverse"))

         (piece-element (index)
           (dom:query el-pieces (piece-selector index)))

         (goto-move (move fen-before fen-after)
           (without-interrupts
             (cond
               ((string= current-fen fen-before)
                (let* ((from (move-from move))
                       (to (move-to move))
                       (white (move-white? move))
                       (capture (when (move-capture? move)
                                  (move-captured-index move)))
                       (piece (piece-element from)))
                  (when (move-promote? move)
                    (let* ((*unicode* nil)
                           (promo-piece (move-promoted-piece move))
                           (promo-char (piece-char promo-piece))
                           (el (dom:from-html
                                (format nil "<div class='piece fade-out' ~
                                                  data-index='~D' data-piece='~C'></div>"
                                        to promo-char))))
                      (dom:append-to el-pieces el)
                      (dom:trigger-reflow el)
                      (dom:remove-class el "fade-out")
                      (dom:add-class piece "fade-out")))
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
                        (dom:style piece :z-index) (+ 10 (incf transitions)))))
               (t
                (morph-to-fen el-pieces fen-after)))
             (setf current-fen fen-after)))

         (on-move-click (target event)
           (highlight-clear)
           (setf (dom:checked target) t)
           (dom:scroll-into-view (dom:parent-element target))
           (goto-move (parse-integer (dom:dataset target :move))
                      (dom:dataset target :fen-before)
                      (dom:dataset target :fen-after)))

         (on-animation-end (target event)
           (without-interrupts
             (unless (dom:has-animations el-pieces)
               ;; (format t "RESET: ~A~%" current-fen)
               (let ((g (make-game)))
                 (reset-from-fen g current-fen)
                 (setf (dom:inner-html el-pieces)
                       (pieces-html (game-board g)))
                 (highlight-check)))))

         (highlight-clear ()
           (dom:do-query (el el-pieces ".piece.highlight")
             (dom:remove-element el)))

         (highlight-fields (indexes &optional (classes ""))
           (without-interrupts
             (dolist (idx indexes)
               (dom:append-to
                el-pieces
                (dom:from-html
                 (format nil "<div class='piece highlight ~A' data-index='~D'></div>"
                         classes idx))))))

         (highlight-check ()
           (let* ((g (reset-from-fen (make-game) current-fen)))
             (when (attacked? g)
               (highlight-fields (list (king-index g)) "check"))))

         (reset ()
           (setf current-fen start-fen)
           (highlight-clear)
           (morph-to-fen el-pieces current-fen)
           (dom:do-query (el dlg "input[name='move']")
             (setf (dom:checked el) nil))
           (setf (dom:scroll-top (dom:query dlg "._cont-list")) 0))

         (next-move (backwards)
           (let* ((move-elements (dom:query-all dlg "input[name='move']"))
                  (index (position-if #'dom:checked
                                      (if backwards
                                          (setf move-elements
                                                (nreverse move-elements))
                                          move-elements))))
             (cond
               ((and (not index) (not backwards))
                (on-move-click (car move-elements) nil)
                (cdr move-elements))
               ((not index)
                (reset)
                nil)
               ((< (incf index) (length move-elements))
                (let ((next (elt move-elements index)))
                  (on-move-click next nil)
                  (< (1+ index) (length move-elements))))
               (backwards
                (reset)
                move-elements))))

         (on-start (button)
           (reset))

         (on-end (button)
           (let ((moves (nreverse (dom:query-all dlg "input[name='move']"))))
             (on-move-click (car moves) nil)))

         (on-prev (button)
           (next-move t))

         (on-next (button)
           (next-move nil))

         (toggle-play (btn)
           (rotatef (dom:inner-text btn)
                    (dom:dataset btn :toggle))
           (dom:toggle-class btn "pressed"))

         (on-play (button)
           (when (next-move nil)
             (toggle-play button)
             (let ((timer))
               (dom:with-events (dlg
                                 ("keydown" :signal :play-keydown))
                 (labels
                     ((signal ()
                        (%:%sendmsg thread :play-next))
                      (restart-timer ()
                        (clear-timeout timer)
                        (setf timer (set-timeout *autoplay-timeout* #'signal)))
                      (next ()
                        (if (next-move nil)
                            (restart-timer)
                            'play-done))
                      (stop ()
                        (clear-timeout timer)
                        'play-done)
                      (exit (&rest _)
                        (stop)
                        (throw 'exit-thread nil))
                      (action (btn event)
                        (cond
                          ((eq btn button)
                           (dom:prevent-default event)
                           (stop))
                          (t
                           (restart-timer)
                           (on-action btn event))))
                      (keydown (target event)
                        (labels ((notf-timeout ()
                                   (ymacs:signal-info (format nil "Speed: ~,2Fs"
                                                              (/ *autoplay-timeout* 1000))
                                                      :timeout 1000 :anchor button)))
                          (without-interrupts
                            (case (dom:key event)
                              ("Space"
                               (dom:prevent-default event)
                               (dom:stop-immediate-propagation event)
                               (stop))
                              ("0"
                               (setf *autoplay-timeout* 1000)
                               (notf-timeout))
                              (("+" "=")
                               (setf *autoplay-timeout* (/ *autoplay-timeout* 1.25))
                               (notf-timeout))
                              ("-"
                               (setf *autoplay-timeout* (* *autoplay-timeout* 1.25))
                               (notf-timeout))
                              (t
                               (when (on-keydown target event)
                                 (restart-timer))))))))
                   (let ((receivers (make-hash
                                     :close          #'exit
                                     :play-keydown   #'keydown
                                     :play-next      #'next
                                     :action         #'action
                                     :animation-end  #'on-animation-end)))
                     (restart-timer)
                     (loop until (eq 'play-done (%:%receive receivers)))
                     (toggle-play button)))))))

         (active-blink (el)
           (dom:add-class el "active")
           (clear-timeout (dom:dataset el :timer))
           (setf (dom:dataset el :timer)
                 (set-timeout 100 (lambda ()
                                    (dom:remove-class el "active")))))

         (on-keydown (target event)
           (without-interrupts
             (case (dom:key event)
               ("Escape"
                (dom:prevent-default event)
                (dom:close-dialog dlg))
               ("r"
                (dom:prevent-default event)
                (dom:close-dialog dlg)
                (display-game pgn))
               (t
                (let ((btn (dom:query dlg (format nil "button[data-key~~=\"~A\"]" (dom:key event)))))
                  (when btn
                    (on-action btn event)
                    t))))))

         (on-fen (target)
           (if (dom:clipboard-write-text current-fen)
               (ymacs:signal-info (format nil "FEN copied" current-fen)
                                  :timeout 1000 :anchor target)
               (ymacs:signal-info "Didn't work (permissions?)" :timeout 3000)))

         (on-action (el event)
           (without-interrupts
             (active-blink el)
             (dom:prevent-default event)
             (ecase (dom:dataset el :action)
               ("reverse" (on-reverse el))
               ("start" (on-start el))
               ("prev" (on-prev el))
               ("next" (on-next el))
               ("end" (on-end el))
               ("fen" (on-fen el))
               ("play" (on-play el)))))

         (coords-to-index (rel-x rel-y)
           (multiple-value-bind (board-x board-y board-width board-height)
               (dom:bounding-client-rect el-board)
             (declare (ignore board-x board-y))
             (let* ((col (floor (* 8 (/ rel-x board-width))))
                    (row (- 7 (floor (* 8 (/ rel-y board-height))))))
               (when (and (typep col '(integer 0 7))
                          (typep row '(integer 0 7)))
                 (board-index row col)))))

         (on-piece-mousedown (piece event)
           (without-interrupts
             (let* ((index (parse-integer (dom:dataset piece :index)))
                    (g (reset-from-fen (make-game) current-fen))
                    (all-moves (game-compute-moves g))
                    (moves (remove index all-moves :test-not #'eql :key #'move-from))
                    (reversed (dom:has-class el-board "reverse"))
                    (start-x (dom:client-x event))
                    (start-y (dom:client-y event))
                    (target-field nil))
               (unless moves (return-from on-piece-mousedown nil))
               (dom:prevent-default event)
               (dom:stop-immediate-propagation event)
               (dom:add-class piece "dragging")
               (dom:with-events (dom:document
                                 ("mousemove" :capture t :signal :move)
                                 ("mouseup" :capture t :signal :done))
                 (multiple-value-bind (board-x board-y board-width board-height)
                     (dom:bounding-client-rect el-board)
                   (multiple-value-bind (piece-x piece-y piece-width piece-height)
                       (dom:bounding-client-rect piece el-board)
                     (labels
                         ((on-move (target ev)
                            (without-interrupts
                              (let* ((diff-x (- (dom:client-x ev) start-x))
                                     (diff-y (- (dom:client-y ev) start-y))
                                     (drag-x (+ piece-x diff-x))
                                     (drag-y (+ piece-y diff-y)))
                                (when reversed
                                  (setf drag-x (- board-width drag-x piece-width)
                                        drag-y (- board-height drag-y piece-height)))
                                (setf (dom:style piece :translate)
                                      (format nil "~Fpx ~Fpx" drag-x drag-y))
                                (let ((index (coords-to-index
                                              (+ drag-x (/ piece-width 2))
                                              (+ drag-y (/ piece-height 2)))))
                                  (unless (eql index target-field)
                                    (highlight-clear)
                                    (cond
                                      ((member index moves :key #'move-to)
                                       (setf target-field index)
                                       (highlight-fields (list index) "target"))
                                      (t
                                       (setf target-field nil))))))))
                          (on-done (target ev)
                            (without-interrupts
                              (highlight-clear)
                              (dom:remove-class piece "dragging")
                              (setf (dom:style piece :translate) nil)
                              (when target-field
                                (let ((move (find target-field moves :key #'move-to)))
                                  (game-move g move)
                                  (setf current-fen (game-fen g))
                                  (setf (dom:dataset piece :index) target-field
                                        (dom:style piece :z-index) 10)
                                  (morph-to-fen el-pieces current-fen)))
                              'drag-done)))
                       (loop with drag-receivers = (make-hash
                                                    :move #'on-move
                                                    :done #'on-done)
                             until (eq (%:%receive drag-receivers) 'drag-done))))))))))

      (reset)

      (labels
          ((main ()
             (%:%catch-all-errors)
             (dom:with-events
               (dlg
                ("close" :signal :close)
                ("click" :selector "[data-action]" :signal :action)
                ("mousedown" :selector "[data-piece]" :signal :piece-mousedown)
                ("input" :selector "input[name='move']" :signal :move)
                ("keydown" :signal :keydown)
                ("transitionend" :signal :animation-end))
               (catch 'exit-thread
                 (loop with receivers = (make-hash
                                         :close            #'on-close
                                         :action           #'on-action
                                         :move             #'on-move-click
                                         :animation-end    #'on-animation-end
                                         :keydown          #'on-keydown
                                         :piece-mousedown  #'on-piece-mousedown)
                       do (handler-case
                              (%:%receive receivers)
                            (error (err)
                              (format *error-output* "!ERROR: ~A~%" err)))))
               (format *error-output*
                       "Thread exit ~A~%"
                       (current-thread)))))

        (setf thread (make-thread #'main))))))

(defun get-header (pgn name &optional default)
  (let* ((headers (getf pgn :headers))
         (cell (assoc (string name) headers
                      :test #'string-equal)))
    (if cell
        (cdr cell)
        default)))

(defun piece-selector (index)
  (format nil ".piece[data-index='~D']:not(.fade-out, ._morph)" index))

(defun morph-to-fen (el-pieces fen)
  (without-interrupts
    (let* ((*unicode* nil)
           (g (make-game))
           (board (game-board g)))
      (reset-from-fen g fen)

      ;; 1. pieces already in-place should stay so.
      (dom:do-query (el el-pieces ".piece[data-piece]:not(.fade-out)")
        (let ((index (parse-integer (dom:dataset el :index)))
              (piece (char-piece (svref (dom:dataset el :piece) 0))))
          (when (= piece (board-get board index))
            (board-set board index 0)
            (dom:add-class el "_morph"))))

      ;; 2. any non-zero board fields should now be resolved.
      (board-foreach
       board
       (lambda (piece row col index)
         (let* ((pchar (piece-char piece))
                (el (dom:query-all
                     el-pieces
                     (format nil ".piece[data-piece='~C']:not(.fade-out, ._morph)"
                             pchar))))
           ;; 2.1. maybe the piece we want is somewhere..
           (cond
             (el
              (setf el (elt el (random (length el))))
              (setf (dom:dataset el :index) index)
              (dom:add-class el "_morph"))
             (t
              ;; 2.2. not found, we need to invent it.
              (setf el (dom:from-html (format nil "<div class='piece _morph fade-out' ~
                                                        data-index='~D' data-piece='~C'></div>"
                                              index pchar)))
              (dom:append-to el-pieces el)
              (dom:trigger-reflow el)
              (dom:remove-class el "fade-out"))))))

      ;; 3. any unused elements (not _morph) should disappear
      (dom:do-query (el el-pieces ".piece[data-piece]:not(.fade-out)")
        (cond
          ((dom:has-class el "_morph")
           (dom:remove-class el "_morph"))
          (t
           (dom:add-class el "fade-out")))))))

(defun make-layout (pgn)
  (let* ((game (getf pgn :game))
         (title (format nil "<div><b>~A</b> [~A~A] - <b>~A</b> [~A~A] (~A)</div>"
                        (get-header pgn "White" "White")
                        (get-header pgn "WhiteElo" "?")
                        (get-header pgn "WhiteRatingDiff" "")
                        (get-header pgn "Black" "Black")
                        (get-header pgn "BlackElo" "?")
                        (get-header pgn "BlackRatingDiff" "")
                        (get-header pgn "Result" "*"))))
    (format nil "
<div class='layout'>
  <div class='cont-board'>
    <div class='board _board'>
      <div class='_pieces'></div>
    </div>
  </div>
  <div class='cont-ctrl'>
    <button data-action='reverse' data-key='ArrowUp ArrowDown' title='Reverse board'>🗘</button>
    <div style='padding-left: 20px'></div>
    <button data-action='start' data-key='Home' title='Start position'>⏮</button>
    <button data-action='prev' data-key='ArrowLeft' title='Previous move'>❮</button>
    <button data-action='play' data-key='Space' title='Play' data-toggle='⏸'>▶</button>
    <button data-action='next' data-key='ArrowRight' title='Next move'>❯</button>
    <button data-action='end' data-key='End' title='Last position'>⏭</button>
    <div style='padding-left: 20px'></div>
    <button data-action='fen' data-key='F f C c' title='Copy FEN to clipboard'>F</button>
  </div>
  <div class='cont-list _cont-list'><form>~A</form></div>
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
                 (format output "<div class='~A'>~
                   <label class='move'>~
                     <input name='move' type='radio' data-fen-before='~A' data-fen-after='~A' data-move='~A' />~
                     ~A~
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

(defun test-promo ()
  (display-game "1. e4 Nf6 2. e5 d5 3. exd6 Ne4 4. dxc7 Nxf2 5. cxd8=N Nxd1 6.
  Kxd1 e5 7. Ke2 e4 8. Kf2 e3+ 9. Kg3 e2 10. Kh4 e1=Q+"))