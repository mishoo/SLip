(load "examples/queen.lisp")
(load "lib/dom.lisp")

(defpackage :chess-board
  (:use :sl :queen :ffi))

(in-package :chess-board)

(defun display-game (pgn)
  (let* ((pgn (parse-pgn pgn :ext-moves t))
         (content (dom:from-html (make-layout pgn)))
         (dlg (dom:make-dialog 700 500
                               :content content
                               :class-name "pgn-viewer"))
         (running t))
    (labels
        ((on-close (target event)
           (setf running nil))
         (on-reverse (target event)
           (dom:toggle-class (dom:query "._board" dlg) "reverse")))
      (let ((receivers (make-hash :close #'on-close
                                  :reverse #'on-reverse)))
        (make-thread
         (lambda ()
           (dom:on-event dlg "close" :signal :close)
           (dom:on-event dlg "click" :selector "._reverse" :signal :reverse)
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
                            "*")))
         (board (board-html (game-board game))))
    (format nil "
<div class='layout'>
  <div class='cont-board'>~A</div>
  <div class='cont-ctrl'>
    <label><input type='checkbox' class='_reverse' /> Reverse board</label>
  </div>
  <div class='cont-list'>Ze list</div>
  <div class='cont-title _drag-dialog'>~A</div>
</div>
"
            board
            title)))

(defconstant +svg-chessboard+
  "<svg fill='currentColor' viewBox='0 0 512 512' xmlns='http://www.w3.org/2000/svg'><path d='M255.9.2h-64v64h64zM0 64.17v64h64v-64zM128 .2H64v64h64zm64 255.9v64h64v-64zM0 192.12v64h64v-64zM383.85.2h-64v64h64zm128 0h-64v64h64zM128 256.1H64v64h64zM511.8 448v-64h-64v64zm0-128v-64h-64v64zM383.85 512h64v-64h-64zm128-319.88v-64h-64v64zM128 512h64v-64h-64zM0 512h64v-64H0zm255.9 0h64v-64h-64zM0 320.07v64h64v-64zm319.88-191.92v-64h-64v64zm-64 128h64v-64h-64zm-64 128v64h64v-64zm128-64h64v-64h-64zm0-127.95h64v-64h-64zm0 191.93v64h64v-64zM64 384.05v64h64v-64zm128-255.9v-64h-64v64zm191.92 255.9h64v-64h-64zm-128-191.93v-64h-64v64zm128-127.95v64h64v-64zm-128 255.9v64h64v-64zm-64-127.95H128v64h64zm191.92 64h64v-64h-64zM128 128.15H64v64h64zm0 191.92v64h64v-64z'/></svg>")

(defun board-html (board)
  (with-output-to-string (output)
    (write-string "<div class='board _board'>" output)
    (write-string +svg-chessboard+ output)
    (board-foreach
     board
     (lambda (piece row col index)
       (declare (ignore index))
       (format output "<div class='piece' ~
                            data-row='~D' data-col='~D' ~
                            data-piece='~C'></div>"
               row col
               (piece-char piece))))
    (write-string "</div>" output)))

(defparameter g (make-game))
(reset-from-fen g +fen-start+)

(defun test ()
  (display-game (sl-stream:open-url "https://lichess.org/api/games/user/vlbz?max=1")))

((lambda-js () "
  let link = document.querySelector('#chess-board-css');
  if (!link) {
    link = document.createElement('link');
    link.id = 'chess-board-css';
    link.rel = 'stylesheet';
    link.type = 'text/css';
    document.querySelector('head').appendChild(link);
  }
  link.href = './examples/chess-board.css?kc=' + Date.now();
"))