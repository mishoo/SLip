(load "examples/pgn-viewer.lisp")

(in-package :pgn-viewer)

;; need a new thread here because on startup the IDE loads everything synchronously.
(make-thread
 (lambda ()
   (format t "~%~%Fetching/parsing PGN from Lichess. That might take a few seconds.~%~%~%")
   (display-game (sl-stream:open-url "https://lichess.org/api/games/user/vlbz?max=1"))))