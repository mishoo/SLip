;;; SLip - Lisp in JavaScript
;;;
;;; © Mihai Bazon 2012
;;; http://mihai.bazon.net/blog

;; Type Lisp here and use the keybindings: (in Emacs parlance, that is,
;; C stands for CTRL, M stands for META which is usually the ALT key)
;;
;; - C-c C-k -- evaluate the whole buffer
;; - M-C-x (or C-c C-c) -- evaluate the current toplevel expression
;; - C-c C-r -- evaluate the selection
;; - C-c ENTER -- macroexpand-1 the current expression (the cursor must be on the open paren)
;; - C-c M-m -- macroexpand-all the current expression
;; - C-c DELETE -- clear the output buffer
;; - C-x C-s -- save the current buffer (in localStorage)
;; - C-x C-f -- load a file from localStorage
;; - S-TAB -- complete the current symbol
;;
;; For more keybindings take a look at
;; http://www.ymacs.org/userdocs.html

;; If you go to the REPL (the buffer below this one) you can type an
;; expression and press ENTER to evaluate it right there.  To quickly
;; move to another frame, use M-ARROWS (for example M-ARROW_DOWN would
;; move below); or just click there.  In addition to the above, the
;; following key bindings are available in the REPL:
;;
;; - ENTER -- evaluate the expression at prompt and display the result
;; - M-p or C-ARROW_UP -- previous history item
;; - M-n or C-ARROW_DOWN -- next history item
;; - C-DELETE -- kill input, return to prompt
;; - TAB -- complete current symbol

;; MUST DO:
;;
;; - lock primitives and some internal functions. we really don't want
;;   them to be redefined
;; - error handling: HANDLER-CASE and UNWIND-PROTECT for now
;; - 
