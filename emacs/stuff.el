(defun ss-lisp-hacks ()
  (font-lock-add-keywords 
   nil
   '(("#/\\(\\\\/\\|[^/]\\)*/[gmiy]*" . font-lock-string-face)
     ("#\\\\.[a-zA-Z0-9_-]*" . font-lock-constant-face)
     ("\\<\\(t\\|nil\\)\\>" . font-lock-constant-face))))
