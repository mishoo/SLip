# SLip - A Lisp system in JavaScript

    © Mihai Bazon 2012-2025
    https://lisperator.net/slip/

## Basic info

This mimics EMACS+Slime, to some extent. Here's a few keybindings you can use
in Lisp buffers (there are a *lot* of key bindings; to get a crude list of all
of them, use `C-h m`):

- `C-v` -- paste from the OS clipboard (it might ask for permission once).

- `M-.` -- go to definition of symbol at point. Note that many symbols will
  lack xref info (e.g. primitive functions). But those defined in Lisp should
  have it, for example try `loop`.

- `M-C-x` (or `C-c C-c`) -- evaluate the current toplevel expression.

- `C-c C-r` -- evaluate the selection.

- `C-c Enter` -- macroexpand-1 the current expression (the cursor must be on
  the opening paren). The output goes to the REPL. We don't have a pretty
  printer for now, so it'll be on one line; the way I use to inspect it is to
  paste it in the SLIME repl...

- `C-c M-m` -- macroexpand-all the current expression.

- `C-c Delete` -- clear the output buffer.

- `S-Tab` -- complete the current symbol.

Generally, in any buffer you can load and save files (but see the section
about the "filesystem" below):

- `C-x C-f` -- load a file.

- `C-x C-s` -- save the current buffer.

In the REPL you can type an expression and press `Enter` to evaluate it right
there.  To quickly move to another frame, use M-ARROWS (for example
`M-ArrowDown` would move below); or just click to focus.  In addition to the
above, the following key bindings are available in the REPL:

- `C-c M-p` -- change REPL package. Note that `%` is the base package, where
  primitives are defined.

- `Enter` -- evaluate the expression at prompt and display the result; but if
  the expression is incomplete (e.g. missing a closing paren), this will just
  newline and indent.

- `C-Enter` -- always newline and indent.

- `M-p` or `C-ArrowUP` -- previous history item.

- `M-n or C-ArrowDown` -- next history item.

- to search history, type what you want to search and then `M-p` / `M-n`.

- `M-C-Delete` -- kill input, return to prompt.

- `Tab` -- complete current symbol.

## The filesystem

