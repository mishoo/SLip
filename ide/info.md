# SLip - A Lisp system in the browser

    © Mihai Bazon 2012-2025
    https://lisperator.net/slip/

## Quick demo

Type `(load "examples/clock.lisp")` to load the old crazy clock demo. The
canvas that shows up isn't fixed, you can drag it around with the mouse. You
can then type `C-c M-p` and select the `TURTLE` package, and then, for
example, `clock` and `M-.` on it to jump to definition. Poke at the code and
press `C-M-x` to reload the toplevel expression. Your changes should apply
immediately.

## Emacsen

(In fact, I should say that if you're not familiar with Emacs you probably
won't enjoy this anyway; I'm sorry about that.)

Note about some common Emacs key bindings: both Chrome and Firefox (the only
browsers I test this with) disallow pages to catch `Ctrl-w`, `Ctrl-t` and
`Ctrl-n`. In Emacs, we use `C-w` for “kill region”, but browser makers decided
it should kill the tab instead. Given that this action is malign (and I've
done it accidentally a zillion times, while typing in this editor), I've
installed an annoying popup that warns you before closing the tab/window.

I found a workaround for Firefox [1], but it's not very easy to setup. On the
good side, once done it's been somewhat stable in the face of upgrades (it
only broke once).

Chrome permits the keybindings if you are in full-screen mode, which you can
easily enter with `M-x request_full_screen` (completion available).

If you still can't use `C-w` for kill region, there are alternative keys -
`C-k` or `C-Delete` while the selection is active. As for “yank word in
isearch” you can use `C-S-s` (Ctrl-Shift-s). This enters isearch mode if it
wasn't already on.

## Basic info

In the REPL you can type an expression and press `Enter` to evaluate it right
away. Use `C-v` to paste from the OS clipboard (it might ask for permission
once). Emacs' frame management keys are available (`C-x 2`, `C-x 3`, `C-x 0`
and `C-x 1`). To quickly move to another frame, use M-ARROWS (for example
`M-ArrowRight` would focus the frame to the right of the cursor); also, the
venerable `C-x o` is there (`M-x other_frame`; alias `M-o`), or just click to
focus.

Here's a few keybindings you can use in Lisp buffers (there are *many* key
bindings; to get a crude list of all of them, press `C-h m`):

- `C-c s r` -- at any time this switches to the REPL buffer.

- `C-c s s` -- at any time this switches to the output buffer. This is where
  text sent to `*standard-output*`, `*error-output*` and `*trace-output*`
  goes. Output goes to the REPL buffer as well, just above the prompt, but
  it's buffered until newline.

- `M-.` -- go to definition of symbol at point (after which you can use `M-,`
  to go back). Note that some symbols lack xref info (e.g. primitive
  functions). But those defined in Lisp should have it, for example try `loop`.

- `M-C-x` (or `C-c C-c`) -- evaluate the current toplevel expression.

- `C-c C-r` -- evaluate the selection (region).

- `C-c Enter` -- macroexpand-1 the current expression (the cursor must be on
  the opening paren). The output goes to the REPL. We don't have a pretty
  printer for now, so it'll be on one line; the way I use to inspect it is to
  paste it in the Real Slime REPL...

- `C-c M-m` -- macroexpand-all the current expression.

- `C-c Delete` -- clear the REPL buffer.

- `S-Tab` -- complete the current symbol.

You can load and save files (see the section about the “filesystem” below):

- `C-x C-f` -- load a file.

- `C-x C-s` -- save the current buffer.

In addition to the above, the following key bindings are available in the REPL:

- `C-c M-p` -- change REPL package. Note that `%` is the bootstrap package,
  where primitives are defined. It's unfortunate that it shows up first, due
  to alphabetic ordering. I'll fix this someday. Do not go there. 😅

- `C-c M-o` -- clear the REPL buffer.

- `Enter` -- evaluate the expression at prompt and display the result; but if
  the expression is incomplete (e.g. missing a closing paren), this will just
  newline and indent. If the cursor is somewhere above the prompt, this will
  just copy the current expression to prompt, like in SLIME.

- `C-Enter` -- always newline and indent.

- `M-p` or `C-ArrowUp` -- previous history item.

- `M-n` or `C-ArrowDown` -- next history item.

- to search history, type what you want to search at the prompt, and then
  `M-p` or `M-n`.

- `M-C-Delete` -- delete input, return to prompt.

- `Tab` -- complete current symbol.

## The filesystem

When you save a file (`C-x C-s`), it goes to your browser's localStorage [2].
This persists if you leave the page. `C-x C-f` will offer completion for the
files that you have saved. You can still open a file directly from server, if
you wish, but you'll have to type the path manually. Try `C-x C-f` and type
`lisp/loop.lisp`, for example. You can modify the file and press `C-x C-s` to
save it. Then if you refresh the page and press `C-x C-f` you'll find it there.

> Note, another way to open loop.lisp would be to type `loop` at the REPL, and
> press `M-.` to go to definition. (yes, I know you'll try that in this
> markdown buffer as well, but it doesn't work here)

You can do any changes and use `C-M-x` to evaluate toplevel expressions, then
test your changes in the REPL. And if you wish to recompile the whole file,
save it first and then press `C-c C-k`. An object file (`.fasl`) will be saved
to your localStorage, and next time you load the page, the VM will load your
own FASL instead of the one from the server. This way you can customize and
have your own Lisp (and in fact, this is pretty much how I've developed it,
except that instead of localStorage, on my own laptop the IDE is saving files
directly to disk via HTTP PUT requests; obviously, I can't enable this on my
server).

One thing to be aware of: the FASL-s from your local store will be ignored if
the build tag (git commit) on the server changed since your last visit. You
should get a notice in the REPL if that's the case. This is just a safety
precaution, in case there were incompatible changes in the VM. Lisp files will
remain, so you can just recompile them.

## Lisp stuff

There's some info on the project page [3] about what we support. It's not
exactly Common Lisp, but that's what I'm dreaming.

### Local store primitives

- `(%:%ls-dump-store)` -- if you have anything interesting in your local store
  that you wish to show me, that's the easiest way. It will open a window with
  a JSON dump of your localStorage files saved from this page. Feel free to
  send it over (censor it first, if necessary).

- `(%:%ls-purge-fasls)` -- discard object files (.fasl) from your local storage.

- `(%:%ls-clear-store)` -- purge all SLip files saved into your localStorage.
  This will not affect your REPL history, as it's saved on another key (check
  local storage in your browser's developer console).

- `(%:%ls-delete-path path)` -- delete the given `path` from your
  localStorage. You can pass a “directory” — it and everything below will be
  purged, no questions asked.

- `(%:%ls-get-file-contents filename)` and `(%:%ls-set-file-contents filename
  content)` — get or set the content of the given file.

[1] https://lisperator.net/blog/dogfooding-ymacs/#fix-ctrl-w
[2] https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage
[3] https://lisperator.net/slip/
