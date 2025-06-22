# Hacking SLip on your machine

(also read `ide/info.md` from the repo)

In order to work on it locally, you need to clone the repository and put it
under a local web server with WebDAV enabled, preferrably under HTTPS (unless
it's in secure context, Ymacs can't access the OS clipboard, so `C-v` won't
work).

    git clone https://github.com/mishoo/SLip.git

I'm using Nginx, and my vhost looks like this:

    server {
        listen 443 ssl;
        server_name slip.local;   # define this in /etc/hosts
        charset utf-8;
    
        ssl_certificate         /home/nginx/ssl/test.crt;
        ssl_certificate_key     /home/nginx/ssl/test.key;
        ssl_ciphers             HIGH:!aNULL:!MD5;
    
        location / {
            root /path/to/the/slip/repo/;  # path to the repo
            autoindex on;
            dav_methods PUT DELETE MKCOL COPY MOVE;
            create_full_put_path on;
            dav_access group:rw all:r;
        }
    }

With this setup, if you open https://slip.local/ in your browser you should
get a REPL identical to the one on my server (https://lisperator.net/s/slip/),
but when you save a file, Ymacs will also save it on disk via a PUT request
(that's where WebDAV comes in handy).

To open the “IDE” and automatically load something at startup — for example
the test suite — append `?load=...` to the URL. For example:
https://slip.local/?load=test/all.lisp (note it takes about 2 seconds
currently, and it'll get slower as we add more tests).

To open a file, I usually type in the REPL a symbol which I know is defined
there and press `M-.` (jump to definition). For example: `eval` and `M-.` will
open “lisp/compiler.lisp”.

To quickly test a function you can press `M-C-x` to evaluate it, then go to
the REPL to test it. Make sure the REPL is in the right package (`C-c M-p` to
change it).

To recompile a whole file you can use `C-c C-k`. This will save the FASL both
via WebDAV and to local storage (which might be inconvenient sometimes; more
below). And to recompile all files that SLip knows about (see `*core-files*`
at the bottom of compiler.lisp), you can type `M-x sl_recompile_everything`.
This is useful when you modify the compiler, or some macro that's being used
in multiple files.

## When you break something

So Ymacs saves files both to disk via WebDAV, and to browser's local storage.
The latter was useful in order to provide a meaningful demo on my website,
where I can't enable WebDAV for obvious reasons. Normally it's not an
inconvenient; on the contrary, once a file is saved from Ymacs, it can then be
found with completion with `C-x C-f`, which is nice (that's something we don't
have via WebDAV at the moment). But you need to remember that browser's local
storage takes precedence when files are loaded. So if you break anything and
decide, say, to `git reset`, remember to also delete the browser local
storage; otherwise when loading the files you'll load the broken ones again.

To clear the local storage you either evaluate `(%:%ls-clear-store)` in the
REPL, if you still got one, or open browser's developer tools and remove the
`.slip` key from the local store.
