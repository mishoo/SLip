//----------------------------
// This is a mess that works.
//----------------------------

import { Ymacs, Ymacs_Keymap, Ymacs_Buffer,
         Ymacs_Interactive, Ymacs_Tokenizer,
         Ymacs_Lang_Lisp,
         Ymacs_Exception } from "./ymacs/ymacs.mjs";

function MACHINE() {
    return window.MACHINE;
}

(function(){

    var WEBDAV_ROOT = "./";

    function webdav_url(filename) {
        return WEBDAV_ROOT + filename + "?killCache=" + Date.now();
    };

    function automode(buf) {
        if (/\.lisp$/i.test(buf.name)) return buf.cmd("sl_mode");
        if (/\.js$/i.test(buf.name)) return buf.cmd("javascript_mode");
        if (/\.css$/i.test(buf.name)) return buf.cmd("css_mode");
        if (/\.(html?|xml)$/i.test(buf.name)) return buf.cmd("xml_mode");
    };

    Ymacs_Keymap.get("emacs").defineKeys({
        "C-\\"    : "switch_to_buffer",
        "C-z"     : "switch_to_buffer",
        "C-x C-s" : "webdav_save_file_buffer",
        "C-x C-f" : "webdav_load_file_buffer",
    });

    function webdav_load(filename, cont) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", webdav_url(filename), true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                if (xhr.status == 200) {
                    cont(false, xhr.responseText);
                } else {
                    cont(true);
                }
            }
        };
        xhr.send(null);
    };

    function webdav_save(filename, content, cont) {
        var xhr = new XMLHttpRequest();
        xhr.open("PUT", webdav_url(filename), true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                if (xhr.status >= 200 && xhr.status < 300) {
                    cont(false);
                } else {
                    cont(true);
                }
            }
        };
        xhr.send(content);
    };

    Ymacs_Buffer.newCommands({
        webdav_load: function(filename, cont) {
            return webdav_load(filename, cont);
        },
        webdav_save: function(filename, content, cont) {
            return webdav_save(filename, content, cont);
        },
        webdav_url: function(filename) {
            return webdav_url(filename);
        },
        webdav_load_file_buffer: Ymacs_Interactive("sFile name: ", function(filename){
            var self = this;
            var ymacs = self.ymacs;
            var buf = ymacs.getBuffer(filename);
            if (buf) {
                ymacs.switchToBuffer(buf);
            } else {
                buf = ymacs.createBuffer({ name: filename });
                webdav_load(filename, function(error, code){
                    if (error) {
                        self.signalInfo("New file: " + filename);
                        code = "";
                    }
                    buf.setq("webdav_filename", filename);
                    buf.setCode(code);
                    ymacs.switchToBuffer(buf);
                    automode(buf);
                    var cont = buf.getq("webdav_after_load");
                    if (cont) cont(buf);
                });
            }
            return buf;
        }),
        webdav_save_file_buffer: Ymacs_Interactive(function(){
            var self = this;
            function doit(filename) {
                webdav_save(filename, self.getCode(), function(error){
                    if (error) {
                        self.signalError("Failed to save " + filename);
                    } else {
                        self.dirty(false);
                        self.cmd("rename_buffer", filename);
                        self.setq("webdav_filename", filename);
                        self.signalInfo("File " + filename + " saved.", false, 1000);
                        return;
                    }
                });
            };
            var filename = self.getq("webdav_filename");
            if (filename == null) {
                self.cmd("minibuffer_prompt", "Write file: ");
                self.cmd("minibuffer_read_string", null, function(filename){
                    doit(filename);
                });
            } else doit(filename);
        })
    });

})();

class Repl_Tokenizer extends Ymacs_Lang_Lisp {
    readCustom() {
        let s = this._stream, m;
        if (s.col == 0 && (m = s.lookingAt(/^[^<\s,'`]+?>/))) {
            this.t("sl-prompt builtin", m[0].length);
            this.newArg();
            return true;
        }
        if (s.col == 0 && (m = s.lookingAt(/^!(WARN|ERROR):/))) {
            this.t("sl-error", m[0].length);
            this.newArg();
            return true;
        }
        return super.readCustom();
    }
}

Ymacs_Tokenizer.define("sl_lisp_repl", (stream, tok, options = {}) =>
    new Repl_Tokenizer({ stream, tok, ...options }));

let Ymacs_Keymap_SL = Ymacs_Keymap.define("slip", {
    "C-c C-c && C-M-x"                      : "sl_eval_sexp",
    "C-c C-k"                               : "sl_eval_buffer",
    "C-c C-r"                               : "sl_eval_region",
    "C-c M-o && C-c Delete && C-c C-Delete" : "sl_clear_output",
    "C-c Enter"                             : "sl_macroexpand_1",
    "C-c M-m"                               : "sl_macroexpand_all",
    "M-q"                                   : "sl_indent_sexp",
    "S-M-q"                                 : "fill_paragraph",
    "S-Tab"                                 : "sl_complete_symbol",
    //"Tab"                                   : "sl_complete_symbol_indent",
    "M-."                                   : "sl_xref_symbol",
    "M-,"                                   : "sl_xref_back_history",
});

let PENDING_COMMANDS = {};
let REQ_ID = 0;

function handle_lisp_response(req_id, what, value) {
    var v = PENDING_COMMANDS[req_id];
    delete PENDING_COMMANDS[req_id];
    if (v) {
        v.handler(value, what, req_id);
    }
};

function handle_lisp_notification(what, value) {
    var buf = get_repl_buffer();
    switch (what.toUpperCase()) {
      case "MESSAGE":
        buf.cmd("sl_lisp_notify_message", value);
        break;
      case "ERROR":
        buf.cmd("sl_lisp_notify_message", "!ERROR: " + value);
        //THE_EDITOR.getActiveBuffer().signalError(value);
        break;
      case "WARNING":
        buf.cmd("sl_lisp_notify_message", "!WARN: " + value);
        //THE_EDITOR.getActiveBuffer().signalError(value);
        break;
    }
};

class Ymacs_SL extends Ymacs {
    constructor(...args) {
        super(...args);
        this.addEventListener({
            onLispResponse: handle_lisp_response,
            onLispNotify: handle_lisp_notification
        });
    }
    sl_log(thing) {
        if (typeof thing != "string")
            thing = MACHINE().dump(thing);
        sl_log(thing);
        return null;
    }
    run_lisp(what, ...args) {
        args.unshift(++REQ_ID);
        if (args.at(-1) instanceof Function) {
            var handler = args.pop();
            PENDING_COMMANDS[REQ_ID] = {
                time: Date.now(),
                args: args,
                handler: handler
            };
        }
        var thread = MACHINE().eval_string("YMACS", "*THREAD*");
        thread.sendmsg(thread, what, window.LispCons.fromArray(args));
    }
}

function flash_region(buffer, begin, end) {
    var s = buffer._positionToRowCol(begin);
    var e = buffer._positionToRowCol(end);
    buffer.setOverlay("flash-code", {
        line1: s.row, col1: s.col,
        line2: e.row, col2: e.col
    });
    setTimeout(function(){
        buffer.deleteOverlay("flash-code");
    }, 333);
};

function find_toplevel_sexp(buffer, blink, noerror) {
    var p = buffer.cmd("lisp_make_quick_parser");
    p.parse(buffer.point());
    var exp = p.cont_exp();
    while (exp && exp.parent && exp.parent.parent) {
        exp = exp.parent;
    }
    if (exp.type == "caret" && exp.parent && !exp.parent.parent) {
        exp = exp.parent.value[exp.index + 1] || exp.parent.value[exp.index - 1];
    }
    if (exp == null || !exp.parent) {
        if (!noerror)
            throw new Ymacs_Exception("Can't figure out toplevel expression");
        return null;
    }
    if (exp.partial) throw new Ymacs_Exception("Incomplete toplevel expression");
    if (blink) {
        flash_region(buffer, exp.start, exp.end);
    }
    return [ exp.start, exp.end ];
};

function find_in_package(buffer, start) {
    return buffer.cmd("save_excursion", function(){
        this.cmd("goto_char", start);
        if (this.cmd("search_backward", "(in-package")) {
            var a = this.point();
            var txt = this.cmd("buffer_substring", 0, a);
            if (/(^|\n)\s*$/.test(txt)) {
                if (this.cmd("search_forward", ")")) {
                    var b = this.point();
                    return this.cmd("buffer_substring", a, b);
                }
            }
        }
    });
};

function find_package(buffer, start) {
    if (buffer.getq("sl_repl_history")) {
        // for the REPL buffer, return null so that it
        // uses the current REPL package.
        return null;
    }
    if (start == null) start = buffer.point();
    var pak = find_in_package(buffer, start);
    if (pak) try {
        var exp = MACHINE().read(null, pak)[0];
        pak = exp.cdr.car;
        return MACHINE().eval(pak);
    } catch(ex) {};
    return null;
};

function sl_log(txt, { newline = true } = {}) {
    var output = get_repl_buffer();
    output.preventUpdates();
    output.cmd("end_of_buffer");
    output._disableUndo(() => {
        var pos = output._positionToRowCol(output.point());
        if (pos.col > 0)
            output.cmd("newline");
        output.cmd("insert", txt);
        if (newline) {
            output.cmd("newline");
        }
    });
    output.forAllFrames(function(frame){
        frame.ensureCaretVisible();
        frame.redrawModelineWithTimer();
    });
    output.resumeUpdates();
};

Ymacs_Buffer.newCommands({
    sl_get_repl_buffer: get_repl_buffer,
    sl_log: sl_log,
    sl_get_symbol: function(pos){
        var p = this.cmd("lisp_make_quick_parser");
        p.parse(pos);
        var expr = p.cont_exp();
        if (expr && expr.type == "caret")
            expr = expr.parent.value[expr.index + 1];
        if (expr && (expr.type == "symbol" || expr.type == "function"))
            return expr;
    },
    sl_repl_eval_or_copy_sexp: Ymacs_Interactive("d", function(point){
        var m = this.getq("sl_repl_marker").getPosition();
        if (point >= m) this.cmd("sl_repl_eval");
        else if (point < m) {
            var exp = find_toplevel_sexp(this, false, true);
            if (exp && point >= exp[0] && point <= exp[1]) {
                set_repl_input(this, this.cmd("buffer_substring", exp[0], exp[1]));
                this.cmd("goto_char", m + exp[1] - exp[0]);
            }
            else
                this.cmd("newline_and_indent");
        }
    }),
    sl_repl_beginning_of_input: Ymacs_Interactive("d", function(point){
        var m = this.getq("sl_repl_marker").getPosition();
        if (this._positionToRowCol(point).row == this._positionToRowCol(m).row)
            this.cmd("goto_char", m);
        else this.cmd("beginning_of_indentation_or_line");
    }),
    sl_xref_symbol: Ymacs_Interactive("d", function(point){
        var sym = this.cmd("sl_get_symbol", point);
        if (sym) {
            var debug = MACHINE().eval_string(
                find_package(this),
                "(%:%get-symbol-prop '" + sym.value + " \"XREF\")"
            );
            if (debug) debug = [...debug].filter(function(stuff){
                switch (stuff[0]) {
                  case "DEFMACRO":
                  case "DEFUN":
                  case "DEFPARAMETER":
                  case "DEFVAR":
                  case "DEFGLOBAL":
                  case "DEFCONSTANT":
                    return true;
                }
            });
            if (!debug || debug.length == 0)
                throw new Ymacs_Exception("No xref information for symbol " + sym.value);
            var history = this.getq("sl_xref_history");
            history.push({ buffer: this.name, point: point });
            var stuff = debug.at(-1);
            var filename = stuff[1];
            var position = stuff[2];
            var ymacs = this.ymacs;
            var buf = ymacs.getBuffer(filename);
            function cont(buf) {
                buf.cmd("goto_char", position);
                buf.cmd("ensure_caret_visible");
            };
            if (buf) {
                ymacs.switchToBuffer(buf);
                cont(buf);
            } else {
                buf = this.cmd("webdav_load_file_buffer", filename);
                buf.setq("webdav_after_load", cont);
            }
        } else {
            throw new Ymacs_Exception("No symbol at point");
        }
    }),
    sl_xref_back_history: Ymacs_Interactive(function(){
        var history = this.getq("sl_xref_history");
        var pos = history.pop();
        if (!pos) {
            this.signalError(`No more history`, 2000);
            return;
        }
        var ymacs = this.ymacs;
        var buf = ymacs.getBuffer(pos.buffer);
        if (buf) {
            if (buf !== this) ymacs.switchToBuffer(buf);
            if (+buf.point() != +pos.point) buf.cmd("goto_char", pos.point);
        } else {
            this.signalError(`Buffer ${pos.buffer} has been deleted`);
        }
    }),
    sl_clear_output: Ymacs_Interactive(function(){
        var buf = get_repl_buffer();
        buf.setCode("");
        buf.cmd("sl_repl_prompt");
    }),
    sl_macroexpand_1: Ymacs_Interactive("d", function(point){
        var code = this._bufferSubstring(point);
        try {
            var tmp = MACHINE().read(find_package(this), code);
        } catch(ex) {
            throw new Ymacs_Exception(`Couldn't read Lisp expression starting at point`);
        }
        let expr = this.cmd("buffer_substring", point, point + tmp[1]);
        eval_lisp(this, "(sl::print-object-to-string (%::macroexpand-1 '" + expr + "))", find_package(this));
    }),
    sl_macroexpand_all: Ymacs_Interactive("d", function(point){
        var code = this._bufferSubstring(point);
        try {
            var tmp = MACHINE().read(find_package(this), code);
        } catch(ex) {
            throw new Ymacs_Exception(`Couldn't read Lisp expression starting at point`);
        }
        let expr = this.cmd("buffer_substring", point, point + tmp[1]);
        eval_lisp(this, "(sl::print-object-to-string (sl::macroexpand-all '" + expr + "))", find_package(this));
    }),
    sl_eval_buffer: Ymacs_Interactive(function(){
        compile_lisp(this, this.getCode());
    }),
    sl_eval_sexp: Ymacs_Interactive(function(){
        var points = find_toplevel_sexp(this, true);
        var expr = this.cmd("buffer_substring", points[0], points[1]);
        setTimeout(() => {
            eval_lisp(this, expr, find_package(this, points[0]));
        }, 1);
    }),
    sl_indent_sexp: Ymacs_Interactive(function(){
        var points = find_toplevel_sexp(this, false, true);
        if (points) {
            this.cmd("indent_region", points[0], points[1]);
        }
    }),
    sl_eval_region: Ymacs_Interactive("r", function(begin, end){
        eval_lisp(this, this.cmd("buffer_substring", begin, end), find_package(this, begin));
    }),
    sl_repl_prompt: function(){
        if (this._positionToRowCol(this.point()).col > 0)
            this.cmd("newline");
        var m = this.getq("sl_repl_marker");
        if (m) m.destroy();
        var pak = MACHINE().eval_string(null, "%::*PACKAGE*");
        var name = pak.name;
        this._disableUndo(() => {
            this.cmd("insert", name + "> ");
            this.cmd("end_of_line");
        });
        m = this.createMarker(null, true);
        this.setq("sl_repl_marker", m);
        this.forAllFrames(function(frame){ // this stinks. :-\
            frame.ensureCaretVisible();
            frame.redrawModelineWithTimer();
            frame.redrawCaret(true);
        });
        this.tokenizer.start();
    },
    sl_repl_eval: Ymacs_Interactive(function() {
        var self = this;
        var repl_start = self.getq("sl_repl_marker").getPosition();
        var parser = self.cmd("lisp_make_quick_parser", repl_start);
        var expr = parser.read();
        if (!expr) {
            self.signalInfo("No input", false, 2000);
            return true;
        }
        if (expr.partial)
            return self.cmd("newline_and_indent");
        var code = self.cmd("buffer_substring", expr.start, expr.end);
        var h = self.getq("sl_repl_history");
        if (h[0] != code) {
            h.unshift(code);
            self.ymacs.ls_setFileContents("~/.sl-lisp-history", JSON.stringify(h.slice(0, 1000)));
        }
        self.ymacs.run_lisp("READ-EVAL-PRINT", code, function(ret){
            if (typeof ret != "string") ret = MACHINE().dump(ret);
            sl_log(ret);
            self.cmd("sl_repl_prompt");
        });
    }),
    sl_repl_history_back: Ymacs_Interactive(function(){
        var a = HISTORY_COMPLETIONS;
        if (!/^sl_repl_history/.test(this.previousCommand)) {
            a = HISTORY_COMPLETIONS = get_relevant_history(this);
        } else if (this.previousCommand == "sl_repl_history_forward") {
            a.push(a.shift());
        }
        if (a.length > 0) {
            var txt = a.shift();
            a.push(txt);
            set_repl_input(this, txt);
        }
    }),
    sl_repl_history_forward: Ymacs_Interactive(function(){
        var a = HISTORY_COMPLETIONS;
        if (!/^sl_repl_history/.test(this.previousCommand)) {
            a = HISTORY_COMPLETIONS = get_relevant_history(this);
        } else if (this.previousCommand == "sl_repl_history_back") {
            a.unshift(a.pop());
        }
        if (a.length > 0) {
            var txt = a.pop();
            a.unshift(txt);
            set_repl_input(this, txt);
        }
    }),
    sl_repl_kill_input: Ymacs_Interactive(function(){
        var m = this.getq("sl_repl_marker");
        this._deleteText(m, this.getCodeSize());
        this.cmd("goto_char", m);
    }),
    sl_complete_symbol: Ymacs_Interactive("d", function(point){
        var ctx;
        switch (this.previousCommand) {
          case "sl_complete_symbol":
          case "undo":
          case "sl_repl_complete_symbol":
          case "sl_complete_symbol_indent":
            ctx = this.getq("sl_complete_symbol_context");
        }
        if (!ctx) {
            var start = this.cmd("save_excursion", function(){
                if (!this.cmd("search_backward_regexp", /[^-a-z0-9_*&^%$@!=+\/.<>?:]/ig))
                    this.cmd("beginning_of_buffer");
                else
                    this.cmd("forward_char");
                return this.point();
            });
            var text = this.cmd("buffer_substring", start, point).trim();
            var list = this.cmd("sl_get_symbol_completions", text);
            ctx = {
                start: start,
                list: list
            };
            this.setq("sl_complete_symbol_context", ctx);
        }
        if (ctx.list.length > 0) {
            var sym = ctx.list.shift();
            ctx.list.push(sym);
            this._replaceText(ctx.start, point, sym.toLowerCase());
            return true;
        }
    }),
    sl_complete_symbol_indent: Ymacs_Interactive("d", function(point){
        this.cmd("indent_line");
        this.cmd("sl_complete_symbol", point);
    }),
    sl_repl_complete_symbol: Ymacs_Interactive("d", function(point){
        if (!this.cmd("sl_complete_symbol", point))
            this.cmd("indent_line");
    }),
    sl_get_symbol_completions: function(query) {
        return window.LispCons.toArray(MACHINE().eval_string(
            find_package(this),
            "(ymacs::exec-list-symbol-completions " + JSON.stringify(query) + ")"
        ));
    },
    sl_compile_file: Ymacs_Interactive(function() {
        var self = this;
        var filename = self.getq("webdav_filename");
        if (filename == null) {
            return self.signalError("Buffer not saved");
        }
        self.ymacs.run_lisp("COMPILE-FILE", self.cmd("webdav_url", filename), function(fasl_content){
            var filename_fasl = filename.replace(/(\.lisp)?$/, ".fasl");
            self.cmd("webdav_save", filename_fasl, fasl_content, function(error){
                if (error) {
                    self.signalError("Error saving " + filename_fasl);
                } else {
                    self.signalInfo(filename_fasl + " saved.", false, 1000);
                }
            });
        });
    }),
    sl_recompile_everything: Ymacs_Interactive(function(){
        window.location.replace((window.location+"").replace(/\?.*$/, "") + "?recompile");
    }),
    sl_lisp_notify_message: function(msg) {
        // msg should be string, but if not...
        if (typeof msg != "string")
            msg = MACHINE().dump(msg);
        var buf = get_repl_buffer();
        var m = buf.getq("sl_repl_marker");
        var pos = buf._positionToRowCol(m.getPosition());
        msg = msg.replace(/[\s\n\t]*$/, "\n");
        buf._disableUndo(() => {
            buf.cmd("save_excursion", function(){
                buf.cmd("goto_line", pos.row + 1);
                buf.cmd("beginning_of_line");
                buf.cmd("insert", msg);
            });
        });
        buf.tokenizer.start();
    },
    sl_repl_set_package: Ymacs_Interactive(function(){
        this.ymacs.run_lisp("LIST-PACKAGES", pak_names => {
            this.cmd("minibuffer_prompt", "Package: ");
            this.cmd("minibuffer_read_string", pak_names, (name) => {
                this.ymacs.run_lisp("SET-PACKAGE", name, () => {
                    this.cmd("sl_repl_prompt");
                });
            });
        });
    }),
});

var HISTORY_COMPLETIONS;

function uniq(a) {
    if (a.length <= 1) return a;
    for (var ret = [ a[0] ], i = 1; i < a.length; ++i) {
        var el = a[i];
        if (el != ret.at(-1)) ret.push(el);
    }
    return ret;
};

function get_relevant_history(buf) {
    var h = buf.getq("sl_repl_history").slice();
    var m = buf.getq("sl_repl_marker");
    var input = buf.cmd("buffer_substring", m, buf.point());
    if (input) {
        h = h.filter(function(el){
            return el.toLowerCase().indexOf(input) >= 0;
        }).sort(function(a, b){
            return a.indexOf(input) - b.indexOf(input);
        });
        h = uniq(h);
    }
    return h;
};

function get_repl_input(buf) {
    var m = buf.getq("sl_repl_marker");
    return buf.cmd("buffer_substring", m).trim();
};

function set_repl_input(buf, text) {
    var m = buf.getq("sl_repl_marker");
    buf._replaceText(m, buf.getCodeSize(), text);
};

function eval_lisp(buf, expr, pack) {
    if (!pack) pack = null;
    var start = new Date().getTime();
    buf.ymacs.run_lisp("EVAL-STRING", pack, expr, function(val){
        var end = new Date().getTime();
        if (typeof val != "string") val = MACHINE().dump(val);
        sl_log(val + "\n;; <== in " + ((end - start) / 1000).toFixed(3) + "s");
        get_repl_buffer().cmd("sl_repl_prompt");
    });
};

function compile_lisp(buf, code) {
    var start = new Date().getTime();
    buf.ymacs.run_lisp("COMPILE-STRING", code, buf.name, function(val){
        var end = new Date().getTime();
        // sl_log(val);
        window.last_fasl = val;
        sl_log(`;; ${buf.name} compiled in ` + ((end - start) / 1000).toFixed(3) + "s");
        get_repl_buffer().cmd("sl_repl_prompt");
    });
};

Ymacs_Keymap.get("emacs").defineKeys({
    "C-c s r": Ymacs_Interactive(function(){
        this.cmd("switch_to_buffer", get_repl_buffer());
    }),
    "C-c s s": Ymacs_Interactive(function(){
        this.cmd("switch_to_buffer", "*sl-output*");
    })
});

let Ymacs_Keymap_SL_REPL = Ymacs_Keymap.define("slip_repl", {
    "Enter" : "sl_repl_eval_or_copy_sexp",
    "C-Enter" : "newline_and_indent",
    "C-c Enter" : "sl_macroexpand_1",
    "M-p && C-ArrowUp" : "sl_repl_history_back",
    "M-n && C-ArrowDown" : "sl_repl_history_forward",
    "C-Delete" : "sl_repl_kill_input",
    "Tab" : "sl_repl_complete_symbol",
    "C-c M-p": "sl_repl_set_package",
    "Home && C-a" : "sl_repl_beginning_of_input",
});

Ymacs_Buffer.setGlobal("sl_xref_history", []);

Ymacs_Buffer.newMode("sl_mode", function(){
    this.cmd("lisp_mode", true);
    this.pushKeymap(Ymacs_Keymap_SL);
    var save_modeline = this.getq("modeline_custom_handler");
    this.setq("modeline_custom_handler", function(){
        this.preventUpdates();
        var ret = [ "SL" ];
        var pak = find_in_package(this, this.point());
        if (pak) {
            pak = pak.replace(/^\(in-/, "(%::find-").replace(/\)/, ")"); // that's a pervert hack
            try {
                pak = MACHINE().eval_string(null, pak);
            } catch(ex) {
                pak = null;
            }
        } else {
            pak = MACHINE().eval_string(null, "%::*PACKAGE*");
        }
        if (pak) pak = pak.name;
        else pak = "<span style='color:red'>(package not defined)</span>";
        ret.push(pak);
        this.resumeUpdates();
        return ret.join(" ");
    });
    return function() {
        this.popKeymap(Ymacs_Keymap_SL);
        this.cmd("lisp_mode", false);
        this.setq("modeline_custom_handler", save_modeline);
    };
});

Ymacs_Buffer.newMode("sl_repl_mode", function(){
    this.cmd("sl_mode");
    this.setTokenizer(new Ymacs_Tokenizer({ type: "sl_lisp_repl", buffer: this }));
    this.pushKeymap(Ymacs_Keymap_SL_REPL);
    this.setq("modeline_custom_handler", null);
    var history = this.ymacs.ls_getFileContents("~/.sl-lisp-history", true) || "[]";
    this.setq("sl_repl_history", JSON.parse(history));
    return function() {
        this.popKeymap(Ymacs_Keymap_SL_REPL);
        this.cmd("sl_mode", false);
    };
});

var THE_EDITOR;

function get_repl_buffer() {
    var ed = THE_EDITOR;
    var out = ed.getBuffer("*sl-repl*");
    if (!out) {
        var frame = ed.getActiveFrame(), buf = ed.getActiveBuffer();
        out = ed.createBuffer({ name: "*sl-repl*" });
        out.setCode(";; Hacks and glory await!\n");
        out.cmd("end_of_buffer");
        out.cmd("sl_repl_mode");
        buf.cmd("switch_to_buffer", "*sl-repl*");
        ed.setActiveFrame(frame);
    }
    return out;
};

function load(url, cont) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function(){
        if (this.readyState == 4 && this.status == 200)
            cont(this.responseText);
    };
    xhr.open("GET", url + "?kc=" + new Date().getTime());
    xhr.send();
};

export function make_desktop(load_files = []) {
    window.addEventListener("beforeunload", ev => {
        ev.preventDefault();
        return ev.returnValue = true;
    });
    var ymacs = THE_EDITOR = window.YMACS = new Ymacs_SL({ ls_keyName: ".slip" });
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
        ymacs.setColorTheme([ "ef-elea-dark" ]);
    } else {
        ymacs.setColorTheme([ "ef-cyprus" ]);
    }
    ymacs.addClass("Ymacs-hl-line");
    ymacs.getActiveBuffer().cmd("sl_mode");
    document.body.appendChild(ymacs.getElement());

    load("./ide/scratch.lisp", function(code){
        ymacs.getBuffer("*scratch*").setCode(code);
    });

    load_files.forEach(name => {
        let buf = get_repl_buffer();
        buf.cmd("split_frame_horizontally");
        buf.cmd("webdav_load_file_buffer", name);
    });

    // hook on *standard-output*, *error-output* and *trace-output*
    [ "*STANDARD-OUTPUT*",
      "*ERROR-OUTPUT*",
      "*TRACE-OUTPUT*"
    ].forEach(function(sym){
        var stream = MACHINE().eval_string("SL", sym);
        var buffer_for_repl = "";
        stream.onData = function(stream, str) {
            var ed = THE_EDITOR;
            var out = ed.getBuffer("*sl-output*");
            if (!out) {
                out = ed.createBuffer({ name: "*sl-output*" });
            }
            out._disableUndo(() => {
                out.preventUpdates();
                out.cmd("end_of_buffer");
                out.cmd("insert", str);
                out.forAllFrames(function(frame){
                    frame.ensureCaretVisible();
                    frame.redrawModelineWithTimer();
                });
                out.resumeUpdates();
            });
            var m = /^(.*?)\n(.*)$/s.exec(str);
            if (m) {
                sl_log(buffer_for_repl + m[1]);
                buffer_for_repl = m[2];
            } else {
                buffer_for_repl += str;
            }
        };
    });

    setTimeout(function(){
        ymacs.focus();
        get_repl_buffer().cmd("sl_repl_prompt");
    }, 10);
};
