Ymacs_Keymap_Emacs().defineKeys({
        "C-\\"    : "switch_to_buffer",
        "C-x C-s" : "save_file",
        "C-x C-f" : "load_file",
});

DEFINE_SINGLETON("Ymacs_Keymap_SS", Ymacs_Keymap, function(D, P){

        var WINDOW = window.opener;
        var MACHINE = WINDOW.machine;

        function getPP(p) {
                var pp = p.context.passedParens;
                return pp instanceof Function ? pp() : pp;
        };

        function compareRowCol(p1, p2) {
                return p1.line < p2.line ? -1 : p1.line > p2.line ? 1 : p1.col - p2.col;
        };

        function flash_region(buffer, begin, end) {
                var s = buffer._positionToRowCol(begin);
                var e = buffer._positionToRowCol(end);
                buffer.setOverlay("flash-code", {
                        line1: s.row, col1: s.col,
                        line2: e.row, col2: e.col
                });
                (function(){
                        buffer.deleteOverlay("flash-code");
                }).delayed(500);
        };

        function find_toplevel_sexp(buffer, blink) {
                var rc = buffer._rowcol, parser = buffer.tokenizer.finishParsing();
                if (parser) {
                        var lc = { line: rc.row, col: rc.col };
                        var p = getPP(parser).grep("closed").mergeSort(compareRowCol).grep_first(function(p){
                                return compareRowCol(p, lc) <= 0 && compareRowCol(p.closed, lc) >= 0;
                        });
                        if (p == null) p = getPP(parser).grep("closed").mergeSort(function(a, b){
                                return compareRowCol(a.closed, b.closed);
                        }).grep_last(function(p){
                                return compareRowCol(p, lc) < 0 && compareRowCol(p.closed, lc) < 0;
                        });
                        if (p == null)
                                throw new Ymacs_Exception("Can't figure out toplevel expression");
                        var s = p, e = p.closed;
                        if (blink) {
                                buffer.setOverlay("flash-code", {
                                        line1: s.line, col1: s.col,
                                        line2: e.line, col2: e.col + 1
                                });
                                (function(){
                                        buffer.deleteOverlay("flash-code");
                                }).delayed(500);
                        }
                        return [ buffer._rowColToPosition(s.line, s.col),
                                 buffer._rowColToPosition(e.line, e.col + 1) ];
                }
        };

        function find_package(buffer, start) {
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

        function ss_log(txt) {
                var output = get_output_buffer();
                output.preventUpdates();
                output.cmd("end_of_buffer");
                var pos = output._positionToRowCol(output.point());
                if (pos.col > 0)
                        output.cmd("newline");
                output.cmd("insert", txt);
                output.cmd("newline");
                output.forAllFrames(function(frame){
                        frame.ensureCaretVisible();
                        frame.redrawModeline();
                });
                output.resumeUpdates();
        };

        DEFINE_CLASS("Ymacs_SS", Ymacs, function(D, P){
                P.ss_log = function(thing) {
                        if (typeof thing != "string")
                                thing = MACHINE.dump(thing);
                        ss_log(thing);
                        return null;
                };
        });

        Ymacs_Buffer.newCommands({
                ss_get_output_buffer: get_output_buffer,
                ss_log: ss_log,
                ss_clear_output: Ymacs_Interactive(function(){
                        var buf = get_output_buffer();
                        buf.setCode("");
                        buf.cmd("ss_repl_prompt");
                }),
                ss_macroexpand_1: Ymacs_Interactive("d", function(point){
                        var code = this._bufferSubstring(point);
                        try {
                                var tmp = MACHINE.read(code);
                        } catch(ex) {
                                throw new Ymacs_Exception("Couldn't read Lisp expression starting at point");
                        }
                        expr = this.cmd("buffer_substring", point, point + tmp[1]);
                        eval(this, "(%::macroexpand-1 '" + expr + ")");
                }),
                ss_macroexpand_all: Ymacs_Interactive("d", function(point){
                        var code = this._bufferSubstring(point);
                        try {
                                var tmp = MACHINE.read(code);
                        } catch(ex) {
                                throw new Ymacs_Exception("Couldn't read Lisp expression starting at point");
                        }
                        expr = this.cmd("buffer_substring", point, point + tmp[1]);
                        eval(this, "(ss::macroexpand-all '" + expr + ")");
                }),
                ss_eval_buffer: Ymacs_Interactive(function(){
                        eval(this, this.getCode());
                }),
                ss_eval_sexp: Ymacs_Interactive(function(){
                        var points = find_toplevel_sexp(this, true);
                        var expr = this.cmd("buffer_substring", points[0], points[1]);
                        (function(){
                                eval(this, expr, find_package(this, points[0]));
                        }).delayed(1, this);
                }),
                ss_eval_region: Ymacs_Interactive("r", function(begin, end){
                        eval(this, this.cmd("buffer_substring", begin, end), find_package(this, begin));
                }),
                ss_repl_prompt: function(){
                        if (this._positionToRowCol(this.point()).col > 0)
                                this.cmd("newline");
                        var m = this.getq("ss_repl_marker");
                        if (m) m.destroy();
                        var pak = MACHINE.eval(MACHINE.read("%::*package*")[0]);
                        var name = pak.name;
                        this.cmd("insert", name + "> ");
                        this.cmd("end_of_line");
                        m = this.createMarker(null, true);
                        this.setq("ss_repl_marker", m);
                        this.forAllFrames(function(frame){ // this stinks. :-\
                                frame.ensureCaretVisible();
                                frame.redrawModeline();
                                frame._redrawCaret(true);
                        });
                },
                ss_repl_eval: Ymacs_Interactive(function() {
                        var code = get_repl_input(this);
                        try {
                                var tmp = MACHINE.read(code);
                                var expr = tmp[0], pos = tmp[1];
                                var m = this.getq("ss_repl_marker");
                                flash_region(this, m.getPosition(), pos + m.getPosition());
                                if (expr === WINDOW.LispSymbol.get("EOF"))
                                        return this.cmd("ss_repl_prompt");
                                var code = code.substr(0, pos).trim();
                                var h = this.getq("ss_repl_history");
                                if (h[0] != code) h.unshift(code);
                        } catch(ex) {
                                // XXX:
                                return this.cmd("newline_and_indent");
                        }
                        var ret = MACHINE.eval(expr);
                        if (typeof ret != "string") ret = MACHINE.dump(ret);
                        ss_log("==> " + ret);
                        this.cmd("ss_repl_prompt");
                }),
                ss_repl_history_back: Ymacs_Interactive(function(){
                        var a = HISTORY_COMPLETIONS;
                        if (!HISTORY_COMPLETIONS || !/^ss_repl_history/.test(this.previousCommand)) {
                                a = HISTORY_COMPLETIONS = get_relevant_history(this);
                        }
                        if (a.length > 0) {
                                var txt = a.shift();
                                a.push(txt);
                                set_repl_input(this, txt);
                        }
                }),
                ss_repl_history_forward: Ymacs_Interactive(function(){
                        var a = HISTORY_COMPLETIONS;
                        if (!HISTORY_COMPLETIONS || !/^ss_repl_history/.test(this.previousCommand)) {
                                a = HISTORY_COMPLETIONS = get_relevant_history(this);
                        }
                        if (a.length > 0) {
                                var txt = a.pop();
                                a.push(txt);
                                set_repl_input(this, txt);
                        }
                }),
                ss_repl_kill_input: Ymacs_Interactive(function(){
                        var m = this.getq("ss_repl_marker");
                        this._deleteText(m, this.getCodeSize());
                        this.cmd("goto_char", m);
                })
        });

        var HISTORY_COMPLETIONS;

        function get_relevant_history(buf) {
                var h = buf.getq("ss_repl_history");
                var m = buf.getq("ss_repl_marker");
                var input = buf.cmd("buffer_substring", m, buf.point());
                if (input) {
                        h = h.grep(function(el){
                                return el.toLowerCase().indexOf(input) >= 0;
                        }).mergeSort(function(a, b){
                                return a.indexOf(input) - b.indexOf(input);
                        });
                }
                return h;
        };

        function get_repl_input(buf) {
                var m = buf.getq("ss_repl_marker");
                return buf.cmd("buffer_substring", m).trim();
        };

        function set_repl_input(buf, text) {
                var m = buf.getq("ss_repl_marker");
                buf._replaceText(m, buf.getCodeSize(), text);
        };

        function eval(buf, expr, preamble) {
                try {
                        if (preamble)
                                expr = preamble + expr;
                        var start = new Date().getTime();
                        var val = MACHINE.eval_string(expr);
                        ss_log("==> " + MACHINE.dump(val)
                               + " <== in " + ((new Date().getTime() - start) / 1000).toFixed(3) + "s");
                        get_output_buffer().cmd("ss_repl_prompt");
                } catch(ex) {
                        ss_log("**> " + MACHINE.dump(ex));
                }
        };

        D.KEYS = {
                "C-c C-c && C-M-x"                      : "ss_eval_sexp",
                "C-c C-k"                               : "ss_eval_buffer",
                "C-c C-r"                               : "ss_eval_region",
                "C-c M-o && C-c DELETE && C-c C-DELETE" : "ss_clear_output",
                "C-c ENTER"                             : "ss_macroexpand_1",
                "C-c M-m"                               : "ss_macroexpand_all"
        };

        DEFINE_SINGLETON("Ymacs_Keymap_SS_REPL", Ymacs_Keymap, function(D, P){
                D.KEYS = {
                        "ENTER" : "ss_repl_eval",
                        "M-p && C-ARROW_UP" : "ss_repl_history_back",
                        "M-n && C-ARROW_DOWN" : "ss_repl_history_forward",
                        "C-DELETE" : "ss_repl_kill_input",
                        "HOME && C-a" : Ymacs_Interactive("d", function(point){
                                var m = this.getq("ss_repl_marker").getPosition();
                                if (point >= m) this.cmd("goto_char", m);
                                else this.cmd("beginning_of_line");
                        })
                };
        });

        Ymacs_Buffer.newMode("ss_mode", function(){
                this.cmd("lisp_mode", true);
                this.pushKeymap(Ymacs_Keymap_SS());
                var save_modeline = this.getq("modeline_custom_handler");
                this.setq("modeline_custom_handler", function(){
                        this.preventUpdates();
                        var ret = [ "SS" ];
                        var pak = find_package(this, this.point());
                        if (pak) {
                                pak = pak.replace(/^\(in-/, "(%::%find-").replace(/\)/, " t)"); // that's a pervert hack
                        } else {
                                pak = "%::*package*";
                        }
                        try {
                                pak = MACHINE.eval_string(pak);
                        } catch(ex) {
                                pak = null;
                        }
                        if (pak) pak = pak.name;
                        else pak = "<span style='color:red'>(package not defined)</span>";
                        ret.push(pak);
                        this.resumeUpdates();
                        return ret.join(" ");
                });
                return function() {
                        this.popKeymap(Ymacs_Keymap_SS());
                        this.cmd("lisp_mode", false);
                        this.setq("modeline_custom_handler", save_modeline);
                };
        });

        Ymacs_Buffer.newMode("ss_repl_mode", function(){
                this.cmd("ss_mode");
                this.pushKeymap(Ymacs_Keymap_SS_REPL());
                this.setq("modeline_custom_handler", null);
                this.setq("ss_repl_history", []);
                return function() {
                        this.popKeymap(Ymacs_Keymap_SS_REPL());
                        this.cmd("ss_mode", false);
                };
        });

});

var THE_EDITOR;

function get_output_buffer() {
        var ed = THE_EDITOR;
        var out = ed.getBuffer("*ss*");
        if (!out) {
                var frame = ed.getActiveFrame(), buf = ed.getActiveBuffer();
                out = ed.createBuffer({ name: "*ss*" });
                out.setCode(";; Take this REPL, brother, and may it serve you well.")
                out.cmd("end_of_buffer");
                out.cmd("ss_repl_mode");
                buf.cmd("split_frame_vertically", "70%");
                buf.cmd("other_frame");
                buf.cmd("switch_to_buffer", "*ss*");
                ed.setActiveFrame(frame);
                out.forAllFrames(function(frame){
                        frame.__lineNumbers = false; // :-\
                        frame.delClass("Ymacs-line-numbers");
                });
        }
        return out;
};

function make_desktop() {
        var desktop = new DlDesktop();
        desktop.fullScreen(true);

        var dlg = new DlDialog({
                title: "",
                resizable: true
        });

        var layout = new DlLayout({ parent: dlg });

        var toolbar = new DlContainer({ className: "DlToolbar" });
        var menu = new DlHbox({ parent: toolbar });

        function btn(label, action) {
                var b = new DlButton({ parent: menu, label: label });
                b.addEventListener("onClick", function(){
                        THE_EDITOR.focus();
                        action();
                });
                return b;
        };

        function buffer(){ return THE_EDITOR.getActiveBuffer() };

        btn("Eval buffer", function(){ buffer().cmd("ss_eval_buffer") });
        btn("Eval expression", function(){ buffer().cmd("ss_eval_sexp") });
        btn("Eval selection", function(){ buffer().cmd("ss_eval_region") });
        btn("Macroexpand", function(){ buffer().cmd("ss_macroexpand_1", buffer().point()) });

        menu.addSeparator("wide-separator");

        btn("Copy to system clipboard", function(){ buffer().cmd("copy_for_operating_system") });
        btn("Paste from system clipboard", function(){ buffer().cmd("yank_from_operating_system") });

        var ymacs = THE_EDITOR = window.opener.YMACS = new Ymacs_SS({ buffers: [] });
        //ymacs.setColorTheme([ "light", "standard" ]);
        ymacs.setColorTheme([ "dark", "mishoo" ]);
        ymacs.getActiveBuffer().cmd("ss_mode");

        layout.packWidget(toolbar, { pos: "top" });
        layout.packWidget(ymacs, { pos: "bottom", fill: "*" });

        dlg.setSize({ x: 640, y: 480 });
        dlg.show(true);
        dlg.maximize(true);

        load("./scratch.lisp", function(code){
                ymacs.getBuffer("*scratch*").setCode(code);
        });

        ymacs.focus();
        get_output_buffer().cmd("ss_repl_prompt");
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

make_desktop();
