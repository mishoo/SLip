var LispType = DEFCLASS("LispType", null, function(D, P){
        P.print = function() {
                return "<" + this.type + ">";
        };
});

function DEFTYPE(name, func, base) {
        return DEFCLASS(name.replace(/-/g, "_"), base || LispType, function(D, P){
                P.type = D.type = name;
                var ret = func ? func(D, P) : null;
                return ret;
        }, true);
};

var LispChar = DEFTYPE("char", function(D, P){
        // TODO: this table should be really long.
        var NAMES_TO = {
                " "      : "SPACE",
                "\t"     : "TAB",
                "\r"     : "RETURN",
                "\n"     : "NEWLINE",
                "\x0C"   : "PAGE",
                "\x08"   : "BACKSPACE",
                "\u2028" : "LINE_SEPARATOR",
                "\u2029" : "PARAGRAPH_SEPARATOR"
        };
        var NAMES_FROM = (function(h){
                for (var i in NAMES_TO) {
                        if (HOP(NAMES_TO, i)) {
                                h[NAMES_TO[i]] = i;
                        }
                }
                h.LINEFEED = "\n";
                return h;
        })({});
        var OBJECTS = {};
        D.fromName = function(name) {
                if (name.length == 1)
                        return D.get(name); // hack
                name = name.toUpperCase();
                if (HOP(NAMES_FROM, name))
                        return D.get(NAMES_FROM[name]);
                return null;
        };
        D.fromCode = function(code) {
                return new D(String.fromCharCode(code));
        };
        D.get = function(char) {
                return OBJECTS[char] || (
                        OBJECTS[char] = new LispChar(char)
                );
        };
        D.sanitize = function(val) {
                return val.replace(/[\u0000\u00ad\u0600\u0604\u070f\u17b4\u17b5\u200c\u200f\u2028\u2029\u202f\u2060\u206f\ufeff\ufff0-\uffff]/g, function(s){
                        var v = s.charCodeAt(0).toString(16);
                        while (v.length < 4) v = "0" + v;
                        return "\\u" + v;
                });
        };
        P.INIT = function(val){ this.value = val };
        P.valueOf = P.toString = function(){ return this.value };
        P.name = function() {
                return NAMES_TO[this.value] || this.value;
        };
        P.code = function() {
                return this.value.charCodeAt(0);
        };
        P.print = function() {
                var ch = this.value;
                return "#\\" + (HOP(NAMES_TO, ch) ? NAMES_TO[ch] : ch);
        };
        P.serialize = function() {
                var ch = D.sanitize(JSON.stringify(this.value));
                return "c(" + ch + ")";
        };
});

var LispClosure = DEFTYPE("closure", function(D, P){
        P.INIT = function(code, name, env) {
                this.code = code;
                this.name = name || null;
                this.env = env || null;
        };
        P.print = function() {
                return "<function" + (this.name ? " " + this.name : "") + ">";
        };
});

var LispCondition = DEFTYPE("condition", function(D, P){
        P.INIT = function(message, cont) {
                this.message = message;
                this.cont = cont;
        };
});

var LispStream = DEFTYPE("stream", function(D, P){
        P.INIT = function(text) {
                this.text = text || "";
                this.line = 1;
                this.col = 0;
                this.pos = 0;
        };
});

var LispInputStream = DEFTYPE("input_stream", function(D, P){
        P.peek = function() {
                return this.pos < this.text.length
                        ? LispChar.get(this.text.charAt(this.pos))
                        : null;
        };
        P.next = function() {
                if (this.pos < this.text.length) {
                        var ch = this.text.charAt(this.pos++);
                        if (ch == "\n") ++this.line, this.col = 0;
                        else ++this.col;
                        return LispChar.get(ch);
                }
                return null;
        };
        P.skip_to = function(ch) {
                var pos = this.text.indexOf(ch, this.pos);
                if (pos <= 0) pos = this.text.length;
                var diff = pos - this.pos;
                this.pos = pos;
                return diff;
        };
}, LispStream);

var LispOutputStream = DEFTYPE("output_stream", function(D, P){
        P.put = function(str) {
                var lines = str.split(/\r?\n/);
                this.line += lines.length - 1;
                this.col = lines.length > 1
                        ? lines[lines.length - 1].length
                        : this.col + lines[0].length;
                this.pos += str.length;
                return this.text += str;
        };
        P.get = function() {
                return this.text;
        };
}, LispStream);

var LispHash = DEFTYPE("namespace", function(D, P){
        P.INIT = function(parent) {
                function ctor(){};
                if (parent) {
                        ctor.prototype = parent.data;
                        this.level = parent.level + 1;
                } else {
                        this.level = 0;
                }
                this.data = new ctor;
                this.parent = parent || null;
        };
        P.get = function(name) {
                return HOP(this.data, name) ? this.data[name] : null;
        };
        P.set = function(name, val) {
                return this.data[name] = val;
        };
        P.has = function(name) {
                var p = this;
                while (p != null) {
                        if (HOP(p.data, name)) return p;
                        p = p.parent;
                }
                return null;
        };
        P.size = function() {
                var count = 0;
                for (var i in this.data) if (HOP(this.data, i)) ++count;
                return count;
        };
        P.serialize = function() {
                return "h(" + LispChar.sanitize(JSON.stringify(this.data)) + ")";
        };
        P.copy = function() {
                var copy = new LispHash(this.parent);
                for (var i in this.data) if (HOP(this.data, i)) {
                        copy.data[i] = this.data[i];
                }
                return copy;
        };
        P.extend = function() {
                return new LispHash(this);
        };
});

var LispPackage = DEFTYPE("package", function(D, P){
        var PACKAGES = {};
        P.INIT = function(name) {
                this.name = name + "";
                this.symbols = new LispHash();
                this.exports = [];
                this.uses = [];
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "p(" + this.name + ")";
        };
        P.intern = function(name) {
                return this.symbols.get(name) ||
                        this.symbols.set(name, new LispSymbol(name, this));
        };
        P.export = function(sym) {
                sym = this.find(LispSymbol.symname(sym));
                if (sym && this.exports.indexOf(sym) < 0) {
                        this.exports.push(sym);
                        return true;
                }
                return null;
        };
        P.import = function(sym) {
                this.symbols.set(LispSymbol.symname(sym), sym);
        };
        P.shadow = function(name) {
                var sym = this.symbols.get(name);
                if (sym && sym.pak === this) return sym;
                sym = new LispSymbol(name, this);
                this.symbols.set(name, sym);
                return sym;
        };
        P.find_accessible = function(name) {
                var a = this.exports;
                for (var i = a.length; --i >= 0;) {
                        var sym = a[i];
                        if (LispSymbol.symname(sym) == name)
                                return sym;
                }
                a = this.uses;
                for (var i = a.length; --i >= 0;) {
                        var sym = a[i].find_accessible(name);
                        if (sym) return sym;
                }
                return null;
        };
        P.find = function(name) {
                var sym = this.symbols.get(name);
                if (sym) return sym;
                for (var i = this.uses.length; --i >= 0;) {
                        sym = this.uses[i].find_accessible(name);
                        if (sym) return sym;
                }
                return null;
        };
        P.find_or_intern = function(name) {
                return this.find(name) || this.intern(name);
        };
        P.external = function(sym) {
                return this.exports.indexOf(sym) >= 0;
        };
        P.alias = function(nickname) {
                PACKAGES[nickname] = this;
        };
        P.use = function(pak) {
                if (this.uses.indexOf(pak) < 0) {
                        this.uses.push(pak);
                        return pak;
                }
                return null;
        };
        D.get = function(name) {
                return HOP(PACKAGES, name) ? PACKAGES[name] : (
                        PACKAGES[name] = new D(name)
                );
        };
        D.get_existing = function(name) {
                return PACKAGES[name] || null;
        };
});

var LispSymbol = DEFTYPE("symbol", function(D, P){
        var SYMBOLS = {};
        D.symname = function(sym) {
                return sym === null ? "NIL" : sym === true ? "T" : sym.name;
        };
        D.is = function(thing) {
                return thing === true || thing === null || thing instanceof D;
        };
        P.INIT = function(name, pak) {
                if (name) {
                        this.name = name + "";
                        this.pak = pak || null;
                        this.value = null;
                        this.plist = {};
                        this.vlist = {};
                }
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "s(" +
                        JSON.stringify(this.name) +
                        (this.pak ? ("," + JSON.stringify(this.pak.name)) : "") +
                        ")";
        };
        P.set = function(key, val) {
                return this.plist[key] = val;
        };
        P.get = function(key) {
                return HOP(this.plist, key) ? this.plist[key] : null;
        };
        P.setv = function(key, val) {
                return this.vlist[key] = val;
        };
        P.getv = function(key) {
                return HOP(this.vlist, key) ? this.vlist[key] : null;
        };
        P.macro = function() {
                return this.getv("macro") || null;
        };
        P.special = function() {
                return this.getv("special") || null;
        };
        P.primitive = function() {
                return this.getv("primitive") || null;
        };
        P.func = function() {
                return this.getv("function") || null;
        };
        D.get = function(name, pak) {
                if (pak == null) pak = BASE_PACK;
                var ret = pak ? pak.intern(name) : HOP(SYMBOLS, name) ? SYMBOLS[name] : (
                        SYMBOLS[name] = new D(name)
                );
                return ret;
        };
        P.dump = function() {
                if (this.pak && this.pak.name == "KEYWORD") return ":" + this.name;
                return this.name;
        };
        var BASE_PACK = LispPackage.get("%");
});

(function(BASE_PACK){
        var pak = BASE_PACK.intern("*PACKAGE*");
        pak.value = BASE_PACK;
        pak.setv("special", true);
})(LispPackage.get("%"));

var LispMutex = DEFTYPE("mutex", function(D, P){
        P.INIT = function(name) {
                this.name = name || null;
                this.waiters = [];
                this.locked = null;
        };
});

var LispProcess = DEFTYPE("process", function(D, P){

        // many ideas from http://norstrulde.org/ilge10/ — Kudos Eric Bergstrome!

        LispMutex.extend({
                acquire: function(process) {
                        if (!this.locked) {
                                this.locked = process;
                                return process;
                        } else {
                                this.waiters.push(process);
                                process.m.status = "locked";
                                return null;
                        }
                },
                release: function() {
                        if (!this.locked) return null;
                        if (this.waiters.length > 0) {
                                var process = this.waiters.shift();
                                this.locked = process;
                                process.resume();
                                return process;
                        } else {
                                this.locked = null;
                                return true;
                        }
                }
        });

        var PROCESSES = {};
        var PID = 0;
        var QUEUE = [];
        P.INIT = function(parent_machine, closure) {
                var pid = this.pid = ++PID;
                PROCESSES[pid] = this;
                var m = this.m = new LispMachine(parent_machine);
                this.handlers = null;
                m.process = this;
                m.set_closure(closure);
                this.resume();
        };

        P.resume = function() {
                this.m.status = "running";
                QUEUE.push(this);
                start();
        };

        P.run = function(quota) {
                var m = this.m, err;
                switch (m.status) {
                    case "running":
                        err = m.run(quota);
                        if (err) {
                                console.error("Error in PID: ", this.pid, err);
                        }
                        else if (m.status == "running") {
                                QUEUE.push(this);
                        }
                        else if (m.status == "finished") {
                                delete PROCESSES[this.pid];
                        }
                        break;
                    case "finished":
                        delete PROCESSES[this.pid];
                        break;
                }
        };

        P.handlemsg = function(sender, signal, datum) {
                if (this.m.status != "waiting")
                        throw new Error("Not waiting for message");
                
        };

        P.receive = function(handlers) {
                if (this.m.status != "running")
                        throw new Error("Process not running");
                this.m.status = "waiting";
                this.handlers = handlers;
                return false;
        };

        var TIMER = null;
        function start() {
                if (!TIMER) TIMER = setTimeout(run, 0);
        };
        function stop() {
                if (TIMER) {
                        clearTimeout(TIMER);
                        TIMER = null;
                }
        };
        function run() {
                var start_time = Date.now();
                while (Date.now() - start_time < 20) {
                        if (QUEUE.length == 0) break;
                        var p = QUEUE.shift();
                        if (D.is(p)) {
                                p.run(100);
                        } else throw "Not implemented";
                }
                TIMER = QUEUE.length > 0 ? setTimeout(run, 0) : null;
        };
});
