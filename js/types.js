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
    var NAMES_TO = Object.assign(Object.create(null), {
        " "      : "SPACE",
        "\t"     : "TAB",
        "\r"     : "RETURN",
        "\n"     : "NEWLINE",
        "\x0C"   : "PAGE",
        "\x08"   : "BACKSPACE",
        "\u2028" : "LINE_SEPARATOR",
        "\u2029" : "PARAGRAPH_SEPARATOR",
        "\xA0"   : "NO-BREAK_SPACE"
    });
    var NAMES_FROM = (function(h){
        for (var i in NAMES_TO)
            h[NAMES_TO[i]] = i;
        h.LINEFEED = "\n";
        return h;
    })(Object.create(null));
    var OBJECTS = {};
    D.fromName = function(name) {
        if (name.length == 1)
            return D.get(name); // hack
        name = name.toUpperCase();
        if (Object.hasOwn(NAMES_FROM, name))
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
        return "#\\" + (Object.hasOwn(NAMES_TO, ch) ? NAMES_TO[ch] : ch);
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
        this.noval = false;
    };
    P.copy = function() {
        return new D(this.code, this.name, this.env);
    };
    P.print = function() {
        return "<function" + (this.name ? " " + this.name : "") + ">";
    };
});

var LispPrimitiveError = DEFTYPE("primitive-error", function(D, P){
    P.INIT = function(msg) {
        this.message = msg;
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

var LispInputStream = DEFTYPE("input-stream", function(D, P){
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
    P.prev = function() {
        if (this.pos > 0) {
            var ch = this.text.charAt(--this.pos);
            if (this.col-- == 0) this._resetPos();
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
    P._resetPos = function() {
        var a = this.text.substr(0, this.pos).split(/\r?\n/);
        this.line = a.length;
        this.col = a[this.line - 1].length;
    };
}, LispStream);

var LispOutputStream = DEFTYPE("output-stream", function(D, P){
    P.onData = function(){};
    P.put = function(str) {
        var lines = str.split(/\r?\n/);
        this.line += lines.length - 1;
        this.col = lines.length > 1
            ? lines[lines.length - 1].length
            : this.col + lines[0].length;
        this.pos += str.length;
        this.text += str;
        this.onData(this, str);
        return this.text;
    };
    P.get = function() {
        return this.text;
    };
}, LispStream);

var LispHash = DEFTYPE("simple-hash", function(D, P){
    D.fromObject = function(obj) {
        var hash = new LispHash;
        hash.data = new Map(Object.entries(obj));
        return hash;
    };
    P.toObject = function() {
        let obj = Object.create(null);
        this.data.entries().forEach(([ key, val ]) => obj[key] = val);
        return obj;
    };
    P.INIT = function(parent = null) {
        this.data = new Map();
        this.parent = parent;
    };
    P.get = function(key) {
        let hash = this;
        while (hash) {
            if (hash.data.has(key))
                return hash.data.get(key);
            hash = hash.parent;
        }
        return null;
    };
    P.set = function(name, val) {
        this.data.set(name, val);
        return val;
    };
    P.delete = function(name) {
        this.data.delete(name);
    };
    P.has = function(key) {
        let hash = this;
        while (hash) {
            if (hash.data.has(key))
                return hash;
            hash = hash.parent;
        }
        return null;
    };
    P.size = function() {
        return this.data.size;
    };
    P.serialize = function() {
        return "h(" + LispChar.sanitize(JSON.stringify(this.toObject())) + ")";
    };
    P.copy = function() {
        return new LispHash(this);
    };
    P.keys = function() {
        return [...this.data.keys()];
    };
    P.values = function() {
        return [...this.data.values()];
    };
    P.iterator = P[Symbol.iterator] = function() {
        return this.data.entries();
    };
});

var LispObject = DEFTYPE("object", function(D, P){
    P.print = function() {
        return "<object " + this.vector[0].vector[2] + ">";
    };
    P.INIT = function(size) {
        var a = this.vector = new Array(size);
        while (--size >= 0) a[size] = null;
    };
});

var LispPackage = DEFTYPE("package", function(D, P){
    var PACKAGES = Object.create(null);
    P.INIT = function(name) {
        this.name = name + "";
        this.symbols = new LispHash();
        this.exports = [];
        this.uses = [];
    };
    P.toString = function() { return this.name };
    P.print = function() { return "<package " + this.name + ">" };
    P.serialize = function(cache) {
        if (cache) {
            let id = cache.get(this);
            if (id != null) return `p(${id})`;
            cache.set(this, cache.size());
        }
        return "p(" + JSON.stringify(this.name) + ")";
    };
    P.intern = function(name) {
        return this.symbols.get(name) ||
            this.symbols.set(name, new LispSymbol(name, this));
    };
    P.unintern = function(name) {
        this.symbols.delete(name);
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
    P.find_exported = function(name) {
        var a = this.exports;
        for (var i = a.length; --i >= 0;) {
            var sym = a[i];
            if (LispSymbol.symname(sym) == name)
                return sym;
        }
        return null;
    };
    P.find_internal = function(name) {
        return this.symbols.get(name);
    };
    P.find_accessible = function(name) {
        var sym = this.find_exported(name);
        if (!sym) {
            var a = this.uses;
            for (var i = a.length; --i >= 0;) {
                sym = a[i].find_accessible(name);
                if (sym) break;
            }
        }
        return sym;
    };
    P.all_accessible = function(external) {
        var ret = external
            ? this.exports.slice()
            : this.symbols.values();
        var a = this.uses;
        for (var i = a.length; --i >= 0;) {
            ret.push.apply(ret, a[i].all_accessible(true));
        }
        return ret;
    };
    P.all_interned = function() {
        return this.symbols.values();
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
        return PACKAGES[name] || (
            PACKAGES[name] = new D(name)
        );
    };
    D.get_existing = function(name) {
        return PACKAGES[name] || null;
    };
    D.all = function() {
        return PACKAGES;
    };
});

var LispSymbol = DEFTYPE("symbol", function(D, P){
    var SYMBOLS = Object.create(null);
    var BASE_PACK = LispPackage.get("%");
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
            this.plist = Object.create(null);
            this.vlist = Object.create(null);
        }
    };
    P.toString = function() { return this.name };
    P.serialize = function(cache) {
        if (cache) {
            let id = cache.get(this);
            if (id != null) return `s(${id})`;
        }
        let code = "s(" +
            JSON.stringify(this.name) +
            (this.pak ? ("," + this.pak.serialize(cache)) : "") +
            ")";
        // note: pak.serialize will cache package as well; order is
        // important, we have to put ourselves in the cache *after*
        // the package.
        if (cache) cache.set(this, cache.size());
        return code;
    };
    P.set = function(key, val) {
        return this.plist[key] = val;
    };
    P.get = function(key) {
        return key in this.plist ? this.plist[key] : null;
    };
    P.setv = function(key, val) {
        return this.vlist[key] = val;
    };
    P.getv = function(key) {
        return key in this.vlist ? this.vlist[key] : null;
    };
    P.macro = function() {
        return this.getv("macro");
    };
    P.special = function() {
        return this.getv("special");
    };
    P.global = function() {
        return this.getv("global");
    };
    P.primitive = function() {
        return this.getv("primitive");
    };
    P.func = function() {
        return this.getv("function");
    };
    D.get = function(name, pak) {
        if (pak == null) pak = BASE_PACK;
        var ret = pak ? pak.intern(name) : (SYMBOLS[name] || (
            SYMBOLS[name] = new D(name)
        ));
        return ret;
    };
    P.print = function() {
        if (this.pak?.name == "KEYWORD")
            return ":" + this.name;
        return this.name;
    };
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

    class Message {
        constructor(sender, target, signal, args) {
            this.sender = sender;
            this.target = target;
            this.signal = signal;
            this.args = args;
        }
    }

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

    var PID = 0;
    var QUEUE = [];
    P.INIT = function(parent_machine, closure) {
        var pid = this.pid = ++PID;
        var m = this.m = new LispMachine(parent_machine);
        this.receivers = null;
        this.mailbox = [];
        this.timeouts = {};
        this.noint = null;
        m.process = this;
        m.set_closure(closure);
        this.resume();
    };

    P.print = function() {
        return "<process " + this.pid + ">";
    };

    P.resume = function(at_start) {
        this.receivers = null;
        this.m.status = "running";
        if (at_start) QUEUE.unshift(this);
        else QUEUE.push(this);
        start();
    };

    P.run = function(quota) {
        var m = this.m, err;
        do {
            if (m.status == "running") err = m.run(quota);
            else break;
            if (err) break;
        } while (this.noint);
        if (err) {
            console.error("Error in PID: ", this.pid);
            console.dir(err);
            console.dir(err.stack);
            console.log(this);
        } else {
            switch (m.status) {
              case "running":
                QUEUE.push(this);
                break;
              case "waiting":
                this.checkmail();
                break;
            }
        }
    };

    P.sendmsg = function(target, signal, args) {
        QUEUE.push(new Message(this, target, signal, args));
        start();
        return target;
    };

    D.sendmsg = function(target, signal, args) {
        QUEUE.push(new Message(null, target, signal, args));
        start();
        return target;
    };

    P.receive = function(receivers) {
        if (this.m.status != "running")
            throw new Error("Process not running");
        this.receivers = receivers;
        this.m.status = "waiting";
        return false;
    };

    P.handle = function(msg) {
        this.mailbox.push(msg);
        if (this.m.status == "waiting")
            this.checkmail();
    };

    P.checkmail = function() {
        if (this.mailbox.length > 0) {
            var msg = this.mailbox.shift();
            var f = this.receivers.get(msg.signal);
            if (f) {
                this.m._callnext(f, msg.args);
                this.resume();
            } else {
                console.warn("No receiver for message ", msg, " in process ", this.pid);
            }
        }
    };

    P.has_timeouts = function() {
        for (var i in this.timeouts) if (Object.hasOwn(this.timeouts, i)) return true;
        return null;
    };

    P.set_timeout = function(timeout, closure) {
        var self = this;
        closure = closure.copy();
        closure.noval = true;
        var tm = setTimeout(function(){
            delete self.timeouts[tm];
            self.m._callnext(closure, null);
            self.resume(true);
        }, timeout);
        self.timeouts[tm] = true;
        return tm;
    };

    P.clear_timeout = function(tm) {
        delete this.timeouts[tm];
        clearTimeout(tm);
        return null;
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
        while (Date.now() - start_time < 50) {
            if (QUEUE.length == 0) break;
            var p = QUEUE.shift();
            if (D.is(p)) {
                p.run(200);
            }
            else if (p instanceof Message) {
                p.target.handle(p);
            }
            else throw new Error("Unknown object in scheduler queue");
        }
        TIMER = QUEUE.length > 0 ? setTimeout(run, 0) : null;
    };
});
