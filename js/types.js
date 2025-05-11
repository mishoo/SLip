import { LispMachine } from "./machine.js";

export class LispType {
    static is(x) { return x instanceof LispType }
    print() {
        return "<" + this.constructor.type + ">";
    }
}

export class LispChar extends LispType {
    static type = "char";
    static is(x) { return x instanceof LispChar }

    // TODO: this table should be really long.
    static #NAMES_TO = Object.assign(Object.create(null), {
        " "      : "SPACE",
        "\t"     : "TAB",
        "\r"     : "RETURN",
        "\n"     : "NEWLINE",
        "\x0C"   : "PAGE",
        "\x08"   : "BACKSPACE",
        "\u2028" : "LINE_SEPARATOR",
        "\u2029" : "PARAGRAPH_SEPARATOR",
        "\xA0"   : "NO-BREAK_SPACE",
    });

    static #NAMES_FROM = ((h) => {
        for (var i in this.#NAMES_TO)
            h[this.#NAMES_TO[i]] = i;
        h.LINEFEED = "\n";
        return h;
    })(Object.create(null));

    static #OBJECTS = Object.create(null);

    static fromName(name) {
        if (name.length == 1)
            return LispChar.get(name); // hack
        name = name.toUpperCase();
        if (Object.hasOwn(LispChar.#NAMES_FROM, name))
            return LispChar.get(LispChar.#NAMES_FROM[name]);
        return null;
    }

    static fromCode(code) {
        return new LispChar(String.fromCharCode(code));
    }

    static get(char) {
        return LispChar.#OBJECTS[char] || (
            LispChar.#OBJECTS[char] = new LispChar(char)
        );
    }

    static sanitize(val) {
        return val.replace(/[\u0000\u00ad\u0600\u0604\u070f\u17b4\u17b5\u200c\u200f\u2028\u2029\u202f\u2060\u206f\ufeff\ufff0-\uffff]/g, function(s){
            var v = s.charCodeAt(0).toString(16);
            while (v.length < 4) v = "0" + v;
            return "\\u" + v;
        });
    }

    constructor(val) {
        super();
        this.value = val;
    }

    valueOf() {
        return this.value;
    }

    toString() {
        return this.value;
    }

    name() {
        return LispChar.#NAMES_TO[this.value] || this.value;
    }

    code() {
        return this.value.charCodeAt(0);
    }

    print() {
        var ch = this.value;
        return "#\\" + (LispChar.#NAMES_TO[ch] || ch);
    }

    serialize() {
        var ch = LispChar.sanitize(JSON.stringify(this.value));
        return "c(" + ch + ")";
    }
}

export class LispClosure extends LispType {
    static type = "function";
    static is(x) { return x instanceof LispClosure }
    constructor(code, name, env) {
        super();
        this.code = code;
        this.name = name || null;
        this.env = env || null;
        this.noval = false;
    }
    copy() {
        return new LispClosure(this.code, this.name, this.env);
    }
    print() {
        return "<function" + (this.name ? " " + this.name : "") + ">";
    }
}

export class LispPrimitiveError extends LispType {
    static type = "primitive-error";
    static is(x) { return x instanceof LispPrimitiveError }
    constructor(msg) {
        super();
        this.message = msg;
    }
}

export class LispStream extends LispType {
    static type = "stream";
    static is(x) { return x instanceof LispStream }
    constructor(text) {
        super();
        this.text = text || "";
        this.line = 1;
        this.col = 0;
        this.pos = 0;
    }
}

export class LispInputStream extends LispStream {
    static type = "input-stream";
    static is(x) { return x instanceof LispInputStream }
    peek() {
        return this.pos < this.text.length
            ? LispChar.get(this.text.charAt(this.pos))
            : null;
    }
    next() {
        if (this.pos < this.text.length) {
            var ch = this.text.charAt(this.pos++);
            if (ch == "\n") ++this.line, this.col = 0;
            else ++this.col;
            return LispChar.get(ch);
        }
        return null;
    }
    prev() {
        if (this.pos > 0) {
            var ch = this.text.charAt(--this.pos);
            if (this.col-- == 0) this._resetPos();
            return LispChar.get(ch);
        }
        return null;
    }
    skip_to(ch) {
        var pos = this.text.indexOf(ch, this.pos);
        if (pos <= 0) pos = this.text.length;
        var diff = pos - this.pos;
        this.pos = pos;
        return diff;
    }
    _resetPos() {
        var a = this.text.substr(0, this.pos).split(/\r?\n/);
        this.line = a.length;
        this.col = a[this.line - 1].length;
    }
}

export class LispOutputStream extends LispStream {
    static type = "output-stream";
    static is(x) { return x instanceof LispOutputStream }
    onData(){}
    put(str) {
        var lines = str.split(/\r?\n/);
        this.line += lines.length - 1;
        this.col = lines.length > 1
            ? lines[lines.length - 1].length
            : this.col + lines[0].length;
        this.pos += str.length;
        this.text += str;
        this.onData(this, str);
        return this.text;
    }
    get() {
        return this.text;
    }
}

export class LispHash extends LispType {
    static type = "simple-hash";
    static is(x) { return x instanceof LispHash }
    static fromObject(obj) {
        var hash = new LispHash;
        hash.data = new Map(Object.entries(obj));
        return hash;
    }
    constructor(parent = null) {
        super();
        this.data = new Map();
        this.parent = parent;
    }
    toObject() {
        let obj = Object.create(null);
        this.data.entries().forEach(([ key, val ]) => obj[key] = val);
        return obj;
    }
    get(key) {
        let hash = this;
        while (hash) {
            if (hash.data.has(key))
                return hash.data.get(key);
            hash = hash.parent;
        }
        return null;
    }
    set(name, val) {
        this.data.set(name, val);
        return val;
    }
    delete(name) {
        this.data.delete(name);
    }
    has(key) {
        let hash = this;
        while (hash) {
            if (hash.data.has(key))
                return hash;
            hash = hash.parent;
        }
        return null;
    }
    size() {
        return this.data.size;
    }
    serialize() {
        return "h(" + LispChar.sanitize(JSON.stringify(this.toObject())) + ")";
    }
    copy() {
        return new LispHash(this);
    }
    keys() {
        return [ ...this.data.keys() ];
    }
    values() {
        return [ ...this.data.values() ];
    }
    iterator() {
        return this.data.entries();
    }
    [Symbol.iterator]() {
        return this.data.entries();
    }
}

export class LispObject extends LispType {
    static type = "object";
    static is(x) { return x instanceof LispObject }
    print() {
        return "<object " + this.vector[0].vector[2] + ">";
    }
    constructor(size) {
        super();
        this.vector = new Array(size).fill(null);
    }
}

export class LispPackage extends LispType {
    static type = "package";
    static is(x) { return x instanceof LispPackage }
    static #PACKAGES = Object.create(null);
    static all() {
        return LispPackage.#PACKAGES;
    };
    static get(name) {
        return LispPackage.#PACKAGES[name] || (
            LispPackage.#PACKAGES[name] = new LispPackage(name)
        );
    }
    static get_existing(name) {
        return LispPackage.#PACKAGES[name] || null;
    }
    static BASE_PACK = LispPackage.get("%");
    constructor(name) {
        super();
        this.name = name + "";
        this.symbols = new LispHash();
        this.exports = [];
        this.uses = [];
    }
    toString() { return this.name }
    print() { return "<package " + this.name + ">" }
    serialize(cache) {
        if (cache) {
            let id = cache.get(this);
            if (id != null) return `p(${id})`;
            cache.set(this, cache.size());
        }
        return "p(" + JSON.stringify(this.name) + ")";
    }
    intern(name) {
        let sym = this.symbols.get(name);
        if (!sym) {
            sym = this.symbols.set(name, new LispSymbol(name, this));
            if (this === LispPackage.BASE_PACK) {
                this.exports.push(sym);
            }
        }
        return sym;
    }
    unintern(name) {
        this.symbols.delete(name);
    }
    export(sym) {
        sym = this.find(LispSymbol.symname(sym));
        if (sym && this.exports.indexOf(sym) < 0) {
            this.exports.push(sym);
            return true;
        }
        return null;
    }
    import(sym) {
        this.symbols.set(LispSymbol.symname(sym), sym);
    }
    shadow(name) {
        var sym = this.symbols.get(name);
        if (sym && sym.pak === this) return sym;
        sym = new LispSymbol(name, this);
        this.symbols.set(name, sym);
        return sym;
    }
    find_exported(name) {
        var a = this.exports;
        for (var i = a.length; --i >= 0;) {
            var sym = a[i];
            if (LispSymbol.symname(sym) == name)
                return sym;
        }
        return null;
    }
    find_internal(name) {
        return this.symbols.get(name);
    }
    find_accessible(name) {
        var sym = this.find_exported(name);
        if (!sym) {
            var a = this.uses;
            for (var i = a.length; --i >= 0;) {
                sym = a[i].find_accessible(name);
                if (sym) break;
            }
        }
        return sym;
    }
    all_accessible() {
        var ret = this.symbols.values();
        var a = this.uses;
        for (var i = a.length; --i >= 0;) {
            ret.push.apply(ret, a[i].exports);
        }
        return ret;
    }
    all_exported() {
        return [ ...this.exports ];
    }
    all_interned() {
        return this.symbols.values();
    }
    find(name) {
        var sym = this.symbols.get(name);
        if (sym) return sym;
        for (var i = this.uses.length; --i >= 0;) {
            sym = this.uses[i].find_exported(name);
            if (sym) return sym;
        }
        return null;
    }
    find_or_intern(name) {
        return this.find(name) || this.intern(name);
    }
    external(sym) {
        return this.exports.indexOf(sym) >= 0;
    }
    alias(nickname) {
        LispPackage.#PACKAGES[nickname] = this;
    }
    use(pak) {
        if (this.uses.indexOf(pak) < 0) {
            this.uses.push(pak);
            return pak;
        }
        return null;
    }
}

export class LispSymbol extends LispType {
    static type = "symbol";
    static #SYMBOLS = Object.create(null);
    static symname(sym) {
        return sym == null ? "NIL" : sym === true ? "T" : sym.name;
    }
    static is(x) {
        return x === true || x == null || x instanceof LispSymbol;
    }
    static get(name, pak) {
        if (pak == null) pak = LispPackage.BASE_PACK;
        var ret = pak ? pak.intern(name) : (LispSymbol.#SYMBOLS[name] || (
            LispSymbol.#SYMBOLS[name] = new LispSymbol(name)
        ));
        return ret;
    }
    constructor(name, pak) {
        super();
        if (name) {
            this.name = name + "";
            this.pak = pak || null;
            this.value = null;
            this.vlist = Object.create(null);
            this.primitive = null;
            this.function = null;
        }
    }
    toString() { return this.print() }
    serialize(cache) {
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
    }
    setv(key, val) {
        return this.vlist[key] = val;
    }
    getv(key) {
        return this.vlist[key] ?? null;
    }
    macro() {
        return this.getv("macro");
    }
    special() {
        return this.getv("special");
    }
    global() {
        return this.getv("global");
    }
    print() {
        if (this.pak?.name == "KEYWORD")
            return ":" + this.name;
        return this.name;
    }
}

(function(BASE_PACK){
    var pak = BASE_PACK.intern("*PACKAGE*");
    pak.value = BASE_PACK;
    pak.setv("special", true);
    BASE_PACK.PACKAGE_VAR = pak;
})(LispPackage.get("%"));

export class LispMutex extends LispType {
    static type = "mutex";
    static is(x) { return x instanceof LispMutex }
    constructor(name) {
        super();
        this.name = name || null;
        this.waiters = [];
        this.locked = null;
    }
    acquire(process) {
        if (!this.locked) {
            this.locked = process;
            return process;
        } else {
            this.waiters.push(process);
            process.m.status = "locked";
            return null;
        }
    }
    release() {
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
}

// many ideas from http://norstrulde.org/ilge10/ — Kudos Eric Bergstrome!

class Message {
    constructor(sender, target, signal, args) {
        this.sender = sender;
        this.target = target;
        this.signal = signal;
        this.args = args;
    }
}

let PID = 0;
let QUEUE = [];

const run = () => {
    var start_time = Date.now();
    while (Date.now() - start_time < 100) {
        if (QUEUE.length == 0) break;
        var p = QUEUE.shift();
        if (p instanceof LispProcess) {
            p.run(200);
        }
        else if (p instanceof Message) {
            p.target.handle(p);
        }
        else throw new Error("Unknown object in scheduler queue");
    }
    if (QUEUE.length > 0) setTimeout(run, 0);
};

const start = () => {
    setTimeout(run, 0);
};

export class LispProcess extends LispType {
    static type = "process";
    static is(x) { return x instanceof LispProcess }

    constructor(parent_machine, closure) {
        super();
        var pid = this.pid = ++PID;
        var m = this.m = new LispMachine(parent_machine);
        this.receivers = null;
        this.mailbox = [];
        this.timeouts = Object.create(null);
        this.noint = null;
        m.process = this;
        m.set_closure(closure);
        this.resume();
    }

    print() {
        return "<process " + this.pid + ">";
    }

    resume(at_start) {
        this.receivers = null;
        this.m.status = "running";
        if (at_start) QUEUE.unshift(this);
        else QUEUE.push(this);
        start();
    }

    run(quota) {
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
    }

    sendmsg(target, signal, args) {
        QUEUE.push(new Message(this, target, signal, args));
        start();
        return target;
    }

    sendmsg(target, signal, args) {
        QUEUE.push(new Message(null, target, signal, args));
        start();
        return target;
    }

    receive(receivers) {
        if (this.m.status != "running")
            throw new Error("Process not running");
        this.receivers = receivers;
        this.m.status = "waiting";
        return false;
    }

    handle(msg) {
        this.mailbox.push(msg);
        if (this.m.status == "waiting")
            this.checkmail();
    }

    checkmail() {
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
    }

    has_timeouts() {
        for (var i in this.timeouts) if (Object.hasOwn(this.timeouts, i)) return true;
        return null;
    }

    set_timeout(timeout, closure) {
        closure = closure.copy();
        closure.noval = true;
        var tm = setTimeout(() => {
            delete this.timeouts[tm];
            this.m._callnext(closure, null);
            this.resume(true);
        }, timeout);
        this.timeouts[tm] = true;
        return tm;
    }

    clear_timeout(tm) {
        delete this.timeouts[tm];
        clearTimeout(tm);
        return null;
    }
}
