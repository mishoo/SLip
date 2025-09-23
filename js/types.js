import { LispMachine,
         STATUS_RUNNING,
         STATUS_WAITING,
         STATUS_LOCKED,
         STATUS_HALTED,
         LispRet,
       } from "./machine.js";
import { LispCons } from "./list.js";
import { LispPrimitiveError } from "./error.js";

export class LispChar {
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
        return false;
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
        this.value = val;
    }

    valueOf() {
        return this.value;
    }

    name() {
        return LispChar.#NAMES_TO[this.value] || this.value;
    }

    code() {
        return this.value.charCodeAt(0);
    }

    toString() {
        var ch = this.value;
        return "#\\" + (LispChar.#NAMES_TO[ch] || ch);
    }

    serialize() {
        var ch = LispChar.sanitize(JSON.stringify(this.value));
        return "c(" + ch + ")";
    }
}

export class LispClosure {
    static type = "function";
    static is(x) { return x instanceof LispClosure }
    constructor(code, name, env) {
        this.code = code;
        this.name = name || false;
        this.env = env || false;
    }
    copy() {
        return new LispClosure(this.code, this.name, this.env);
    }
    toString() {
        return "#<FUNCTION" + (this.name ? " " + this.name : "") + ">";
    }
}

export class LispStream {
    static type = "stream";
    static is(x) { return x instanceof LispStream }
    constructor(text) {
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
            : false;
    }
    next() {
        if (this.pos < this.text.length) {
            var ch = this.text.charAt(this.pos++);
            if (ch == "\n") ++this.line, this.col = 0;
            else ++this.col;
            return LispChar.get(ch);
        }
        return false;
    }
    prev() {
        if (this.pos > 0) {
            var ch = this.text.charAt(--this.pos);
            if (this.col-- == 0) this._resetPos();
            return LispChar.get(ch);
        }
        return false;
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

export class LispHash {
    static type = "simple-hash";
    static is(x) { return x instanceof LispHash }
    static fromObject(obj) {
        return new LispHash(Object.entries(obj));
    }
    constructor(init = null, weak = false) {
        this.weak = weak;
        this.data = weak ? new WeakMap(init) : new Map(init);
    }
    toObject() {
        if (this.weak) throw new LispPrimitiveError("Cannot serialize weak map");
        let obj = Object.create(null);
        this.data.entries().forEach(([ key, val ]) => obj[key] = val);
        return obj;
    }
    get(key) {
        return this.data.get(key);
    }
    set(name, val) {
        this.data.set(name, val);
        return val;
    }
    delete(name) {
        return this.data.delete(name);
    }
    has(key) {
        return this.data.has(key);
    }
    size() {
        return this.weak ? 0 : this.data.size;
    }
    clear() {
        if (!this.weak) this.data.clear();
    }
    serialize() {
        return "h(" + LispChar.sanitize(JSON.stringify(this.toObject())) + ")";
    }
    copy() {
        return this.weak ? false : new LispHash(this.data);
    }
    keys() {
        return this.weak ? false : [ ...this.data.keys() ];
    }
    values() {
        return this.weak ? false : [ ...this.data.values() ];
    }
    iterator() {
        return this.weak ? false : this.data.entries();
    }
    [Symbol.iterator]() {
        return this.weak ? false : this.data.entries();
    }
    toString() {
        return this.weak ? `#<WEAK-HASH[${this.size()}]>` : `#<HASH[${this.size()}]>`;
    }
}

export class LispObject {
    static type = "object";
    static is(x) { return x instanceof LispObject }
    toString() {
        return "#<OBJECT " + this.vector[0].vector[2] + ">";
    }
    constructor(size) {
        this.vector = new Array(size).fill(false);
    }
}

export class LispStdInstance {
    static type = "std-instance";
    static is(x) { return x instanceof LispStdInstance }
    toString() {
        return "#<STD-INSTANCE>";
    }
    constructor(klass, slots) {
        this.klass = klass;
        this.slots = slots;
    }
}

export class LispPackage {
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
        return LispPackage.#PACKAGES[name] || false;
    }
    static BASE_PACK = LispPackage.get("%");
    constructor(name) {
        this.name = name + "";
        this.symbols = new LispHash();
        this.exports = new Map();
        this.uses = [];
    }
    toString() { return "#<PACKAGE " + this.name + ">" }
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
                this.exports.set(name, sym);
            }
        }
        return sym;
    }
    unintern(name) {
        this.symbols.delete(name);
        this.exports.delete(name);
    }
    export(sym) {
        let name = LispSymbol.symname(sym);
        sym = this.find(name);
        if (sym && !this.exports.has(name)) {
            this.exports.set(name, sym);
            return true;
        }
        return false;
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
        return this.exports.get(name) || false;
    }
    find_internal(name) {
        return this.symbols.get(name) || false;
    }
    all_accessible() {
        var ret = [ ...this.symbols.values() ];
        var a = this.uses;
        for (var i = a.length; --i >= 0;) {
            ret.push(...a[i].exports.values());
        }
        return [ ...new Set(ret) ];
    }
    all_exported() {
        return [ ...this.exports.values() ];
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
        return false;
    }
    find_or_intern(name) {
        return this.find(name) || this.intern(name);
    }
    alias(nickname) {
        LispPackage.#PACKAGES[nickname] = this;
    }
    use(pak) {
        if (this.uses.indexOf(pak) < 0) {
            this.uses.push(pak);
            return pak;
        }
        return false;
    }
}

export class LispSymbol {
    static type = "symbol";
    static symname(sym) {
        return sym === false ? "NIL" : sym === true ? "T" : sym.name;
    }
    static is(x) {
        return x === true || x === false || x instanceof LispSymbol;
    }
    static get(name, pak = LispPackage.BASE_PACK) {
        return pak.intern(name);
    }
    static keyword(name) {
        return this.get(name, LispPackage.get("KEYWORD"));
    }
    constructor(name, pak) {
        if (name) {
            this.name = name + "";
            this.pak = pak || false;
            this.value = undefined;
            this.vlist = Object.create(null);
            this.plist = false;
            this.primitive = false;
            this.function = false;
        }
    }
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
        return this.vlist[key] ?? false;
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
    toString() {
        if (this.pak?.name == "KEYWORD")
            return ":" + this.name;
        return this.name;
    }
}

(function(BASE_PACK){
    BASE_PACK.PACKAGE_VAR = special("*PACKAGE*", BASE_PACK);
    global("MOST-POSITIVE-FIXNUM", Number.MAX_SAFE_INTEGER);
    global("MOST-NEGATIVE-FIXNUM", Number.MIN_SAFE_INTEGER);

    function special(name, value = false) {
        let sym = BASE_PACK.intern(name);
        sym.setv("special", true);
        sym.setv("global", true);
        sym.value = value;
        return sym;
    }
    function global(name, value = false) {
        let sym = BASE_PACK.intern(name);
        sym.setv("global", true);
        sym.value = value;
        return sym;
    }
})(LispPackage.get("%"));

export class LispMutex {
    static type = "mutex";
    static is(x) { return x instanceof LispMutex }
    constructor(name) {
        this.name = name || false;
        this.waiters = [];
        this.locked = false;
    }
    acquire(process) {
        if (!this.locked) {
            this.locked = process;
            return process;
        } else {
            this.waiters.push(process);
            process.m.status = STATUS_LOCKED;
            return false;
        }
    }
    release() {
        if (!this.locked) return false;
        if (this.waiters.length > 0) {
            var process = this.waiters.shift();
            this.locked = process;
            process.resume();
            return process;
        } else {
            this.locked = false;
            return true;
        }
    }
}

export class LispQueue {
    constructor() {
        this.list = new LispCons(false, false);
        this.tail = this.list;
    }
    push(el) {
        this.tail = this.tail.cdr = new LispCons(el, false);
    }
    reenq(cell) {
        this.tail = this.tail.cdr = cell;
        cell.cdr = false;
    }
    pop() {
        let cell = this.list.cdr;
        if (!cell) return false;
        if (!(this.list.cdr = cell.cdr)) {
            this.tail = this.list;
        }
        return cell;
    }
}

// many ideas from http://norstrulde.org/ilge10/ — Kudos Eric Bergstrome!

class Message {
    constructor(target, signal, args) {
        this.target = target;
        this.signal = signal;
        this.args = args;
    }
}

let PID = 0;
let QUEUE = new LispQueue();

const run = () => {
    let count = 0, startTime = Date.now(), process, cell;
    try {
        while (true) {
            if ((++count & 511) === 0) {
                if (Date.now() - startTime > 10) {
                    break;
                }
            }
            cell = QUEUE.pop();
            if (!cell) return;
            process = cell.car;
            if (process instanceof LispProcess) {
                process.run(100);
                if (process.m.status === STATUS_RUNNING) {
                    // still running, re-enqueue; we avoid consing a new cell.
                    QUEUE.reenq(cell);
                }
            }
            else {
                console.error("Unknown object in scheduler queue", process);
            }
        }
    } catch(ex) {
        if (ex instanceof LispPrimitiveError) {
            var pe = LispSymbol.get("PRIMITIVE-ERROR", LispPackage.get("SL"));
            if (process.m.status === STATUS_RUNNING) {
                if (pe && pe.function) {
                    // RETHROW as Lisp error.
                    process.m._callnext(pe.function, LispCons.fromArray(["~A", ex.message]));
                }
                QUEUE.reenq(cell);
            }
        } else {
            // we fucked up.
            process.m.status = STATUS_HALTED;
            console.error("Error in PID: ", process.pid);
            console.log(process.m.backtrace());
            console.dir(ex);
            console.dir(ex.stack);
            console.log(process);
        }
    }
    setTimeout(run, 0);
};

const start = () => setTimeout(run, 0);

export class LispProcess {
    static type = "process";
    static is(x) { return x instanceof LispProcess }

    constructor(parent_machine, closure) {
        this.pid = ++PID;
        var m = this.m = new LispMachine(parent_machine);
        this.receivers = false;
        this.mailbox = new LispQueue();
        this.noint = false;
        m.process = this;
        m.set_closure(closure);
        this.resume();
    }

    toString() {
        return "#<PROCESS " + this.pid + ">";
    }

    resume() {
        this.m.status = STATUS_RUNNING;
        QUEUE.push(this);
        start();
    }

    run(quota) {
        do {
            this.m.run(quota);
        } while (this.noint && this.m.status === STATUS_RUNNING);
    }

    sendmsg(target, signal, args) {
        let msg = new Message(target, signal, args);
        target.handle(msg);
        return target;
    }

    receive(receivers) {
        if (this.m.status !== STATUS_RUNNING) {
            throw new Error("Process not running");
        }
        this.receivers = receivers;
        this.m.status = STATUS_WAITING;
        return false;
    }

    handle(msg) {
        this.mailbox.push(msg);
        if (this.m.status === STATUS_WAITING)
            this.checkmail();
    }

    checkmail() {
        let cell = this.mailbox.pop();
        if (cell) {
            let msg = cell.car;
            let f = this.receivers.get(msg.signal);
            if (f) {
                this.m._callnext(f, msg.args);
                this.resume();
            } else {
                console.warn("No receiver for message ", msg, " in process ", this.pid);
            }
        }
    }

    set_timeout(timeout, closure) {
        closure = closure.copy();
        var tm = setTimeout(() => {
            this.m.push(new LispRet(this.m, this.m.pc, true));
            this.m.n_args = 0;
            this.m._callnext(closure);
            this.resume();
        }, timeout);
        return tm;
    }

    clear_timeout(tm) {
        clearTimeout(tm);
        return false;
    }
}
