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
    static #NAMES_TO = new Map(Object.entries({
        " ": "SPACE",
        "\t": "TAB",
        "\r": "RETURN",
        "\n": "NEWLINE",
        "\x0C": "PAGE",
        "\x08": "BACKSPACE",
        "\u2028": "LINE_SEPARATOR",
        "\u2029": "PARAGRAPH_SEPARATOR",
        "\xA0": "NO-BREAK_SPACE",
    }));

    static #NAMES_FROM = ((h) => {
        for (let [key, val] of this.#NAMES_TO)
            h.set(val, key);
        h.set("LINEFEED", "\n");
        return h;
    })(new Map);

    static #OBJECTS = new Map();

    static fromName(name) {
        if (name.length == 1) return LispChar.get(name); // hack
        let ch = LispChar.#NAMES_FROM.get(name.toUpperCase());
        return ch ? LispChar.get(ch) : false;
    }

    static fromCode(code) {
        return LispChar.get(String.fromCharCode(code));
    }

    static get(char) {
        return LispChar.#OBJECTS.get(char) || (
            LispChar.#OBJECTS.set(char, char = new LispChar(char)), char
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
        return LispChar.#NAMES_TO.get(this.value) || this.value;
    }

    code() {
        return this.value.charCodeAt(0);
    }

    toString() {
        var ch = this.value;
        return "#\\" + (LispChar.#NAMES_TO.get(ch) || ch);
    }

    toUpperCase() {
        return LispChar.get(this.value.toUpperCase());
    }

    toLowerCase() {
        return LispChar.get(this.value.toLowerCase());
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

let HASH_EQUAL_CACHE = new WeakMap();
let HASH_EQUAL_COUNT = 0;
export function hash_equal_key(x) {
    if (x === true) return "1";
    if (x === false) return "0";
    if (typeof x === "number") return "N" + x;
    if (typeof x === "string") return "S" + x;
    if (LispCons.is(x)) return "(" + hash_equal_key(x.car) + hash_equal_key(x.cdr) + ")";
    let id = HASH_EQUAL_CACHE.get(x);
    if (!id) {
        id = ++HASH_EQUAL_COUNT;
        HASH_EQUAL_CACHE.set(x, id);
    }
    return "=" + id;
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
    get(key) {
        return this.data.get(key);
    }
    set(key, val) {
        this.data.set(key, val);
        return val;
    }
    delete(key) {
        return this.data.delete(key);
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
    copy() {
        return this.weak ? false : new this.constructor(this.data);
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
        return this.iterator();
    }
    toString() {
        return this.weak ? `#<WEAK-HASH[${this.size()}]>` : `#<HASH[${this.size()}]>`;
    }
}

export class LispHashEqual extends LispHash {
    constructor() {
        super();
    }
    get(key) {
        return this.data.get(hash_equal_key(key))[1];
    }
    set(key, val) {
        this.data.set(hash_equal_key(key), [key, val]);
        return val;
    }
    delete(key) {
        return this.data.delete(hash_equal_key(key));
    }
    has(key) {
        return this.data.has(hash_equal_key(key));
    }
    keys() {
        return [ ...this.data.values().map(x => x[0]) ];
    }
    values() {
        return [ ...this.data.values().map(x => x[1]) ];
    }
    toString() {
        return `#<HASH[${this.size()}] :TEST #'EQUAL>`;
    }
    iterator() {
        let it = this.data.entries();
        return {
            next() {
                let x = it.next();
                if (x.done) return x;
                return { value: x.value[1], done: false };
            }
        };
    }
}

export function forEach(seq, func) {
    if (LispCons.is(seq)) return LispCons.forEach(seq, func);
    if (Array.isArray(seq)) return seq.forEach(func);
    if (typeof seq === "string") return [...seq].map(LispChar.get).forEach(func);
    throw new LispPrimitiveError("Unknown sequence in primitive forEach");
}

export class LispArray extends Array {
    static is(x) { return x instanceof LispArray }

    #dimensions;
    #element_type;
    #initial_element;

    constructor({
        dimensions,
        element_type = true,
        initial_element = false,
        initial_content = false,
    }) {
        super(dimensions.reduce((p, v) => p * v, 1));
        this.#dimensions = dimensions;
        this.#element_type = element_type;
        this.#initial_element = initial_element;
        if (initial_content) {
            let grok = (content, subscripts) =>
                (subscripts.length === this.#dimensions.length
                 ? this.set(subscripts, content) // XXX: this can be optimized to compute less indices.
                 : forEach(content, (content, i) => grok(content, [...subscripts, i])));
            grok(initial_content, []);
        } else {
            this.fill(initial_element);
        }
    }

    dimensions() { return this.#dimensions }
    element_type() { return this.#element_type }
    initial_element() { return this.#initial_element }
    index(subscripts) {
        let index = 0, n = subscripts.length;
        for (let i = 0; i < n; ++i) {
            let p = subscripts[i];
            if (p < 0 || p >= this.#dimensions[i]) {
                throw new LispPrimitiveError(`Array index ${p} out of bounds for dimension ${i} (should be 0..${this.#dimensions[i] - 1})`);
            }
            for (let j = i + 1; j < n; ++j) {
                p *= this.#dimensions[j];
            }
            index += p;
        }
        if (index < 0 || index >= this.length) {
            throw new LispPrimitiveError(`Computed array index ${index} out of bounds (subscripts: ${subscripts.join(",")}, dimensions: ${this.#dimensions.join(",")})`);
        }
        return index;
    }
    get(subscripts) {
        return this[this.index(subscripts)];
    }
    set(subscripts, value) {
        return this[this.index(subscripts)] = value;
    }
}

export class LispStruct {
    static type = "struct";
    static is(x) { return x instanceof LispStruct }
    constructor(struct, data) {
        this.struct = struct === false ? this : struct;
        this.data = data;
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
        this.props = new Map();
    }
    setProp(key, val) {
        return this.props.set(key, val), val;
    }
    getProp(key) {
        return this.props.get(key) ?? false;
    }
    toString() { return "#<PACKAGE " + this.name + ">" }
    intern(name, sym) {
        if (sym) {
            if (!sym.pak) sym.pak = this;
            this.symbols.set(name, sym);
            return sym;
        }
        sym = this.symbols.get(name);
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
    static PROP_GLOBAL = {};
    static PROP_SPECIAL = {};
    static PROP_MACRO = {};
    static PROP_SIDE_EFFECTS = {};
    static PROP_XREF = {};
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
            this.vlist = new Map();
            this.plist = false;
            this.primitive = false;
            this.function = false;
        }
    }
    setv(key, val) {
        return this.vlist.set(key, val), val;
    }
    getv(key) {
        return this.vlist.get(key) ?? false;
    }
    macro() {
        return this.getv(LispSymbol.PROP_MACRO);
    }
    special() {
        return this.getv(LispSymbol.PROP_SPECIAL);
    }
    global() {
        return this.getv(LispSymbol.PROP_GLOBAL);
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
    global("PI", Math.PI);

    function special(name, value = false) {
        let sym = BASE_PACK.intern(name);
        sym.setv(LispSymbol.PROP_SPECIAL, true);
        sym.setv(LispSymbol.PROP_GLOBAL, true);
        sym.value = value;
        return sym;
    }
    function global(name, value = false) {
        let sym = BASE_PACK.intern(name);
        sym.setv(LispSymbol.PROP_GLOBAL, true);
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
    push_front(el) {
        if (!LispCons.find(this.list.cdr, el)) {
            this.list.cdr = new LispCons(el, this.list.cdr);
            if (this.tail === this.list) this.tail = this.list.cdr;
        }
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

const start = () => {
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
        if (!(ex instanceof LispPrimitiveError) && process.catch_all) {
            console.dir(ex);
            ex = new LispPrimitiveError(String(ex));
        }
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
    setTimeout(start, 0);
};

export class LispProcess {
    static type = "process";
    static is(x) { return x instanceof LispProcess }
    static timer_thread = null;

    constructor(parent_machine, closure) {
        this.pid = ++PID;
        var m = this.m = new LispMachine(parent_machine);
        this.receivers = false;
        this.mailbox = new LispQueue();
        this.noint = false;
        this.catch_all = false;
        m.process = this;
        m.set_closure(closure);
        this.resume();
    }

    toString() {
        return "#<PROCESS " + this.pid + ">";
    }

    resume() {
        this.m.status = STATUS_RUNNING;
        QUEUE.push_front(this);
        start();
    }

    pause() {
        this.m.status = STATUS_WAITING;
    }

    run(quota) {
        do {
            this.m.run(quota);
        } while (this.noint && this.m.status === STATUS_RUNNING);
    }

    static sendmsg(target, signal, args) {
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
        this.checkmail();
        // This must return undefined. Return value comes later from
        // one of the handlers.
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
                // XXX: it's becoming useful to just ignore the case
                // where a handler is not defined. Perhaps we should
                // have an option for %receive - ignore, warn, or error?
                //
                // console.warn("No receiver for message ", msg, " in process ", this.pid);
            }
        }
    }

    set_timeout(timeout, closure, use_this_thread) {
        var tm = setTimeout(() => {
            let tt = use_this_thread ? this : LispProcess.timer_thread;
            if (!tt) {
                LispProcess.timer_thread = new LispProcess(new LispMachine(), closure);
            } else {
                tt.m.push(new LispRet(tt.m, tt.m.pc, true));
                tt.m.n_args = 0;
                tt.m._callnext(closure);
                tt.resume();
            }
        }, timeout);
        return tm;
    }

    clear_timeout(tm) {
        clearTimeout(tm);
        return false;
    }
}
