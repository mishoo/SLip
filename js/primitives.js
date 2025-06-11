import { LispCons } from "./list.js";
import { LispSymbol, LispPackage, LispHash, LispProcess, LispMutex, LispStream, LispInputStream, LispOutputStream, LispChar, LispClosure, LispPrimitiveError, LispObject } from "./types.js";
import { LispMachine, OP } from "./machine.js";
import { repeat_string, UNICODE } from "./utils.js";

let BASE_PACK = LispPackage.BASE_PACK;
let KEYWORD_PACK = LispPackage.get("KEYWORD");

var S_NIL = LispSymbol.get("NIL");
var S_T = LispSymbol.get("T");
var ALL_PRIMITIVES = null;

var LispList = {
    is: LispCons.isList,
    type: "list"
};

var LispArray = {
    is: Array.isArray,
    type: "array"
};

var LispString = {
    is: function(x) { return typeof x == "string" },
    type: "string"
};

var LispNumber = {
    is: function(x) { return typeof x == "number" },
    type: "number"
};

var LispInteger = {
    is: function(x) { return typeof x == "number" && Math.floor(x) == x },
    type: "integer"
};

var LispRegexp = {
    is: function(x) { return x instanceof RegExp },
    type: "regexp"
};

var LispDomElement = {
    is: function(x) { return x instanceof Element },
    type: "dom-element"
};

var LispNativeFunction = {
    is: function(x) { return x instanceof Function },
    type: "native-function"
};

var LispIterator = {
    // XXX: is there some good way to check if it's actually a
    // "good boy" iterator? Seems instanceof Iterator is not
    // widely supported..
    is: function(x) {
        return x != null
            && typeof x == "object"
            && x.next instanceof Function;
    },
    type: "iterator"
};

function boxit(stuff) {
    if (stuff === undefined || stuff === false) return null;
    return stuff;
};

function defp(name, seff, func) {
    name = name.toUpperCase();
    var sym = BASE_PACK.intern(name);
    Object.defineProperty(func, "name", {
        value: name,
        writable: false,
    });
    sym.primitive = func;
    sym.setv("primitive-side-effects", seff ? true : null);
    sym.function = new LispClosure([ OP.PRIM, sym, -1, OP.RET ], sym);
    ALL_PRIMITIVES = new LispCons(sym, ALL_PRIMITIVES);
};

/// utilities

function error(msg) {
    console.error(msg);
    throw new LispPrimitiveError(msg);
};

function checknargs(n, min, max) {
    if (min != null && n < min) error("Not enough arguments");
    if (max != null && n > max) error("Too many arguments");
};

function checktype(x, type) {
    if (!type.is(x)) error("Invalid type, expecting " + type.type + ", got: " + LispMachine.dump(x));
    return x;
};

function as_string(thing) {
    if (LispSymbol.is(thing)) return LispSymbol.symname(thing);
    if (LispChar.is(thing)) return thing.value;
    if (LispPackage.is(thing)) return thing.name;
    return thing;
};

function as_list(thing) {
    if (!LispList.is(thing)) return new LispCons(thing, null);
    return thing;
};

/// primitive definitions

/* -----[ conditionals ]----- */

function eq(a, b) {
    return (a === S_NIL && b === null)
        || (a === null && b === S_NIL)
        || (a === S_T && b === true)
        || (a === true && b === S_T)
        || a === b ? true : null;
};

function equal(a, b) {
    if (LispList.is(a) && LispList.is(b)) {
        while (a !== null && b !== null) {
            if (!equal(a.car, b.car)) return null;
            a = a.cdr;
            b = b.cdr;
            if (!LispCons.is(a) || !LispCons.is(b))
                return equal(a, b);
        }
        return a === b;
    } else if (LispArray.is(a) && LispArray.is(b)) {
        var i = a.length;
        if (i !== b.length) return null;
        while (--i >= 0) {
            if (!equal(a[i], b[i])) return null;
        }
        return true;
    } else if (LispRegexp.is(a) && LispRegexp.is(b)) {
        return a.toString() == b.toString();
    } else return eq(a, b);
};

defp("eq", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var a = m.pop(), b = m.pop();
    return eq(a, b);
});

defp("eql", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var a = m.pop(), b = m.pop();
    return eq(a, b);
});

defp("equal", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var a = m.pop(), b = m.pop();
    return equal(a, b);
});

defp("equalp", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var a = m.pop(), b = m.pop();
    if (typeof a == "string" && typeof b == "string") {
        return a.toLowerCase() == b.toLowerCase();
    }
    return equal(a, b);
});

function all_different(a) {
    for (var i = a.length; --i >= 0;) {
        for (var j = i; --j >= 0;) {
            if (a[i] === a[j]) return null;
        }
    }
    return true;
}

defp("/=", false, function(m, nargs){
    checknargs(nargs, 1);
    if (nargs === 1) {
        m.pop_number(error);
        return true;
    }
    if (nargs === 2) {
        return m.pop_number(error) === m.pop_number(error) ? null : true;
    }
    var a = [];
    while (nargs-- > 0) a.push(m.pop_number(error));
    return all_different(a);
});

defp("char/=", false, function(m, nargs){
    checknargs(nargs, 1);
    var a = [];
    while (nargs-- > 0) a.push(checktype(m.pop(), LispChar));
    return all_different(a);
});

defp("null", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return m.pop() === null ? true : null;
});

defp("not", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return m.pop() === null ? true : null;
});

/* -----[ arithmetic ]----- */

defp("+", false, function(m, nargs){
    var ret = 0;
    while (nargs-- > 0) {
        ret += m.pop_number(error);
    }
    return ret;
});

defp("*", false, function(m, nargs){
    var ret = 1;
    while (nargs-- > 0) {
        ret *= m.pop_number(error);
    }
    return ret;
});

defp("-", false, function(m, nargs){
    checknargs(nargs, 1);
    var i = nargs;
    var a = [];
    while (--i >= 0) {
        a[i] = m.pop_number(error);
    }
    var ret = a[++i];
    if (nargs == 1) ret = -ret;
    while (++i < nargs) {
        ret = ret - a[i];
    }
    return ret;
});

defp("/", false, function(m, nargs){
    checknargs(nargs, 1);
    var i = nargs;
    var a = [];
    while (--i >= 0) {
        a[i] = m.pop_number(error);
    }
    var ret = a[++i];
    if (nargs == 1) ret = 1/ret;
    while (++i < nargs) {
        ret = ret / a[i];
    }
    return ret;
});

defp("1+", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return m.pop_number(error) + 1;
});

defp("1-", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return m.pop_number(error) - 1;
});

[
    [ "floor", Math.floor ],
    [ "ceiling", Math.ceil ],
    [ "round", Math.round ]
].forEach(function(f){
    var func = f[1];
    defp(f[0], false, function(m, nargs){
        checknargs(nargs, 1, 2);
        var divisor = nargs == 2 ? m.pop() : 1;
        var number = m.pop();
        checktype(number, LispNumber);
        checktype(divisor, LispNumber);
        let quot = func(number / divisor);
        m.stack.set_values_array([ quot, number - quot * divisor ]);
        return false;
    });
});

defp("mod", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var a = m.pop_number(error), b = m.pop_number(error);
    return b % a;
});

[
    [ "abs", Math.abs ],
    [ "sin", Math.sin ],
    [ "asin", Math.asin ],
    [ "cos", Math.cos ],
    [ "acos", Math.acos ],
    [ "tan", Math.cos ],
    [ "atan", Math.acos ],
    [ "exp", Math.exp ],
    [ "log", Math.log ],
    [ "sqrt", Math.sqrt ]

].forEach(function(f){
    var func = f[1];
    defp(f[0], false, function(m, nargs){
        checknargs(nargs, 1, 1);
        return func(m.pop_number(error));
    });
});

defp("expt", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    let exp = m.pop_number(error);
    let base = m.pop_number(error);
    return Math.pow(base, exp);
});

defp("random", false, function(m, nargs){
    checknargs(nargs, 0, 1);
    if (nargs == 1) {
        var arg = m.pop();
        checktype(arg, LispNumber);
        return Math.floor(arg * Math.random());
    }
    return Math.random();
});

defp("min", false, function(m, nargs){
    checknargs(nargs, 1);
    let args = m.pop_frame(nargs);
    args.forEach(x => checktype(x, LispNumber));
    return Math.min(...args);
});

defp("max", false, function(m, nargs){
    checknargs(nargs, 1);
    let args = m.pop_frame(nargs);
    args.forEach(x => checktype(x, LispNumber));
    return Math.max(...args);
});

/* -----[ list/sequence manipulation ]----- */

defp("cons", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var b = m.pop(), a = m.pop();
    return new LispCons(a, b);
});

defp("length", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispCons.isList(x)) return LispCons.len(x);
    if (LispString.is(x)) return x.length;
    if (LispArray.is(x)) return x.length;
    if (LispHash.is(x)) return x.size();
    error("Unrecognized sequence in length");
});

defp("elt", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var i = m.pop(), x = m.pop();
    checktype(i, LispNumber);
    if (LispCons.isList(x)) return LispCons.elt(x, i);
    if (LispArray.is(x)) return i < x.length ? x[i] : null;
    if (LispString.is(x)) return i < x.length ? LispChar.get(x.charAt(i)) : null;
    error("Unrecognized sequence in elt");
});

defp("rplaca", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var val = m.pop(), cons = m.pop();
    checktype(cons, LispCons);
    return cons.car = val;
});

defp("rplacd", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var val = m.pop(), cons = m.pop();
    checktype(cons, LispCons);
    return cons.cdr = val;
});

defp("nthcdr", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var list = m.pop(), n = m.pop();
    checktype(n, LispInteger);
    var p = list;
    while (p !== null && n-- > 0) {
        checktype(p, LispList);
        p = p.cdr;
    }
    return p;
});

defp("%nhalf-list", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    let a = m.pop(), b = a;
    if (a === null) return null;
    while (true) {
        b = LispCons.cddr(b);
        if (b === null) {
            b = a.cdr;
            a.cdr = null;
            return b;
        }
        if (a === b) {
            error("Circular list detected");
        }
        a = a.cdr;
    }
});

defp("reverse", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispList.is(x)) return LispCons.reverse(x);
    if (LispArray.is(x)) return [...x].reverse();
    if (LispString.is(x)) {
        for (var i = x.length, ret = ""; --i >= 0;) ret += x.charAt(i);
        return ret;
    }
    error("Unrecognized sequence in reverse");
});

defp("nreverse", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispList.is(x)) return LispCons.nreverse(x);
    if (LispArray.is(x)) return x.reverse();
    error("Unrecognized sequence in nreverse");
});

defp("%memq", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var seq = m.pop(), item = m.pop();
    if (LispList.is(seq))
        return LispCons.find(seq, item, eq);
    if (LispArray.is(seq)) {
        var pos = seq.indexOf(item);
        return pos >= 0 ? pos : null;
    }
    if (LispString.is(seq)) {
        checktype(item, LispChar);
        var pos = seq.indexOf(item.value);
        return pos >= 0 ? pos : null;
    }
    error("Unrecognized sequence in %memq");
});

defp("%assq", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var list = m.pop(), item = m.pop();
    while (list !== null) {
        checktype(list, LispList);
        checktype(list.car, LispCons);
        if (eq(list.car.car, item)) return list.car;
        list = list.cdr;
    }
    return null;
});

defp("getf", false, function(m, nargs){
    checknargs(nargs, 2, 3);
    var not_found = nargs == 3 ? m.pop() : null;
    var item = m.pop(), list = m.pop();
    while (list !== null) {
        checktype(list, LispList);
        if (list.cdr === null) error("Malformed plist");
        if (eq(list.car, item))
            return list.cdr.car;
        list = list.cdr.cdr;
    }
    return not_found;
});

defp("%putf", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var value = m.pop(), item = m.pop(), list = m.pop();
    var p = list;
    while (p !== null) {
        checktype(p, LispList);
        if (!p.cdr) error("Malformed plist");
        if (eq(p.car, item)) break;
        p = p.cdr.cdr;
    }
    return p ? (p.cdr.car = value, list)
        : new LispCons(item, new LispCons(value, list));
});

(function(N){
    defp("gensym", false, function(m, nargs) {
        checknargs(nargs, 0, 1);
        var name = "SYM";
        if (nargs == 1) {
            name = m.pop();
            checktype(name, LispString);
        }
        return new LispSymbol(name + (++N));
    });
})(0);

(function(make){
    for (let name of Object.getOwnPropertyNames(LispCons)) {
        if (/^c[ad]+r$/.test(name)) {
            defp(name, false, make(LispCons[name]));
        }
    }
})(function(func){
    return function (m, nargs) {
        checknargs(nargs, 1, 1);
        return func(m.pop());
    };
});

defp("%dump", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispMachine.dump(m.pop());
});

defp("console.log", true, function(m, nargs){
    var a = [];
    while (nargs-- > 0) a.unshift(m.pop());
    console.log(a.map(LispMachine.dump).join(" "));
    return null;
});

defp("console.dir", true, function(m, nargs){
    var a = [];
    while (nargs-- > 0) a.unshift(m.pop());
    console.log.apply(console, a);
    return null;
});

defp("console.error", true, function(m, nargs){
    var a = [];
    while (nargs-- > 0) a.unshift(m.pop());
    console.error.apply(console, a);
    return null;
});

defp("console.print", true, function(m, nargs){
    var a = [];
    while (nargs-- > 0) a.unshift(m.pop());
    console.log(a.join(" "));
    return null;
});

defp("list", false, function(m, nargs) {
    var p = null;
    while (nargs-- > 0)
        p = new LispCons(m.pop(), p);
    return p;
});

defp("list*", false, function(m, nargs) {
    checknargs(nargs, 1);
    var p = m.pop();
    while (--nargs > 0)
        p = new LispCons(m.pop(), p);
    return p;
});

defp("last", false, function(m, nargs) {
    checknargs(nargs, 1, 1);
    return LispCons.last(m.pop());
});

defp("copy-list", false, function(m, nargs){
    checknargs(nargs, 1);
    return LispCons.copy(checktype(m.pop(), LispList));
});

defp("append", false, function(m, nargs) {
    return nargs == 0 ? null : LispCons.append(m.pop_frame(nargs));
});

defp("nconc", true, function(m, nargs){
    return nargs == 0 ? null : LispCons.nconc(m.pop_frame(nargs));
});

defp("revappend", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var tail = m.pop(), list = m.pop();
    return LispCons.revappend(list, tail);
});

defp("nreconc", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var tail = m.pop(), list = m.pop();
    return LispCons.nreconc(list, tail);
});

/* -----[ arrays ]----- */

defp("vector", false, function(m, nargs){
    var a = [];
    while (--nargs >= 0) a[nargs] = m.pop();
    return a;
});

defp("make-vector", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var init = m.pop(), n = m.pop();
    checktype(n, LispNumber);
    var a = new Array(n);
    while (--n >= 0) a[n] = init;
    return a;
});

defp("as-vector", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var list = m.pop();
    checktype(list, LispList);
    return LispCons.toArray(list);
});

defp("as-list", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var vec = m.pop();
    if (LispList.is(vec)) return vec;
    if (LispArray.is(vec)) return LispCons.fromArray(vec);
    error("Unsupported sequence");
});

defp("vector-push", true, function(m, nargs){
    checknargs(nargs, 2);
    var a = m.pop_frame(nargs - 1);
    var vector = m.pop();
    checktype(vector, LispArray);
    vector.push(...a);
    return vector;
});

defp("vector-pop", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var vector = m.pop();
    checktype(vector, LispArray);
    return vector.length > 0 ? vector.pop() : null;
});

defp("vector-splice", true, function(m, nargs){
    checknargs(nargs, 3, 4);
    var content = nargs == 4 ? m.pop() : null;
    var len = m.pop();
    var start = m.pop();
    var vector = m.pop();
    checktype(vector, LispArray);
    checktype(start, LispNumber);
    checktype(len, LispNumber);
    var a = [ start, len ];
    if (content) {
        checktype(content, LispArray);
        a.push.apply(a, content);
    }
    return vector.splice.apply(vector, a);
});

defp("vector-subseq", false, function(m, nargs){
    checknargs(nargs, 1, 3);
    var end = nargs == 3 ? m.pop() : null;
    var start = nargs >= 2 ? m.pop() : null;
    var vector = m.pop();
    checktype(vector, LispArray);
    if (start !== null) checktype(start, LispNumber);
    else start = 0;
    if (end !== null) checktype(end, LispNumber);
    else end = vector.length;
    return vector.slice(start, end);
});

defp("svref", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var index = m.pop(), vector = m.pop();
    checktype(vector, LispArray);
    checktype(index, LispNumber);
    return index >= 0 && index < vector.length ? vector[index] : null;
});

defp("vector-set", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var val = m.pop(), index = m.pop(), vector = m.pop();
    checktype(vector, LispArray);
    checktype(index, LispNumber);
    return vector[index] = val;
});

function seq(m, nargs) {
    var ret = [];
    while (nargs-- > 0) {
        var x = m.pop();
        if (x !== null) {
            if (LispCons.is(x)) {
                ret.unshift.apply(ret, LispCons.toArray(x));
            }
            else if (LispArray.is(x)) {
                ret.unshift.apply(ret, x);
            }
            else ret.unshift(x);
        }
    }
    return ret;
};

defp("%seq", false, function(m, nargs){
    return seq(m, nargs);
});

defp("%seq-cat", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var list = m.pop(), seq = m.pop();
    checktype(list, LispList);
    checktype(seq, LispArray);
    LispCons.forEach(list, function(x){
        if (x !== null) {
            if (LispCons.is(x)) {
                seq.push.apply(seq, LispCons.toArray(x));
            }
            else if (LispArray.is(x)) {
                seq.push.apply(seq, x);
            }
            else seq.push(x);
        }
    });
    return seq;
});

/* -----[ strings ]----- */

function strcat(m, n) {
    var ret = "";
    while (n-- > 0) {
        var arg = m.pop();
        switch (typeof arg) {
          case "string":
          case "number":
            ret = arg + ret;
            break;
          default:
            if (LispChar.is(arg)) ret = arg.value + ret;
            else if (LispSymbol.is(arg)) ret = LispSymbol.symname(arg) + ret;
            else if (LispPackage.is(arg)) ret = arg.name + ret;
            else error("Unrecognized argument type in STRCAT " + arg);
        }
    }
    return ret;
};

defp("strcat", false, function(m, nargs){
    return strcat(m, nargs);
});

defp("substr", false, function(m, nargs){
    checknargs(nargs, 2, 3);
    if (nargs == 3) {
        var len = m.pop();
        checktype(len, LispNumber);
    }
    var from = m.pop(), str = m.pop();
    checktype(from, LispNumber);
    checktype(str, LispString);
    return from < str.length ? len !== null ? str.substr(from, len) : str.substr(from) : null;
});

defp("downcase", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispString.is(x)) return x.toLowerCase();
    if (LispChar.is(x)) return LispChar.get(x.value.toLowerCase());
    error("Unsupported argument type");
});

defp("upcase", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispString.is(x)) return x.toUpperCase();
    if (LispChar.is(x)) return LispChar.get(x.value.toUpperCase());
    error("Unsupported argument type");
});

defp("char-name", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var ch = m.pop();
    checktype(ch, LispChar);
    return ch.name();
});

defp("name-char", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var name = m.pop();
    checktype(name, LispString);
    return LispChar.fromName(name);
});

defp("char-code", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var ch = m.pop();
    checktype(ch, LispChar);
    return ch.code();
});

defp("code-char", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var code = m.pop();
    checktype(code, LispNumber);
    return LispChar.fromCode(code);
});

defp("letterp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var ch = m.pop();
    checktype(ch, LispChar);
    return UNICODE.letter.test(ch.value) ? true : null;
});

// number/string/char comparators
(defcmp => {
    defcmp("="  , (a, b) => a === b);
    defcmp("/=" , (a, b) => a !== b);
    defcmp("<=" , (a, b) => a <= b);
    defcmp(">=" , (a, b) => a >= b);
    defcmp("<"  , (a, b) => a < b);
    defcmp(">"  , (a, b) => a > b);
    defcmp("-equal", (a, b) => a.toLowerCase() == b.toLowerCase());
})((name, cmp) => {
    if (name != "-equal" && name != "/=") {
        // numeric ops; numeric /= is defined elsewhere
        defp(name, false, function(m, nargs){
            checknargs(nargs, 1);
            let prev = m.pop_number(error);
            if (nargs === 1) {
                return true;
            }
            if (nargs === 2) {
                return cmp(m.pop_number(error), prev) ? true : null;
            }
            let ret = true;
            while (--nargs > 0) {
                let el = m.pop_number(error);
                if (ret && !cmp(el, prev)) ret = null;
                prev = el;
            }
            return ret;
        });
    }
    if (name != "/=") {
        // char ops; char/= is defined elsewhere
        defp("char" + name, false, function(m, nargs){
            checknargs(nargs, 1);
            let prev = m.pop();
            checktype(prev, LispChar);
            let ret = true;
            while (--nargs > 0) {
                let el = m.pop();
                checktype(el, LispChar);
                if (ret && !cmp(el.value, prev.value)) ret = null;
                prev = el;
            }
            return ret;
        });
    }
    defp("string" + name, false, function(m, nargs){
        // string ops
        checknargs(nargs, 1);
        let prev = m.pop();
        checktype(prev, LispString);
        let ret = true;
        while (--nargs > 0) {
            let el = m.pop();
            checktype(el, LispString);
            if (ret && !cmp(el, prev)) ret = null;
            prev = el;
        }
        return ret;
    });
});

/* -----[ regexps ]----- */

defp("make-regexp", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var mods = "";
    if (nargs == 2) {
        mods = m.pop();
        checktype(mods, LispString);
    }
    var str = m.pop();
    checktype(str, LispString);
    try {
        return new RegExp(str, mods);
    } catch(ex) {
        error("Invalid regexp (" + ex + ")");
    }
});

defp("regexp-test", false, function(m, nargs){
    checknargs(nargs, 2);
    var str = m.pop(), rx = m.pop();
    checktype(str, LispString);
    checktype(rx, LispRegexp);
    return rx.test(str) ? true : null;
});

defp("regexp-exec", false, function(m, nargs){
    checknargs(nargs, 2);
    var str = m.pop(), rx = m.pop();
    checktype(str, LispString);
    checktype(rx, LispRegexp);
    return rx.exec(str);
});

defp("replace-regexp", false, function(m, nargs){
    checknargs(nargs, 3);
    var replacement = as_string(m.pop()), string = as_string(m.pop()), rx = m.pop();
    checktype(rx, LispRegexp);
    checktype(string, LispString);
    checktype(replacement, LispString);
    return string.replace(rx, replacement);
});

defp("quote-regexp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var str = as_string(m.pop());
    checktype(str, LispString);
    return str.replace(/([\[\]\(\)\{\}\.\*\+\?\|\\])/g, "\\$1");
});

/* -----[ types ]----- */

defp("regexpp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispRegexp.is(m.pop()) ? true : null;
});

defp("stringp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispString.is(m.pop()) ? true : null;
});

defp("charp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispChar.is(m.pop()) ? true : null;
});

defp("digitp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var ch = m.pop();
    checktype(ch, LispChar);
    ch = ch.value.charCodeAt(0);
    return ch >= 48 && ch <= 57 ? true : null;
});

defp("numberp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispNumber.is(m.pop()) ? true : null;
});

defp("hashp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispHash.is(m.pop()) ? true : null;
});

defp("functionp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispClosure.is(m.pop()) ? true : null;
});

defp("vectorp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispArray.is(m.pop()) ? true : null;
});

defp("listp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispCons.isList(m.pop()) ? true : null;
});

defp("consp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispCons.is(m.pop()) ? true : null;
});

defp("atom", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispCons.is(m.pop()) ? null : true;
});

defp("symbolp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    return LispSymbol.is(x) ? true : null;
});

defp("packagep", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    return LispPackage.is(x) ? true : null;
});

defp("keywordp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    return x !== null && x !== true && LispSymbol.is(x) && x.pak === KEYWORD_PACK ? true : null;
});

defp("threadp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispProcess.is(m.pop()) ? true : null;
});

defp("zerop", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop_number(error);
    return x == 0 ? true : null;
});
defp("minusp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop_number(error);
    return x < 0 ? true : null;
});
defp("plusp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop_number(error);
    return x > 0 ? true : null;
});
defp("evenp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop_number(error);
    return x % 2 == 0 ? true : null;
});
defp("oddp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop_number(error);
    return x % 2 == 1 ? true : null;
});

defp("parse-number", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    checktype(x, LispString);
    var ret = parseFloat(x);
    return isNaN(ret) ? null : ret;
});

defp("parse-integer", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var radix = nargs == 2 ? m.pop() : 10;
    checktype(radix, LispNumber);
    var x = m.pop();
    checktype(x, LispString);
    var ret = parseInt(x, radix);
    return isNaN(ret) ? null : ret;
});

defp("number-fixed", false, function(m, nargs){
    checknargs(nargs, 2);
    var fixed = m.pop();
    var number = m.pop();
    checktype(number, LispNumber);
    checktype(fixed, LispNumber);
    return number.toFixed(fixed);
});

defp("number-string", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var radix = nargs == 2 ? m.pop() : 10;
    var number = m.pop();
    checktype(number, LispNumber);
    checktype(radix, LispNumber);
    return number.toString(radix);
});

defp("%pad-string", false, function(m, nargs){
    checknargs(nargs, 2, 6);
    var min = nargs >= 6 ? m.pop() : 0;
    var inc = nargs >= 5 ? m.pop() : 1;
    var left = nargs >= 4 ? m.pop() : null;
    var chr = nargs >= 3 ? as_string(m.pop()) : " ";
    var width = m.pop();
    var str = m.pop();
    checktype(str, LispString);
    checktype(width, LispNumber);
    checktype(chr, LispString);
    checktype(inc, LispNumber);
    checktype(min, LispNumber);
    if (min > 0) {
        if (left !== null) {
            str = repeat_string(chr, min) + str;
        } else {
            str = str + repeat_string(chr, min);
        }
    }
    if (inc != 1) chr = repeat_string(chr, inc);
    if (left !== null) {
        while (str.length < width) str = chr + str;
    } else {
        while (str.length < width) str = str + chr;
    }
    return str;
});

defp("%add-commas", false, function(m, nargs){
    checknargs(nargs, 1, 3);
    var interval = nargs >= 3 ? m.pop() : 3;
    var ch = nargs >= 2 ? as_string(m.pop()) : ',';
    var str = as_string(m.pop());
    checktype(str, LispString);
    checktype(ch, LispString);
    checktype(interval, LispNumber);
    interval = Math.floor(interval);
    if (interval <= 0) error("Interval must be strictly positive");
    var ret = "";
    for (var i = str.length, j = interval; --i >= 0; --j) {
        if (!j) {
            ret = ch + ret;
            j = interval;
        }
        ret = str.charAt(i) + ret;
    }
    return ret;
});

/* -----[ simple hashes ]----- */

defp("make-hash", false, function(m, nargs){
    if (nargs & 1) error("Odd number of arguments");
    var keys = [], values = [];
    while (nargs > 0) {
        values.push(m.pop());
        keys.push(m.pop());
        nargs -= 2;
    }
    var hash = new LispHash();
    keys.forEach(function(key, i){
        hash.set(key, values[i]);
    });
    return hash;
});

defp("hash-get", false, function(m, nargs){
    checknargs(nargs, 2, 3);
    var def = (nargs == 3) ? m.pop() : null;
    var key = m.pop(), hash = m.pop();
    checktype(hash, LispHash);
    var h = hash.has(key);
    if (h) return h.get(key);
    return def;
});

defp("hash-add", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var val = m.pop(), key = m.pop(), hash = m.pop();
    checktype(hash, LispHash);
    return hash.set(key, val);
});

defp("hash-set", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var val = m.pop(), key = m.pop(), hash = m.pop();
    checktype(hash, LispHash);
    var h = hash.has(key);
    if (h) return h.set(key, val);
    return hash.set(key, val);
});

defp("hash-copy", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var hash = m.pop();
    checktype(hash, LispHash);
    return hash.copy();
});

defp("hash-keys", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var hash = m.pop();
    checktype(hash, LispHash);
    return hash.keys();
});

defp("hash-values", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var hash = m.pop();
    checktype(hash, LispHash);
    return hash.values();
});

defp("hash-iterator", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var hash = m.pop();
    checktype(hash, LispHash);
    return hash.iterator();
});

defp("iterator-next", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var it = m.pop();
    checktype(it, LispIterator);
    var result = it.next();
    return new LispCons(boxit(result.value), boxit(result.done));
});

/* -----[ simple streams ]----- */

defp("%stream-line", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispStream);
    return stream.line;
});

defp("%stream-col", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispStream);
    return stream.col;
});

defp("%stream-pos", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispStream);
    return stream.pos;
});

defp("%make-input-stream", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var text = m.pop();
    checktype(text, LispString);
    return new LispInputStream(text);
});

defp("%stream-peek", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispInputStream);
    return stream.peek();
});

defp("%stream-skip-to", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var ch = m.pop(), stream = m.pop();
    checktype(stream, LispInputStream);
    if (LispChar.is(ch)) ch = ch.value;
    checktype(ch, LispString);
    return stream.skip_to(ch);
});

defp("%stream-next", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispInputStream);
    return stream.next();
});

defp("%stream-prev", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispInputStream);
    return stream.prev();
});

defp("%make-output-stream", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return new LispOutputStream();
});

defp("%stream-put", true, function(m, nargs){
    checknargs(nargs, 1);
    var text = strcat(m, nargs - 1);
    var stream = m.pop();
    checktype(stream, LispOutputStream);
    return stream.put(text);
});

defp("%stream-get", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var stream = m.pop();
    checktype(stream, LispOutputStream);
    return stream.get();
});

defp("%get-file-contents", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var cont = nargs == 2 ? m.pop() : null;
    if (cont !== null) checktype(cont, LispClosure);
    var url = m.pop();
    checktype(url, LispString);

    if (!/^(?:https?:)?\/\//i.test(url)) {
        // local storage takes priority
        let content = ls_get_file_contents(url);
        if (content != null) {
            if (cont) {
                return m._callnext(cont, new LispCons(content, null));
            } else {
                return content;
            }
        }
    }

    var xhr = new XMLHttpRequest();
    url += "?killCache=" + Date.now(); // is this a good idea?
    if (cont) {
        xhr.open("GET", url, true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4)
                m._call(cont, new LispCons(xhr.status == 200 ? xhr.responseText : null, null));
        };
        xhr.send(null);
    } else {
        xhr.open("GET", url, false); // XXX: synchronous is deprecated
        xhr.send(null);
        return cont ? null : xhr.status == 200 ? xhr.responseText : null;
    }
});

/* -----[ local storage ]----- */

let LOCAL_STORE = null;

document.addEventListener("slip-reset-local-storage", ev => {
    LOCAL_STORE = ev.store;
});

function ls_get() {
    return LOCAL_STORE || (
        LOCAL_STORE = JSON.parse(localStorage.getItem(".slip") || "{}")
    );
}

function ls_set(store) {
    LOCAL_STORE = store;
    localStorage.setItem(".slip", JSON.stringify(store));
    let event = new Event("ymacs-reset-local-storage");
    event.store = store;
    document.dispatchEvent(event);
}

function ls_normalize_path(path) {
    path = path.replace(/^[~\x2f]+/, "").split(/\x2f+/);
    var ret = [];
    while (path.length > 0) {
        var x = path.shift();
        if (x != ".") {
            if (x == "..") {
                ret.pop();
            } else if (x == "~") {
                ret = [];
            } else {
                ret.push(x);
            }
        }
    }
    return ret.join("/");
}

function ls_get_file_contents(path) {
    let dir = ls_get();
    path = ls_normalize_path(path).split("/");
    while (dir != null && path.length > 1) {
        dir = dir[path.shift()];
    }
    return dir ? dir[path[0]] : null;
}

function ls_set_file_contents(path, content) {
    if (/\.fasl$/.test(path) && window.SLIP_COMMIT) {
        if (window.SLIP_COMMIT != localStorage.getItem(".slip_commit")) {
            ls_purge_fasls();
            localStorage.setItem(".slip_commit", window.SLIP_COMMIT);
        }
    }
    let store = ls_get();
    let dir = store;
    path = ls_normalize_path(path).split("/");
    for (let i = 0; i < path.length - 1; ++i) {
        let cur = path[i];
        let dive = dir[cur];
        if (dive == null) {
            dir = dir[cur] = {};
        } else if (typeof dive == "string") {
            error(`${cur} is not a directory`);
        } else {
            dir = dive;
        }
    }
    dir[path.at(-1)] = content;
    ls_set(store);
}

function ls_delete(path) {
    let store = ls_get();
    let dir = store;
    path = ls_normalize_path(path).split("/");
    while (path.length > 1) dir = dir[path.shift()];
    delete dir[path[0]];
    ls_set(store);
}

function ls_webdav_save_all(prefix = "./") {
    let store = ls_get();
    (function dig(prefix, store){
        for (let [ key, val ] of Object.entries(store)) {
            if (/^\./.test(key)) continue;
            if (typeof val == "string") {
                let filename = prefix + key;
                let xhr = new XMLHttpRequest();
                xhr.open("PUT", filename, false); // XXX: sync is deprecated
                xhr.onreadystatechange = function() {
                    if (xhr.readyState == 4) {
                        if (xhr.status >= 200 && xhr.status < 300) {
                            console.log(`Saved ${filename}`);
                        } else {
                            console.error(`Couldn't save ${filename} (${xhr.status})`);
                        }
                    }
                };
                xhr.send(val);
            } else if (val != null) {
                dig(prefix + key + "/", val);
            }
        }
    })(prefix, store);
}

function ls_clear() {
    let store = ls_get();
    Object.keys(store).forEach(key => {
        if (!/^\./.test(key)) delete store[key];
    });
    ls_set(store);
}

function ls_dump() {
    let json = JSON.stringify(ls_get(), null, 2);
    let blob = new Blob([ json ], { type: "application/json" });
    let url = URL.createObjectURL(blob);
    window.open(url);
}

function ls_purge_fasls() {
    let cleanup = (store) => {
        if (store) for (let [ key, val ] of Object.entries(store)) {
            if (typeof val == "object") {
                cleanup(val);
            } else if (/\.fasl$/i.test(key)) {
                delete store[key];
            }
        }
        return store;
    };
    ls_set(cleanup(ls_get()));
}

LispMachine.ls_get_file_contents = ls_get_file_contents;
LispMachine.ls_set_file_contents = ls_set_file_contents;
LispMachine.ls_purge_fasls = ls_purge_fasls;

defp("%ls-get-file-contents", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    let path = m.pop();
    checktype(path, LispString);
    return ls_get_file_contents(path) ?? null;
});

defp("%ls-set-file-contents", true, function(m, nargs){
    checknargs(nargs, 1, 2);
    let content = m.pop();
    let path = m.pop();
    checktype(path, LispString);
    checktype(content, LispString);
    ls_set_file_contents(path, content);
});

defp("%ls-delete-path", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    let path = m.pop();
    checktype(path, LispString);
    ls_delete(path);
});

defp("%ls-clear-store", true, function(m, nargs){
    checknargs(nargs, 0, 0);
    ls_clear();
});

defp("%ls-purge-fasls", true, function(m, nargs){
    checknargs(nargs, 0, 0);
    ls_purge_fasls();
});

defp("%ls-dump-store", true, function(m, nargs){
    checknargs(nargs, 0, 0);
    ls_dump();
});

defp("%ls-webdav-save-all", true, function(m, nargs){
    checknargs(nargs, 0, 1);
    let prefix;
    if (nargs > 0) {
        prefix = m.pop();
        checktype(prefix, LispString);
    }
    ls_webdav_save_all(prefix);
});

/* -----[ /local storage ]----- */

defp("%input-stream-p", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispInputStream.is(m.pop()) ? true : null;
});

defp("%output-stream-p", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispOutputStream.is(m.pop()) ? true : null;
});

/* -----[ object allocation utils ]----- */

defp("%objectp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispObject.is(m.pop()) ? true : null;
});

defp("%object-vector", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var obj = m.pop();
    checktype(obj, LispObject);
    return obj.vector;
});

defp("%make-object", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var size = m.pop();
    checktype(size, LispNumber);
    return new LispObject(size);
});

/* -----[ macros/primitives ]----- */

defp("%macro", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var symbol = m.pop();
    checktype(symbol, LispSymbol);
    return symbol !== null && symbol !== true ? symbol.macro() : null;
});

defp("disassemble", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var func = m.pop();
    if (func instanceof LispSymbol) func = func.function;
    checktype(func, LispClosure);
    return LispMachine.disassemble(func.code);
});

defp("apply", true, function(m, nargs){
    checknargs(nargs, 2);
    var func = m.stack.replace(-nargs, m.mkret(m.pc));
    if (func instanceof LispSymbol) func = func.function;
    checktype(func, LispClosure);
    var args = m.pop();
    while (args !== null) {
        m.push(LispCons.car(args));
        args = args.cdr;
        nargs++;
    }
    m.n_args = nargs - 2;
    return m._callnext(func, false);
});

defp("funcall", true, function(m, nargs){
    checknargs(nargs, 1);
    var func = m.stack.replace(-nargs, m.mkret(m.pc));
    if (func instanceof LispSymbol) func = func.function;
    checktype(func, LispClosure);
    m.n_args = nargs - 1;
    return m._callnext(func, false);
});

defp("values", false, function(m, nargs){
    m.stack.set_values(nargs);
    return false;
});

defp("multiple-value-list", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    return LispCons.fromArray(m.stack.pop_values());
});

defp("%primitivep", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var sym = m.pop();
    checktype(sym, LispSymbol);
    return sym.primitive ? true : null;
});

defp("%prim-side-effects", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var sym = m.pop();
    checktype(sym, LispSymbol);
    return sym.getv("primitive-side-effects");
});

defp("%macro!", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var func = m.pop(), sym = m.pop();
    checktype(func, LispClosure);
    checktype(sym, LispSymbol);
    sym.setv("macro", func);
    sym.function = null;
    return sym;
});

/* -----[ symbols, packages ]----- */

defp("%special!", true, function(m, nargs){
    checknargs(nargs, 1);
    while (nargs-- > 0) {
        var name = m.pop();
        checktype(name, LispSymbol);
        name.setv("special", true);
        name.setv("global", true);
    }
    return null;
});

defp("%global!", true, function(m, nargs){
    checknargs(nargs, 1);
    while (nargs-- > 0) {
        var name = m.pop();
        checktype(name, LispSymbol);
        name.setv("global", true);
    }
    return null;
});

defp("%specialp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var sym = m.pop();
    checktype(sym, LispSymbol);
    return sym !== null && sym !== true && sym.special() ? true : null;
});

defp("%globalp", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var sym = m.pop();
    checktype(sym, LispSymbol);
    return sym !== null && sym !== true && sym.global() ? true : null;
});

defp("%function-name", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var f = m.pop();
    checktype(f, LispClosure);
    return f.name;
});

defp("make-package", true, function(m, nargs){
    checknargs(nargs, 1, 3);
    var nicknames = nargs >= 3 ? m.pop() : null;
    var uses = nargs >= 2 ? m.pop() : null;
    var name = as_string(m.pop());
    checktype(name, LispString);
    var pak = LispPackage.get(name);
    LispCons.forEach(uses, function(use){
        use = as_string(use);
        var p = LispPackage.get_existing(use);
        if (!LispPackage.is(p))
            error("Cannot find package " + use);
        pak.use(p);
    });
    LispCons.forEach(nicknames, function(nick){
        pak.alias(as_string(nick));
    });
    return pak;
});

defp("make-symbol", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var name = m.pop();
    checktype(name, LispString);
    return new LispSymbol(name);
});

defp("intern", true, function(m, nargs){
    checknargs(nargs, 1, 2);
    var pak = nargs == 2 ? m.pop() : null;
    var name = m.pop();
    if (pak === null) {
        pak = m.gvar(BASE_PACK.PACKAGE_VAR);
    }
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(pak, LispPackage);
    checktype(name, LispString);
    var sym = pak.find_or_intern(name);
    return sym === S_NIL ? null : sym === S_T ? true : sym;
});

defp("unintern", true, function(m, nargs){
    checknargs(nargs, 1, 2);
    var pak = nargs == 2 ? m.pop() : null;
    var name = as_string(m.pop());
    if (pak === null) {
        pak = m.gvar(BASE_PACK.PACKAGE_VAR);
    }
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(pak, LispPackage);
    checktype(name, LispString);
    pak.unintern(name);
    return S_NIL;
});

defp("find-symbol", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var pak = nargs == 2 ? m.pop() : null;
    var name = as_string(m.pop());
    if (pak === null) {
        pak = m.gvar(BASE_PACK.PACKAGE_VAR);
    }
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(name, LispString);
    checktype(pak, LispPackage);
    var sym = pak.find(name);
    if (!sym) error("Symbol " + name + " not found in " + pak.name);
    return sym;
});

defp("shadow", true, function(m, nargs){
    checknargs(nargs, 1, 2);
    var pak = nargs == 2 ? m.pop() : null;
    var syms = as_list(m.pop());
    if (pak === null) {
        pak = m.gvar(BASE_PACK.PACKAGE_VAR);
    }
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(pak, LispPackage);
    LispCons.forEach(syms, function(sym){
        pak.shadow(as_string(sym));
    });
    return pak;
});

defp("%accessible-symbols", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var ext = nargs == 2 ? m.pop() : null;
    var pak = m.pop();
    checktype(pak, LispPackage);
    return ext ? pak.all_exported() : pak.all_accessible();
});

defp("%external-symbols", false, function(m, nargs){
    checknargs(nargs, 1);
    var pak = m.pop();
    checktype(pak, LispPackage);
    return pak.all_exported();
});

defp("%symbol-accessible", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var pak = m.pop(), sym = m.pop();
    checktype(sym, LispSymbol);
    checktype(pak, LispPackage);
    return pak.all_accessible().indexOf(sym) >= 0 ? true : null; // XXX: optimize this
});

defp("%interned-symbols", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var pak = m.pop();
    checktype(pak, LispPackage);
    return pak.all_interned();
});

defp("%find-exported-symbol", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var pak = m.pop(), name = as_string(m.pop());
    checktype(name, LispString);
    checktype(pak, LispPackage);
    return pak.find_exported(name);
});

defp("%find-internal-symbol", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var pak = m.pop(), name = as_string(m.pop());
    checktype(name, LispString);
    checktype(pak, LispPackage);
    return pak.find_internal(name);
});

defp("find-package", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var name = m.pop();
    if (LispPackage.is(name)) return name;
    name = as_string(name);
    checktype(name, LispString);
    return LispPackage.get_existing(name);
});

defp("%list-packages", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return LispCons.fromArray([...new Set(Object.values(LispPackage.all()))]);
});

defp("package-name", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var name = m.pop();
    if (LispPackage.is(name)) return name.name;
    name = as_string(name);
    checktype(name, LispString);
    var pak = LispPackage.get_existing(name);
    if (!pak) error("Package " + name + " not found");
    return pak.name;
});

defp("%export", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var pak = m.pop(), syms = as_list(m.pop());
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(pak, LispPackage);
    LispCons.forEach(syms, function(sym){
        if (LispString.is(sym))
            sym = pak.intern(sym);
        pak.export(sym);
    });
    return pak;
});

defp("%import", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var pak = m.pop(), syms = as_list(m.pop());
    if (!LispPackage.is(pak)) {
        pak = LispPackage.get_existing(as_string(pak));
    }
    checktype(pak, LispPackage);
    LispCons.forEach(syms, function(sym){
        pak.import(sym);
    });
    return pak;
});

defp("symbol-name", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var symbol = m.pop();
    checktype(symbol, LispSymbol);
    return LispSymbol.symname(symbol);
});

defp("symbol-package", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var symbol = m.pop();
    checktype(symbol, LispSymbol);
    return symbol === null || symbol === true ? BASE_PACK : symbol.pak;
});

defp("%use-package", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var current = m.pop(), imported = m.pop();
    checktype(current, LispPackage);
    checktype(imported, LispPackage);
    return current.use(imported);
});

defp("%all-primitives", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return ALL_PRIMITIVES;
});

defp("set", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var val = m.pop(), sym = m.pop();
    checktype(sym, LispSymbol);
    return sym.value = val;
});

defp("symbol-value", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    let sym = m.pop();
    checktype(sym, LispSymbol);
    let binding = m.find_dvar(sym);
    return binding.value ?? null;
});

defp("set-symbol-value!", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    let value = m.pop();
    let sym = m.pop();
    if (sym === S_T)
        error("Veritas aeterna. (can't set SYMBOL-VALUE of T)");
    if (sym === S_NIL || sym === null)
        error("Nihil ex nihil. (can't set SYMBOL-VALUE of NIL)");
    checktype(sym, LispSymbol);
    let binding = m.find_dvar(sym);
    return binding.value = value;
});

defp("symbol-function", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var sym = m.pop();
    checktype(sym, LispSymbol);
    return sym.function;
});

defp("set-symbol-function!", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var func = m.pop(), sym = m.pop();
    checktype(sym, LispSymbol);
    checktype(func, LispClosure);
    sym.setv("macro", null);
    return sym.function = func;
});

defp("%set-symbol-prop", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var val = m.pop(), key = as_string(m.pop()), sym = m.pop();
    if (sym === null) sym = S_NIL;
    else if (sym === true) sym = S_T;
    checktype(key, LispString);
    checktype(sym, LispSymbol);
    return sym.setv(key, val);
});

defp("%get-symbol-prop", false, function(m, nargs){
    checknargs(nargs, 2, 2);
    var key = as_string(m.pop()), sym = m.pop();
    if (sym === null) sym = S_NIL;
    else if (sym === true) sym = S_T;
    checktype(key, LispString);
    checktype(sym, LispSymbol);
    return sym.getv(key);
});

/* -----[ processes ]----- */

defp("make-thread", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var func = m.pop();
    checktype(func, LispClosure);
    var p = new LispProcess(m, func);
    return p;
});

defp("current-thread", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return m.process;
});

defp("make-mutex", true, function(m, nargs){
    checknargs(nargs, 0, 1);
    var name = nargs == 1 ? m.pop() : null;
    return new LispMutex(name);
});

defp("mutex-acquire", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var mutex = m.pop();
    checktype(mutex, LispMutex);
    return mutex.acquire(m.process);
});

defp("mutex-release", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var mutex = m.pop();
    checktype(mutex, LispMutex);
    return mutex.release();
});

defp("%sendmsg", true, function(m, nargs){
    checknargs(nargs, 2);
    var args = null;
    while (nargs-- > 2) args = new LispCons(m.pop(), args);
    var signal = as_string(m.pop()), process = m.pop();
    checktype(process, LispProcess);
    checktype(signal, LispString);
    if (m.process) return m.process.sendmsg(process, signal, args);
    return LispProcess.sendmsg(process, signal, args);
});

defp("%receive", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var handlers = m.pop();
    checktype(handlers, LispHash);
    return m.process.receive(handlers);
});

defp("set-timeout", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var closure = m.pop(), timeout = m.pop();
    checktype(closure, LispClosure);
    checktype(timeout, LispNumber);
    return m.process.set_timeout(timeout, closure);
});

defp("clear-timeout", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    return m.process.clear_timeout(m.pop());
});

defp("%no-interrupts", true, function(m, nargs){
    if (!m.process) return null;
    checknargs(nargs, 1, 2);
    var process = nargs == 2 ? m.pop() : m.process;
    var noint = m.pop();
    checktype(process, LispProcess);
    var old = process.noint;
    process.noint = noint;
    return old;
});

/* -----[ DOM ]----- */

defp("dom.get-element-by-id", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var id = as_string(m.pop());
    checktype(id, LispString);
    return document.getElementById(id);
});

var DOM_EVENTS = {
    "CLICK"      : "click",
    "MOUSE-MOVE" : "mousemove",
    "MOUSE-DOWN" : "mousedown",
    "MOUSE-UP"   : "mouseup",
    "MOUSE-OVER" : "mouseover",
    "MOUSE-OUT"  : "mouseout",
    "KEY-PRESS"  : "keypress",
    "KEY-DOWN"   : "keydown",
    "KEY-UP"     : "keyup"
};

function dom_event(name) {
    return DOM_EVENTS[name];
};

defp("dom.subscribe", true, function(m, nargs){
    checknargs(nargs, 2, 3);
    var process = nargs == 3 ? m.pop() : m.process;
    var events = m.pop();
    var element = m.pop();
    checktype(process, LispProcess);
    checktype(events, LispList);
    checktype(element, LispDomElement);
    LispCons.forEach(events, function(e){
        e = as_string(e);
        element.addEventListener(dom_event(e), function(ev){
            var args = LispHash.fromObject({
                "SCREEN-X" : ev.screenX,
                "SCREEN-Y" : ev.screenY,
                "CLIENT-X" : ev.clientX,
                "CLIENT-Y" : ev.clientY,
                "CTRL"     : ev.ctrlKey,
                "ALT"      : ev.altKey,
                "META"     : ev.metaKey,
                "BUTTON"   : ev.button,
                "TARGET"   : ev.target,
                "RELATED"  : ev.relatedTarget
            });
            args = new LispCons(args, null);
            m.process.sendmsg(process, e, args);
        }, true);
    });
    return null;
});

/* -----[ conditions ]----- */

defp("%error", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    let error = m.pop();
    if (typeof window != "undefined" && window.document) {
        let event = new Event("ymacs-slip-error", { cancelable: true });
        event.error = error;
        if (document.dispatchEvent(event)) {
            throw error;
        }
    } else {
        throw error;
    }
});

defp("%warn", true, function(m, nargs){
    let a = [];
    while (nargs-- > 0) a.unshift(m.pop());
    let message = a.join(" ");
    if (typeof window != "undefined" && window.document) {
        let event = new Event("ymacs-slip-warning", { cancelable: true });
        event.message = message;
        if (document.dispatchEvent(event)) {
            console.warn(message);
        }
    } else {
        console.warn(message);
    }
    return null;
});

/* -----[ other ]----- */

var S_SKIP_COUNT = LispSymbol.get("%SKIP-COUNT");
defp("%find-in-env", false, function(m, nargs){
    checknargs(nargs, 3, 3);
    let env = checktype(m.pop(), LispList);
    let type = m.pop();
    let name = m.pop();
    for (let i = 0; env !== null; env = env.cdr) {
        let frame = env.car, skip = false;
        if (frame instanceof LispCons) {
            skip = frame.car === S_SKIP_COUNT;
            frame = frame.cdr;
        }
        checktype(frame, LispArray);
        for (let j = frame.length; --j >= 0;) {
            let lst = checktype(frame[j], LispList);
            if (lst.car === name && lst.cdr.car === type) {
                return new LispCons(i, new LispCons(j, LispCons.cddr(lst)));
            }
        }
        if (!skip) i++;
    }
    return null;
});

defp("%debugger", true, function(m, nargs){
    checknargs(nargs, 0, 0);
    debugger;
});

defp("%step-debug-mode", true, function(m, nargs){
    checknargs(nargs, 0, 0);
    m.debug = true;
});

defp("%grok-xref-info", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var xref = m.pop();
    var filename = m.pop();
    xref.forEach(function(data){
        var sym = data[0], type = data[1], pos = data[2];
        if (sym instanceof LispSymbol) {
            var a = sym.getv("XREF");
            if (a === null) a = sym.setv("XREF", []);
            a.push([ type, filename, pos ]);
        }
    });
    LispMachine.XREF[filename] = xref;
    return null;
});

defp("%machine.dynamic-environment", false, function(m, nargs){
    return m.denv;
});

defp("%machine.stack", false, function(m, nargs){
    return [...m.stack.data];
});

defp("%eval-bytecode", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var code = m.pop();
    checktype(code, LispArray);
    code = LispMachine.assemble(code);
    var f = new LispClosure(code, null, new LispCons([], null));
    return m._callnext(f, null);
});

// The following is called only in COMPILE-STRING (that is, at
// compile time).  Its purpose is to evaluate the given code
// into the compiler environment, and return the assembled
// code.  COMPILE-STRING guarantees that the code passed here
// doesn't leave a return value on the stack.
defp("%exec-code", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var code = m.pop();
    checktype(code, LispArray);
    code = LispMachine.assemble(code);
    // hack: this function needs to return the assembled code, so
    // we push two instructions to do it.  we need to copy the
    // code in order to not include these two instructions in the
    // return value.
    code.push(...LispMachine.assemble([
        [ "CONST", [...code] ],
        [ "RET" ]
    ]));
    var f = new LispClosure(code, null, new LispCons([], null));
    return m._callnext(f, null);
});

defp("%relocate-code", true, function(m, nargs){
    checknargs(nargs, 2, 2);
    var addr = m.pop();
    var code = m.pop();
    checktype(code, LispArray);
    checktype(addr, LispNumber);
    return LispMachine.relocate(code, addr);
});

defp("%serialize-code", false, function(m, nargs){
    checknargs(nargs, 1, 2);
    var cache = nargs == 2 ? m.pop() : null;
    var code = m.pop();
    checktype(code, LispArray);
    if (nargs == 2) checktype(cache, LispHash);
    return LispMachine.serialize(code, true, cache);
});

defp("%js-eval", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var code = m.pop();
    checktype(code, LispString);
    var func = new Function("$machine", "return(" + code + ")");
    return func(m);
});

defp("%js-apply", true, function(m, nargs){
    checknargs(nargs, 3, 3);
    var args = m.pop(), instance = m.pop(), func = m.pop();
    if (LispList.is(args))
        args = LispCons.toArray(args);
    checktype(func, LispNativeFunction);
    checktype(args, LispArray);
    return boxit(func.apply(instance, args));
});

defp("%js-closure", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var closure = m.pop();
    checktype(closure, LispClosure);
    return (...args) => m.atomic_call(closure, args.map(boxit));
});

defp("%js-camelcase-name", true, function(m, nargs){
    checknargs(nargs, 1, 1);
    var name = as_string(m.pop());
    checktype(name, LispString);
    name = name.toLowerCase();
    var m = /^\+(.*)\+$/.exec(name);
    if (m) {
        name = m[1].toUpperCase().replace(/-/g, "_");
    } else {
        name = name.replace(/^\*([a-z])(.*)\*$/, function(str, p1, p2){
            return p1.toUpperCase() + p2;
        });
        name = name.replace(/-([a-z])/g, function(str, p){
            return p.toUpperCase();
        });
    }
    return name;
});

defp("get-internal-run-time", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return performance.now();
});

defp("%get-time", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    return Date.now();
});

defp("%local-date", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    var time = new Date();
    return LispCons.fromArray([ time.getFullYear(),
                                time.getMonth() + 1,
                                time.getDate(),
                                time.getHours(),
                                time.getMinutes(),
                                time.getSeconds(),
                                time.getMilliseconds(),
                                time.getTimezoneOffset() ]);
});

defp("%utc-date", false, function(m, nargs){
    checknargs(nargs, 0, 0);
    var time = new Date();
    return LispCons.fromArray([ time.getUTCFullYear(),
                                time.getUTCMonth() + 1,
                                time.getUTCDate(),
                                time.getUTCHours(),
                                time.getUTCMinutes(),
                                time.getUTCSeconds(),
                                time.getUTCMilliseconds()  ]);
});

defp("%backtrace", false, function(m, nargs) {
    checknargs(nargs, 0, 0);
    return m.backtrace();
});

/**
 * JS Implementation of MurmurHash3 (as of April 6, 2011)
 *
 * @author <a href="mailto:gary.court@gmail.com">Gary Court</a>
 * @see http://github.com/garycourt/murmurhash-js
 * @author <a href="mailto:aappleby@gmail.com">Austin Appleby</a>
 * @see http://sites.google.com/site/murmurhash/
 *
 * @param {string} key ASCII only
 * @param {number} seed Positive integer only
 * @return {number} 32-bit positive integer hash
 */
function murmurhash3_32_gc(key, seed) {
    var remainder, bytes, h1, h1b, c1, c2, k1, i;
    remainder = key.length & 3; // key.length % 4
    bytes = key.length - remainder;
    h1 = seed;
    c1 = 0xcc9e2d51;
    c2 = 0x1b873593;
    i = 0;
    while (i < bytes) {
        k1 =
            ((key.charCodeAt(i) & 0xff)) |
            ((key.charCodeAt(++i) & 0xff) << 8) |
            ((key.charCodeAt(++i) & 0xff) << 16) |
            ((key.charCodeAt(++i) & 0xff) << 24);
        ++i;
        k1 = ((((k1 & 0xffff) * c1) + ((((k1 >>> 16) * c1) & 0xffff) << 16))) & 0xffffffff;
        k1 = (k1 << 15) | (k1 >>> 17);
        k1 = ((((k1 & 0xffff) * c2) + ((((k1 >>> 16) * c2) & 0xffff) << 16))) & 0xffffffff;
        h1 ^= k1;
        h1 = (h1 << 13) | (h1 >>> 19);
        h1b = ((((h1 & 0xffff) * 5) + ((((h1 >>> 16) * 5) & 0xffff) << 16))) & 0xffffffff;
        h1 = (((h1b & 0xffff) + 0x6b64) + ((((h1b >>> 16) + 0xe654) & 0xffff) << 16));
    }
    k1 = 0;
    switch (remainder) {
      case 3:
        k1 ^= (key.charCodeAt(i + 2) & 0xff) << 16;
      case 2:
        k1 ^= (key.charCodeAt(i + 1) & 0xff) << 8;
      case 1:
        k1 ^= (key.charCodeAt(i) & 0xff);
        k1 = (((k1 & 0xffff) * c1) + ((((k1 >>> 16) * c1) & 0xffff) << 16)) & 0xffffffff;
        k1 = (k1 << 16) | (k1 >>> 16);
        k1 = (((k1 & 0xffff) * c2) + ((((k1 >>> 16) * c2) & 0xffff) << 16)) & 0xffffffff;
        h1 ^= k1;
    }
    h1 ^= key.length;
    h1 ^= h1 >>> 16;
    h1 = (((h1 & 0xffff) * 0x85ebca6b) + ((((h1 >>> 16) * 0x85ebca6b) & 0xffff) << 16)) & 0xffffffff;
    h1 ^= h1 >>> 13;
    h1 = ((((h1 & 0xffff) * 0xc2b2ae35) + ((((h1 >>> 16) * 0xc2b2ae35) & 0xffff) << 16))) & 0xffffffff;
    h1 ^= h1 >>> 16;
    return h1 >>> 0;
}

defp("sxhash", false, function(m, nargs){
    checknargs(nargs, 1, 1);
    var x = m.pop();
    if (LispString.is(x)) x = "STR" + x;
    else if (LispNumber.is(x)) x = "NUM" + x.toString();
    else if (LispChar.is(x)) x = "CHR" + x.value;
    else if (LispSymbol.is(x)) x = "SYM" + x.serialize();
    else if (LispPackage.is(x)) x = "PAK" + x.serialize();
    else if (LispRegexp.is(x)) x = "RGX" + x.toString();
    else error("I don't know how to generate hash for this object type");
    return murmurhash3_32_gc(x);
});
