(function(BASE_PACK, KW, undefined){

        var S_NIL = LispSymbol.get("NIL");
        var S_T = LispSymbol.get("T");
        var ALL_PRIMITIVES = null;

        var LispList = {
                is: LispCons.isList,
                type: "list"
        };

        var LispArray = {
                is: function(x) { return x instanceof Array },
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

        var LispDomDocument = {
                is: function(x) { return x instanceof Document },
                type: "dom-document"
        };

        var LispNativeFunction = {
                is: function(x) { return x instanceof Function },
                type: "native-function"
        };

        function boxit(stuff) {
                if (stuff === undefined || stuff === false) return null;
                return stuff;
        };

        function defp(name, seff, func) {
                name = name.toUpperCase();
                var sym = BASE_PACK.intern(name);
                BASE_PACK.export(sym);
                sym.setv("primitive", func);
                sym.setv("primitive-side-effects", seff);
                sym.setv("function", new LispClosure(LispMachine.assemble([
                        [ "PRIM", sym, -1 ],
                        [ "RET" ]
                ]), sym));
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
                return (a === S_NIL && b === null) ||
                        (a === null && b === S_NIL) ||
                        (a === S_T && b === true) ||
                        (a === true && b === S_T) ||
                        a === b ? true : null;
        };

        function equal(a, b) {
                if (LispList.is(a) && LispList.is(b)) {
                        while (a !== null && b !== null) {
                                if (!equal(a.car, b.car)) return null;
                                a = a.cdr;
                                b = b.cdr;
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

        defp("equal", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var a = m.pop(), b = m.pop();
                return equal(a, b);
        });

        defp("/=", false, function(m, nargs){
                checknargs(nargs, 1);
                var a = [];
                while (nargs-- > 0) a.push(m.pop_number(error));
                for (var i = a.length; --i >= 0;) {
                        for (var j = i; --j >= 0;) {
                                if (a[i] == a[j]) return null;
                        }
                }
                return true;
        });

        (function(defcmp){
                defcmp("=", new Function("a", "b", "return a==b"));
                defcmp("<=");
                defcmp(">=");
                defcmp("<");
                defcmp(">");
        })(function(name, cmp){
                if (!cmp) cmp = new Function("a", "b", "return a" + name + "b");
                defp(name, false, function(m, nargs){
                        checknargs(nargs, 1);
                        var prev = m.pop_number(error);
                        var ret = true;
                        while (--nargs > 0) {
                                var el = m.pop_number(error);
                                if (ret && !cmp(el, prev)) ret = null;
                                prev = el;
                        }
                        return ret;
                });
        });

        defp("nullp", false, function(m, nargs){
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
                        return func(number / divisor);
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

        defp("power", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                return Math.pow(m.pop_number(error), m.pop_number(error));
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
                error("Unrecognized sequence");
        });

        defp("elt", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var i = m.pop(), x = m.pop();
                checktype(i, LispNumber);
                if (LispCons.isList(x)) return LispCons.elt(x, i);
                if (LispArray.is(x)) return i < x.length ? x[i] : null;
                if (LispString.is(x)) return i < x.length ? LispChar.get(x.charAt(i)) : null;
                error("Unrecognized sequence");
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
                        checktype(list, LispList);
                        p = p.cdr;
                }
                return p;
        });

        defp("reverse", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (LispList.is(x)) return LispCons.reverse(x);
                if (LispArray.is(x)) return x.slice().reverse();
                if (LispString.is(x)) {
                        for (var i = x.length, ret = ""; --i >= 0;) ret += x.charAt(i);
                        return ret;
                }
                error("Unrecognized sequence");
        });

        defp("nreverse", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (LispList.is(x)) return LispCons.nreverse(x);
                if (LispArray.is(x)) return x.reverse();
                error("Unrecognized sequence");
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
                error("Unrecognized sequence");
        });

        defp("assq", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var list = m.pop(), item = m.pop();
                checktype(list, LispList);
                while (list !== null) {
                        checktype(list.car, LispCons);
                        if (eq(list.car.car, item)) return list.car;
                        list = list.cdr;
                }
                return null;
        });

        defp("%getf", false, function(m, nargs){
                checknargs(nargs, 2, 3);
                var not_found = nargs == 3 ? m.pop() : null;
                var item = m.pop(), list = m.pop();
                while (list !== null) {
                        checktype(list, LispList);
                        if (list.car === item) {
                                if (!list.cdr) error("Malformed plist");
                                return list.cdr.car;
                        }
                        if (!list.cdr) error("Malformed plist");
                        list = LispCons.cddr(list);
                }
                return not_found;
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

        (function(make, i){
                for (i in LispCons) if (HOP(LispCons, i) && /^c[ad]+r$/.test(i)) {
                        defp(i, false, make(LispCons[i]));
                }
        })(function(func){
                return function (m, nargs) {
                        checknargs(nargs, 1, 1);
                        var list = m.pop();
                        checktype(list, LispList);
                        return func(list);
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

        function list_(m, nargs) {
                var p = m.pop();
                checktype(p, LispList);
                while (--nargs > 0)
                        p = new LispCons(m.pop(), p);
                return p;
        };

        defp("list*", false, function(m, nargs) {
                checknargs(nargs, 1);
                return list_(m, nargs);
        });

        defp("append", false, function(m, nargs) {
                return nargs == 0 ? null : LispCons.append(m.stack.splice(-nargs));
        });

        defp("nconc", true, function(m, nargs){
                return nargs == 0 ? null : LispCons.nconc(m.stack.splice(-nargs));
        });

        defp("revappend", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var tail = m.pop(), list = m.pop();
                checktype(list, LispList);
                checktype(tail, LispList);
                return LispCons.revappend(list, tail);
        });

        defp("nreconc", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var tail = m.pop(), list = m.pop();
                checktype(list, LispList);
                checktype(tail, LispList);
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
                var a = [];
                while (nargs-- > 1) a[nargs - 1] = m.pop();
                var vector = m.pop();
                checktype(vector, LispArray);
                vector.push.apply(vector, a);
                return vector;
        });

        defp("vector-pop", true, function(m, nargs){
                checknargs(nargs, 1);
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
                if (start != null) checktype(start, LispNumber);
                else start = 0;
                if (end != null) checktype(end, LispNumber);
                else end = vector.length;
                return vector.slice(start, end);
        });

        defp("vector-ref", false, function(m, nargs){
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
                return from < str.length ? len != null ? str.substr(from, len) : str.substr(from) : null;
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

        defp("string-equal", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var a = m.pop(), b = m.pop();
                checktype(a, LispString);
                checktype(b, LispString);
                return a.toLowerCase() == b.toLowerCase() ? true : null;
        });

        defp("char-equal", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var a = m.pop(), b = m.pop();
                checktype(a, LispChar);
                checktype(b, LispChar);
                return a.value.toLowerCase() == b.value.toLowerCase() ? true : null;
        });

        // string/char comparators
        (function(defcmp){
                defcmp("=", new Function("a", "b", "return a==b"));
                defcmp("<=");
                defcmp(">=");
                defcmp("<");
                defcmp(">");
                defcmp("-equal", new Function("a", "b", "return a.toLowerCase()==b.toLowerCase()"));
        })(function(name, cmp){
                if (!cmp) cmp = new Function("a", "b", "return a" + name + "b");
                defp("char" + name, false, function(m, nargs){
                        checknargs(nargs, 1);
                        var prev = m.pop();
                        checktype(prev, LispChar);
                        var ret = true;
                        while (--nargs > 0) {
                                var el = m.pop();
                                checktype(el, LispChar);
                                if (ret && !cmp(el.value, prev.value)) ret = null;
                                prev = el;
                        }
                        return ret;
                });
                defp("string" + name, false, function(m, nargs){
                        checknargs(nargs, 1);
                        var prev = m.pop();
                        checktype(prev, LispString);
                        var ret = true;
                        while (--nargs > 0) {
                                var el = m.pop();
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

        defp("regexp", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                return LispRegexp.is(m.pop()) ? true : null;
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
                return x !== null && x !== true && LispSymbol.is(x) && x.pak === KW ? true : null;
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
                        if (LispSymbol.is(key)) key = LispSymbol.symname(key);
                        hash.set(key, values[i]);
                });
                return hash;
        });

        defp("hash-get", false, function(m, nargs){
                checknargs(nargs, 2, 3);
                var def = (nargs == 3) ? m.pop() : null;
                var key = as_string(m.pop()), hash = m.pop();
                checktype(key, LispString);
                checktype(hash, LispHash);
                var h = hash.has(key);
                if (h) return h.get(key);
                return def;
        });

        defp("hash-add", true, function(m, nargs){
                checknargs(nargs, 3, 3);
                var val = m.pop(), key = as_string(m.pop()), hash = m.pop();
                checktype(key, LispString);
                checktype(hash, LispHash);
                return hash.set(key, val);
        });

        defp("hash-set", true, function(m, nargs){
                checknargs(nargs, 3, 3);
                var val = m.pop(), key = as_string(m.pop()), hash = m.pop();
                checktype(key, LispString);
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

        defp("hash-extend", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var hash = m.pop();
                checktype(hash, LispHash);
                return hash.extend();
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
                var xhr = new XMLHttpRequest();
                url += "?killCache=" + Date.now(); // is this a good idea?
                xhr.open("GET", url, cont ? true : false);
                if (cont) xhr.onreadystatechange = function() {
                        if (xhr.readyState == 4)
                                m.call(cont, new LispCons(xhr.status == 200 ? xhr.responseText : null, null));
                };
                xhr.send(null);
                return cont ? null : xhr.status == 200 ? xhr.responseText : null;
        });

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

        defp("macroexpand-1", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var form = m.pop();
                if (!LispCons.is(form)) return form;
                var first = form.car;
                if (!first.macro()) return form;
                return m._callnext(first.macro(), LispCons.cdr(form));
        });

        defp("%macro", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var symbol = m.pop();
                checktype(symbol, LispSymbol);
                return symbol !== null && symbol !== true ? symbol.macro() : null;
        });

        defp("disassemble", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var func = m.pop();
                checktype(func, LispClosure);
                return LispMachine.disassemble(func.code);
        });

        defp("apply", true, function(m, nargs){
                checknargs(nargs, 2);
                var args = list_(m, nargs - 1);
                var func = m.pop();
                checktype(func, LispClosure);
                return m._callnext(func, args);
        });

        defp("%apply", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var args = m.pop();
                checktype(args, LispList);
                var func = m.pop();
                checktype(func, LispClosure);
                return m._callnext(func, args);
        });

        defp("%prim-apply", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var args = m.pop(), name = m.pop();
                checktype(name, LispSymbol);
                nargs = 0;
                while (args !== null) {
                        checktype(args, LispCons);
                        m.push(args.car);
                        args = args.cdr;
                        ++nargs;
                }
                return name.primitive()(m, nargs);
        });

        defp("%primitivep", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var sym = m.pop();
                checktype(sym, LispSymbol);
                return sym.primitive();
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
                sym.setv("function", null);
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

        defp("%make-package", true, function(m, nargs){
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

        defp("%make-symbol", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var name = m.pop();
                checktype(name, LispString);
                return new LispSymbol(name);
        });

        defp("%intern", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), name = m.pop();
                checktype(pak, LispPackage);
                checktype(name, LispString);
                var sym = pak.find_or_intern(name);
                return sym === S_NIL ? null : sym === S_T ? true : sym;
        });

        defp("%accessible-symbols", false, function(m, nargs){
                checknargs(nargs, 1, 2);
                var ext = nargs == 2 ? m.pop() : null;
                var pak = m.pop();
                checktype(pak, LispPackage);
                return pak.all_accessible(ext);
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

        defp("%find-symbol", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), name = as_string(m.pop());
                checktype(name, LispString);
                checktype(pak, LispPackage);
                var sym = pak.find(name);
                if (!sym) error("Symbol " + name + " not found in " + pak.name);
                return sym;
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

        defp("%find-package", false, function(m, nargs){
                checknargs(nargs, 1, 2);
                var noerr = nargs == 2 ? m.pop() : null;
                var name = m.pop();
                if (LispPackage.is(name)) return name;
                name = as_string(name);
                checktype(name, LispString);
                var pak = LispPackage.get_existing(name);
                if (!pak && !noerr) error("Package " + name + " not found");
                return pak;
        });

        defp("%export", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), syms = as_list(m.pop());
                if (!LispPackage.is(pak)) {
                        pak = LispPackage.get(as_string(pak));
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
                        pak = LispPackage.get(as_string(pak));
                }
                checktype(pak, LispPackage);
                LispCons.forEach(syms, function(sym){
                        pak.import(sym);
                });
                return pak;
        });

        defp("%shadow", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), syms = as_list(m.pop());
                checktype(pak, LispPackage);
                LispCons.forEach(syms, function(sym){
                        pak.shadow(as_string(sym));
                });
                return pak;
        });

        defp("%symbol-name", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var symbol = m.pop();
                checktype(symbol, LispSymbol);
                return LispSymbol.symname(symbol);
        });

        defp("%package-name", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var pak = m.pop();
                checktype(pak, LispPackage);
                return pak.name;
        });

        defp("%symbol-package", false, function(m, nargs){
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
                var sym = m.pop();
                checktype(sym, LispSymbol);
                return HOP(sym, "value") ? sym.value : null;
        });

        defp("symbol-function", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var sym = m.pop();
                checktype(sym, LispSymbol);
                return sym.func();
        });

        defp("set-symbol-function!", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var func = m.pop(), sym = m.pop();
                checktype(sym, LispSymbol);
                checktype(func, LispClosure);
                sym.setv("macro", null);
                return sym.setv("function", func);
        });

        defp("%set-symbol-prop", true, function(m, nargs){
                checknargs(nargs, 3, 3);
                var val = m.pop(), key = as_string(m.pop()), sym = m.pop();
                checktype(key, LispString);
                checktype(sym, LispSymbol);
                return sym.setv(key, val);
        });

        defp("%get-symbol-prop", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var key = as_string(m.pop()), sym = m.pop();
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

        defp("dom.sizzle", false, function(m, nargs){
                checknargs(nargs, 1, 2);
                var ctx = null;
                if (nargs == 2) {
                        ctx = m.pop();
                        checktype(ctx, LispDomElement);
                }
                var ctx = nargs == 2 ? m.pop() : document;
                var selector = m.pop();
                checktype(selector, LispString);
                return LispCons.fromArray(Sizzle(selector, ctx));
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
                // that's a hard error.
                throw m.pop();
        });

        defp("%warn", true, function(m, nargs){
                var a = [];
                while (nargs-- > 0) a.unshift(m.pop());
                console.warn(a.join(" "));
                return null;
        });

        /* -----[ other ]----- */

        defp("%debugger", true, function(m, nargs){
                debugger;
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
                return m.stack.slice();
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
                // hack: this function needs to return the assembled
                // code, so we push two instructions to do it.  we
                // need to copy the code (slice) in order to not
                // include these two instructions in the return value.
                code.push.apply(code, LispMachine.assemble([
                        [ "CONST", code.slice() ],
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
                checknargs(nargs, 1, 1);
                var code = m.pop();
                checktype(code, LispArray);
                return LispMachine.serialize(code, true);
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

        var HASH_SEED = 0xdeadbeef;
        defp("sxhash", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (!LispString.is(x)) {
                        if (LispNumber.is(x)) x = x.toString(2);
                        else if (LispChar.is(x)) x = x.value;
                        else if (LispSymbol.is(x)) x = LispSymbol.symname(x);
                        else if (LispPackage.is(x)) x = x.name;
                        else if (LispRegexp.is(x)) x = x.toString();
                        else error("I don't know how to generate hash for this object type");
                }
                return murmurhash3_32_gc(x);
        });

})(LispPackage.get("%"), LispPackage.get("KEYWORD"));
