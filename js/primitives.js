(function(CL){

        var CURRENT = null;
        var MACHINE = null;

        function defp(name, seff, func) {
                name = name.toUpperCase();
                var sym = CL.intern(name);
                CL.export(sym);
                sym.set("primitive", function(m, nargs){
                        CURRENT = name;
                        MACHINE = name;
                        return func(m, nargs);
                });
                sym.set("primitive-side-effects", seff);
        };

        /// utilities

        function error(msg) {
                throw new Error(msg + ", in " + CURRENT);
        };

        function checknargs(n, min, max) {
                if (min != null && n < min) error("Not enough arguments");
                if (max != null && n > max) error("Too many arguments");
        };

        function checktype(x, type) {
                if (typeof type == "string") {
                        if (typeof x != type)
                                error("Invalid type, expecting " + type + ", got: " + LispMachine.dump(x));
                }
                else if (!type.is(x)) error("Invalid type, expecting " + type.type + ", got: " + LispMachine.dump(x));
        };

        /// primitive definitions

        /* -----[ conditionals ]----- */

        defp("eq", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                return m.pop() === m.pop() ? true : null;
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
                if (typeof x == "string") return x.length;
                if (LispArray.is(x)) return x.length();
                error("Unrecognized sequence");
        });

        defp("elt", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var i = m.pop(), x = m.pop();
                checktype(i, "number");
                if (LispCons.isList(x)) return LispCons.elt(x, i);
                if (LispArray.is(x)) return x.elt(i) || null;
                if (typeof x == "string") return LispChar.get(x.charAt(i)) || null;
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

        defp("reverse", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (LispList.is(x)) return LispCons.reverse(x);
                if (LispArray.is(x)) return new LispArray(x.value.slice().reverse());
                if (typeof x == "string") {
                        for (var i = x.length, ret = ""; --i >= 0;) ret += x.charAt(i);
                        return ret;
                }
                error("Unrecognized sequence");
        });

        (function(N){
                defp("gensym", false, function(m, nargs) {
                        checknargs(nargs, 0, 1);
                        var name = "SYM";
                        if (nargs == 1) {
                                name = m.pop();
                                checktype(name, "string");
                        }
                        return new LispSymbol(name + (++N));
                });
        })(0);

        var LispList = {
                is: LispCons.isList,
                type: "list"
        };

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

        defp("clog", true, function(m, nargs){
                var a = [];
                while (nargs-- > 0) a.unshift(m.pop());
                console.log(a.map(LispMachine.dump).join(" "));
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

        defp("append", false, function(m, nargs) {
                var p = null;
                while (nargs-- > 0) {
                        if (p === null) p = m.pop();
                        else {
                                var last = null;
                                var list = m.pop();
                                if (list !== null) {
                                        list = LispCons.map(list, function(x, i, dot, cell){
                                                if (dot) error("Improper list");
                                                last = cell;
                                                return x;
                                        });
                                        last.cdr = p;
                                        p = list;
                                }
                        }
                }
                return p;
        });

        /* -----[ arrays ]----- */

        defp("vector", false, function(m, nargs){
                var a = [];
                while (--nargs >= 0) a[nargs] = m.pop();
                return new LispArray(a);
        });

        defp("as-vector", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var list = m.pop();
                checktype(list, LispList);
                return new LispArray(LispCons.toArray(list));
        });

        /* -----[ strings ]----- */

        defp("strcat", false, function(m, nargs){
                var ret = "";
                while (nargs-- > 0) {
                        var arg = m.pop();
                        switch (typeof arg) {
                            case "string":
                            case "number":
                                ret = arg + ret;
                                break;
                            default:
                                if (LispChar.is(arg)) ret = arg.value + ret;
                                else error("Unrecognized argument type");
                        }
                }
                return ret;
        });

        defp("substr", false, function(m, nargs){
                checknargs(nargs, 2, 3);
                if (nargs == 3) {
                        var len = m.pop();
                        checktype(len, "number");
                }
                var from = m.pop(), str = m.pop();
                checktype(from, "number");
                checktype(str, "string");
                return from < str.length ? len != null ? str.substr(from, len) : str.substr(from) : null;
        });

        defp("downcase", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (typeof x == "string") return x.toLowerCase();
                if (LispChar.is(x)) return LispChar.get(x.value.toLowerCase());
                error("Unsupported argument type");
        });

        defp("upcase", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                if (typeof x == "string") return x.toUpperCase();
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
                checktype(name, "string");
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
                checktype(code, "number");
                return LispChar.fromCode(code);
        });

        // string/char comparators
        (function(defcmp){
                defcmp("=", new Function("a", "b", "return a==b"));
                defcmp("<=");
                defcmp(">=");
                defcmp("<");
                defcmp(">");
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
                        checktype(prev, "string");
                        var ret = true;
                        while (--nargs > 0) {
                                var el = m.pop();
                                checktype(el, "string");
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
                        checktype(mods, "string");
                }
                var str = m.pop();
                checktype(str, "string");
                try {
                        return new LispRegexp(new RegExp(str, mods));
                } catch(ex) {
                        error("Invalid regexp (" + ex + ")");
                }
        });

        defp("regexp-test", false, function(m, nargs){
                checknargs(nargs, 2);
                var str = m.pop(), rx = m.pop();
                checktype(str, "string");
                checktype(rx, LispRegexp);
                return rx.test(str);
        });

        defp("regexp-exec", false, function(m, nargs){
                checknargs(nargs, 2);
                var str = m.pop(), rx = m.pop();
                checktype(str, "string");
                checktype(rx, LispRegexp);
                return rx.exec(str);
        });

        /* -----[ types ]----- */

        defp("stringp", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                return typeof m.pop() == "string" ? true : null;
        });

        defp("charp", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                return LispChar.is(m.pop()) ? true : null;
        });

        defp("numberp", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                return typeof m.pop() == "number" ? true : null;
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
                return LispSymbol.is(m.pop()) ? true : null;
        });

        defp("zerop", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                checktype(x, "number");
                return x == 0 ? true : null;
        });

        defp("parse-number", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var x = m.pop();
                checktype(x, "string");
                var ret = parseFloat(x);
                return isNaN(ret) ? null : ret;
        });

        defp("parse-integer", false, function(m, nargs){
                checknargs(nargs, 1, 2);
                var radix = nargs == 2 ? m.pop() : 10;
                checktype(radix, "number");
                var x = m.pop();
                checktype(x, "string");
                var ret = parseInt(x, radix);
                return isNaN(ret) ? null : ret;
        });

        /* -----[ macros ]----- */

        defp("macroexpand-1", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var form = m.pop();
                if (!LispCons.is(form)) return form;
                var first = form.car;
                if (!first.macro()) return form;
                m._callnext(first.macro(), LispCons.cdr(form));
                return false;
        });

        defp("%macrop", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var symbol = m.pop();
                checktype(symbol, LispSymbol);
                return symbol.macro();
        });

        /* -----[ symbols, packages ]----- */

        defp("%special", false, function(m, nargs){
                checknargs(nargs, 1);
                while (nargs-- > 0) {
                        var name = m.pop();
                        checktype(name, LispSymbol);
                        name.set("special", true);
                }
                return null;
        });

        defp("%set-function-name", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var symbol = m.pop(), f = m.pop();
                checktype(symbol, LispSymbol);
                checktype(f, LispClosure);
                f.name = symbol;
                return f;
        });

        defp("%function-name", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var f = m.pop();
                checktype(f, LispClosure);
                return f.name;
        });

        defp("%make-package", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var name = m.pop();
                checktype(name, "string");
                return LispPackage.get(name);
        });

        defp("%make-symbol", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var name = m.pop();
                checktype(name, "string");
                return new LispSymbol(name);
        });

        defp("%intern", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), name = m.pop();
                checktype(pak, LispPackage);
                checktype(name, "string");
                return pak.find_or_intern(name);
        });

        defp("%find-symbol", false, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), name = m.pop();
                checktype(pak, LispPackage);
                checktype(name, "string");
                return pak.find(name);
        });

        defp("%find-package", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var name = m.pop();
                checktype(name, "string");
                return LispPackage.get(name);
        });

        defp("%export", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var pak = m.pop(), symbol = m.pop();
                checktype(pak, LispPackage);
                checktype(symbol, LispSymbol);
                return pak.export(symbol);
        });

        defp("%symbol-package", false, function(m, nargs){
                checknargs(nargs, 1, 1);
                var symbol = m.pop();
                checktype(symbol, LispSymbol);
                return symbol.pak;
        });

        defp("%use-package", true, function(m, nargs){
                checknargs(nargs, 2, 2);
                var current = m.pop(), imported = m.pop();
                checktype(current, LispPackage);
                checktype(imported, LispPackage);
                return current.use(imported);
        });

        /* -----[ conditions ]----- */

        // pretty sucky, need to think how to do it properly
        defp("%error", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var msg = m.pop();
                checktype(msg, "string");
                error(msg);
        });

        /* -----[ other ]----- */

        defp("%js-eval", true, function(m, nargs){
                checknargs(nargs, 1, 1);
                var code = m.pop();
                checktype(code, "string");
                var func = new Function("machine", "return(" + code + ")");
                var ret = func(m);
                if (ret instanceof Array) return new LispArray(ret);
                if (typeof ret == "boolean") return ret ? true : null; // avoid false
                return ret;
        });

})(LispPackage.get("%"));
