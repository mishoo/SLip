(function(CL){

        var CURRENT = null;

        function defp(name, seff, func) {
                name = name.toUpperCase();
                var sym = CL.intern(name);
                CL.export(sym);
                sym.set("primitive", function(m, nargs){
                        CURRENT = name;
                        return func(m, nargs);
                });
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
                                error("Invalid type, expecting " + type);
                }
                else if (!type.is(x)) error("Invalid type, expecting " + type.type);
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
                if (typeof x == "string") return x.charAt(i) || null;
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
                while (nargs-- > 0)
                        console.log(LispMachine.dump(m.pop()));
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

})(LispPackage.get("COMMON-LISP"));
