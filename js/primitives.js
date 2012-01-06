var LispPrimitive = (function(){
        var PR = {};

        function find_var(name, env) {
                for (var i = 0; i < env.length; ++i) {
                        var frame = env[i];
                        for (var j = 0; j < frame.length; ++j) {
                                if (frame[j] == name)
                                        return [ i, j ];
                        }
                }
        };

        function get(name) { return PR[name] };
        function prim(name, seff, func) {
                var m = /function.*\(\s*((.|\n)*?)\s*\)\s*\{((.|\n)*)\}/.exec(func);
                name = name.toUpperCase();
                var args = m[1].split(/\s*,\s*/);
                var code = m[3];
                args.reverse();
                code = "var " + args.map(function(arg){
                        return arg + "=$.pop()";
                }).join(",") + ";\n" + code;
                func = new Function("$", code);
                PR[name] = {
                        name : name,
                        args : args,
                        seff : seff,
                        func : func
                };
        };

        function prim2(name, seff, func) {
                name = name.toUpperCase();
                LispPrimitive.PR[name] = {
                        name: name,
                        seff: seff,
                        func: func
                };
        };

        get.is = function(name, env) {
                return !find_var(name, env) && HOP(PR, name);
        };
        get.seff = function(name) {
                return PR[name] && PR[name].seff;
        };
        get.def = prim;
        get.def2 = prim2;
        get.PR = PR;
        return get;
})();

LispPrimitive.def("=", false, function(a, b){ return a.value == b.value ? true : null });
LispPrimitive.def("/=", false, function(a, b){ return a.value != b.value ? true : null });
LispPrimitive.def("eq", false, function(a, b){ return a === b ? true : null });

LispPrimitive.def2("<", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in <");
        var prev = m.pop_number();
        while (--nargs > 0) {
                if (!(m.pop_number() < prev)) return null;
        }
        return true;
});

LispPrimitive.def2("<=", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in <=");
        var prev = m.pop_number();
        while (--nargs > 0) {
                if (!(m.pop_number() <= prev)) return null;
        }
        return true;
});

LispPrimitive.def2(">", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in >");
        var prev = m.pop_number();
        while (--nargs > 0) {
                if (!(m.pop_number() > prev)) return null;
        }
        return true;
});

LispPrimitive.def2(">=", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in >=");
        var prev = m.pop_number();
        while (--nargs > 0) {
                if (!(m.pop_number() >= prev)) return null;
        }
        return true;
});

LispPrimitive.def2("=", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in =");
        var prev = m.pop_number();
        while (--nargs > 0) {
                if (!(m.pop_number() == prev)) return null;
        }
        return true;
});

LispPrimitive.def2("/=", false, function(m, nargs){
        if (nargs == 0) throw new Error("Invalid number of arguments in /=");
        var a = [];
        while (nargs-- > 0) a.push(m.pop_number());
        for (var i = a.length; --i >= 0;) {
                for (var j = i; --j >= 0;) {
                        if (a[i] == a[j]) return null;
                }
        }
        return true;
});

LispPrimitive.def2("+", false, function(m, nargs){
        var ret = 0;
        while (nargs-- > 0) {
                ret += m.pop_number();
        }
        return new LispNumber(ret);
});

LispPrimitive.def2("*", false, function(m, nargs){
        var ret = 1;
        while (nargs-- > 0) {
                ret *= m.pop_number();
        }
        return new LispNumber(ret);
});

LispPrimitive.def2("-", false, function(m, nargs){
        var i = nargs;
        if (i == 0) throw new Error("Invalid number of arguments in -");
        var a = [];
        while (--i >= 0) {
                a[i] = m.pop_number();
        }
        var ret = a[++i];
        if (nargs == 1) ret = -ret;
        while (++i < nargs) {
                ret = ret - a[i];
        }
        return new LispNumber(ret);
});

LispPrimitive.def2("/", false, function(m, nargs){
        var i = nargs;
        if (i == 0) throw new Error("Invalid number of arguments in /");
        var a = [];
        while (--i >= 0) {
                a[i] = m.pop_number();
        }
        var ret = a[++i];
        if (nargs == 1) ret = 1/ret;
        while (++i < nargs) {
                ret = ret / a[i];
        }
        return new LispNumber(ret);
});

LispPrimitive.def("cons", false, function(a, b){ return new LispCons(a, b) });
LispPrimitive.def("listp", false, function(x){ return LispCons.isList(x) ? true : null });
LispPrimitive.def("consp", false, function(x){ return LispCons.is(x) ? true : null });
LispPrimitive.def("nullp", false, function(x){ return x === null ? true : null });
LispPrimitive.def("not", false, function(x){ return x === null ? true : null });
LispPrimitive.def("length", false, function(x){
        if (LispCons.isList(x)) return new LispNumber(LispCons.len(x));
        if (LispString.is(x)) return new LispNumber(x.value.length);
        if (LispArray.is(x)) return new LispNumber(x.length());
        throw new Error("Unrecognized sequence in LENGTH");
});
LispPrimitive.def("elt", false, function(x, i){
        if (!LispNumber.is(i)) throw new Error("ELT expects a numeric index");
        if (LispCons.isList(x)) return LispCons.elt(x, i);
        if (LispArray.is(x)) return x.elt(i) || null;
        if (LispString.is(x)) return x.value.charAt(i) || null;
        throw new Error("Unrecognized sequence in ELT");
});

LispPrimitive.def2("special", false, function(m, nargs) {
        while (nargs-- > 0) {
                var name = m.pop();
                if (!LispSymbol.is(name)) throw new Error("SPECIAL expects only symbol arguments");
                name.set("special", true);
        }
        return null;
});

(function(N){
        LispPrimitive.def2("gensym", false, function(m, nargs) {
                if (nargs > 1) throw new Error("Too many arguments in GENSYM");
                var name = (nargs == 0 ? "SYM" : name) + (++N);
                return new LispSymbol(name);
        });
})(0);

(function(make, i){
        for (i in LispCons) if (HOP(LispCons, i) && /^c[ad]+r$/.test(i)) {
                LispPrimitive.def2(i, false, make(LispCons[i]));
        }
})(function(func){
        return function (m) {
                return func(m.pop());
        };
});

LispPrimitive.def("clog", true, function(o){ console.log(LispMachine.dump(o)); return null; });

LispPrimitive.def2("list", false, function(m, nargs) {
        var p = null;
        while (nargs-- > 0)
                p = new LispCons(m.pop(), p);
        return p;
});

LispPrimitive.def2("append", false, function(m, nargs) {
        var p = null;
        while (nargs-- > 0) {
                if (p === null) p = m.pop();
                else {
                        var last = null;
                        var list = m.pop();
                        if (list !== null) {
                                list = LispCons.map(list, function(x, i, dot, cell){
                                        if (dot) throw new Error("Improper list in APPEND");
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

///// symbols and packages

LispPrimitive.def("make-package", true, function(name){
        if (!LispString.is(name)) throw new Error("MAKE-PACKAGE expects a string");
        return LispPackage.get(name);
});

LispPrimitive.def("make-symbol", true, function(name){
        if (!LispString.is(name)) throw new Error("MAKE-SYMBOL expects a string");
        return new LispSymbol(name);
});

LispPrimitive.def("intern", true, function(name, pak){
        if (!LispString.is(name)) throw new Error("INTERN expects a string name");
        if (!LispPackage.is(pak)) throw new Error("PAK must be a package");
        return pak.find_or_intern(name);
});

LispPrimitive.def("find-symbol", false, function(name, pak){
        if (!LispString.is(name)) throw new Error("FIND-SYMBOL expects a string name");
        if (!LispPackage.is(pak)) throw new Error("PAK must be a package");
        return pak.find(name);
});
