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

        get.is = function(name, env, args) {
                return !find_var(name, env) && HOP(PR, name);
        };
        get.seff = function(name) {
                return PR[name] && PR[name].seff;
        };
        get.def = prim;
        get.PR = PR;
        return get;
})();

LispPrimitive.def("+", false, function(a, b){ return new LispNumber(a.value + b.value) });
LispPrimitive.def("-", false, function(a, b){ return new LispNumber(a.value - b.value) });
LispPrimitive.def("*", false, function(a, b){ return new LispNumber(a.value * b.value) });
LispPrimitive.def("/", false, function(a, b){ return new LispNumber(a.value / b.value) });
LispPrimitive.def("<", false, function(a, b){ return a.value < b.value ? true : null });
LispPrimitive.def("<=", false, function(a, b){ return a.value <= b.value ? true : null });
LispPrimitive.def(">", false, function(a, b){ return a.value > b.value ? true : null });
LispPrimitive.def(">=", false, function(a, b){ return a.value >= b.value ? true : null });
LispPrimitive.def("=", false, function(a, b){ return a.value == b.value ? true : null });
LispPrimitive.def("/=", false, function(a, b){ return a.value != b.value ? true : null });
LispPrimitive.def("eq", false, function(a, b){ return a === b ? true : null });
LispPrimitive.def("cons", false, function(a, b){ return new LispCons(a, b) });
LispPrimitive.def("listp", false, function(x){ return LispCons.isList(x) ? true : null });
LispPrimitive.def("consp", false, function(x){ return LispCons.is(x) ? true : null });
LispPrimitive.def("nullp", false, function(x){ return x === null ? true : null });
LispPrimitive.def("not", false, function(x){ return x === null ? true : null });

(function(N){
        LispPrimitive.PR.GENSYM = {
                name: "GENSYM",
                seff: false,
                func: function(m, nargs) {
                        if (nargs > 1) throw new Error("Too many arguments in GENSYM");
                        var name = (nargs == 0 ? "SYM" : name) + (++N);
                        return new LispSymbol(name);
                }
        };
})(0);

(function(make, i){
        for (i in LispCons) if (HOP(LispCons, i) && /^c[ad]+r$/.test(i)) {
                var name = i.toUpperCase();
                LispPrimitive.PR[name] = {
                        name: name,
                        args: [ "x" ],
                        seff: false,
                        func: make(LispCons[i])
                };
        }
})(function(func){
        return function (m) {
                return func(m.pop());
        };
});

LispPrimitive.def("clog", true, function(o){ console.log(o) });

LispPrimitive.PR.LIST = {
        name: "LIST",
        seff: false,
        func: function(m, nargs) {
                var a = [];
                while (nargs-- > 0) {
                        a.unshift(m.pop());
                }
                return LispCons.fromArray(a);
        }
};

LispPrimitive.PR.APPEND = {
        name: "APPEND",
        seff: false,
        func: function(m, nargs) {
                var a = [];
                while (nargs-- > 0) {
                        a.unshift.apply(a, LispCons.toArray(m.pop()));
                }
                return LispCons.fromArray(a);
        }
};
