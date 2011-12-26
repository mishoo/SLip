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
                var m = /function\s*\(\s*((.|\n)*?)\s*\)\s*\{((.|\n)*)\}/.exec(func);
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
                return !find_var(name, env) && HOP(PR, name) && PR[name].args.length == args;
        };
        get.seff = function(name) {
                return PR[name] && PR[name].seff;
        };
        get.def = prim;
        return get;
})();

LispPrimitive.def("+", false, function(a, b){ return a + b });
LispPrimitive.def("-", false, function(a, b){ return a - b });
LispPrimitive.def("*", false, function(a, b){ return a * b });
LispPrimitive.def("/", false, function(a, b){ return a / b });
LispPrimitive.def("=", false, function(a, b){ return a == b ? true : null });
LispPrimitive.def("eq", false, function(a, b){ return a === b ? true : null });
LispPrimitive.def("cons", false, function(a, b){ return new LispCons(a, b) });
LispPrimitive.def("car", false, function(c){ return c.car });
LispPrimitive.def("cdr", false, function(c){ return c.cdr });



LispPrimitive.def("clog", true, function(o){ console.log(o) });
