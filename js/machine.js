var LispMachine = DEFCLASS("LispMachine", null, function(D, P){
        function LispRet(code, pc, env, denv) {
                this.code = code;
                this.pc = pc;
                this.env = env;
                this.denv = denv;
        };

        function LispCC(stack, denv) {
                this.stack = stack;
                this.denv = denv;
        };

        var LispBinding = DEFCLASS("LispBinding", null, function(D, P){
                P.INIT = function(symbol, value) {
                        this.symbol = symbol;
                        this.value = value;
                };
        });

        /// constructor
        P.INIT = function() {
                this.code = null;
                this.pc = null;
                this.stack = null;
                this.env = null;
                this.denv = null;
                this.n_args = null;
        };

        P.lvar = function(i, j) {
                var e = this.env;
                while (i-- > 0) e = e.cdr;
                return e.car[j];
        };
        P.lset = function(i, j, val) {
                var e = this.env;
                while (i-- > 0) e = e.cdr;
                e.car[j] = val;
        };
        P.find_dvar = function(symbol) {
                if (symbol.special()) {
                        var p = this.denv;
                        while (p != null) {
                                var el = p.car;
                                if (LispBinding.is(el) && el.symbol === symbol)
                                        return el;
                                p = p.cdr;
                        }
                }
                return symbol;
        };
        P.gvar = function(symbol) {
                return this.find_dvar(symbol).value;
        };
        P.gset = function(symbol, val) {
                this.find_dvar(symbol).value = val;
        };
        P.bind = function(symbol, i) {
                this.denv = new LispCons(
                        new LispBinding(symbol, this.env.car[i]),
                        this.denv
                );
        };
        P.push = function(v) {
                this.stack.push(v);
        };
        P.pop = function() {
                return this.stack.pop();
        };
        P.pop_number = function() {
                var n = this.pop();
                if (typeof n != "number") {
                        throw new Error("Number argument expected");
                }
                return n;
        };
        P.mkret = function(pc) {
                return new LispRet(this.code, pc, this.env, this.denv);
        };
        P.unret = function(ret) {
                this.code = ret.code;
                this.pc = ret.pc;
                this.env = ret.env;
                this.denv = ret.denv;
        };
        P.mkcont = function() {
                return new LispCC(this.stack.slice(), this.denv);
        };
        P.uncont = function() {
                var cont = this.top();
                this.stack = cont.stack.slice();
                this.denv = cont.denv;
        };
        P.top = function() {
                return this.stack[this.stack.length - 1];
        };
        P.run = function(code) {
                this.code = code;
                this.env = new LispCons([], null);
                this.stack = [ [] ];
                this.pc = 0;
                while (true) {
                        if (this.pc == null) return this.pop();
                        this.code[this.pc++].run(this);
                }
        };

        var OPS = {};

        D.assemble = function assemble(code) {
                var ret = [];
                for (var i = 0; i < code.length; ++i) {
                        var el = code[i];
                        if (el instanceof LispLabel) el.index = ret.length;
                        else ret.push(el);
                }
                for (var i = ret.length; --i >= 0;) {
                        var el = ret[i];
                        switch (el[0]) {
                            case "FN":
                                ret[i] = OPS.FN.make(assemble(el[1]));
                                break;
                            case "JUMP":
                            case "TJUMP":
                            case "FJUMP":
                            case "SAVE":
                                el[1] = el[1].index;
                            default:
                                ret[i] = OPS[el[0]].make.apply(null, el.slice(1));
                        }
                }
                return ret;
        };

        D.serialize = function(code) {
                return "[" + code.map(function(op){
                        return op._disp();
                }).join(",") + "]";
        };

        function serialize_const(val) {
                if (LispSymbol.is(val)) return val.serialize();
                if (val === null || val === true) return val + "";
                if (val instanceof Array) return "v(" + val.map(serialize_const).join(",") + ")";
                if (LispCons.is(val)) return "l(" + LispCons.toArray(val).map(serialize_const).join(",") + ")";
                if (LispRegexp.is(val)) return val.value.toString();
                if (typeof val == "string") return JSON.stringify(val);
                return val + "";
        };

        D.serialize_const = serialize_const;

        var OP = DEFCLASS("NOP", null, function(D, P){
                P._disp = function() {
                        var self = this;
                        return self._name + "(" + self._args.map(function(el){
                                return serialize_const(self[el]);
                        }).join(",") + ")";
                };
        });

        function defop(name, args, proto) {
                args = args ? args.split(" ") : [];
                var ctor = new Function(
                        "return function " + name + "(" + args.join(", ") + "){ " +
                                args.map(function(arg){
                                        return "this." + arg + " = " + arg;
                                }).join("; ") + "; this.INIT() };"
                )();
                ctor.prototype = new OP;
                ctor.make = new Function(
                        "OP",
                        "return function(" + args.join(",") + "){return new OP(" + args.join(",") + ")}"
                )(ctor);
                proto._name = name;
                proto._args = args;
                for (var i in proto) if (HOP(proto, i)) {
                        ctor.prototype[i] = proto[i];
                }
                OPS[name] = ctor;
        };

        [
                ["LVAR", "i j", {
                        run: function(m) {
                                m.push(m.lvar(this.i, this.j));
                        }
                }],
                ["LSET", "i j", {
                        run: function(m) {
                                m.lset(this.i, this.j, m.top());
                        }
                }],
                ["GVAR", "name", {
                        run: function(m) {
                                m.push(m.gvar(this.name));
                        }
                }],
                ["GSET", "name", {
                        run: function(m) {
                                m.gset(this.name, m.top());
                        }
                }],
                ["BIND", "name i", {
                        run: function(m) {
                                m.bind(this.name, this.i);
                        }
                }],
                ["POP", 0, {
                        run: function(m) {
                                m.pop();
                        }
                }],
                ["CONST", "val", {
                        run: function(m) {
                                m.push(this.val);
                        }
                }],
                ["JUMP", "addr", {
                        run: function(m) {
                                m.pc = this.addr;
                        }
                }],
                ["TJUMP", "addr", {
                        run: function(m) {
                                if (m.pop() !== null) m.pc = this.addr;
                        }
                }],
                ["FJUMP", "addr", {
                        run: function(m) {
                                if (m.pop() === null) m.pc = this.addr;
                        }
                }],
                ["SETCC", 0, {
                        run: function(m) {
                                m.uncont();
                        }
                }],
                ["SAVE", "addr", {
                        run: function(m) {
                                m.push(m.mkret(this.addr));
                        }
                }],
                ["RET", 0, {
                        run: function(m) {
                                var val = m.pop();
                                m.unret(m.pop());
                                m.push(val);
                        }
                }],
                ["CALL", "count", {
                        run: function(m){
                                m.n_args = this.count;
                                var closure = m.pop();
                                m.code = closure.code; m.env = closure.env; m.pc = 0;
                        }
                }],
                ["ARGS", "count", {
                        run: function(m){
                                var count = this.count;
                                if (m.n_args != count) {
                                        throw new Error("Wrong number of arguments");
                                }
                                var frame;
                                // for a small count, pop is *much* more
                                // efficient than splice; in FF the difference is enormous.
                                switch (count) {
                                    case 0: frame = []; break;
                                    case 1: frame = [ m.pop() ]; break;
                                    case 2: frame = m.pop(); frame = [ m.pop(), frame ]; break;
                                    case 3: var c = m.pop(), b = m.pop(), a = m.pop(); frame = [ a, b, c ]; break;
                                    case 4: var d = m.pop(), c = m.pop(), b = m.pop(), a = m.pop(); frame = [ a, b, c, d ]; break;
                                    default: frame = m.stack.splice(m.stack.length - count, count);
                                }
                                m.env = new LispCons(frame, m.env);
                        }
                }],
                ["ARG_", "count", {
                        run: function(m) {
                                var count = this.count;
                                var passed = m.n_args;
                                if (passed < count) throw new Error("Insufficient number of arguments");
                                var frame = [];
                                var p = null;
                                while (passed > count) {
                                        p = new LispCons(m.pop(), p);
                                        passed--;
                                }
                                frame[count] = p;
                                while (--count >= 0) {
                                        frame[count] = m.pop();
                                }
                                m.env = new LispCons(frame, m.env);
                        }
                }],
                ["FN", "code", {
                        run: function(m) {
                                m.push(new LispClosure(this.code, m.env));
                        },
                        _disp: function() {
                                return "FN(" + D.serialize(this.code) + ")";
                        }
                }],
                ["PRIM", "name nargs", {
                        run: function(m) {
                                m.push(this.func(m, this.nargs));
                        },
                        INIT: function() {
                                var prim = LispPrimitive(this.name);
                                this.func = prim.func;
                        }
                }]

        ].map(function(_){ defop(_[0], _[1], _[2]) });

        defop("CC", 0, {
                run: (function(cc){
                        return function(m) {
                                m.push(new LispClosure(cc, new LispCons([ m.mkcont() ], null)));
                        }
                })(
                        D.assemble([
                                ["ARGS", 1],
                                ["LVAR", 1, 0],
                                ["SETCC"],
                                ["LVAR", 0, 0],
                                ["RET"]
                        ])
                )
        });

        D.dump = function(thing) {
                if (thing === null) return "NIL";
                if (thing === true) return "T";
                if (typeof thing == "string") return JSON.stringify(thing);
                if (LispPackage.is(thing)) return thing.name;
                if (LispSymbol.is(thing)) return thing.name;
                if (LispCons.is(thing)) {
                        var ret = "(", first = true;
                        while (thing !== null) {
                                if (!first) ret += " ";
                                else first = false;
                                ret += D.dump(LispCons.car(thing));
                                thing = LispCons.cdr(thing);
                                if (!LispCons.isList(thing)) {
                                        ret += " . " + D.dump(thing);
                                        break;
                                }
                        }
                        return ret + ")";
                }
                if (LispType.is(thing)) return thing.print();
                return thing + "";
        };

});

function LispLabel(name) {
        this.name = name;
        this.index = null;
};
LispLabel.prototype.toString = function() {
        return this.name;
};
