function LispCons(a, b) {
        this.car = a;
        this.cdr = b;
};
LispCons.prototype = {
        type: "cons",
        display: function() {
        }
};

function LispClosure(code, env) {
        this.name = null;
        this.code = code;
        this.env = env;
};
LispClosure.prototype = {
        type: "function",
        display: function() {
                return "<function" + (this.name ? " " + this.name : "") + ">";
        }
};

function LispRet(code, pc, env) {
        this.code = code;
        this.pc = pc;
        this.env = env;
};

function LispMachine(glob_env) {
        this.glob_env = glob_env || {};
        this.code = null;
        this.pc = null;
        this.stack = null;
        this.env = null;
        this.n_args = null;
};
LispMachine.prototype = {
        lvar: function(i, j) {
                var e = this.env;
                while (i-- > 0) e = e.cdr;
                return e.car[j];
        },
        lset: function(i, j, val) {
                var e = this.env;
                while (i-- > 0) e = e.cdr;
                e.car[j] = val;
        },
        gvar: function(name) {
                return this.glob_env[name];
        },
        gset: function(name, val) {
                this.glob_env[name] = val;
        },
        push: function(v) {
                this.stack.push(v);
        },
        pop: function() {
                return this.stack.pop();
        },
        top: function() {
                return this.stack[this.stack.length - 1];
        },
        run_threaded: function(code, onfinish) {
                var self = this;
                self.code = code;
                self.env = null;
                self.stack = [ [] ];
                self.pc = 0;
                var quota = 1000;
                function runit() {
                        for (var i = quota; --i > 0;) {
                                if (self.pc == null) return onfinish(self.pop());
                                self.code[self.pc++].run(self);
                        }
                        setTimeout(runit, 0);
                };
                runit();
        },
        run: function(code) {
                this.code = code;
                this.env = new LispCons([], null);
                this.stack = [ [] ];
                this.pc = 0;
                while (true) {
                        if (this.pc == null) return this.pop();
                        this.code[this.pc++].run(this);
                }
        }
};

LispMachine.OPS = {};

LispMachine.prototype.assemble = function assemble(code) {
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
                        ret[i] = new LispMachine.OPS.FN(assemble(el[1]));
                        break;
                    case "JUMP":
                    case "TJUMP":
                    case "FJUMP":
                    case "SAVE":
                        el[1] = el[1].index;
                    default:
                        ret[i] = new LispMachine.OPS[el[0]](el[1], el[2]);
                }
        }
        return ret;
};

LispMachine.defop = function(name, args, proto) {
        args = args ? args.split(" ") : [];
        var ctor = new Function(
                "return function " + name + "(" + args.join(", ") + "){ " +
                        args.map(function(arg){
                                return "this." + arg + " = " + arg;
                        }).join("; ") + "; this._init() };"
        )();
        ctor.prototype = {
                _name: name,
                _init: noop
        };
        for (var i in proto) if (HOP(proto, i)) {
                ctor.prototype[i] = proto[i];
        }
        LispMachine.OPS[name] = ctor;
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

        ["SAVE", "addr", {
                run: function(m) {
                        m.push(new LispRet(m.code, this.addr, m.env));
                }
        }],

        ["RET", 0, {
                run: function(m) {
                        var val = m.pop(), r = m.pop();
                        m.code = r.code; m.pc = r.pc; m.env = r.env;
                        m.push(val);
                }
        }],

        ["CALLJ", "count", {
                run: function(m){
                        m.n_args = this.count;
                        var a = m.pop();
                        m.code = a.code; m.env = a.env; m.pc = 0;
                }
        }],

        ["ARGS", "count", {
                run: function(m){
                        var count = this.count;
                        if (m.n_args != count) throw new Error("Wrong number of arguments");
                        var frame;
                        // for a small count, pop is *much* more
                        // efficient than splice; in FF the difference enormous.
                        switch (count) {
                            case 0: frame = []; break;
                            case 1: frame = [ m.pop() ]; break;
                            case 2: frame = m.pop(); frame = [ m.pop(), frame ]; break;
                            case 3: var c = m.pop(), b = m.pop(), a = m.pop(); frame = [ a, b, c ]; break;
                            default: frame = m.stack.splice(m.stack.length - count, count);
                        }
                        m.env = new LispCons(frame, m.env);
                }
        }],

        ["FN", "code", {
                run: function(m) {
                        m.push(new LispClosure(this.code, m.env));
                }
        }],

        ["PRIM", "name func", {
                run: function(m) {
                        m.push(this.func(m));
                },
                _init: function() {
                        var prim = LispPrimitive(this.name);
                        this.func = prim.func;
                }
        }]

].map(function(_){ LispMachine.defop(_[0], _[1], _[2]) });
