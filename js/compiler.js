// basic stream

function InputStream(text) {
        this.pos = 0;
        this.line = 0;
        this.col = 0;
        this.text = text;
        this.len = text.length;
};

InputStream.prototype = {
        peek: function() {
                if (this.pos < this.len)
                        return this.text.charAt(this.pos);
                return null;
        },
        next: function() {
                if (this.pos < this.len) {
                        var ch = this.text.charAt(this.pos++);
                        if (ch == "\n") {
                                ++this.line;
                                this.col = 0;
                        } else {
                                ++this.col;
                        }
                        return ch;
                }
                return null;
        }
};

////////////////// basic parser
function lisp_reader(code) {
        var input = new InputStream(code);
        var list = LispCons.fromArray;
        function next() { return input.next(); };
        function peek() { return input.peek(); };
        function croak(msg) {
                throw new Error(msg
                                + " / line: " + input.line
                                + ", col: " + input.col
                                + ", pos: " + input.pos);
        };
        function read_while(pred) {
                var buf = "", ch;
                while ((ch = peek()) && pred(ch)) {
                        buf += next();
                }
                return buf;
        };
        function skip_ws() {
                read_while(function(ch){
                        switch (ch) {
                            case " ":
                            case "\n":
                            case "\t":
                            case "\x0C":
                            case "\u2028":
                            case "\u2029":
                            case "\xA0":
                                return true;
                        }
                });
        };
        function skip(expected) {
                if (next() != expected)
                        croak("Expecting " + expected);
        };
        function read_escaped(start, end, inces) {
                skip(start);
                var escaped = false;
                var str = "";
                while (peek()) {
                        var ch = next();
                        if (escaped) {
                                str += ch;
                                escaped = false;
                        } else if (ch == "\\") {
                                if (inces) str += ch;
                                escaped = true;
                        } else if (ch == end) {
                                break;
                        } else {
                                str += ch;
                        }
                }
                return str;
        };
        function read_string() {
                return read_escaped("\"", "\"");
        };
        function read_regexp() {
                var str = read_escaped("/", "/", true);
                var mods = read_while(function(ch){
                        switch (ch.toLowerCase()) {
                            case "y":
                            case "m":
                            case "g":
                            case "i":
                                return true;
                        }
                }).toLowerCase();
                return new RegExp(str, mods);
        };
        function skip_comment() {
                read_while(function(ch){ return ch && ch != "\n" });
        };
        function read_symbol() {
                var str = read_while(function(ch){
                        if (UNICODE.letter.test(ch) ||
                            (ch >= "0" && ch <= "9"))
                                return true;
                        switch (ch) {
                            case "%": case "$": case "_": case "-":
                            case ":": case ".": case "+": case "*":
                            case "@": case "!": case "?": case "&":
                            case "=": case "<": case ">":
                            case "[": case "]":
                            case "{": case "}":
                            case "/":
                                return true;
                        }
                });
                if (str.length > 0 && /^-?[0-9]*\.?[0-9]*$/.test(str)) {
                        var ret = parseFloat(str);
                        if (!isNaN(ret)) return ret;
                }
                str = str.toUpperCase();
                var m = /^(.*?)::?(.*)$/.exec(str);
                if (m) {
                        var pak = LispPackage.get(m[1] || "KEYWORD");
                        return pak.find_or_intern(m[2]);
                }
                var pak = LispPackage.get("%").intern("*PACKAGE*");
                if (pak.value) return pak.value.find_or_intern(str);
                return LispSymbol.get(str);
        };
        function read_char() {
                var ch = next() + read_while(function(ch){
                        return (ch >= "a" && ch <= "z") ||
                                (ch >= "A" && ch <= "z") ||
                                (ch >= "0" && ch <= "9") ||
                                ch == "-" || ch == "_";
                });
                if (ch.length > 1) {
                        if (/^U[0-9a-f]{4}$/i.test(ch)) {
                                ch = LispChar.fromCode(parseInt(ch.substr(1), 16));
                        } else {
                                ch = LispChar.fromName(ch);
                                if (ch == null)
                                        croak("Unknown character name: " + ch);
                        }
                        return ch;
                }
                return LispChar.get(ch);
        };
        function read_sharp() {
                skip("#");
                switch (peek()) {
                    case "\\": next(); return read_char();
                    case "/": return read_regexp();
                    case "(": return new LispCons(LispSymbol.get("VECTOR"), read_list());
                    case "'": next(); return LispCons.fromArray([ LispSymbol.get("FUNCTION"), read_token() ]);
                    default:
                        croak("Unsupported sharp syntax: #" + peek());
                }
        };
        function read_quote() {
                skip("'");
                return list([ LispSymbol.get("QUOTE"), read_token() ]);
        };
        var in_qq = 0;
        function read_quasiquote() {
                skip("`");
                skip_ws();
                // if (peek() != "(")
                //         return list([ LispSymbol.get("QUOTE"), read_token() ]);
                ++in_qq;
                var ret = list([ LispSymbol.get("QUASIQUOTE"), read_token() ]);
                --in_qq;
                return ret;
        };
        function read_comma() {
                if (in_qq == 0) croak("Comma outside quasiquote");
                skip(",");
                skip_ws();
                var ret;
                --in_qq;
                if (peek() == "@") {
                        next();
                        ret = list([ LispSymbol.get("QQ-SPLICE"), read_token() ]);
                }
                else ret = list([ LispSymbol.get("QQ-UNQUOTE"), read_token() ]);
                ++in_qq;
                return ret;
        };
        function read_token() {
                out: while (true) {
                        skip_ws();
                        switch (peek()) {
                            case ";"  : skip_comment(); continue out;
                            case "\"" : return read_string();
                            case "("  : return read_list();
                            case "#"  : return read_sharp();
                            case "`"  : return read_quasiquote();
                            case ","  : return read_comma();
                            case "'"  : return read_quote();
                            case null : return false; // EOF
                        }
                        return read_symbol();
                }
        };
        function read_list() {
                var ret = null, p;
                skip("(");
                out: while (true) {
                        skip_ws();
                        switch (peek()) {
                            case ")": break out;
                            case null: break out;
                            case ";": skip_comment(); continue out;
                            case ".":
                                next();
                                p.cdr = read_token();
                                skip_ws();
                                break out;
                            default:
                                var tok = read_token();
                                var cell = new LispCons(tok, null);
                                if (ret) p.cdr = cell;
                                else ret = cell;
                                p = cell;
                        }
                }
                skip(")");
                return ret;
        };
        return read_token;
};

///////////////// compiler
(function(LC){

        var cons = LC.cons
        , car = LC.car
        , cdr = LC.cdr
        , cadr = LC.cadr
        , caddr = LC.caddr
        , cadddr = LC.cadddr
        , cddr = LC.cddr
        , cdddr = LC.cdddr
        , length = LC.len
        , list = LC.fromArray;

        function find_var(name, type, env) {
                env = env.stuff;
                for (var i = 0; i < env.length; ++i) {
                        var frame = env[i];
                        for (var j = 0; j < frame.length; ++j) {
                                var el = frame[j];
                                if (el.name == name && el.type == type)
                                        return [ i, j ];
                        }
                }
        };

        var LABEL_NUM = 0;

        var S_LAMBDA   = LispSymbol.get("LAMBDA");
        var S_FN       = LispSymbol.get("%FN");
        var S_FUNCTION = LispSymbol.get("FUNCTION");
        var S_IF       = LispSymbol.get("IF");
        var S_PROGN    = LispSymbol.get("PROGN");
        var S_QUOTE    = LispSymbol.get("QUOTE");
        var S_SET      = LispSymbol.get("SETQ");
        var S_T        = LispSymbol.get("T");
        var S_NIL      = LispSymbol.get("NIL");
        var S_CC       = LispSymbol.get("C/C");
        var S_NOT      = LispSymbol.get("NOT");
        var S_LET      = LispSymbol.get("LET");
        var S_LET$     = LispSymbol.get("LET*");
        var S_LABELS   = LispSymbol.get("LABELS");
        var S_FLET     = LispSymbol.get("FLET");

        var PAK_KEYWORD = LispPackage.get("KEYWORD");

        function append() {
                var ret = [];
                for (var i = 0; i < arguments.length; ++i) {
                        var el = arguments[i];
                        if (el.length > 0)
                                ret.push.apply(ret, el);
                }
                return ret;
        };

        function gen_label() {
                return new LispSymbol;
        };

        var seq = append;

        function gen() {
                return [ slice(arguments) ];
        };

        function nullp(x) {
                return x === S_NIL || x == null || (x instanceof Array && x.length == 0);
        };

        function arg_count(form, min, max) {
                if (max == null) max = min;
                var len = length(cdr(form));
                if (len < min) throw new Error("Expecting at least " + min + " arguments");
                if (len > max) throw new Error("Expecting at most " + max + " arguments");
        };

        function assert(cond, error) {
                if (!cond) throw new Error(error);
        };

        function comp(x, env, VAL, MORE) {
                if (nullp(x)) return comp_const(null, VAL, MORE);
                if (LispSymbol.is(x)) {
                        switch (x) {
                            case S_NIL: return comp_const(null, VAL, MORE);
                            case S_T: return comp_const(true, VAL, MORE);
                        }
                        if (x.pak === PAK_KEYWORD)
                                return comp_const(x, VAL, MORE);
                        return comp_var(x, env, VAL, MORE);
                }
                else if (LispMachine.constantp(x)) {
                        return comp_const(x, VAL, MORE);
                }
                else switch (car(x)) {
                    case S_QUOTE:
                        arg_count(x, 1);
                        return comp_const(cadr(x), VAL, MORE);
                    case S_PROGN:
                        return comp_seq(cdr(x), env, VAL, MORE);
                    case S_SET:
                        arg_count(x, 2);
                        assert(LispSymbol.is(cadr(x)), "Only symbols can be set");
                        return seq(comp(caddr(x), env, true, true),
                                   gen_set(cadr(x), env),
                                   VAL ? [] : gen("POP"),
                                   MORE ? [] : gen("RET"));
                    case S_IF:
                        arg_count(x, 2, 3);
                        return comp_if(cadr(x), caddr(x), cadddr(x), env, VAL, MORE);
                    case S_NOT:
                        arg_count(x, 1);
                        return VAL ? seq(
                                comp(cadr(x), env, true, true),
                                gen("NOT"),
                                MORE ? [] : gen("RET")
                        ) : comp(cadr(x), env, VAL, MORE);
                    case S_CC:
                        arg_count(x, 0);
                        return VAL ? seq(gen("CC")) : [];
                    case S_LET:
                        return comp_let(cadr(x), cddr(x), env, VAL, MORE);
                    case S_LET$:
                        return comp_let$(cadr(x), cddr(x), env, VAL, MORE);
                    case S_LABELS:
                        return comp_flets(cadr(x), cddr(x), env, true, VAL, MORE);
                    case S_FLET:
                        return comp_flets(cadr(x), cddr(x), env, false, VAL, MORE);
                    case S_FN:
                        assert(LispSymbol.is(cadr(x)), "%FN requires a symbol name for the function");
                        return VAL ? seq(
                                comp_lambda(cadr(x), caddr(x), cdddr(x), env),
                                MORE ? [] : gen("RET")
                        ) : [];
                    case S_LAMBDA:
                        return VAL ? seq(
                                comp_lambda(null, cadr(x), cddr(x), env),
                                MORE ? [] : gen("RET")
                        ) : [];
                    case S_FUNCTION:
                        arg_count(x, 1);
                        assert(LispSymbol.is(cadr(x)), "FUNCTION requires a symbol");
                        if (VAL) {
                                var localfunc = find_var(cadr(x), "func", env);
                                if (localfunc) return gen("LVAR", localfunc[0], localfunc[1]);
                                return gen("FGVAR", cadr(x));
                        }
                        return [];
                    default:
                        if (LispSymbol.is(car(x)) && car(x).macro())
                                return comp_macroexpand(car(x), cdr(x), env, VAL, MORE);
                        return comp_funcall(car(x), cdr(x), env, VAL, MORE);
                }
        };

        function gen_set(name, env) {
                if (!name.special()) {
                        var p = find_var(name, "var", env);
                        if (p) return gen("LSET", p[0], p[1]);
                }
                return gen("GSET", name);
        };

        function gen_var(name, env) {
                if (!name.special()) {
                        var pos = find_var(name, "var", env);
                        if (pos) return gen("LVAR", pos[0], pos[1]);
                }
                return gen("GVAR", name);
        };

        function comp_const(x, VAL, MORE) {
                return VAL ? seq(
                        gen("CONST", x === S_NIL ? null : x === S_T ? true : x),
                        MORE ? [] : gen("RET")
                ) : [];
        };

        function comp_var(x, env, VAL, MORE) {
                return VAL ? seq(
                        gen_var(x, env),
                        MORE ? [] : gen("RET")
                ) : [];
        };

        function comp_seq(exps, env, VAL, MORE) {
                if (nullp(exps)) return comp_const(null, VAL, MORE);
                if (nullp(cdr(exps))) return comp(car(exps), env, VAL, MORE);
                return seq(comp(car(exps), env, false, true),
                           comp_seq(cdr(exps), env, VAL, MORE));
        };

        function comp_list(exps, env) {
                if (!nullp(exps)) return seq(
                        comp(car(exps), env, true, true),
                        comp_list(cdr(exps), env)
                );
                return [];
        };

        function comp_if(pred, tthen, telse, env, VAL, MORE) {
                var pcode = comp(pred, env, true, true);
                var tcode = comp(tthen, env, VAL, MORE);
                var ecode = comp(telse, env, VAL, MORE);
                var l1 = gen_label(), l2 = gen_label();
                return seq(
                        pcode,
                        gen("FJUMP", l1),
                        tcode,
                        MORE ? gen("JUMP", l2) : [],
                        [ l1 ],
                        ecode,
                        MORE ? [ l2 ] : []
                );
        };

        function get_bindings(bindings, flet) {
                var names = [], vals = [], specials = [];
                LispCons.forEach(bindings, function(el, i, dot){
                        if (dot) throw new Error("Improper list in LET");
                        if (LC.is(el)) {
                                vals.push(flet ? cdr(el) : cadr(el));
                                el = car(el);
                        } else {
                                vals.push(S_NIL);
                        }
                        if (names.indexOf(el) >= 0)
                                throw new Error("Duplicate name in LET");
                        names.push(el);
                        if (el.special()) specials.push(i);
                });
                return { names: names, vals: vals, specials: specials, len: names.length };
        };

        function comp_let(bindings, body, env, VAL, MORE) {
                if (nullp(bindings)) return comp_seq(body, env, VAL, MORE);
                var b = get_bindings(bindings);
                var ret = seq(
                        seq.apply(null, b.vals.map(function(x){
                                return comp(x, env, true, true);
                        })),
                        gen("LET", b.len),
                        b.specials.map(function(i){
                                return gen("BIND", b.names[i], i)[0];
                        }),
                        comp_seq(body, env.extend("var", b.names), VAL, true),
                        gen("UNFR", 1, b.specials.length, 0),
                        MORE ? [] : gen("RET")
                );
                return ret;
        };

        function comp_let$(bindings, body, env, VAL, MORE) {
                if (nullp(bindings)) return comp_seq(body, env, VAL, MORE);
                var b = get_bindings(bindings);
                var newargs = [];
                var ret = seq(
                        seq.apply(null, b.vals.map(function(x, i){
                                var name = b.names[i];
                                x = seq(
                                        comp(x, env, true, true),
                                        i == 0 ? gen("FRAME") : [],
                                        gen("VAR"),
                                        name.special() ? gen("BIND", name, i) : []
                                );
                                if (i == 0) {
                                        env = env.extend("var", newargs);
                                }
                                env.add(name, "var");
                                return x;
                        })),
                        comp_seq(body, env, VAL, true),
                        gen("UNFR", 1, b.specials.length, 0),
                        MORE ? [] : gen("RET")
                );
                return ret;
        };

        function comp_flets(bindings, body, env, is_labels, VAL, MORE) {
                if (nullp(bindings)) return comp_seq(body, env, VAL, MORE);
                var b = get_bindings(bindings, true);
                if (is_labels) env = env.extend("func", b.names);
                return seq(
                        is_labels ? gen("FRAME") : [],
                        seq.apply(null, b.names.map(function(name, i){
                                return comp_lambda(name, car(b.vals[i]), cdr(b.vals[i]), env)
                        })),
                        is_labels ? [] : gen("FRAME"),
                        b.len > 1 ? gen("VARS", b.len) : gen("VAR"),
                        comp_seq(body, is_labels ? env : env.extend("func", b.names), VAL, true),
                        gen("UNFR", 1, 0),
                        MORE ? [] : gen("RET")
                );
        };

        function comp_funcall(f, args, env, VAL, MORE) {
                function mkret(the_function) {
                        if (MORE) {
                                var k = gen_label();
                                return seq(
                                        gen("SAVE", k),
                                        comp_list(args, env),
                                        the_function,
                                        gen("CALL", length(args)),
                                        [ k ],
                                        VAL ? [] : gen("POP")
                                );
                        }
                        return seq(
                                comp_list(args, env),
                                the_function,
                                gen("CALL", length(args))
                        );
                };
                if (LispSymbol.is(f)) {
                        var localfun = find_var(f, "func", env);
                        if (localfun) return mkret(gen("LVAR", localfun[0], localfun[1]));
                        if (f.primitive()) {
                                if (!VAL && !f.getv("primitive-side-effects")) {
                                        return comp_seq(args, env, false, MORE);
                                }
                                return seq(comp_list(args, env),
                                           gen("PRIM", f, length(args)),
                                           VAL ? [] : gen("POP"),
                                           MORE ? [] : gen("RET"));
                        }
                        if (!f.func()) console.warn("Undefined function", f);
                        return mkret(gen("FGVAR", f));
                }
                if (LC.is(f) && car(f) === S_LAMBDA && nullp(cadr(f))) {
                        assert(nullp(args), "Too many arguments");
                        return comp_seq(cddr(f), env, VAL, MORE);
                }
                return mkret(comp(f, env, true, true));
        };

        function comp_lambda(name, args, body, env) {
                if (LC.isList(args)) {
                        var dot = LC.isDotted(args);
                        var a = LC.toArray(args);
                        if (dot) a.push([ a.pop(), a.pop() ][0]);
                        var dyn = [];
                        for (var i = a.length; --i >= 0;) {
                                if (a[i].special())
                                        dyn.push([ "BIND", a[i], i ]);
                        }
                        if (!dot) {
                                return gen("FN",
                                           seq(a.length > 0 ? gen("ARGS", a.length) : [],
                                               dyn,
                                               comp_seq(body, a.length > 0 ? env.extend("var", a) : env, true, false)),
                                           name);
                        }
                        return gen("FN",
                                   seq(gen("ARG_", dot),
                                       dyn,
                                       comp_seq(body, env.extend("var", a), true, false)),
                                   name);
                } else {
                        return gen("FN",
                                   seq(gen("ARG_", 0),
                                       args.special() ? gen("BIND", args, 0) : [],
                                       comp_seq(body, env.extend("var", [ args ]), true, false)),
                                   name);
                }
        };

        function comp_macroexpand(name, args, env, VAL, MORE) {
                var m = new LispMachine();
                var ast = m._call(name.macro(), args);
                var ret = comp(ast, env, VAL, MORE);
                return ret;
        };

        this.lisp_compile = function(x) {
                return comp_seq(x, new Environment(), true, true);
        };

        var Environment = DEFCLASS("Environment", null, function(D, P){
                P.INIT = function() {
                        this.stuff = [];
                };
                P.extend = function(type, val) {
                        var env = new Environment();
                        env.stuff = [ val.map(function(name){
                                return { name: name, type: type };
                        }) ].concat(this.stuff);
                        return env;
                };
                P.add = function(name, type) {
                        this.stuff[0].push({ name: name, type: type });
                };
        });

})(LispCons);
