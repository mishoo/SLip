// basic stream

function Stream(text) {
        this.pos = 0;
        this.line = 0;
        this.col = 0;
        this.text = text;
        this.len = text.length;
};

Stream.prototype = {
        peek: function() {
                if (this.pos < this.len)
                        return this.text.charAt(this.pos);
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
        }
};

////////////////// basic parser
function lisp_parse(code) {
        var input = new Stream("(" + code + ")");
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
                                return true;
                        }
                });
        };
        function skip(expected) {
                if (next() != expected)
                        croak("Expecting " + expected);
        };
        function read_string() {
                skip("\"");
                var escaped = false;
                var str = read_while(function(ch){
                        if (escaped) {
                                return !(escaped = false);
                        } else if (ch == "\\") {
                                return escaped = true;
                        } else {
                                return ch != "\"";
                        }
                });
                skip("\"");
                return str;
        };
        function skip_comment() {
                read_while(function(ch){ return ch != "\n"; });
        };
        function read_symbol() {
                var str = read_while(function(ch){
                        if ((ch >= "a" && ch <= "z") ||
                            (ch >= "A" && ch <= "Z") ||
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
                if (str.length > 0 && /^[0-9]*\.?[0-9]*$/.test(str))
                        return parseFloat(str);
                return LispSymbol(str);
        };
        function read_char() {
                var ch = next() + read_while(function(ch){
                        return (ch >= "a" && ch <= "z") ||
                                (ch >= "A" && ch <= "z") ||
                                (ch >= "0" && ch <= "9") ||
                                ch == "_" || ch == "_";
                });
                if (ch.length > 1)
                        croak("Character names not supported: " + ch);
                return ch;
        };
        function read_sharp() {
                skip("#");
                var ch = next();
                switch (ch) {
                    case "\\": return read_char();
                    default:
                        croak("Unsupported sharp syntax: #" + ch);
                }
        };
        function read_quote() {
                skip("'");
                return [ LispSymbol("quote"), read_token() ];
        };
        function read_token() {
                skip_ws();
                var ch = peek();
                switch (ch) {
                    case "\"": return read_string();
                    case "(": return read_list();
                    case ";": return skip_comment();
                    case "#": return read_sharp();
                    case "'": return read_quote();
                }
                return read_symbol();
        };
        function read_list() {
                var ret = [];
                skip("(");
                skip_ws();
                out: while (true) switch (peek()) {
                    case ")": break out;
                    case null: break out;
                    default:
                        var tok = read_token();
                        if (tok != null) ret.push(tok);
                        skip_ws();
                }
                skip(")");
                if (ret.length == 0)
                        return LispSymbol("nil");
                return ret;
        };
        return read_token();
};

///////////////// compiler
(function(){

        function find_var(name, env) {
                for (var i = 0; i < env.length; ++i) {
                        var frame = env[i];
                        for (var j = 0; j < frame.length; ++j) {
                                if (frame[j] == name)
                                        return [ i, j ];
                        }
                }
        };

        var LABEL_NUM = 0;

        var S_LAMBDA  = LispSymbol("LAMBDA");
        var S_IF      = LispSymbol("IF");
        var S_PROGN   = LispSymbol("PROGN");
        var S_QUOTE   = LispSymbol("QUOTE");
        var S_SET     = LispSymbol("SET!");
        var S_T       = LispSymbol("T");
        var S_NIL     = LispSymbol("NIL");
        var S_NOT     = LispSymbol("NOT");
        var S_CC      = LispSymbol("C/C");

        function append() {
                var ret = [];
                for (var i = 0; i < arguments.length; ++i) {
                        var el = arguments[i];
                        if (el && el.length > 0)
                                ret.push.apply(ret, el);
                }
                return ret;
        };

        function gen_label() {
                return new LispLabel("L" + (++LABEL_NUM));
        };

        var seq = append;

        function gen() {
                return [ slice(arguments) ];
        };

        function constantp(x) {
                if (x === S_T || x === S_NIL) return true;
                switch (typeof x) {
                    case "string":
                    case "number":
                        return true;
                }
        };

        function equal(a, b) {
                var ta = typeof a;
                if (ta != typeof b) return false;
                if (a instanceof Array) {
                        if (a.length != b.length) return false;
                        for (var i = a.length; --i >= 0;)
                                if (!equal(a[i], b[i]))
                                        return false;
                        return true;
                }
                if (ta == "object") {
                        for (var i in a) if (HOP(a, i)) {
                                if (!equal(a[i], b[i]))
                                        return false;
                        }
                        return true;
                }
                return a == b;
        };

        function nullp(x) {
                return x === S_NIL || x == null || (x instanceof Array && x.length == 0);
        };

        function arg_count(form, min, max) {
                if (max == null) max = min;
                var len = form.length - 1;
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
                        return comp_var(x, env, VAL, MORE);
                }
                else switch (typeof x) {
                    case "string":
                    case "number":
                        return comp_const(x, VAL, MORE);
                    default:
                        switch (x[0]) {
                            case S_QUOTE:
                                arg_count(x, 1);
                                return comp_const(x[1], VAL, MORE);
                            case S_PROGN:
                                return comp_seq(x.slice(1), env, VAL, MORE);
                            case S_SET:
                                arg_count(x, 2);
                                assert(LispSymbol.is(x[1]), "Only symbols can be set");
                                return seq(comp(x[2], env, true, true),
                                           gen_set(x[1], env),
                                           VAL ? null : gen("POP"),
                                           MORE ? null : gen("RET"));
                            case S_IF:
                                arg_count(x, 2, 3);
                                return comp_if(x[1], x[2], x[3], env, VAL, MORE);
                            case S_CC:
                                arg_count(x, 0);
                                return VAL ? seq(gen("CC")) : [];
                            case S_LAMBDA:
                                return VAL ? seq(
                                        comp_lambda(x[1], x.slice(2), env),
                                        MORE ? null : gen("RET")
                                ) : [];
                            default:
                                return comp_funcall(x[0], x.slice(1), env, VAL, MORE);
                        }
                }
        };

        /////

        function gen_set(x, env) {
                var p = find_var(x, env);
                if (p) {
                        return gen("LSET", p[0], p[1]);
                }
                return gen("GSET", x);
        };

        function gen_var(name, env) {
                var pos = find_var(name, env);
                if (pos) {
                        return gen("LVAR", pos[0], pos[1]);
                }
                return gen("GVAR", name);
        };

        function comp_const(x, VAL, MORE) {
                if (VAL) return seq(
                        gen("CONST", x),
                        MORE ? null : gen("RET")
                );
        };

        function comp_var(x, env, VAL, MORE) {
                if (VAL) return seq(
                        gen_var(x, env),
                        MORE ? null : gen("RET")
                );
        };

        function comp_seq(exps, env, VAL, MORE) {
                switch (exps.length) {
                    case 0: return comp_const(null, VAL, MORE);
                    case 1: return comp(exps[0], env, VAL, MORE);
                }
                return seq(comp(exps[0], env, false, true),
                           comp_seq(exps.slice(1), env, VAL, MORE));
        };

        function comp_list(exps, env) {
                if (exps.length > 0) return seq(
                        comp(exps[0], env, true, true),
                        comp_list(exps.slice(1), env)
                );
        };

        function comp_if(pred, tthen, telse, env, VAL, MORE) {
                if (nullp(pred)) {
                        return comp(telse, env, VAL, MORE);
                }
                if (constantp(pred)) {
                        return comp(tthen, env, VAL, MORE);
                }
                if (pred instanceof Array && pred[0] === S_NOT && pred.length == 2) {
                        return comp_if(pred[1], telse, tthen, env, VAL, MORE);
                }
                var pcode = comp(pred, env, true, true);
                var tcode = comp(tthen, env, VAL, MORE);
                var ecode = comp(telse, env, VAL, MORE);
                var l1, l2;

                // not really sure this optimization is worthy
                if (equal(tcode, ecode)) return seq(
                        comp(pred, env, false, true),
                        ecode
                );

                if (nullp(tcode)) {
                        l2 = gen_label();
                        return seq(
                                pcode,
                                gen("TJUMP", l2),
                                ecode,
                                [ l2 ],
                                MORE ? null : gen("RET")
                        );
                }
                if (nullp(ecode)) {
                        l1 = gen_label();
                        return seq(
                                pcode,
                                gen("FJUMP", l1),
                                tcode,
                                [ l1 ],
                                MORE ? null : gen("RET")
                        );
                }
                l1 = gen_label();
                if (MORE) l2 = gen_label();
                return seq(
                        pcode,
                        gen("FJUMP", l1),
                        tcode,
                        MORE && gen("JUMP", l2),
                        [ l1 ],
                        ecode,
                        MORE && [ l2 ]
                );
        };

        function comp_funcall(f, args, env, VAL, MORE) {
                if (LispPrimitive.is(f, env, args.length)) {
                        if (!VAL && !LispPrimitive.seff(f)) {
                                return comp_seq(args, env, false, MORE);
                        }
                        return seq(comp_list(args, env),
                                   gen("PRIM", f),
                                   VAL ? null : gen("POP"),
                                   MORE ? null : gen("RET"));
                }
                if (f instanceof Array && f[0] === S_LAMBDA && f[1].length == 0) {
                        assert(nullp(args), "Too many arguments");
                        return comp_seq(f.slice(2), env, VAL, MORE);
                }
                if (MORE) {
                        var k = gen_label();
                        return seq(
                                gen("SAVE", k),
                                comp_list(args, env),
                                comp(f, env, true, true),
                                gen("CALLJ", args.length),
                                [ k ],
                                VAL ? null : gen("POP")
                        );
                }
                return seq(
                        comp_list(args, env),
                        comp(f, env, true, true),
                        gen("CALLJ", args.length)
                );
        };

        function comp_lambda(args, body, env) {
                return gen("FN",
                           seq(gen("ARGS", nullp(args) ? 0 : args.length),
                               comp_seq(body, [ args ].concat(env), true, false))
                          );
        };

        this.compile = function(x) {
                return comp_seq(x, [], true, false);
        };

        var INDENT_LEVEL = 8;

        function indent(level) {
                return repeat_string(' ', level * INDENT_LEVEL);
        };

        function show_code(x, level) {
                var ret = [];
                var line = "";
                var skip_indent = false;
                for (var i = 0; i < x.length; ++i) {
                        var el = x[i];
                        if (el instanceof LispLabel) {
                                line += pad_string(el.name + ":", level * INDENT_LEVEL);
                                skip_indent = true;
                                continue;
                        }
                        if (!skip_indent) line += indent(level);
                        skip_indent = false;
                        if (el[0] == "FN") {
                                line += "FN\n";
                                line += show_code(el[1], level + 1);
                        }
                        else {
                                line += el.map(function(el, i){
                                        if (i > 0 && typeof el == "string") el = JSON.stringify(el);
                                        return pad_string(el, 8);
                                }).join("");
                        }
                        ret.push(line);
                        line = "";
                }
                return ret.join("\n");
        };

        this.comp_show = function(x) {
                return show_code(x, 1);
        };

})();
