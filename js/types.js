var LispType = DEFCLASS("LispType", null, function(D, P){
        P.print = function() {
                return "<" + this.type + ">";
        };
});

function DEFTYPE(name, func) {
        return DEFCLASS(name.replace(/-/g, "_"), LispType, function(D, P){
                P.type = D.type = name;
                var ret = func ? func(D, P) : null;
                return ret;
        }, true);
};

var LispChar = DEFTYPE("char", function(D, P){
        // TODO: this table should be really long.
        var NAMES_TO = {
                " "      : "SPACE",
                "\t"     : "TAB",
                "\r"     : "RETURN",
                "\n"     : "NEWLINE",
                "\x0C"   : "PAGE",
                "\x08"   : "BACKSPACE",
                "\u2028" : "LINE_SEPARATOR",
                "\u2029" : "PARAGRAPH_SEPARATOR"
        };
        var NAMES_FROM = (function(h){
                for (var i in NAMES_TO) {
                        if (HOP(NAMES_TO, i)) {
                                h[NAMES_TO[i]] = i;
                        }
                }
                h.LINEFEED = "\n";
                return h;
        })({});
        var OBJECTS = {};
        D.fromName = function(name) {
                if (name.length == 1)
                        return D.get(name); // hack
                name = name.toUpperCase();
                if (HOP(NAMES_FROM, name))
                        return D.get(NAMES_FROM[name]);
                return null;
        };
        D.fromCode = function(code) {
                return new D(String.fromCharCode(code));
        };
        D.get = function(char) {
                return OBJECTS[char] || (
                        OBJECTS[char] = new LispChar(char)
                );
        };
        D.sanitize = function(val) {
                return val.replace(/[\u0000\u00ad\u0600\u0604\u070f\u17b4\u17b5\u200c\u200f\u2028\u2029\u202f\u2060\u206f\ufeff\ufff0-\uffff]/g, function(s){
                        var v = s.charCodeAt(0).toString(16);
                        while (v.length < 4) v = "0" + v;
                        return "\\u" + v;
                });
        };
        P.INIT = function(val){ this.value = val };
        P.valueOf = P.toString = function(){ return this.value };
        P.name = function() {
                return NAMES_TO[this.value] || this.value;
        };
        P.code = function() {
                return this.value.charCodeAt(0);
        };
        P.print = function() {
                var ch = this.value;
                return "#\\" + (HOP(NAMES_TO, ch) ? NAMES_TO[ch] : ch);
        };
        P.serialize = function() {
                var ch = D.sanitize(JSON.stringify(this.value));
                return "c(" + ch + ")";
        };
});

var LispArray = DEFTYPE("array", function(D, P){
        P.INIT = function(val) {
                this.value = val != null ? val : [];
        };
        P.length = function() {
                return this.value.length;
        };
        P.get = function(i) {
                return this.value[i];
        };
        P.set = function(i, val) {
                this.value[i] = val;
        };
        P.print = function() {
                return "#(" + this.value.map(LispMachine.dump).join(" ") + ")";
        };
});

var LispRegexp = DEFTYPE("regexp", function(D, P){
        P.INIT = function(val) {
                this.value = val;
        };
        P.print = function() {
                return "<regexp " + this.value + ">";
        };
        P.test = function(str) {
                return this.value.test(str) ? true : null;
        };
        P.exec = function(str) {
                var m = this.value.exec(str);
                return m ? new LispArray(m) : null;
        };
        P.serialize = function() {
                return "rx(" + this.value + ")";
        };
});

var LispClosure = DEFTYPE("closure", function(D, P){
        P.INIT = function(code, env, name) {
                this.name = name;
                this.code = code;
                this.env = env;
        };
        P.print = function() {
                return "<function" + (this.name ? " " + this.name : "") + ">";
        };
});

var LispCondition = DEFTYPE("condition", function(D, P){
        P.INIT = function(message, cont) {
                this.message = message;
                this.cont = cont;
        };
});

var LispInputStream = DEFTYPE("input_stream", function(D, P){
        P.INIT = function(text) {
                this.text = text;
                this.line = 1;
                this.col = 0;
                this.pos = 0;
        };
        P.peek = function() {
                return this.pos < this.text.length
                        ? LispChar.get(this.text.charAt(this.pos))
                        : null;
        };
        P.next = function() {
                if (this.pos < this.text.length) {
                        var ch = this.text.charAt(this.pos++);
                        if (ch == "\n") ++this.line, this.col = 0;
                        else ++this.col;
                        return LispChar.get(ch);
                }
                return null;
        };
});

var LispOutputStream = DEFTYPE("output_stream", function(D, P){
        P.INIT = function() {
                this.text = "";
        };
        P.put = function(str) {
                return this.text += str;
        };
        P.get = function() {
                return this.text;
        };
});

var LispNamespace = DEFTYPE("namespace", function(D, P){
        P.INIT = function(parent) {
                function ctor(){};
                if (parent) ctor.prototype = parent.data;
                this.data = new ctor;
                this.parent = parent;
        };
        P.get = function(name) {
                return HOP(this.data, name) ? this.data[name] : null;
        };
        P.set = function(name, val) {
                return this.data[name] = val;
        };
        P.has = function(name) {
                var p = this;
                while (p != null) {
                        if (HOP(p.data, name)) return p;
                        p = p.parent;
                }
                return null;
        };
});

var LispPackage = DEFTYPE("package", function(D, P){
        var PACKAGES = {};
        P.INIT = function(name) {
                this.name = name + "";
                this.symbols = new LispNamespace();
                this.exports = [];
                this.uses = [];
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "p(" + this.name + ")";
        };
        P.intern = function(name) {
                return this.symbols.get(name) ||
                        this.symbols.set(name, new LispSymbol(name, this));
        };
        P.export = function(sym) {
                sym = this.find(sym.name);
                if (sym && this.exports.indexOf(sym) < 0) {
                        this.exports.push(sym);
                        return true;
                }
                return null;
        };
        P.find_accessible = function(name) {
                var a = this.exports;
                for (var i = a.length; --i >= 0;) {
                        var sym = a[i];
                        if (sym.name == name)
                                return sym;
                }
                a = this.uses;
                for (var i = a.length; --i >= 0;) {
                        var sym = a[i].find_accessible(name);
                        if (sym) return sym;
                }
                return null;
        };
        P.find = function(name) {
                var sym = this.symbols.get(name);
                if (sym) return sym;
                for (var i = this.uses.length; --i >= 0;) {
                        sym = this.uses[i].find_accessible(name);
                        if (sym) return sym;
                }
                return null;
        };
        P.find_or_intern = function(name) {
                return this.find(name) || this.intern(name);
        };
        P.external = function(sym) {
                return this.exports.indexOf(sym) >= 0;
        };
        P.alias = function(nickname) {
                PACKAGES[nickname] = this;
        };
        P.use = function(pak) {
                if (this.uses.indexOf(pak) < 0) {
                        this.uses.push(pak);
                        return pak;
                }
                return null;
        };
        D.get = function(name) {
                return HOP(PACKAGES, name) ? PACKAGES[name] : (
                        PACKAGES[name] = new D(name)
                );
        };
});

var LispSymbol = DEFTYPE("symbol", function(D, P){
        var SYMBOLS = {};
        D.is = function(thing) {
                return thing === true || thing === null || thing instanceof D;
        };
        P.INIT = function(name, pak) {
                if (name) {
                        this.name = name + "";
                        this.pak = pak || null;
                        this.plist = {};
                }
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "s(" +
                        JSON.stringify(this.name) +
                        (this.pak ? ("," + JSON.stringify(this.pak.name)) : "") +
                        ")";
        };
        P.set = function(key, val) {
                this.plist[key] = val;
        };
        P.get = function(key) {
                return HOP(this.plist, key) ? this.plist[key] : null;
        };
        P.macro = function() {
                return this.get("macro");
        };
        P.special = function() {
                return this.get("special");
        };
        P.primitive = function() {
                return this.get("primitive");
        };
        D.get = function(name, pak) {
                if (pak == null) pak = BASE_PACK.intern("*PACKAGE*").value;
                var ret = pak ? pak.intern(name) : HOP(SYMBOLS, name) ? SYMBOLS[name] : (
                        SYMBOLS[name] = new D(name)
                );
                return ret;
        };
        P.dump = function() {
                if (this.pak && this.pak.name == "KEYWORD") return ":" + this.name;
                return this.name;
        };
        var BASE_PACK = LispPackage.get("%");
});

(function(BASE_PACK){
        var pak = BASE_PACK.intern("*PACKAGE*");
        pak.value = BASE_PACK;
        pak.set("special", true);
})(LispPackage.get("%"))
