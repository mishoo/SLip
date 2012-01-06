function DEFTYPE(name, func) {
        return DEFCLASS(name.replace(/-/g, "_"), null, function(D, P){
                P.type = name;
                if (func) return func(D, P);
        });
};

var LispChar = DEFTYPE("char", function(D, P){
        P.INIT = function(val){ this.value = val };
        P.valueOf = P.toString = function(){ return this.value };
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
});

var LispRegexp = DEFTYPE("regexp", function(D, P){
        P.INIT = function(val) {
                this.value = new RegExp(val);
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
                }
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
        D.get = function(name) {
                return HOP(PACKAGES, name) ? PACKAGES[name] : (
                        PACKAGES[name] = new D(name)
                );
        };
});

var LispSymbol = DEFTYPE("symbol", function(D, P){
        var SYMBOLS = {};
        P.INIT = function(name, pak) {
                this.name = name + "";
                this.pak = pak || null;
                this.plist = {};
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
                return this.plist[key];
        };
        P.macro = function() {
                return this.get("macro") || null;
        };
        P.special = function() {
                return this.get("special") ? true : null;
        };
        D.get = function(name) {
                return HOP(SYMBOLS, name) ? SYMBOLS[name] : (
                        SYMBOLS[name] = new D(name)
                );
        };
});
