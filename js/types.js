function DEFTYPE(name, func) {
        return DEFCLASS(name, null, function(D, P){
                P.type = name;
                return func(D, P);
        });
};

var LispNumber = DEFTYPE("number", function(D, P){
        P.INIT = function(val) {
                this.value = val;
        };
        P.valueOf = function() {
                return this.value;
        };
});

var LispChar = DEFTYPE("char", function(D, P){
        P.INIT = function(val) {
                this.value = val;
        };
        P.valueOf = P.toString = function() {
                return this.value;
        };
});

var LispString = DEFTYPE("string", function(D, P){
        P.INIT = function(val) {
                this.value = val;
        };
        P.valueOf = P.toString = function() {
                return this.value;
        };
});

var LispPackage = DEFTYPE("package", function(D, P){
        var PACKAGES = {};
        P.INIT = function(name) {
                this.name = name + "";
                this.symbols = {};
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "p(" + this.name + ")";
        };
        D.get = function(name) {
                name = name.toUpperCase();
                return HOP(PACKAGES, name) ? PACKAGES[name] : (
                        PACKAGES[name] = new D(name)
                );
        };
});

var LispSymbol = DEFTYPE("symbol", function(D, P){
        var SYMBOLS = {};
        P.INIT = function(name) {
                this.name = name;
                this.plist = {};
        };
        P.toString = function() { return this.name };
        P.serialize = function() {
                return "s(" + JSON.stringify(this.name) + ")";
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
                name = name.toUpperCase();
                return HOP(SYMBOLS, name) ? SYMBOLS[name] : (
                        SYMBOLS[name] = new D(name)
                );
        };
});
