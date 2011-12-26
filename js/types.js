function DEFTYPE(name, func) {
        return DEFCLASS(name, null, function(D, P){
                D.is = function(thing) { return thing instanceof D };
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

var LispString = DEFTYPE("string", function(D, P){
        P.INIT = function(val) {
                this.value = val;
        };
        P.valueOf = P.toString = function() {
                return this.value;
        };
});
