var LispCons = DEFTYPE("cons", function(D, P){
        P.INIT = function(a, b) {
                this.car = a;
                this.cdr = b;
        };
        D.is = function(thing) {
                return thing === null || thing instanceof D;
        };
        P.forEach = function(callback) {
                var p = this;
                while (p !== null) {
                        callback(p.car);
                        p = p.cdr;
                }
        };
        P.map = function(callback) {
                var ret = null, p;
                this.forEach(function(el){
                        var cell = new D(callback(el), null);
                        if (ret) p.cdr = cell;
                        else ret = cell;
                        p = cell;
                });
                return ret;
        };
        D.fromArray = function(a) {
                var ret = null;
                a.forEach(function(el){
                        var cell = new D(el, null);
                        if (ret) p.cdr = cell;
                        else ret = cell;
                        p = cell;
                });
                return ret;
        };
        P.toArray = function() {
                var a = [];
                this.forEach(function(el){
                        a.push(el);
                });
                return a;
        };
});
