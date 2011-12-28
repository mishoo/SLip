var LispCons = DEFTYPE("cons", function(D, P){
        P.INIT = function(a, b) {
                this.car = a;
                this.cdr = b;
        };
        D.isList = function(thing) {
                return thing === null || thing instanceof D;
        };
        D.forEach = function(p, callback) {
                while (p !== null) {
                        callback(p.car);
                        p = p.cdr;
                }
        };
        D.map = function(list, callback) {
                var ret = null, p;
                D.forEach(list, function(el){
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
        D.len = function(list) {
                var len = 0;
                while (list !== null) {
                        ++len;
                        list = list.cdr;
                }
                return len;
        };
        D.toArray = function(list) {
                var a = [];
                D.forEach(list, function(el){
                        a.push(el);
                });
                return a;
        };
        D.cons = function(a, b) {
                return new D(a, b);
        };

        function car(cell) {
                return cell === null ? null : cell.car;
        };
        function cdr(cell) {
                return cell === null ? null : cell.cdr;
        };

        D.car = car;
        D.cdr = cdr;
        D.cadr = compose(car, cdr);
        D.caddr = compose(car, cdr, cdr);
        D.cadddr = compose(car, cdr, cdr, cdr);
        D.cddr = compose(cdr, cdr);
});
