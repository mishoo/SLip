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

        D.caar = function(l){return car(car(l))};
        D.cadr = function(l){return car(cdr(l))};
        D.cdar = function(l){return cdr(car(l))};
        D.cddr = function(l){return cdr(cdr(l))};
        D.caaar = function(l){return car(car(car(l)))};
        D.caadr = function(l){return car(car(cdr(l)))};
        D.cadar = function(l){return car(cdr(car(l)))};
        D.caddr = function(l){return car(cdr(cdr(l)))};
        D.cdaar = function(l){return cdr(car(car(l)))};
        D.cdadr = function(l){return cdr(car(cdr(l)))};
        D.cddar = function(l){return cdr(cdr(car(l)))};
        D.cdddr = function(l){return cdr(cdr(cdr(l)))};
        D.caaaar = function(l){return car(car(car(car(l))))};
        D.caaadr = function(l){return car(car(car(cdr(l))))};
        D.caadar = function(l){return car(car(cdr(car(l))))};
        D.caaddr = function(l){return car(car(cdr(cdr(l))))};
        D.cadaar = function(l){return car(cdr(car(car(l))))};
        D.cadadr = function(l){return car(cdr(car(cdr(l))))};
        D.caddar = function(l){return car(cdr(cdr(car(l))))};
        D.cadddr = function(l){return car(cdr(cdr(cdr(l))))};
        D.cdaaar = function(l){return cdr(car(car(car(l))))};
        D.cdaadr = function(l){return cdr(car(car(cdr(l))))};
        D.cdadar = function(l){return cdr(car(cdr(car(l))))};
        D.cdaddr = function(l){return cdr(car(cdr(cdr(l))))};
        D.cddaar = function(l){return cdr(cdr(car(car(l))))};
        D.cddadr = function(l){return cdr(cdr(car(cdr(l))))};
        D.cdddar = function(l){return cdr(cdr(cdr(car(l))))};
        D.cddddr = function(l){return cdr(cdr(cdr(cdr(l))))};

});
