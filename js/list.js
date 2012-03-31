var LispCons = DEFTYPE("cons", function(D, P){
        var DOT = {
                toString: function(){ return "DOT" }
        };
        function listp(thing) {
                return thing === null || thing instanceof D;
        };
        function check_list(list, func) {
                if (!listp(list)) throw new LispPrimitiveError("Non-list (improper list?) argument in " + func);
        };
        P.INIT = function(a, b) {
                this.car = a;
                this.cdr = b;
        };
        D.DOT = DOT;
        D.isList = listp;
        D.forEach = function(p, callback) {
                var i = 0;
                while (p !== null) {
                        callback(p.car, i++);
                        p = p.cdr;
                        if (!listp(p)) {
                                callback(p, i, true);
                                break;
                        }
                }
        };
        D.map = function(list, callback) {
                var ret = null, p;
                D.forEach(list, function(el, i, dot){
                        var cell = new D(null, null);
                        cell.car = callback(el, i, dot, cell);
                        if (ret) p.cdr = cell;
                        else ret = cell;
                        p = cell;
                });
                return ret;
        };
        D.last = function(list) {
                while (list && cdr(list)) {
                        check_list(list, "LAST");
                        list = cdr(list);
                }
                return list;
        };
        D.reverse = function(list) {
                var a = null;
                while (list != null) {
                        check_list(list, "REVERSE");
                        a = new LispCons(car(list), a);
                        list = cdr(list);
                }
                return a;
        };
        D.nreverse = function(list) {
                if (list === null || cdr(list) === null) return list;
                var p = null;
                while (true) {
                        check_list(list, "NREVERSE");
                        var next = list.cdr;
                        list.cdr = p;
                        p = list;
                        if (next === null) return list;
                        list = next;
                }
        };
        D.append = function(lists) {
                if (lists.length == 0) return null;
                var ret = null, p = null;
                while (lists.length > 1) {
                        var l = lists.shift();
                        while (l !== null) {
                                check_list(l, "APPEND");
                                var cell = new LispCons(l.car, null);
                                if (p) p.cdr = cell;
                                else ret = cell;
                                p = cell;
                                l = l.cdr;
                        }
                }
                if (p) {
                        p.cdr = lists[0];
                        return ret;
                }
                return lists[0];
        };
        D.nconc = function(lists) {
                var ret = null, p = null;
                for (var i = 0; i < lists.length; ++i) {
                        var l = lists[i];
                        check_list(l, "NCONC");
                        if (!l) continue;
                        if (!ret) ret = l;
                        if (p) p.cdr = l;
                        p = D.last(l);
                }
                return ret;
        };
        D.nreconc = function(list, tail) {
                if (list === null) return tail;
                var tmp = list;
                list = D.nreverse(list);
                tmp.cdr = tail;
                return list;
        };
        D.revappend = function(list, tail) {
                if (list === null) return tail;
                var a = null, last = null;
                while (list != null) {
                        check_list(list, "REVERSE");
                        a = new LispCons(car(list), a);
                        if (!last) last = a;
                        list = cdr(list);
                }
                last.cdr = tail;
                return a;
        };
        D.fromArray = function(a) {
                var ret = null, dot = false;
                a.forEach(function(el){
                        if (el === DOT) {
                                dot = true;
                        } else {
                                if (dot) p.cdr = el;
                                else {
                                        var cell = new D(el, null);
                                        if (ret) p.cdr = cell;
                                        else ret = cell;
                                        p = cell;
                                }
                        }
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
                D.forEach(list, function(el, i, dot){
                        if (dot) a.push(DOT);
                        a.push(el);
                });
                return a;
        };
        D.cons = function(a, b) {
                return new D(a, b);
        };
        D.isDotted = function(x) {
                var i = 0;
                while (x !== null) {
                        if (!listp(x)) return i;
                        x = x.cdr;
                        i++;
                }
                return false;
        };
        D.elt = function(list, i) {
                var p = list;
                while (p !== null && i-- > 0) {
                        p = cdr(p);
                }
                return car(p);
        };
        D.find = function(list, item, cmp) {
                while (list !== null && !cmp(list.car, item))
                        list = list.cdr;
                return list;
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
