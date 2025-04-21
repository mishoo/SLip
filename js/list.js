import { LispType, LispPrimitiveError } from "./types.js";

function listp(thing) {
    return thing === null || thing instanceof LispCons;
}

function check_list(list, func) {
    if (!listp(list)) throw new LispPrimitiveError("Non-list (improper list?) argument in " + func);
}

function car(cell) {
    check_list(cell, "CAR");
    return cell === null ? null : cell.car;
}

function cdr(cell) {
    check_list(cell, "CDR");
    return cell === null ? null : cell.cdr;
}

const DOT = {
    toString() { return "DOT" }
};

export class LispCons extends LispType {
    static type = "cons";
    static is(x) { return x instanceof LispCons }
    static DOT = DOT;
    static isList = listp;
    constructor(a, b) {
        super();
        this.car = a;
        this.cdr = b;
    }
    static forEach(p, callback) {
        var i = 0;
        while (p !== null) {
            callback(p.car, i++);
            p = p.cdr;
            if (!listp(p)) {
                callback(p, i, true);
                break;
            }
        }
    }
    static map(list, callback) {
        var ret = null, p;
        LispCons.forEach(list, function(el, i, dot){
            var cell = new LispCons(null, null);
            cell.car = callback(el, i, dot, cell);
            if (ret) p.cdr = cell;
            else ret = cell;
            p = cell;
        });
        return ret;
    }
    static last(list) {
        while (list && cdr(list) !== null && listp(list.cdr)) {
            list = list.cdr;
        }
        return list;
    }
    static copy(list) {
        let copy = new LispCons(null, null), p = copy;
        while (list != null) {
            p = p.cdr = new LispCons(car(list), null);
            list = list.cdr;
            if (!listp(list)) {
                p.cdr = list;
                break;
            }
        }
        return copy.cdr;
    }
    static reverse(list) {
        var a = null;
        while (list != null) {
            a = new LispCons(car(list), a);
            list = cdr(list);
        }
        return a;
    }
    static nreverse(list) {
        if (list === null || cdr(list) === null) return list;
        var p = null;
        while (true) {
            var next = cdr(list);
            list.cdr = p;
            p = list;
            if (next === null) return list;
            list = next;
        }
    }
    static append(lists) {
        if (lists.length == 0) return null;
        var ret = null, p = null;
        while (lists.length > 1) {
            var l = lists.shift();
            while (l !== null) {
                var cell = new LispCons(car(l), null);
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
    }
    static nconc(lists) {
        var ret = null, p = null;
        for (var i = 0; i < lists.length; ++i) {
            var l = lists[i];
            if (l === null) continue;
            if (!ret) ret = l;
            if (p) {
                check_list(p, "nconc");
                p.cdr = l;
            }
            p = LispCons.last(l);
        }
        return ret;
    }
    static nreconc(list, tail) {
        if (list === null) return tail;
        var tmp = list;
        list = LispCons.nreverse(list);
        tmp.cdr = tail;
        return list;
    }
    static revappend(list, tail) {
        if (list === null) return tail;
        var a = null, last = null;
        while (list != null) {
            a = new LispCons(car(list), a);
            if (!last) last = a;
            list = cdr(list);
        }
        last.cdr = tail;
        return a;
    }
    static fromArray(array, start = 0, end = array.length) {
        let ret = null, p = null, dot = false;
        for (let i = start; i < end; ++i) {
            let el = array[i];
            if (el === DOT) {
                dot = true;
            } else {
                if (dot) p.cdr = el;
                else {
                    let cell = new LispCons(el, null);
                    if (ret) p.cdr = cell;
                    else ret = cell;
                    p = cell;
                }
            }
        }
        return ret;
    }
    static len(list) {
        var len = 0;
        while (list !== null) {
            ++len;
            list = list.cdr;
        }
        return len;
    }
    static toArray(list) {
        var a = [];
        LispCons.forEach(list, function(el, i, dot){
            if (dot) a.push(DOT);
            a.push(el);
        });
        return a;
    }
    static cons(a, b) {
        return new LispCons(a, b);
    }
    static isDotted(x) {
        var i = 0;
        while (x !== null) {
            if (!listp(x)) return i;
            x = x.cdr;
            i++;
        }
        return false;
    }
    static elt(list, i) {
        var p = list;
        while (p !== null && i-- > 0) {
            p = cdr(p);
        }
        return car(p);
    }
    static find(list, item, cmp) {
        while (list !== null && !cmp(list.car, item))
            list = list.cdr;
        return list;
    }

    static car = car;
    static cdr = cdr;

    static caar(l){return car(car(l))}
    static cadr(l){return car(cdr(l))}
    static cdar(l){return cdr(car(l))}
    static cddr(l){return cdr(cdr(l))}
    static caaar(l){return car(car(car(l)))}
    static caadr(l){return car(car(cdr(l)))}
    static cadar(l){return car(cdr(car(l)))}
    static caddr(l){return car(cdr(cdr(l)))}
    static cdaar(l){return cdr(car(car(l)))}
    static cdadr(l){return cdr(car(cdr(l)))}
    static cddar(l){return cdr(cdr(car(l)))}
    static cdddr(l){return cdr(cdr(cdr(l)))}
    static caaaar(l){return car(car(car(car(l))))}
    static caaadr(l){return car(car(car(cdr(l))))}
    static caadar(l){return car(car(cdr(car(l))))}
    static caaddr(l){return car(car(cdr(cdr(l))))}
    static cadaar(l){return car(cdr(car(car(l))))}
    static cadadr(l){return car(cdr(car(cdr(l))))}
    static caddar(l){return car(cdr(cdr(car(l))))}
    static cadddr(l){return car(cdr(cdr(cdr(l))))}
    static cdaaar(l){return cdr(car(car(car(l))))}
    static cdaadr(l){return cdr(car(car(cdr(l))))}
    static cdadar(l){return cdr(car(cdr(car(l))))}
    static cdaddr(l){return cdr(car(cdr(cdr(l))))}
    static cddaar(l){return cdr(cdr(car(car(l))))}
    static cddadr(l){return cdr(cdr(car(cdr(l))))}
    static cdddar(l){return cdr(cdr(cdr(car(l))))}
    static cddddr(l){return cdr(cdr(cdr(cdr(l))))}
}
