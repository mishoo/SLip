
function $parcel$export(e, n, v, s) {
  Object.defineProperty(e, n, {get: v, set: s, enumerable: true, configurable: true});
}
/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
let $ca727f7f34cfa7f3$var$TEMPLATE = document.createElement("template");
let $ca727f7f34cfa7f3$var$OVERLAY;
class $ca727f7f34cfa7f3$var$Raw {
    constructor(value){
        this._value = value;
    }
    toString() {
        return this._value;
    }
    valueOf() {
        return this._value;
    }
}
let $ca727f7f34cfa7f3$export$cb0933279c36a66b = new class {
    fromHTML(html) {
        $ca727f7f34cfa7f3$var$TEMPLATE.innerHTML = html.trim();
        let cont = $ca727f7f34cfa7f3$var$TEMPLATE.content;
        return cont.children.length > 1 ? cont : cont.children[0];
    }
    addClass(el, cls) {
        el.classList.add(cls);
    }
    delClass(el, cls) {
        if (cls instanceof RegExp) el.className = el.className.replace(cls, "");
        else el.classList.remove(cls);
    }
    hasClass(el, cls) {
        return el.classList.contains(cls);
    }
    condClass(el, cond, clsTrue, clsFalse) {
        el.classList.toggle(clsTrue, !!cond);
        if (clsFalse) el.classList.toggle(clsFalse, !cond);
    }
    toggleClass(el, ...args) {
        el.classList.toggle(...args);
    }
    trash(el) {
        el && el.remove();
    }
    on(element, event, handler, options) {
        if (typeof event == "string") element.addEventListener(event, handler, options || {});
        else Object.keys(event).forEach((ev)=>$ca727f7f34cfa7f3$export$cb0933279c36a66b.on(element, ev, event[ev], handler || {}));
    }
    off(element, event, handler, options) {
        if (typeof event == "string") element.removeEventListener(event, handler, options || {});
        else Object.keys(event).forEach((ev)=>$ca727f7f34cfa7f3$export$cb0933279c36a66b.off(element, ev, event[ev], handler || {}));
    }
    overlayOn(cls) {
        document.body.appendChild($ca727f7f34cfa7f3$var$OVERLAY);
        if (cls) $ca727f7f34cfa7f3$var$OVERLAY.className = cls;
        return $ca727f7f34cfa7f3$var$OVERLAY;
    }
    overlayOff() {
        $ca727f7f34cfa7f3$var$OVERLAY.remove();
    }
    mousePos(ev, el) {
        let box = el.getBoundingClientRect();
        return {
            x: ev.clientX - box.left,
            y: ev.clientY - box.top
        };
    }
    relPos(src, tgt = src.offsetParent) {
        let srcbox = src.getBoundingClientRect();
        let tgtbox = tgt.getBoundingClientRect();
        return {
            x: srcbox.left - tgtbox.left,
            y: srcbox.top - tgtbox.top
        };
    }
    htmlEscape(str) {
        return str instanceof $ca727f7f34cfa7f3$var$Raw ? str + "" : (str + "").replace(/&/g, "&amp;").replace(/\x22/g, "&quot;").replace(/\x27/g, "&#x27;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/\u00A0/g, "&#xa0;");
    }
    htmlSafe(str) {
        return new $ca727f7f34cfa7f3$var$Raw(str);
    }
};
const $ca727f7f34cfa7f3$var$nextDone = {
    next: ()=>({
            done: true
        })
};
const $ca727f7f34cfa7f3$export$d5414a779ebfba6b = new class NIL {
    car = null;
    cdr = this;
    toString() {
        return "NIL";
    }
    valueOf() {
        return null;
    }
    [Symbol.iterator]() {
        return $ca727f7f34cfa7f3$var$nextDone;
    }
    get length() {
        return 0;
    }
};
class $ca727f7f34cfa7f3$export$74c44e616647f5c0 {
    constructor(car, cdr = $ca727f7f34cfa7f3$export$d5414a779ebfba6b){
        this.car = car;
        this.cdr = cdr;
    }
    [Symbol.iterator]() {
        let p = this;
        return {
            next () {
                if (p === $ca727f7f34cfa7f3$export$d5414a779ebfba6b) return {
                    done: true
                };
                let value = p.car;
                p = p.cdr;
                return {
                    value: value
                };
            }
        };
    }
    get length() {
        let p = this, count = 0;
        while(p !== $ca727f7f34cfa7f3$export$d5414a779ebfba6b)++count, p = p.cdr;
        return count;
    }
}
$ca727f7f34cfa7f3$var$OVERLAY = $ca727f7f34cfa7f3$export$cb0933279c36a66b.fromHTML(`<div style="position: fixed; z-index: 20000; left: 0; top: 0; right: 0; bottom: 0"></div>`);
class $ca727f7f34cfa7f3$export$76712d54f90f7348 {
    constructor(options){
        this._ev_handlers = Object.create(null);
        this.o = Object.assign(Object.create(this.constructor.options || null), options);
    }
    addEventListener(event, handler) {
        if (typeof event == "string") {
            let a = this._getHandlers(event);
            $ca727f7f34cfa7f3$export$cd7f480d6b8286c3(a, handler);
            a.push(handler);
        } else Object.keys(event).forEach((ev)=>this.addEventListener(ev, event[ev]));
    }
    removeEventListener(event, handler) {
        if (typeof event == "string") $ca727f7f34cfa7f3$export$cd7f480d6b8286c3(this._getHandlers(event), handler);
        else Object.keys(event).forEach((ev)=>this.removeEventListener(ev, event[ev]));
    }
    listenOnce(event, handler) {
        this.addEventListener(event, function fn(...args) {
            this.removeEventListener(event, fn);
            return handler.apply(this, args);
        });
    }
    callHooks(ev, ...args) {
        this._getHandlers(ev).forEach((f)=>f.apply(this, args));
    }
    destroy() {
        this.callHooks("onDestroy");
    }
    _getHandlers(ev) {
        return this._ev_handlers[ev] || (this._ev_handlers[ev] = []);
    }
}
class $ca727f7f34cfa7f3$export$a829527ff4e4114a extends $ca727f7f34cfa7f3$export$76712d54f90f7348 {
    constructor(...args){
        super(...args);
        this.createElement();
    }
    createElement() {
        let el = document.createElement("div");
        el.className = this.initClassName();
        el._ymacs_object = this;
        this.el = el;
        return el;
    }
    initClassName() {
        return this.constructor.name; // XXX: fail after minification
    }
    getContentElement() {
        return this.el;
    }
    getElement() {
        return this.el;
    }
    add(thing) {
        if (thing instanceof $ca727f7f34cfa7f3$export$a829527ff4e4114a) thing = thing.getElement();
        this.getContentElement().appendChild(thing);
    }
    addClass(cls) {
        $ca727f7f34cfa7f3$export$cb0933279c36a66b.addClass(this.getElement(), cls);
    }
    delClass(cls) {
        $ca727f7f34cfa7f3$export$cb0933279c36a66b.delClass(this.getElement(), cls);
    }
    hasClass(cls) {
        return $ca727f7f34cfa7f3$export$cb0933279c36a66b.hasClass(this.getElement(), cls);
    }
    condClass(cond, clsTrue, clsFalse) {
        $ca727f7f34cfa7f3$export$cb0933279c36a66b.condClass(this.getElement(), cond, clsTrue, clsFalse);
    }
    toggleClass(...args) {
        $ca727f7f34cfa7f3$export$cb0933279c36a66b.toggleClass(this.getElement(), ...args);
    }
    setContent(cont) {
        this.getContentElement().innerHTML = cont;
    }
    setStyle(prop, val) {
        let style = this.getElement().style;
        if (typeof prop == "string") style[prop] = val;
        else Object.assign(style, prop);
    }
    getBox() {
        return this.getElement().getBoundingClientRect();
    }
    destroy() {
        $ca727f7f34cfa7f3$export$cb0933279c36a66b.trash(this.getElement());
        super.destroy(...arguments);
    }
}
function $ca727f7f34cfa7f3$export$cd7f480d6b8286c3(array, element) {
    let pos = 0;
    while((pos = array.indexOf(element, pos)) >= 0)array.splice(pos, 1);
}
function $ca727f7f34cfa7f3$export$ad41d882ec94ba04(fn, timeout = 0, obj, ...args) {
    if (arguments.length > 2) fn = fn.bind(obj, ...args);
    let timer = null;
    return function() {
        clearTimeout(timer);
        timer = setTimeout(fn, timeout);
    };
}
var $ca727f7f34cfa7f3$var$$1K = 1024, $ca727f7f34cfa7f3$var$$1M = $ca727f7f34cfa7f3$var$$1K * 1024, $ca727f7f34cfa7f3$var$$1G = $ca727f7f34cfa7f3$var$$1M * 1024, $ca727f7f34cfa7f3$var$$1T = $ca727f7f34cfa7f3$var$$1G * 1024;
function $ca727f7f34cfa7f3$export$e1a3971de07c83b5(number, fixed) {
    var sz = number, spec, r;
    if (sz < $ca727f7f34cfa7f3$var$$1K) spec = "B";
    else if (sz < $ca727f7f34cfa7f3$var$$1M) {
        sz /= $ca727f7f34cfa7f3$var$$1K;
        spec = "K";
    } else if (sz < $ca727f7f34cfa7f3$var$$1G) {
        sz /= $ca727f7f34cfa7f3$var$$1M;
        spec = "M";
    } else if (sz < $ca727f7f34cfa7f3$var$$1T) {
        sz /= $ca727f7f34cfa7f3$var$$1G;
        spec = "G";
    }
    // spec = " " + spec;
    r = Math.round(sz);
    if (fixed && sz != r) return sz.toFixed(fixed) + spec;
    else return r + spec;
}
function $ca727f7f34cfa7f3$export$3cdc770bf8b2ed3d(thing, width, zero = "0") {
    if (typeof thing == "number") thing = Math.round(thing);
    var s = "" + thing;
    while(s.length < width)s = zero + s;
    return s;
}
function $ca727f7f34cfa7f3$export$3d93c1b4c8f9bdcb(str) {
    return str.trim().split(/\s+/);
}
function $ca727f7f34cfa7f3$export$da43f679ba2e9167(str) {
    return $ca727f7f34cfa7f3$export$3d93c1b4c8f9bdcb(str).reduce((a, key, i)=>(a[key] = i + 1, a), Object.create(null));
}
function $ca727f7f34cfa7f3$export$90e79c6ee773013e(x, mods) {
    if (typeof x == "string") x = $ca727f7f34cfa7f3$export$3d93c1b4c8f9bdcb(x);
    return new RegExp("^(" + x.join("|") + ")$", mods);
}
function $ca727f7f34cfa7f3$export$b43564a9f178c38c(strings) {
    switch(strings.length){
        case 0:
            return "";
        case 1:
            return strings[0];
        case 2:
            let a = strings[0];
            let b = strings[1];
            let n = Math.min(a.length, b.length);
            let i = 0;
            while(i < n && a.charAt(i) === b.charAt(i))++i;
            return a.substring(0, i);
        default:
            return $ca727f7f34cfa7f3$export$b43564a9f178c38c([
                strings[0],
                $ca727f7f34cfa7f3$export$b43564a9f178c38c(strings.slice(1))
            ]);
    }
}
function $ca727f7f34cfa7f3$export$2ceef881d0b4a563() {
    let themes = [];
    for (let st of document.styleSheets)digStyle(st);
    return themes;
    function digStyle(st) {
        for (let rule of st.cssRules)digRule(rule);
    }
    function digRule(rule) {
        if (rule instanceof CSSImportRule) digStyle(rule.styleSheet);
        else {
            let m = /\.Ymacs-Theme-([\p{L}0-9_-]+)/u.exec(rule.selectorText);
            if (m) {
                if (themes.indexOf(m[1]) < 0) themes.push(m[1]);
            }
        }
    }
}
function $ca727f7f34cfa7f3$var$fuzzy_regexp(query) {
    return new RegExp([
        ...query
    ].map((ch)=>{
        ch = ch.replace(/[\]\[\}\{\)\(\*\+\?\.\\\^\$\|]/ug, "\\$&").replace(/[\s_-]/ug, "[\\s_-]");
        return `(${ch})(.*?)`;
    }).join(""), "guid");
}
function $ca727f7f34cfa7f3$export$acebde4da3d957e6(candidates, query) {
    query = query.trim();
    if (!query) return candidates;
    let query_rx = $ca727f7f34cfa7f3$var$fuzzy_regexp(query);
    let word_rx = /(?<![\p{L}\p{N}])/uy;
    let results = [];
    candidates.forEach((item)=>{
        let label = typeof item == "string" ? item : item.label;
        let value = typeof item == "string" ? item : item.value;
        query_rx.lastIndex = 0;
        while(true){
            let m = query_rx.exec(label);
            if (!m) break;
            let score = 0;
            let hil = "";
            let j = 0;
            for(let i = 1; i < m.indices.length;){
                let [li_beg, li_end] = m.indices[i++];
                let [fi_beg, fi_end] = m.indices[i++];
                score += fi_end - fi_beg;
                word_rx.lastIndex = li_beg;
                if (word_rx.test(label)) score -= 2;
                hil += $ca727f7f34cfa7f3$export$cb0933279c36a66b.htmlEscape(label.substring(j, li_beg)) + `<b>${label.substring(li_beg, li_end)}</b>`;
                j = li_end;
            }
            if (j != null) hil += $ca727f7f34cfa7f3$export$cb0933279c36a66b.htmlEscape(label.substr(j));
            results.push({
                label: $ca727f7f34cfa7f3$export$cb0933279c36a66b.htmlSafe(hil),
                value: value,
                score: score
            });
            query_rx.lastIndex = m.index + 1;
        }
    });
    return results.sort((a, b)=>a.score - b.score).reduce((a, item)=>{
        if (!a.some((el)=>el.value == item.value)) a.push(item);
        return a;
    }, []);
}


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

class $c1c0f578f133faed$export$57a830c2aa4f8317 {
    constructor({ pos: pos = null, editor: editor = null, before: before = false, name: name = null } = {}){
        this.position = pos;
        this.editor = editor;
        this.before = before;
        this.name = name;
        this.editor.markers.push(this);
        this.rowcol = null;
        this.onChange = [];
    }
    destroy() {
        (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.editor.markers, this);
        this.editor = null;
    }
    editorChange(pos, diff, min) {
        var p = this.position;
        if (this.before) --p;
        if (diff != 0 && pos <= p) {
            this.rowcol = null;
            this.position += diff;
            if (this.position < min) this.position = min;
            this.callHooks(this.onChange, this.position);
        }
    }
    callHooks(a, arg) {
        for(var i = a.length; --i >= 0;)a[i].call(this.editor, arg);
    }
    getPosition() {
        return this.position;
    }
    valueOf() {
        return this.position;
    }
    setPosition(pos, noHooks, force) {
        if (force || this.position != pos) {
            this.rowcol = null;
            this.position = pos;
            if (!noHooks) this.callHooks(this.onChange, this.position);
        }
    }
    getRowCol() {
        return this.rowcol || (this.rowcol = this.editor._positionToRowCol(this.position));
    }
    swap(other, noHooks, force) {
        var tmp = this.getPosition();
        this.setPosition(other.getPosition(), noHooks, force);
        other.setPosition(tmp, noHooks, force);
    }
}



/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
let $43a6e7b7a98f117c$var$KEYMAPS = Object.create(null);
class $43a6e7b7a98f117c$export$6b6e17e3540cb3d2 {
    constructor(keys){
        this.definitions = Object.create(null);
        this.defineKeys(keys);
    }
    static define(name, keys) {
        let obj = name && $43a6e7b7a98f117c$var$KEYMAPS[name];
        if (!obj) {
            obj = keys instanceof this ? keys : new this(keys);
            if (name) {
                $43a6e7b7a98f117c$var$KEYMAPS[name] = obj;
                obj.name = name;
            }
        }
        return obj;
    }
    static get(name) {
        return $43a6e7b7a98f117c$var$KEYMAPS[name];
    }
    static parseKey(orig) {
        let key = {
            str: ""
        };
        let p = orig;
        for(;;){
            let m = /^([CMSs])-/.exec(p);
            if (m) {
                if (m[1] == "C") key.ctrlKey = true;
                if (m[1] == "M") key.altKey = true;
                if (m[1] == "S") key.shiftKey = true;
                if (m[1] == "s") key.metaKey = true;
                p = p.substr(2);
            } else {
                key.key = p == "Space" ? " " : p.length == 1 ? p.toLowerCase() : p;
                break;
            }
        }
        if (key.ctrlKey) key.str += "C-";
        if (key.altKey) key.str += "M-";
        if (key.shiftKey) key.str += "S-";
        if (key.metaKey) key.str += "s-";
        key.str += p;
        return key;
    }
    static unparseKey(ev) {
        var key, a = [];
        if ("wheelDelta" in ev) key = ev.wheelDelta > 0 ? "WheelUp" : "WheelDown";
        else {
            key = ev.key;
            if (key == " " || key == "Unidentified" && ev.code == "Space") key = "Space";
            if (key.length == 1) key = key.toLowerCase();
        }
        if (ev.ctrlKey) a.push("C");
        if (ev.altKey || ev.ymacsMeta) a.push("M");
        if (ev.shiftKey && (key.length > 1 || key.toLowerCase() != key.toUpperCase())) a.push("S");
        if (ev.metaKey) a.push("s");
        a.sort();
        a.push(key);
        return a.join("-");
    }
    defineKey(key, func, args) {
        if (func instanceof Array) {
            args = func.slice(1);
            func = func[0];
        }
        key = key.trim().split(/\s*&&\s*/);
        if (key.length > 1) key.forEach((key)=>this.defineKey(key, func, args));
        else {
            key = key[0].trim();
            var dfn = this.definitions;
            if (key.indexOf(" ") >= 0) {
                var a = key.split(/\s+/);
                key = a.pop();
                a.forEach((key)=>{
                    key = this.parseKey(key).str;
                    if (!dfn[key]) dfn[key] = {};
                    dfn = dfn[key];
                });
            }
            key = this.parseKey(key);
            dfn[key.str] = [
                func,
                args
            ];
        }
    }
    defineKeys(map) {
        Object.keys(map).forEach((key)=>this.defineKey(key, map[key]));
    }
    getHandler(keys) {
        let handler = null, def = this.definitions;
        for (let key of keys){
            let tmp = handler ? handler[key] : def[key];
            if (tmp) {
                handler = tmp;
                if (Array.isArray(handler)) break;
            } else {
                handler = null;
                break;
            }
        }
        return handler;
    }
    attached() {}
    detached() {}
}
$43a6e7b7a98f117c$export$6b6e17e3540cb3d2.prototype.parseKey = $43a6e7b7a98f117c$export$6b6e17e3540cb3d2.parseKey;


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT
function $b8f5514dd71ab3c2$export$c411e7bd03572a3a(why) {
    this.message = why;
}


function $9e418a0e990aea25$export$1c2a1b3d78098030(args, func) {
    if (arguments.length == 1) {
        func = args;
        args = null;
    } else {
        let documentation;
        if (!(func instanceof Function)) {
            documentation = func;
            func = arguments[2];
            func.ymacsDoc = documentation;
        }
    }
    func.ymacsInteractive = true;
    if (args != null) {
        if (!Array.isArray(args)) {
            var m = /^[\^\@\*]+/.exec(args);
            if (m) {
                m = m[0];
                args = args.substr(m.length);
                if (m.indexOf("^") >= 0) func.ymacsMarkExtend = true;
                if (m.indexOf("*") >= 0) func.ymacsWarnReadonly = true;
                if (m.indexOf("@") >= 0) func.ymacsSelectFrame = true;
            }
            if (args) args = args.split(/\n+/);
        }
        if (args) {
            let collect;
            let execute = function(...rest) {
                collect = collect.concat(rest);
                return this.callInteractively(func, collect, true);
            };
            while(args.length > 0){
                let next = execute;
                execute = $9e418a0e990aea25$var$createArgumentFunction(args.pop(), function(...rest) {
                    collect = collect.concat(rest);
                    return next.call(this);
                });
            }
            func.ymacsCallInteractively = function() {
                collect = [];
                return execute.call(this);
            };
        }
    }
    return func;
}
function $9e418a0e990aea25$export$8c34793404b3efcb(func) {
    return $9e418a0e990aea25$export$1c2a1b3d78098030("p", function(n) {
        if (n == null) n = 1;
        while(n-- > 0)func.call(this);
    });
}
var $9e418a0e990aea25$var$$TRUE = function() {};
$9e418a0e990aea25$var$$TRUE.toString = function() {
    return "";
};
$9e418a0e990aea25$var$$TRUE.empty = true;
/* -----[ argument reader functions ]----- */ function $9e418a0e990aea25$var$prompt(arg) {
    var pr = this.getPrefixArg(true);
    if (pr) arg = pr + " " + arg;
    return this.cmd("minibuffer_prompt", arg);
}
function $9e418a0e990aea25$var$read_function_name(arg, cont) {
    console.log("read_function_name", arg);
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_function", cont);
// XXX: enforce it!
}
function $9e418a0e990aea25$var$read_existing_buffer_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_buffer", cont);
// XXX: enforce it!
}
function $9e418a0e990aea25$var$read_buffer_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_buffer", cont);
}
function $9e418a0e990aea25$var$read_character(arg, cont) {}
function $9e418a0e990aea25$var$read_command_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_command", cont);
// XXX: enforce it!
}
function $9e418a0e990aea25$var$get_point(arg, cont) {
    return cont.call(this, this.point());
}
function $9e418a0e990aea25$var$get_mouse_event(arg, cont) {}
function $9e418a0e990aea25$var$irrelevant(arg, cont) {
    return cont.call(this, null);
}
function $9e418a0e990aea25$var$read_key_sequence(arg, cont) {}
function $9e418a0e990aea25$var$read_key_sequence2(arg, cont) {}
function $9e418a0e990aea25$var$get_mark(arg, cont) {
    return cont.call(this, this.markMarker.getPosition());
}
function $9e418a0e990aea25$var$read_arbitrary_text(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_string", null, cont);
}
function $9e418a0e990aea25$var$read_number(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_number", cont);
}
function $9e418a0e990aea25$var$read_number_or_prefix(arg, cont) {
    var n = parseInt(this.getPrefixArg(), 10);
    if (!isNaN(n)) return cont.call(this, n);
    else return $9e418a0e990aea25$var$read_number.call(this, arg, cont);
}
function $9e418a0e990aea25$var$get_numeric_prefix(arg, cont) {
    var n = parseInt(this.getPrefixArg(), 10);
    if (isNaN(n)) n = null;
    return cont.call(this, n);
}
function $9e418a0e990aea25$var$get_raw_prefix(arg, cont) {
    arg = this.getPrefixArg();
    if (arg === "") arg = $9e418a0e990aea25$var$$TRUE;
    return cont.call(this, arg);
}
function $9e418a0e990aea25$var$get_point_and_mark(arg, cont) {
    var r = this.getRegion();
    return cont.call(this, r.begin, r.end);
}
function $9e418a0e990aea25$var$read_key_sequence3(arg, cont) {}
function $9e418a0e990aea25$var$read_variable_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_variable", cont);
}
function $9e418a0e990aea25$var$read_existing_file_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_existing_file", cont);
}
function $9e418a0e990aea25$var$read_file_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_file", cont);
}
function $9e418a0e990aea25$var$read_file_or_directory_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_file_or_directory", cont);
}
function $9e418a0e990aea25$var$read_existing_directory_name(arg, cont) {
    $9e418a0e990aea25$var$prompt.call(this, arg);
    return this.cmd("minibuffer_read_directory", cont);
}
var $9e418a0e990aea25$var$ARG_READERS = {
    a: $9e418a0e990aea25$var$read_function_name,
    b: $9e418a0e990aea25$var$read_existing_buffer_name,
    B: $9e418a0e990aea25$var$read_buffer_name,
    c: $9e418a0e990aea25$var$read_character,
    C: $9e418a0e990aea25$var$read_command_name,
    d: $9e418a0e990aea25$var$get_point,
    e: $9e418a0e990aea25$var$get_mouse_event,
    i: $9e418a0e990aea25$var$irrelevant,
    k: $9e418a0e990aea25$var$read_key_sequence,
    K: $9e418a0e990aea25$var$read_key_sequence2,
    m: $9e418a0e990aea25$var$get_mark,
    M: $9e418a0e990aea25$var$read_arbitrary_text,
    n: $9e418a0e990aea25$var$read_number,
    N: $9e418a0e990aea25$var$read_number_or_prefix,
    p: $9e418a0e990aea25$var$get_numeric_prefix,
    P: $9e418a0e990aea25$var$get_raw_prefix,
    r: $9e418a0e990aea25$var$get_point_and_mark,
    s: $9e418a0e990aea25$var$read_arbitrary_text,
    U: $9e418a0e990aea25$var$read_key_sequence3,
    v: $9e418a0e990aea25$var$read_variable_name,
    f: $9e418a0e990aea25$var$read_existing_file_name,
    F: $9e418a0e990aea25$var$read_file_name,
    G: $9e418a0e990aea25$var$read_file_or_directory_name,
    D: $9e418a0e990aea25$var$read_existing_directory_name
};
function $9e418a0e990aea25$var$createArgumentFunction(arg, cont) {
    let reader = $9e418a0e990aea25$var$ARG_READERS[arg.charAt(0)];
    arg = arg.substr(1);
    return function() {
        return reader.call(this, arg, cont);
    };
}


// This is the default keymap, as configured in
// Ymacs_Buffer::makeDefaultKeymap.  It follows closely the standard
// Emacs keybindings, with some small deviations that match my taste
// (search for "my stuff" below).
// It would be nice to have more options, such as an Eclipse keymap,
// or Visual Studio -- but I'm not familiar with any of them.
// Contributions welcome.
// A keymap inherits from Ymacs_Keymap and should define its bindings
// in D.KEYS.  They can also define a defaultHandler property to take
// whatever action they consider necessary -- for example the Isearch
// keymap will, by default, print the entered character in the
// minibuffer and trigger a search action.  Isearch mode is almost
// completely defined in a keymap -- with the minor note that
// isearch_forward and isearch_backward are assigned below to key
// combinations; once pressed, they will push the Isearch keymap onto
// the buffer's keymap stack, and it will be used until isearch is
// ended.
/* KEY DEFINITIONS.  A key is generally defined in standard Emacs
   notation, with the following notes:

   - it is possible to define multiple keys at once for the same
     operation, by using the "&&" combination (separate with spaces
     from the actual keys)

   - some special characters are named literally (see event.key
     https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key)

   Normally you would define commands using Ymacs_Buffer.newCommands
   (see ymacs-commands.js) and specify the command name for key
   bindings, but you can directly use a function as well:

   "C-8": function() {
     alert("You pressed CTRL-8");
   }

   It is advised to define commands for more than simple cases, for
   two reasons:

   1. they can be used for "non-interactive" calls too

   2. the command name is saved in this.currentCommand /
      this.previousCommand, which is useful in a number of cases.

*/ let $3dba2f389b913e3b$var$minibuffer_keys = {
    // movement
    "ArrowLeft   && C-b": "backward_char",
    "ArrowRight  && C-f": "forward_char",
    "Home": "beginning_of_indentation_or_line",
    "End && C-e": "end_of_line",
    "C-a": "beginning_of_line",
    "C-Home && M-<": "beginning_of_buffer",
    "C-End && M->": "end_of_buffer",
    "C-ArrowRight && M-f": "forward_word",
    "C-ArrowLeft && M-b": "backward_word",
    // transient mark
    "S-ArrowLeft     && S-C-b": "backward_char_mark",
    "S-ArrowRight    && S-C-f": "forward_char_mark",
    "S-C-ArrowRight  && S-M-f": "forward_word_mark",
    "S-C-ArrowLeft   && S-M-b": "backward_word_mark",
    "S-Home": "beginning_of_indentation_or_line_mark",
    "S-C-a": "beginning_of_line_mark",
    "S-End && S-C-e": "end_of_line_mark",
    "S-C-Home": "beginning_of_buffer_mark",
    "S-C-End": "end_of_buffer_mark",
    // basic editing
    "Backspace": "backward_delete_char",
    "Delete && C-d": "delete_char",
    "M-d && C-Delete": "kill_word",
    "C-Backspace && M-Backspace && M-Delete": "backward_kill_word",
    "C-k": "kill_line",
    "C-y && S-Insert": "yank",
    "M-y": "yank_pop",
    "C-Space": "set_mark_command",
    "C-x C-x": "exchange_point_and_mark",
    "C-w": "kill_region",
    "M-t": "transpose_words",
    "C-t": "transpose_chars",
    "M-w": "copy_region_as_kill",
    "M-c": "capitalize_word",
    "M-u": "upcase_word",
    "M-l": "downcase_word",
    "F11": "nuke_trailing_whitespace",
    "C-/ && C-x u && C-_ && C-z": "undo",
    "Insert": "overwrite_mode",
    "M-/": "dabbrev_expand",
    "C-g": "keyboard_quit",
    "Escape": "keyboard_quit",
    // "Escape"                                  : "_next_is_meta",
    // "M-Escape Escape"                         : "keyboard_quit",
    "C-S-y && C-v": "yank_from_operating_system",
    "M-S-w": "copy_for_operating_system",
    "C-c /": "close_last_xml_tag",
    "S-Backspace": "backward_delete_whitespace",
    "S-Delete": "delete_whitespace",
    "C-x =": "what_cursor_position"
};
let $3dba2f389b913e3b$var$emacs_keys = Object.assign({}, $3dba2f389b913e3b$var$minibuffer_keys, {
    // movement
    "ArrowUp     && C-p": "backward_line",
    "ArrowDown   && C-n": "forward_line",
    "ArrowLeft   && C-b": "backward_char",
    "ArrowRight  && C-f": "forward_char",
    "Home": "beginning_of_indentation_or_line",
    "End && C-e": "end_of_line",
    "C-a": "beginning_of_line",
    "C-Home && M-<": "beginning_of_buffer",
    "C-End && M->": "end_of_buffer",
    "C-ArrowRight && M-f": "forward_word",
    "C-ArrowLeft && M-b": "backward_word",
    "C-ArrowDown": "forward_paragraph",
    "C-ArrowUp": "backward_paragraph",
    "M-h": "mark_paragraph",
    "C-l": "recenter_top_bottom",
    "PageUp": "scroll_up_half",
    "PageDown": "scroll_down_half",
    "WheelUp": "scroll_up",
    "WheelDown": "scroll_down",
    // transient mark
    "S-ArrowUp       && S-C-p": "backward_line_mark",
    "S-ArrowDown     && S-C-n": "forward_line_mark",
    "S-ArrowLeft     && S-C-b": "backward_char_mark",
    "S-ArrowRight    && S-C-f": "forward_char_mark",
    "S-C-ArrowRight  && S-M-f": "forward_word_mark",
    "S-C-ArrowLeft   && S-M-b": "backward_word_mark",
    "S-C-ArrowDown": "forward_paragraph_mark",
    "S-C-ArrowUp": "backward_paragraph_mark",
    "S-Home": "beginning_of_indentation_or_line_mark",
    "S-C-a": "beginning_of_line_mark",
    "S-End && S-C-e": "end_of_line_mark",
    "S-C-Home": "beginning_of_buffer_mark",
    "S-C-End": "end_of_buffer_mark",
    // basic editing
    "Backspace": "backward_delete_char",
    "Delete && C-d": "delete_char",
    "Enter && C-m": "newline",
    "M-d && C-Delete": "kill_word",
    "C-Backspace && M-Backspace && M-Delete": "backward_kill_word",
    "C-k": "kill_line",
    "C-y && S-Insert": "yank",
    "M-y": "yank_pop",
    "C-Space": "set_mark_command",
    "C-x C-x": "exchange_point_and_mark",
    "C-w": "kill_region",
    "M-t": "transpose_words",
    "C-t": "transpose_chars",
    "C-x C-t": "transpose_lines",
    "M-w": "copy_region_as_kill",
    "M-c": "capitalize_word",
    "M-u": "upcase_word",
    "M-l": "downcase_word",
    "F11": "nuke_trailing_whitespace",
    "Tab": "indent_line",
    "C-M-\\": "indent_region",
    "M-q": "fill_paragraph",
    "C-/ && C-x u && C-_ && C-z": "undo",
    "Insert": "overwrite_mode",
    "M-s": "center_line",
    "M-/": "dabbrev_expand",
    "C-s": "isearch_forward",
    "C-r": "isearch_backward",
    "C-S-s": "isearch_yank_word_or_char",
    "M-C-s": "isearch_forward_regexp",
    "M-C-r": "isearch_backward_regexp",
    "M-%": "query_replace",
    "C-M-%": "query_replace_regexp",
    "C-u": "universal_argument",
    "M-g": "goto_line",
    "C-x h": "mark_whole_buffer",
    "C-g": "keyboard_quit",
    "M-^": "delete_indentation",
    "M-;": "comment_dwim",
    "C-x =": "what_cursor_position",
    "C-x f": "set_fill_column",
    "C-x C-\\": "goto_last_change",
    // vertical editing
    "C-x r t": "string_rectangle",
    "C-x r c": "clear_rectangle",
    "C-x r k": "kill_rectangle",
    "C-x r y": "yank_rectangle",
    // buffers
    "C-x C-ArrowRight && C-x ArrowRight && C-Tab": "next_buffer",
    "C-x C-ArrowLeft && C-x ArrowLeft && C-S-Tab": "previous_buffer",
    "C-x b": "switch_to_buffer",
    "C-x k": "kill_buffer",
    // frames
    "C-x 0": "delete_frame",
    "C-x 1": "delete_other_frames",
    "C-x 2": "split_frame_vertically",
    "C-x 3": "split_frame_horizontally",
    "C-x o && M-o": "other_frame",
    "C-x l": "toggle_line_numbers",
    // eval
    "M-x": "execute_extended_command",
    "C-S-y && C-v": "yank_from_operating_system",
    "M-S-w": "copy_for_operating_system",
    // my stuff, sorry if these have different meanings in the standard Emacs keys
    "M-S-y": "yank_shift",
    "C-c /": "close_last_xml_tag",
    "S-Backspace": "backward_delete_whitespace",
    "S-Delete": "delete_whitespace",
    "C-M-d": "delete_region_or_line",
    "M-Enter": "start_next_paragraph",
    "C-M-|": "cperl_lineup",
    "C-F4": "kill_buffer",
    "M-ArrowLeft": [
        "windmove",
        "left"
    ],
    "M-ArrowRight": [
        "windmove",
        "right"
    ],
    "M-ArrowUp": [
        "windmove",
        "up"
    ],
    "M-ArrowDown": [
        "windmove",
        "down"
    ],
    "C-x e": "kmacro_end_and_call_macro",
    "C-x (": "kmacro_start_macro",
    "C-x )": "kmacro_end_macro",
    // file system commands
    "C-x C-f": "find_file",
    "C-x C-w": "write_file",
    "C-x C-s": "save_buffer",
    "C-x s": "save_some_buffers",
    "C-x r w": "window_configuration_to_register",
    "C-x r j": "jump_to_register",
    "C-h m": "describe_mode"
});
let $3dba2f389b913e3b$export$649e07e86b62d08f = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("emacs_mb", $3dba2f389b913e3b$var$minibuffer_keys);
$3dba2f389b913e3b$export$649e07e86b62d08f.defaultHandler = [
    "self_insert_command"
];
let $3dba2f389b913e3b$export$e144e07a20daa9d = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("emacs", $3dba2f389b913e3b$var$emacs_keys);
$3dba2f389b913e3b$export$e144e07a20daa9d.defaultHandler = [
    "self_insert_command"
];
let $3dba2f389b913e3b$export$7f1c7eed09402475 = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("universal_arg", {});
$3dba2f389b913e3b$export$7f1c7eed09402475.defaultHandler = [
    (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        var ev = this.interactiveEvent();
        var ch = ev.key;
        var prefix = this.getPrefixArg(true);
        if ((/^[0-9]$/.test(ch) || ch === "-" && prefix === "") && !ev.altKey && !ev.ctrlKey) {
            prefix += ch;
            this.setPrefixArg(prefix);
            if (!this.isMinibuffer) this.whenMinibuffer(function(mb) {
                mb.cmd("insert", " ", ch);
            });
            return true;
        }
        this.popKeymap($3dba2f389b913e3b$export$7f1c7eed09402475);
        return false;
    })
];
$3dba2f389b913e3b$export$7f1c7eed09402475.attached = (buffer)=>buffer.setPrefixArg("");


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

class $401f5e49452edb15$export$bdebb5ddaa01123f extends (0, $ca727f7f34cfa7f3$export$76712d54f90f7348) {
    constructor({ buffer: buffer }){
        super(...arguments);
        this.buffer = buffer;
        this.reset();
    }
    reset() {
        this.props = [];
    }
    insertLine(row) {
        if (this.props.length < row) this.props[row] = null;
        else this.props.splice(row, 0, null);
    }
    deleteLine(row) {
        this.props.splice(row, 1);
    }
    replaceLine(row, text) {
        var p = this.props[row];
        if (p && p.length > text.length) // remove extra-properties
        p.splice(text.length);
    }
    addLineProps(row, i, j, prop, val) {
        var p = this.props, o, changed = false;
        if (i < j) {
            p = p[row] || (p[row] = []);
            while(i < j){
                o = p[i] || (p[i] = {});
                if (o[prop] != val) changed = true;
                o[prop] = val;
                ++i;
            }
            if (changed) this.callHooks("onChange", row);
        }
        return changed;
    }
    removeLineProps(row, i, j, prop) {
        var p = this.props[row], o, changed = false;
        if (p && i < j) {
            while(i < j){
                o = p[i];
                if (o && prop in o) {
                    changed = true;
                    delete o[prop];
                }
                ++i;
            }
            if (changed) this.callHooks("onChange", row);
        }
        return changed;
    }
    spliceLineProps(row, i, diff) {
        let p = this.props[row];
        if (p) {
            if (diff > 0) p.splice(i, 0, ...new Array(diff));
            else if (diff < 0) p.splice(i, -diff);
        }
    }
    // this uses the "css" text property to intercalate <span class="$css"> ... </span> tags in the given text.
    // "css" properties are added as the tokenizer parses the code and sends onFoundToken events.
    //
    // XXX: this function will be called a lot of times; seems complicated for what it does. Figure out if it can be
    // optimized.
    //
    // Update: the mess got bigger once I decided to embed the caret in the text, rather than have it absolutely
    // positioned (which seems to be the only practical way to position the cursor at the correct location).  It is
    // ESSENTIAL that the start tag of the element that defines the caret ends with "Ymacs-caret'>", so that the
    // frame widget can find it.
    getLineHTML(row, text, caret) {
        var p = this.props[row];
        if (caret === null) {
            if (text == "") return "<br/>";
            if (!p || p.length == 0) return (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text);
        } else {
            if (text == "") return "<span class='Ymacs-caret'>&nbsp;</span>";
            if (!p || p.length == 0) {
                if (caret === text.length) return (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text) + "<span class='Ymacs-caret'>&nbsp;</span>";
                return (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text.substr(0, caret)) + "<span class='Ymacs-caret'>" + (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text.charAt(caret)) + "</span>" + (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text.substr(caret + 1));
            }
        }
        var i = 0, n = text.length, last = null, o, ret = "", ch;
        while(i < n){
            o = p[i];
            o = o && o.css;
            if (i === caret) o = o ? o + " Ymacs-caret" : "Ymacs-caret";
            if (o && o != last) {
                if (last) ret += "</a>";
                ret += "<a ";
                let prev = o;
                o = o.replace(/:href-([^\s]*)/g, function(s, p) {
                    if (p) ret += `href="${p}" `;
                    return "";
                });
                ret += "class='" + o + "'>";
                o = prev;
            } else if (!o && last) ret += "</a>";
            last = o;
            // XXX: Should have used a hash rather than a
            // switch statement?  I'm not sure but I have
            // a feeling that switch is faster.
            ch = text.charAt(i);
            switch(ch){
                case "<":
                    ret += "&lt;";
                    break;
                case ">":
                    ret += "&gt;";
                    break;
                case "&":
                    ret += "&amp;";
                    break;
                default:
                    ret += ch;
                    break;
            }
            ++i;
        }
        if (last) ret += "</a>";
        if (i === caret) // caret is at EOL
        ret += "<span class='Ymacs-caret'>&nbsp;</span>";
        return ret;
    }
}




let $a49159f89f9c9e7a$var$GLOBAL_VARS = {
    case_fold_search: null,
    case_replace: true,
    line_movement_requested_col: 0,
    fill_column: 78,
    tab_width: 8,
    indent_level: 4,
    sticky_mark: false,
    // syntax variables
    syntax_word: /^[\p{N}\p{L}]$/u,
    syntax_capitalize_word: /([\p{N}\p{L}])([\p{N}\p{L}]*)/ug,
    syntax_word_dabbrev: /^[\p{N}_$\p{L}]$/u,
    syntax_word_sexp: /^[\p{N}_$\p{L}]$/u,
    syntax_paragraph_sep: /\n(?:[^\S\r\n]*\n)+/g
};
const $a49159f89f9c9e7a$var$MAX_UNDO_RECORDS = 50000; // XXX: should we not limit?
function $a49159f89f9c9e7a$var$setq(key, val) {
    if (typeof key == "string") {
        if (val === undefined) delete this[key];
        else this[key] = val;
        if (val instanceof Function) val.ymacsCommand = key;
        return val;
    } else {
        var changed = Object.create(null);
        for(var i in key){
            changed[i] = this[i];
            $a49159f89f9c9e7a$var$setq.call(this, i, key[i]);
        }
        return changed;
    }
}
function $a49159f89f9c9e7a$var$MRK(x) {
    return x instanceof (0, $c1c0f578f133faed$export$57a830c2aa4f8317) ? x.getPosition() : x;
}
class $a49159f89f9c9e7a$export$df331bdfc76955b4 extends (0, $ca727f7f34cfa7f3$export$76712d54f90f7348) {
    static options = {
        name: "*scratch*",
        code: "",
        ymacs: null,
        tokenizer: null
    };
    static COMMANDS = Object.create(null);
    static newCommands(...args) {
        return $a49159f89f9c9e7a$var$setq.apply(this.COMMANDS, args);
    }
    static replaceCommands(cmds) {
        this.COMMANDS = Object.assign(Object.create(null), this.COMMANDS);
        let replacements = Object.create(null);
        Object.keys(cmds).forEach((oldcmd)=>{
            let newcmd = cmds[oldcmd];
            if (typeof newcmd == "string") newcmd = this.COMMANDS[newcmd];
            replacements[oldcmd] = newcmd;
        });
        return this.newCommands(replacements);
    }
    static newMode(name, activate) {
        let modevar = "*" + name + "*", hookvar = modevar + "hooks";
        $a49159f89f9c9e7a$export$df331bdfc76955b4.setGlobal(hookvar, []);
        this.COMMANDS[name] = (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("P", function(force) {
            let status = this.getq(modevar);
            if (status) // currently active
            {
                if (force !== true) {
                    // deactivate
                    this.getq(hookvar).forEach((hook)=>hook.call(this, false));
                    if (status instanceof Function) // clean-up
                    status.call(this);
                    this.setq(modevar, null);
                    (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.modes, name);
                }
            } else // inactive
            if (force !== false) {
                let off = activate.apply(this, arguments);
                if (!(off instanceof Function)) off = true;
                this.setq(modevar, off);
                this.modes.push(name);
                this.getq(hookvar).forEach((hook)=>hook.call(this, true));
            }
            return status;
        });
    }
    static getVariable(key) {
        return $a49159f89f9c9e7a$var$GLOBAL_VARS[key];
    }
    static setVariable() {
        return $a49159f89f9c9e7a$var$setq.apply($a49159f89f9c9e7a$var$GLOBAL_VARS, arguments);
    }
    constructor(...args){
        super(...args);
        this.isMinibuffer = this instanceof $a49159f89f9c9e7a$export$36a0d48d1b18d0fa;
        this.COMMANDS = Object.assign(Object.create(null), this.COMMANDS);
        this.name = this.o.name;
        this.ymacs = this.o.ymacs;
        this.tokenizer = this.o.tokenizer;
        this.__savingExcursion = 0;
        this.__preventUpdates = 0;
        this.__preventUndo = 0;
        this.__undoInProgress = 0;
        this.__dirtyLines = [];
        this.__undoQueue = [];
        this.__undoPointer = 0;
        this.markers = [];
        this.caretMarker = this.createMarker(0, false, "point");
        this.markMarker = this.createMarker(0, true, "mark");
        this.matchData = [];
        this.previousCommand = null;
        this.currentCommand = null;
        this.currentKeys = [];
        this.variables = Object.create(null);
        this.globalVariables = $a49159f89f9c9e7a$var$GLOBAL_VARS;
        this.modes = [];
        this.caretMarker.onChange.push(function(pos) {
            this._rowcol = this.caretMarker.getRowCol();
            // XXX: this shouldn't be needed
            if (!this.__preventUpdates) this.callHooks("onPointChange", this._rowcol, this.point());
        });
        this._tokenizerEvents = {
            "onFoundToken": this._on_tokenizerFoundToken.bind(this)
        };
        this._textProperties = new (0, $401f5e49452edb15$export$bdebb5ddaa01123f)({
            buffer: this
        });
        this._textProperties.addEventListener("onChange", this._on_textPropertiesChange.bind(this));
        this.keymap = [];
        this.pushKeymap(this.makeDefaultKeymap());
        this.setCode(this.o.code);
        this._lastCommandWasKill = 0;
    }
    addModeHook(name, func) {
        if (typeof func == "string") func = this.COMMANDS[func];
        let hookvar = "*" + name + "*hooks";
        this.getq(hookvar).pushUnique(func);
    }
    removeModeHook(name, func) {
        if (typeof func == "string") func = this.COMMANDS[func];
        let hookvar = "*" + name + "*hooks";
        (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.getq(hookvar), func);
    }
    withVariables(vars, cont) {
        var saved = this.variables;
        this.variables = Object.assign(Object.create(this.variables), vars);
        try {
            if (cont instanceof Function) return cont.apply(this, [
                ...arguments
            ].slice(2));
            else return this.cmdApply(cont, [
                ...arguments
            ].slice(2));
        } finally{
            this.variables = saved;
        }
    }
    withCommands(cmds, cont) {
        var saved = this.COMMANDS;
        this.COMMANDS = Object.assign(Object.create(this.COMMANDS), cmds);
        try {
            if (cont instanceof Function) return cont.apply(this, [
                ...arguments
            ].slice(2));
            else return this.cmdApply(cont, [
                ...arguments
            ].slice(2));
        } finally{
            this.COMMANDS = saved;
        }
    }
    getVariable(name) {
        return name in this.variables ? this.variables[name] : $a49159f89f9c9e7a$var$GLOBAL_VARS[name];
    }
    setVariable() {
        return $a49159f89f9c9e7a$var$setq.apply(this.variables, arguments);
    }
    /* -----[ public API ]----- */ lastIndexOfRegexp(str, rx, caret, bound = 0) {
        if (!rx.global) rx = new RegExp(rx.source, rx.flags + "g");
        rx.lastIndex = bound;
        let match;
        while(true){
            let pos = rx.lastIndex;
            let m = rx.exec(str);
            if (!m || rx.lastIndex > caret) break;
            match = m;
            match.after = rx.lastIndex;
            if (rx.lastIndex <= pos) {
                // this can happer when rx matches the empty string in the middle of an unicode
                // surrogate pair (which we don't properly support for now). the browser will rewind
                // lastIndex to the proper start of the code point (:-o I'm impressed!), so let's
                // skip it to avoid an endless loop.
                let skipOne = /[^]/ug;
                skipOne.lastIndex = rx.lastIndex;
                skipOne.exec(str);
                rx.lastIndex = skipOne.lastIndex;
            }
        }
        if (match) return this.matchData = match;
    }
    pushKeymap(keymap) {
        if (keymap instanceof Array) keymap.forEach(this.pushKeymap, this);
        else {
            this.popKeymap(keymap);
            this.keymap.push(keymap);
            keymap.attached(this);
        }
    }
    popKeymap(keymap) {
        (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.keymap, keymap);
        keymap.detached(this);
    }
    makeDefaultKeymap() {
        return this.isMinibuffer ? (0, $3dba2f389b913e3b$export$649e07e86b62d08f) : (0, $3dba2f389b913e3b$export$e144e07a20daa9d);
    }
    signalError(text, isHtml, timeout) {
        // Terminates a running macro
        this.ymacs.indicateError();
        this.callHooks("onMessage", {
            type: "error",
            text: text,
            isHtml: isHtml,
            timeout: timeout
        });
    }
    signalInfo(text, isHtml, timeout) {
        this.callHooks("onMessage", {
            type: "info",
            text: text,
            isHtml: isHtml,
            timeout: timeout
        });
    }
    popupMessage(args) {
        this.callHooks("onMessage", args);
    }
    createMarker(pos, before, name) {
        if (pos == null) pos = this.point();
        return new (0, $c1c0f578f133faed$export$57a830c2aa4f8317)({
            editor: this,
            pos: pos,
            name: name,
            before: before
        });
    }
    point() {
        return this.caretMarker.getPosition();
    }
    dirty(dirty) {
        if (dirty != null) {
            this.__isDirty = dirty;
            // XXX: what was this?
            // this.__undoQueue.forEach(x => {
            //     if (x.type !== 3) x.dirty = true;
            // });
            this.updateModeline();
        }
        return this.__isDirty;
    }
    setCode(code) {
        // this.__code = code = code.replace(/\t/g, " ".repeat(this.getq("tab_width")));
        this.dirty(false);
        this.__code = code;
        this.__size = code.length;
        this.__undoQueue = [];
        this.__undoPointer = 0;
        this.markers.map((m)=>m.setPosition(0, true, true));
        this.code = code.split(/\n/);
        this._textProperties.reset();
        if (this.tokenizer) this.tokenizer.reset();
        this.callHooks("onResetCode", this.code);
        this.caretMarker.setPosition(0, false, true);
        this.markMarker.setPosition(0, true);
        this.forAllFrames((frame)=>{
            frame.ensureCaretVisible();
            frame.redrawModelineWithTimer();
        });
    }
    setTokenizer(tok) {
        if (this.tokenizer != null) this.tokenizer.removeEventListener(this._tokenizerEvents);
        this.tokenizer = tok;
        if (tok) {
            tok.addEventListener(this._tokenizerEvents);
            tok.reset();
        } else {
            this._textProperties.reset();
            this.callHooks("onResetCode", this.code);
        }
    }
    getCode() {
        return this.__code || (this.__code = this.code.join("\n"));
    }
    getCodeSize() {
        if (this.__size) return this.__size;
        var i = this.code.length, size = i > 0 ? -1 : 0;
        while(--i >= 0)size += this.code[i].length + 1;
        return this.__size = size;
    }
    getLine(row) {
        if (row == null) row = this._rowcol.row;
        return this.code[row];
    }
    charAtRowCol(row, col) {
        let n = this.code.length;
        if (row >= n--) return null;
        var line = this.code[row];
        if (col == line.length) return row == n ? null : "\n";
        return line.charAt(col);
    }
    charAt(point) {
        if (point == null) point = this.point();
        else {
            point = $a49159f89f9c9e7a$var$MRK(point);
            if (point < 0) point += this.point();
        }
        var rc = this._positionToRowCol(point);
        return this.charAtRowCol(rc.row, rc.col);
    }
    callInteractively(func, args, finalArgs) {
        if (!args) args = [];
        var cmd;
        if (!(func instanceof Function)) {
            cmd = func;
            func = this.COMMANDS[func];
        } else cmd = func.ymacsCommand || null;
        if (func.ymacsCallInteractively && !finalArgs) // after prompting for eventual
        // arguments, ymacsCallInteractively
        // will actually call back again
        // buffer's callInteractively, so we
        // should STOP here.
        return func.ymacsCallInteractively.apply(this, args);
        this.currentCommand = cmd;
        if (this.previousCommand != cmd) {
            this.sameCommandCount(0);
            if (cmd != "undo") this._placeUndoBoundary();
        } else if (cmd != "self_insert_command" || this.sameCommandCount() % 20 == 0) {
            if (cmd != "undo") this._placeUndoBoundary();
        }
        this.preventUpdates();
        try {
            this.callHooks("beforeInteractiveCommand", cmd, func);
            if (!func.ymacsMarkExtend && !this.getq("sticky_mark")) this.clearTransientMark();
            return func.apply(this, args);
        } catch (ex) {
            if (ex instanceof (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)) this.signalError(ex.message);
            else throw ex;
        } finally{
            if (this.getq("sticky_mark")) this.ensureTransientMark();
            if (cmd != "undo") this.__undoPointer = this.__undoQueue.length;
            this.resumeUpdates();
            this.callHooks("afterInteractiveCommand", cmd, func);
            this.previousCommand = cmd;
            this.sameCommandCount(1);
            if (this.tokenizer) this.tokenizer.start();
        }
    }
    resetOverwriteMode(om) {
        if (arguments.length == 0) om = this.overwriteMode;
        this.callHooks("onOverwriteMode", this.overwriteMode = !om);
        this.signalInfo(om ? "Insert mode" : "Overwrite mode");
    }
    getMinibuffer() {
        return this.whenYmacs(function(ymacs) {
            return ymacs.minibuffer;
        });
    }
    getMinibufferFrame() {
        return this.whenYmacs(function(ymacs) {
            return ymacs.minibuffer_frame;
        });
    }
    setMinibuffer(text) {
        this.whenMinibuffer(function(mb) {
            mb.setCode(text);
            mb.cmd("end_of_buffer");
        });
    }
    cmd(cmd, ...args) {
        return this.COMMANDS[cmd].apply(this, args);
    }
    cmdApply(cmd, args) {
        return this.COMMANDS[cmd].apply(this, args);
    }
    forEachLine(func, begin = this.markMarker(), end = this.point()) {
        begin = $a49159f89f9c9e7a$var$MRK(begin);
        end = $a49159f89f9c9e7a$var$MRK(end);
        if (end < begin) {
            let tmp = begin;
            begin = end;
            end = tmp;
        }
        begin = this._positionToRowCol(begin);
        end = this._positionToRowCol(end);
        for(let i = begin.row; i <= end.row; ++i){
            let c1 = i == begin.row ? begin.col : 0;
            let c2 = i == end.row ? end.col : this.getLine(i).length;
            func(i, c1, c2);
        }
    }
    getActiveFrame() {
        return this.whenYmacs("getActiveFrame");
    }
    // This function receives a string and a continuation.  If
    // there is an object property or variable named $what, then
    // $cont is called in the context of this object and given the
    // value of $what as first argument.  The returned value is
    // passed back to caller.
    //
    // The continuation can also be a string, in which case it's
    // assumed to be a method in the value of $what, thus called
    // on it.
    //
    // This is a bit messy, but should work well as long as we
    // don't use the same name for both an object property and a
    // variable in this.variables.  Otherwise, the property takes
    // precedence.
    when(what, cont, ...args) {
        what = this[what] || this.getq(what);
        if (what != null) {
            if (cont instanceof Function) return cont.call(this, what);
            else return what[cont].apply(what, args);
        }
    }
    // XXX: this is way too ugly. (15 years later: Oh Yeah.)
    whenActiveFrame() {
        var fr = this.getActiveFrame(); // miserable hack
        if (fr.buffer === this) {
            this.activeFrame = fr;
            var a = [
                "activeFrame",
                ...arguments
            ];
            return this.when.apply(this, a);
        } else this.activeFrame = null;
    }
    forAllFrames(cont) {
        if (this.ymacs) this.ymacs.getBufferFrames(this).forEach(cont);
    }
    whenYmacs() {
        var a = [
            "ymacs",
            ...arguments
        ];
        return this.when.apply(this, a);
    }
    whenMinibuffer(cont) {
        // In fact, we should move when() into some base
        // object... but which one?  JS doesn't have multiple
        // inheritance, though we could easily "invent" it.
        return this.whenYmacs(function(ymacs) {
            if (ymacs.minibuffer) return cont.call(this, ymacs.minibuffer);
        });
    }
    withMarkers(cont, ...args) {
        let a = args.map((arg)=>this.createMarker(arg));
        try {
            return cont.apply(this, a);
        } finally{
            a.forEach((m)=>m.destroy());
        }
    }
    preventUpdates() {
        ++this.__preventUpdates;
    }
    resumeUpdates() {
        if ((this.__preventUpdates = Math.max(this.__preventUpdates - 1, 0)) == 0) this.redrawDirtyLines();
    }
    getRegion(begin = this.caretMarker, end = this.markMarker) {
        begin = $a49159f89f9c9e7a$var$MRK(begin);
        end = $a49159f89f9c9e7a$var$MRK(end);
        if (end < begin) {
            var tmp = begin;
            begin = end;
            end = tmp;
        }
        return {
            begin: begin,
            end: end
        };
    }
    redrawDirtyLines() {
        this.callHooks("beforeRedraw");
        this.__dirtyLines.forEach((draw, row)=>{
            if (draw) this.callHooks("onLineChange", row);
        });
        this.__dirtyLines = [];
        this.callHooks("afterRedraw");
    }
    setOverlay(name, props) {
        this.callHooks("onOverlayChange", name, props);
    }
    deleteOverlay(name) {
        this.callHooks("onOverlayDelete", name);
    }
    setMark(pos = this.point()) {
        this.markMarker.setPosition(pos);
    }
    ensureTransientMark() {
        var rc = this._rowcol, tm;
        if (!this.transientMarker) {
            this.transientMarker = this.createMarker();
            this.markMarker.setPosition(this.point());
            tm = rc;
        }
        if (!tm) tm = this.transientMarker.getRowCol();
        this.setOverlay("selection", {
            line1: tm.row,
            col1: tm.col,
            line2: rc.row,
            col2: rc.col
        });
    }
    clearTransientMark() {
        if (this.transientMarker) {
            this.transientMarker.destroy();
            this.transientMarker = null;
            this.deleteOverlay("selection");
            this.setq("sticky_mark", false);
        }
    }
    deleteTransientRegion() {
        if (this.transientMarker) {
            this._deleteText(this.caretMarker, this.transientMarker);
            this.clearTransientMark();
            this._placeUndoBoundary();
            return true;
        }
    }
    static #sameCommandCount = 0;
    sameCommandCount(diff) {
        if (diff == null) return $a49159f89f9c9e7a$export$df331bdfc76955b4.#sameCommandCount;
        if (diff == 0) return $a49159f89f9c9e7a$export$df331bdfc76955b4.#sameCommandCount = 0;
        return $a49159f89f9c9e7a$export$df331bdfc76955b4.#sameCommandCount += diff;
    }
    static #lastKeyEvent = null;
    interactiveEvent(ev) {
        if (arguments.length == 0) return $a49159f89f9c9e7a$export$df331bdfc76955b4.#lastKeyEvent;
        return $a49159f89f9c9e7a$export$df331bdfc76955b4.#lastKeyEvent = ev;
    }
    getPrefixArg(noDiscard) {
        var ret = this.getq("universal_prefix");
        if (!noDiscard && ret !== undefined) {
            this.setq("universal_prefix", undefined);
            if (!this.isMinibuffer) this.setMinibuffer("");
        }
        return ret;
    }
    setPrefixArg(val) {
        return this.setq("universal_prefix", val);
    }
    updateModeline() {
        this.callHooks("onModelineChange");
    }
    renderModelineContent(rc, percent) {
        var ml = (this.dirty() ? "**" : "--") + ` <span class="mode-line-buffer-id">${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(this.name)}</span>` + "\xa0\xa0" + percent + " of " + (0, $ca727f7f34cfa7f3$export$e1a3971de07c83b5)(this.getCodeSize(), 2).toLowerCase() + "\xa0\xa0" + "(" + (rc.row + 1) + "," + rc.col + ") ";
        var custom = this.getq("modeline_custom_handler");
        if (custom) {
            custom = custom.call(this, this, rc);
            if (custom) ml += "[" + custom + "] ";
        }
        return ml;
    }
    /* -----[ not-so-public API ]----- */ // BEGIN: undo queue
    _recordChange(type, pos, len, text) {
        if (len > 0) {
            var q = this.__undoQueue;
            if (!this.__preventUndo) {
                q.push({
                    type: type,
                    pos: pos,
                    len: len,
                    text: text,
                    dirty: this.dirty()
                });
                if (q.length > $a49159f89f9c9e7a$var$MAX_UNDO_RECORDS) q.shift();
                this.dirty(true);
            } else q.forEach((x)=>{
                if (x.type == 3) x.markers.forEach((m)=>{
                    if (m[1] >= pos) m[1] += type == 1 ? len : -len;
                });
                else if (x.pos >= pos) x.pos += type == 1 ? len : -len;
            });
        }
    }
    _placeUndoBoundary() {
        var q = this.__undoQueue;
        var m = this.markers.map((m)=>[
                m,
                m.getPosition()
            ]);
        var last = q.at(-1);
        if (!last || last.type != 3) q.push({
            type: 3,
            markers: m
        });
        else last.markers = m;
    }
    _playbackUndo() {
        var q = this.__undoQueue;
        if (q.length == 0) return false;
        ++this.__undoInProgress;
        var didit = false, action;
        while(--this.__undoPointer >= 0){
            action = q[this.__undoPointer];
            if (action.type == 3) {
                // restore markers
                action.markers.forEach((m)=>m[0].setPosition(m[1]));
                if (!didit) continue;
                break;
            }
            didit = true;
            var pos = action.pos;
            switch(action.type){
                case 1:
                    this._deleteText(pos, pos + action.len);
                    break;
                case 2:
                    this._insertText(action.text, pos);
                    break;
            }
            this.dirty(action.dirty);
        }
        --this.__undoInProgress;
        return didit;
    }
    // END: undo
    _replaceLine(row, text) {
        this.code[row] = text;
        this._textProperties.replaceLine(row, text);
        if (!this.__preventUpdates) this.callHooks("onLineChange", row);
        else this.__dirtyLines[row] = true;
    }
    _deleteLine(row) {
        this.code.splice(row, 1);
        this._textProperties.deleteLine(row);
        if (this.tokenizer) this.tokenizer.quickDeleteLine(row);
        this.__dirtyLines.splice(row, 1);
        this.callHooks("onDeleteLine", row);
    }
    _insertLine(row, text) {
        this.code.splice(row, 0, text);
        this._textProperties.insertLine(row);
        if (this.tokenizer) this.tokenizer.quickInsertLine(row);
        var drawIt = !this.__preventUpdates;
        this.callHooks("onInsertLine", row, drawIt);
        if (!drawIt) {
            if (this.__dirtyLines.length <= row) this.__dirtyLines[row] = true;
            else this.__dirtyLines.splice(row, 0, true);
        }
    }
    _insertText(text, pos) {
        if (text.length == 0) return;
        if (pos == null) pos = this.caretMarker.getPosition();
        pos = $a49159f89f9c9e7a$var$MRK(pos);
        // *** UNDO RECORDING
        this._recordChange(1, pos, text.length);
        var rc = pos == this.point() ? this._rowcol : this._positionToRowCol(pos), i = rc.row;
        if (/^\n+$/.test(text) && rc.col == 0) // handle this case separately, since it's so
        // frequently used (ENTER pressed) and the
        // default algorithm messes up colorization
        // for a fraction of a second, flashing badly.
        for(let j = 0; j < text.length; ++j)this._insertLine(i + j, "");
        else {
            var lines = text.split("\n"), ln = this.code[i], rest = ln.substr(rc.col);
            if (lines.length > 1) {
                this._replaceLine(i, ln.substr(0, rc.col) + lines.shift());
                lines.forEach((text)=>this._insertLine(++i, text));
                this._replaceLine(i, this.code[i] + rest);
            } else this._replaceLine(i, ln.substr(0, rc.col) + lines[0] + ln.substr(rc.col));
        }
        this._updateMarkers(pos, text.length);
        this.callHooks("onChange", 1, pos, text);
    }
    _deleteText(begin, end) {
        begin = this._boundPosition($a49159f89f9c9e7a$var$MRK(begin), 0);
        end = this._boundPosition($a49159f89f9c9e7a$var$MRK(end), 1);
        if (begin == null || end == null || begin == end) return;
        if (end < begin) {
            var tmp = begin;
            begin = end;
            end = tmp;
        }
        // *** UNDO RECORDING
        this._recordChange(2, begin, end - begin, this._bufferSubstring(begin, end));
        var brc = this._positionToRowCol(begin), erc = this._positionToRowCol(end);
        var line = this.code[brc.row];
        if (brc.row == erc.row) {
            // same line, that's easy
            line = line.substr(0, brc.col) + line.substr(erc.col);
            this._replaceLine(brc.row, line);
        } else {
            // fix first line
            line = line.substr(0, brc.col) + this.code[erc.row].substr(erc.col);
            this._replaceLine(brc.row, line);
            // delete lines in between
            line = brc.row + 1;
            for(let n = erc.row - brc.row; n-- > 0;)this._deleteLine(line);
        }
        this._updateMarkers(begin, begin - end, begin);
        this.callHooks("onChange", 2, begin, end);
    }
    _replaceText(begin, end, text) {
        this._deleteText(begin, end);
        this._insertText(text, begin);
    }
    _swapAreas(a) {
        a = a.map($a49159f89f9c9e7a$var$MRK).sort((a, b)=>a - b);
        var b1 = a[0];
        var e1 = a[1];
        var b2 = a[2];
        var e2 = a[3];
        var t1 = this._bufferSubstring(b1, e1);
        var t2 = this._bufferSubstring(b2, e2);
        this._replaceText(b2, e2, t1);
        this._replaceText(b1, e1, t2);
        return e2;
    }
    _bufferSubstring(begin, end) {
        if (begin == null) begin = this.point();
        else begin = $a49159f89f9c9e7a$var$MRK(begin);
        if (end == null) end = this.getCodeSize();
        else end = $a49159f89f9c9e7a$var$MRK(end);
        if (end < begin) {
            var tmp = begin;
            begin = end;
            end = tmp;
        }
        // var brc = this._positionToRowCol(begin),
        //     erc = this._positionToRowCol(end);
        // if (brc.row == erc.row) {
        //         return this.code[brc.row].substring(brc.col, erc.col);
        // } else return [ this.code[brc.row].substr(brc.col) ].
        //         concat(this.code.slice(brc.row + 1, erc.row)).
        //         concat(this.code[erc.row].substr(0, erc.col)).
        //         join("\n");
        return this.getCode().substring(begin, end);
    }
    _killingAction(p1, p2, prepend, noDelete) {
        p1 = $a49159f89f9c9e7a$var$MRK(p1);
        p2 = $a49159f89f9c9e7a$var$MRK(p2);
        var text = this._bufferSubstring(p1, p2);
        this._saveKilledText(text, prepend);
        if (!noDelete) this._deleteText(p1, p2);
        this.clearTransientMark();
    }
    _saveKilledText(text, prepend) {
        if (!this._lastCommandWasKill) this.ymacs.killRingToMaster();
        this.ymacs.pushToKillRing(text, prepend);
        this._lastCommandWasKill++;
        if (this.interactiveEvent()) try {
            navigator.clipboard.writeText(this.ymacs.killRingText());
        } catch  {}
    }
    _positionToRowCol(pos) {
        var line = 0, a = this.code, n = a.length;
        while(pos > 0 && line < n){
            var len = a[line].length;
            if (len >= pos) break;
            pos -= len + 1; // one for the newline
            line++;
        }
        return {
            row: line,
            col: pos
        };
    }
    _rowColToPosition(row, col) {
        var pos = 0, a = this.code, i = Math.min(row, a.length - 1), n = i;
        if (i < 0) return 0;
        while(--i >= 0)pos += a[i].length + 1; // one for the newline
        return pos + Math.min(col, a[n].length);
    }
    _boundPosition(pos) {
        if (pos < 0) return 0;
        return Math.min(pos, this.getCodeSize());
    }
    _repositionCaret(pos) {
        var p = this.caretMarker.getPosition();
        if (pos == null) pos = p;
        pos = $a49159f89f9c9e7a$var$MRK(pos);
        pos = this._boundPosition(pos);
        this.caretMarker.setPosition(pos);
        return pos != p;
    }
    _updateMarkers(offset, delta, min = 0) {
        this.__size = null;
        this.__code = null;
        // if (this.__undoInProgress == 0) {
        this.markers.map((m)=>m.editorChange(offset, delta, min));
        // }
        if (this.tokenizer) {
            let row = this._positionToRowCol(Math.min(offset, offset + delta)).row;
            this.tokenizer.truncate(row);
        }
    }
    _saveExcursion(cont, markerBefore) {
        var tmp = this.createMarker(null, markerBefore);
        ++this.__savingExcursion;
        try {
            return cont.call(this);
        } finally{
            --this.__savingExcursion;
            this.caretMarker.swap(tmp, false, true);
            tmp.destroy();
        }
    }
    _disableUndo(cont) {
        ++this.__preventUndo;
        try {
            return cont.call(this);
        } finally{
            --this.__preventUndo;
        }
    }
    _handleKeyEvent(ev) {
        var handled = false;
        this.interactiveEvent(ev);
        var lcwk = this._lastCommandWasKill;
        if (this.__nextIsMeta) ev.ymacsMeta = true;
        this.__nextIsMeta = false;
        var key = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).unparseKey(ev);
        var cc = this.currentKeys;
        var foundPrefix = false;
        cc.push(key);
        for(let i = this.keymap.length; --i >= 0;){
            let km = this.keymap[i];
            let h = km.getHandler(cc);
            if (h instanceof Array) {
                if (this.callInteractively(h[0], h[1]) !== "ymacs-decline-key") handled = true;
            } else if (h) handled = foundPrefix = true;
            else if (km.defaultHandler && cc.length == 1) handled = this.callInteractively(km.defaultHandler[0], km.defaultHandler[1]);
            if (handled) break;
        }
        if (!foundPrefix) {
            if (!handled) {
                if (cc.length > 1) {
                    this.signalError(cc.join(" ").bold() + " is undefined", true);
                    handled = true;
                }
            }
            cc.splice(0, cc.length);
        }
        if (this._lastCommandWasKill == lcwk && typeof handled != "object" && !this.__nextIsMeta) // selecting a prefix keymap shouldn't clear the killRing
        this._lastCommandWasKill = 0;
        this.callHooks("finishedEvent", handled);
        this.interactiveEvent(null);
        // XXX: always preventDefault() in minibuffer (return true
        // here); seems good, objections?
        return handled || this.isMinibuffer;
    }
    _on_tokenizerFoundToken(row, c1, c2, what) {
        if (what) this._textProperties.addLineProps(row, c1, c2, "css", what);
        else this._textProperties.removeLineProps(row, c1, c2, "css");
    }
    _on_textPropertiesChange(row) {
        if (!this.__preventUpdates) this.callHooks("onLineChange", row);
        else this.__dirtyLines[row] = true;
    }
    formatLineHTML(row, caret) {
        var rc = this._rowcol;
        if (caret instanceof (0, $c1c0f578f133faed$export$57a830c2aa4f8317)) rc = caret.getRowCol();
        caret = row == rc.row ? rc.col : null;
        return this._textProperties.getLineHTML(row, this.code[row], caret);
    }
    looking_at(needle) {
        var haystack = this.getCode();
        if (needle instanceof RegExp) {
            var pos = needle.lastIndex = this.point();
            var ret = needle.exec(haystack);
            if (ret && ret.index == pos) {
                ret.after = needle.lastIndex;
                return this.matchData = ret;
            }
        } else if (typeof needle == "string") // XXX: account for case_fold_search here?
        return haystack.substr(this.point(), needle.length) == needle;
    }
    looking_back(needle) {
        var haystack = this.getCode();
        if (needle instanceof RegExp) {
            var m = this.lastIndexOfRegexp(haystack, needle, this.point());
            if (m && m.after == this.point()) return m;
        } else if (typeof needle == "string") // XXX: account for case_fold_search here?
        return haystack.substr(this.point() - needle.length, needle.length) == needle;
    }
    capitalize(str) {
        return str.replace(this.getq("syntax_capitalize_word"), (_, a, b)=>a.toUpperCase() + b.toLowerCase());
    }
}
// XXX: what a mess..
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.COMMANDS = $a49159f89f9c9e7a$export$df331bdfc76955b4.COMMANDS;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.newCommands = $a49159f89f9c9e7a$export$df331bdfc76955b4.newCommands;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.replaceCommands = $a49159f89f9c9e7a$export$df331bdfc76955b4.replaceCommands;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.newMode = $a49159f89f9c9e7a$export$df331bdfc76955b4.newMode;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.addModeHook = $a49159f89f9c9e7a$export$df331bdfc76955b4.addModeHook;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.removeModeHook = $a49159f89f9c9e7a$export$df331bdfc76955b4.removeModeHook;
$a49159f89f9c9e7a$export$df331bdfc76955b4.setq = $a49159f89f9c9e7a$export$df331bdfc76955b4.setGlobal = $a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.setGlobal = $a49159f89f9c9e7a$export$df331bdfc76955b4.setVariable;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.setq = $a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.setVariable;
$a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.getq = $a49159f89f9c9e7a$export$df331bdfc76955b4.prototype.getVariable;
$a49159f89f9c9e7a$export$df331bdfc76955b4.getq = $a49159f89f9c9e7a$export$df331bdfc76955b4.getVariable;
class $a49159f89f9c9e7a$export$36a0d48d1b18d0fa extends $a49159f89f9c9e7a$export$df331bdfc76955b4 {
    constructor(...args){
        super(...args);
        this.promptMarker = this.createMarker(0, true, "prompt");
        this.setq("minibuffer_validation", (whatever)=>true);
    }
    // prompt(text) {
    //     text = text.trim() + " ";
    //     this._disableUndo(() => {
    //         this.setCode(text);
    //         this._textProperties.addLineProps(0, 0, text.length - 1, "css", "minibuffer-prompt");
    //         this._repositionCaret(text.length);
    //         this.promptMarker.setPosition(text.length, true, true);
    //     });
    // }
    prompt(text) {
        text = text.trim() + " ";
        this._disableUndo(()=>{
            let pos = this.promptMarker.getPosition();
            this.promptMarker.setPosition(0, true, true);
            this._replaceText(0, pos, text);
            this.promptMarker.setPosition(text.length, true, true);
            this._textProperties.spliceLineProps(0, 0, text.length - pos);
            this._textProperties.addLineProps(0, 0, text.length - 1, "css", "minibuffer-prompt");
        });
    }
    _boundPosition(pos) {
        return Math.max($a49159f89f9c9e7a$var$MRK(this.promptMarker), Math.min(pos, this.getCodeSize()));
    }
}


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT



let $0c7e6e98ae65431f$var$MENU = Symbol("MENU");
class $0c7e6e98ae65431f$export$731251e1d39d8621 extends (0, $ca727f7f34cfa7f3$export$a829527ff4e4114a) {
    static options = {};
    constructor(...args){
        super(...args);
        this._cont = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="Ymacs_Menu"></div>`);
        this.getElement().appendChild(this._cont);
    }
    getContentElement() {
        return this._cont;
    }
    initClassName() {
        return "Ymacs_Popup";
    }
}
var $0c7e6e98ae65431f$var$KEYMAP_MENU_ACTIVE = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define(null, {
    "ArrowDown && C-n": $0c7e6e98ae65431f$var$handle_arrow_down,
    "ArrowUp && C-p": $0c7e6e98ae65431f$var$handle_arrow_up,
    "PageDown": $0c7e6e98ae65431f$var$handle_arrow_down,
    "PageUp": $0c7e6e98ae65431f$var$handle_arrow_up,
    "C-End && M->": $0c7e6e98ae65431f$var$handle_popup_end,
    "C-Home && M-<": $0c7e6e98ae65431f$var$handle_popup_home,
    "Enter": $0c7e6e98ae65431f$var$handle_enter,
    "Escape": $0c7e6e98ae65431f$var$handle_escape
});
$0c7e6e98ae65431f$var$KEYMAP_MENU_ACTIVE.defaultHandler = [
    function() {
        let buffer = this;
        let menu = buffer[$0c7e6e98ae65431f$var$MENU];
        menu.kill();
        return false; // say it's not handled though
    }
];
function $0c7e6e98ae65431f$export$7b89d1d66e79a77c({ buffer: buffer, items: items, onSelect: onSelect } = {}) {
    let menuWidget = new $0c7e6e98ae65431f$export$731251e1d39d8621();
    menuWidget.addClass("with-arrow");
    items.forEach((label, index)=>{
        let value = label;
        if (typeof label != "string") {
            value = label.value;
            label = label.label;
        }
        let el = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="Ymacs_Menu_Item" data-value="${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(value)}"
                                          data-index="${index}">${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(label)}</div>`);
        menuWidget.add(el);
    });
    buffer[$0c7e6e98ae65431f$var$MENU] = {
        items: items,
        widget: menuWidget,
        onSelect: onSelect,
        kill () {
            menuWidget.destroy();
            buffer[$0c7e6e98ae65431f$var$MENU] = null;
        }
    };
    let activeElement = document.activeElement;
    let ymacs = buffer.ymacs;
    (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(menuWidget.getContentElement(), {
        click: (ev)=>{
            activeElement.focus();
            let item = ev.target;
            if (!(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).hasClass(item, "Ymacs_Menu_Item")) return;
            $0c7e6e98ae65431f$var$select(buffer, +item.dataset.index);
            $0c7e6e98ae65431f$var$handle_enter.call(buffer);
        }
    });
    ymacs._popupAtCaret(menuWidget.getElement());
    $0c7e6e98ae65431f$var$select(buffer, 0);
    buffer.pushKeymap($0c7e6e98ae65431f$var$KEYMAP_MENU_ACTIVE);
    menuWidget.addEventListener("onDestroy", ()=>{
        buffer.popKeymap($0c7e6e98ae65431f$var$KEYMAP_MENU_ACTIVE);
    });
}
function $0c7e6e98ae65431f$var$handle_arrow_up() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    $0c7e6e98ae65431f$var$select(buffer, menu.selectedIndex - 1);
}
function $0c7e6e98ae65431f$var$handle_arrow_down() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    $0c7e6e98ae65431f$var$select(buffer, menu.selectedIndex + 1);
}
function $0c7e6e98ae65431f$var$handle_popup_home() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    $0c7e6e98ae65431f$var$select(buffer, 0);
}
function $0c7e6e98ae65431f$var$handle_popup_end() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    $0c7e6e98ae65431f$var$select(buffer, -1);
}
function $0c7e6e98ae65431f$var$handle_enter() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    if (!(menu.onSelect(menu.selectedIndex) === false)) menu.kill();
}
function $0c7e6e98ae65431f$var$handle_escape() {
    let buffer = this;
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    menu.kill();
}
function $0c7e6e98ae65431f$var$select(buffer, index) {
    let menu = buffer[$0c7e6e98ae65431f$var$MENU];
    let cont = menu.widget.getContentElement();
    let elements = [
        ...cont.querySelectorAll(".Ymacs_Menu_Item")
    ];
    let n = elements.length;
    index = menu.selectedIndex = (index % n + n) % n;
    elements.forEach((el)=>{
        let current = el.dataset.index == index;
        if (current) menu.selectedItem = el;
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).condClass(el, current, "selected");
    });
    menu.selectedItem.scrollIntoView({
        block: "nearest"
    });
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    popup_menu: function(args) {
        args.buffer = this;
        $0c7e6e98ae65431f$export$7b89d1d66e79a77c(args);
    }
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

function $86e983c67b3f6085$var$textColX(div, col) {
    let range = document.createRange();
    range.selectNodeContents(div);
    let treeWalker = document.createTreeWalker(div, NodeFilter.SHOW_TEXT);
    let len = 0;
    while(treeWalker.nextNode()){
        let node = treeWalker.currentNode;
        let clen = node.nodeValue.length;
        if (len + clen >= col) {
            range.setStart(node, col - len);
            range.setEnd(node, col - len);
            break;
        }
        len += clen;
    }
    return range.getBoundingClientRect().right;
}
function $86e983c67b3f6085$var$normalizeRect(p) {
    return p.line1 > p.line2 || p.line1 == p.line2 && p.col1 > p.col2 ? {
        line1: p.line2,
        col1: p.col2,
        line2: p.line1,
        col2: p.col1
    } : p;
}
var $86e983c67b3f6085$var$DBL_CLICK_SPEED = 300;
var $86e983c67b3f6085$var$CLICK_COUNT = 0, $86e983c67b3f6085$var$CLICK_COUNT_TIMER = null, $86e983c67b3f6085$var$CLICK_LAST_TIME = null;
function $86e983c67b3f6085$var$CLEAR_CLICK_COUNT() {
    $86e983c67b3f6085$var$CLICK_COUNT = null;
}
var $86e983c67b3f6085$var$LINE_DIV = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="line"><br/></div>`);
var $86e983c67b3f6085$var$COUNT = 0;
class $86e983c67b3f6085$export$3230e5a51f22d844 extends (0, $ca727f7f34cfa7f3$export$a829527ff4e4114a) {
    static options = {
        buffer: null,
        ymacs: null,
        isMinibuffer: false,
        point: null
    };
    constructor(...args){
        super(...args);
        this.buffer = this.o.buffer;
        this.ymacs = this.o.ymacs;
        this.isMinibuffer = this.o.isMinibuffer;
        this.id = `ymacs-frame-${++$86e983c67b3f6085$var$COUNT}`;
        this.__caretId = `ymacs-caret-${$86e983c67b3f6085$var$COUNT}`;
        this.redrawModelineWithTimer = (0, $ca727f7f34cfa7f3$export$ad41d882ec94ba04)(this.redrawModeline.bind(this));
        this.getElement().tabIndex = 0;
        this.getElement().innerHTML = "<div class='Ymacs-frame-overlays'><div class='Ymacs-frame-content'></div></div><div class='Ymacs_Modeline'></div>";
        this.addEventListener({
            onDestroy: this._on_destroy
        });
        this._dragSelectHandlers = {
            mousemove: this._dragSelect_onMouseMove.bind(this),
            mouseup: this._dragSelect_onMouseUp.bind(this)
        };
        this._bufferEvents = {
            onLineChange: this._on_bufferLineChange.bind(this),
            onInsertLine: this._on_bufferInsertLine.bind(this),
            onDeleteLine: this._on_bufferDeleteLine.bind(this),
            onPointChange: this._on_bufferPointChange.bind(this),
            onResetCode: this._on_bufferResetCode.bind(this),
            onOverwriteMode: this._on_bufferOverwriteMode.bind(this),
            onModelineChange: this._on_bufferModelineChange.bind(this),
            beforeInteractiveCommand: this._on_bufferBeforeInteractiveCommand.bind(this),
            afterInteractiveCommand: this._on_bufferAfterInteractiveCommand.bind(this),
            onOverlayChange: this._on_bufferOverlayChange.bind(this),
            onOverlayDelete: this._on_bufferOverlayDelete.bind(this)
        };
        this._moreBufferEvents = {
            onMessage: this._on_bufferMessage.bind(this),
            afterInteractiveCommand: (function() {
                if (this.__ensureCaretVisible) this.ensureCaretVisible();
            }).bind(this)
        };
        var buffer = this.buffer;
        this.buffer = null;
        if (buffer) this.setBuffer(buffer, this.o.point);
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(this.getOverlaysContainer(), "scroll", this._on_scroll.bind(this));
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(this.getElement(), {
            mousedown: this._dragSelect_onMouseDown.bind(this),
            focus: this._on_focus.bind(this),
            blur: this._on_blur.bind(this),
            keydown: this._on_keyDown.bind(this),
            keyup: this._on_keyUp.bind(this),
            wheel: this._on_mouseWheel.bind(this)
        });
    }
    initClassName() {
        return `Ymacs_Frame${this.o.isMinibuffer ? ' Ymacs_Minibuffer' : ''}`;
    }
    focus(exitAllowed) {
        this.getElement().focus();
    // if (exitAllowed instanceof Function) {
    //     this.removeEventListener("onBlur", this.__exitFocusHandler);
    //     this.addEventListener("onBlur", this.__exitFocusHandler = function(){
    //         if (exitAllowed.call(this.buffer)) {
    //             this.removeEventListener("onBlur", this.__exitFocusHandler);
    //         } else {
    //             this.focus.delayed(2, this, null);
    //         }
    //     });
    // }
    }
    blur(force) {
    // if (force)
    //     this.removeEventListener("onBlur", this.__exitFocusHandler);
    }
    getOverlaysContainer() {
        return this.getElement().firstChild;
    }
    getModelineElement() {
        return this.getElement().childNodes[1];
    }
    getContentElement() {
        return this.getElement().firstChild.firstChild;
    }
    getCaretElement() {
        return document.getElementById(this.__caretId);
    }
    getLineDivElement(row) {
        return this.getContentElement().childNodes[row] || null;
    }
    ensureCaretVisible() {
        // return true if the scroll position has changed
        let div = this.getOverlaysContainer();
        let st = div.scrollTop;
        this.redrawCaret();
        let caret = this.getCaretElement();
        if (caret) {
            caret.scrollIntoView({
                block: "nearest",
                inline: "nearest"
            });
            if (caret.offsetLeft < div.clientWidth / 2) div.scrollLeft = 0;
        }
        return st != div.scrollTop;
    }
    focusInside() {
        return document.activeElement === this.getElement();
    }
    setBuffer(buffer, point) {
        if (buffer === this.buffer) return;
        if (this.buffer) {
            if (this.caretMarker && !this.o.isMinibuffer) {
                this.caretMarker.destroy();
                this.caretMarker = null;
            }
            this.buffer.removeEventListener(this._bufferEvents);
            this.buffer.removeEventListener(this._moreBufferEvents);
        }
        this.buffer = buffer;
        if (buffer) {
            buffer.addEventListener(this._bufferEvents);
            if (this.focusInside()) buffer.addEventListener(this._moreBufferEvents);
            if (this.o.isMinibuffer) this.caretMarker = buffer.caretMarker;
            else this.caretMarker = buffer.createMarker(point ?? +buffer.caretMarker, false, "framecaret");
            this._redrawBuffer();
            this.redrawCaret(true);
            this.centerOnCaret();
        }
    }
    recenterTopBottom(pos = 1) {
        let row = this.buffer._rowcol.row;
        let line = this.getLineDivElement(row);
        let div = this.getOverlaysContainer();
        let center = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
        let top = Math.round(line.offsetTop);
        let bottom = Math.round(line.offsetTop - div.clientHeight + line.offsetHeight);
        if (pos == 0) div.scrollTop = center;
        else if (pos == 1) div.scrollTop = top;
        else div.scrollTop = bottom;
    }
    centerOnCaret() {
        this.centerOnLine(this.buffer._rowcol.row);
    }
    centerOnLine(row) {
        var line = this.getLineDivElement(row), div = this.getOverlaysContainer();
        div.scrollTop = Math.round(line.offsetTop - div.clientHeight / 2 + line.offsetHeight / 2);
    // this._redrawBuffer();
    }
    setModelineContent(html) {
        this.getModelineElement().innerHTML = html;
    }
    deleteOtherFrames() {
        this.ymacs.keepOnlyFrame(this);
    }
    deleteFrame() {
        this.ymacs.deleteFrame(this);
    }
    _split(horiz) {
        let ovdiv = this.getOverlaysContainer();
        let scroll = ovdiv.scrollTop;
        let fr = this.ymacs.createFrame({
            buffer: this.buffer
        });
        let sc = new $86e983c67b3f6085$export$7b0e4d18ade0c4c9({
            horiz: horiz
        });
        this.getElement().replaceWith(sc.getElement());
        sc.setSplit(this, fr);
        ovdiv.scrollTop = scroll;
        fr.getOverlaysContainer().scrollTop = scroll;
        this.focus(); // XXX: only if necessary
        return fr;
    }
    vsplit() {
        return this._split(true);
    }
    hsplit(percent) {
        return this._split(false);
    }
    __showCaret() {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).addClass(this.getCaretElement(), "Ymacs-caret");
    }
    redrawCaret(force) {
        if (this.o.isMinibuffer) force = true;
        var isActive = this.ymacs.getActiveFrame() === this;
        if (!force && !isActive) return;
        if (isActive && !this.o.isMinibuffer && this.focusInside()) this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
        var rc = this.buffer._rowcol;
        // hide stale carets :-\
        // mess everywhere.
        [
            ...this.getElement().querySelectorAll(".Ymacs-caret, #" + this.__caretId)
        ].forEach((el)=>{
            el.id = "";
            el.className = "";
        });
        // redraw the line where the caret was previously, so that it disappears from there
        if (this.__prevCaretLine != null) this._on_bufferLineChange(this.__prevCaretLine);
        // redraw current line if it's different
        if (this.__prevCaretLine != rc.row) {
            this.__prevCaretLine = rc.row;
            this._on_bufferLineChange(rc.row);
        }
        this.callHooks("onPointChange", rc.row, rc.col);
        this.redrawModelineWithTimer(rc);
    }
    _getLineHTML(row) {
        var html = this.buffer.formatLineHTML(row, this.caretMarker);
        // taking advantage of the fact that a literal > entered by the user will never appear in
        // the generated HTML, since special HTMl characters are escaped.
        var pos = html.indexOf("Ymacs-caret'>");
        if (pos >= 0) html = html.substr(0, pos + 12) + " id='" + this.__caretId + "'" + html.substr(pos + 12);
        return html;
    }
    _redrawBuffer() {
        this.setContent(this.buffer.code.map((line, i)=>`<div class="line">${this._getLineHTML(i)}</div>`).join(""));
    }
    coordinatesToRowCol(x, y) {
        function findLine(r1, r2) {
            if (r1 >= r2) return r1;
            var row = Math.floor((r1 + r2) / 2);
            var div = self.getLineDivElement(row);
            var y1 = div.offsetTop;
            var y2 = y1 + div.offsetHeight;
            if (y2 < y) return findLine(row + 1, r2);
            if (y < y1) return findLine(r1, row - 1);
            return row;
        }
        function findCol(c1, c2) {
            if (c1 >= c2) return c1;
            var col = Math.floor((c1 + c2) / 2);
            var p1 = self.coordinates(row, col), p2 = self.coordinates(row, col + 1);
            if (p2.x < x) return findCol(col + 1, c2);
            if (x < p1.x) return findCol(c1, col - 1);
            return col;
        }
        var self = this, row = findLine(0, this.buffer.code.length - 1), col = findCol(0, this.buffer.code[row].length);
        return {
            row: row,
            col: col
        };
    }
    coordinates(row, col) {
        var box = this.getContentElement().getBoundingClientRect();
        var div = this.getLineDivElement(row);
        return {
            x: $86e983c67b3f6085$var$textColX(div, col) - box.left,
            y: div.offsetTop,
            h: div.offsetHeight
        };
    }
    heightInLines() {
        return Math.floor(this.getOverlaysContainer().clientHeight / this.getContentElement().firstChild.offsetHeight);
    }
    redrawModeline(rc) {
        if (!rc) rc = this.caretMarker.getRowCol();
        var maxline = this.buffer.code.length - 1;
        var firstline = this.firstLineVisible();
        var lastline = this.lastLineVisible();
        var percent = firstline == 0 ? "Top" : lastline == maxline ? "Bot" : Math.round(lastline / maxline * 100) + "%";
        this.setModelineContent(this.buffer.renderModelineContent(rc, percent));
    }
    /* -----[ event handlers ]----- */ _on_bufferLineChange(row) {
        var div = this.getLineDivElement(row);
        if (div) //console.log("Redrawing line %d [%s]", row, this.buffer.code[row]);
        //console.log(new Error().stack);
        div.innerHTML = this._getLineHTML(row);
    }
    _on_bufferInsertLine(row, drawIt) {
        var div = $86e983c67b3f6085$var$LINE_DIV.cloneNode(true);
        this.getContentElement().insertBefore(div, this.getLineDivElement(row));
        if (drawIt) div.innerHTML = this._getLineHTML(row);
    }
    _on_bufferDeleteLine(row) {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).trash(this.getLineDivElement(row));
    }
    _on_bufferPointChange(rc, pos) {
        this.redrawCaret();
    }
    _on_bufferResetCode() {
        this._redrawBuffer();
    }
    _on_bufferOverwriteMode(om) {
        this.condClass(om, "Ymacs-overwrite-mode");
    }
    _on_bufferMessage(args) {
        this.ymacs.popupMessage(args);
    }
    _on_bufferBeforeInteractiveCommand() {
        this.__ensureCaretVisible = true;
        this.ymacs.clearPopupMessage();
    }
    _on_bufferAfterInteractiveCommand() {}
    _on_bufferModelineChange() {
        this.redrawModelineWithTimer(null);
    }
    getOverlayId(name) {
        return this.id + "-ovl-" + name;
    }
    getOverlayHTML(name, props) {
        var str = "";
        props.forEach((p)=>{
            if (p.line1 == p.line2 && p.col1 == p.col2) return;
            p = $86e983c67b3f6085$var$normalizeRect(p);
            var p1 = this.coordinates(p.line1, p.col1);
            var p2 = this.coordinates(p.line2, p.col2);
            var p0 = p.col1 == 0 ? p1 : this.coordinates(p.line1, 0);
            if (p.line1 == p.line2) str += `<div class="${name}" style="
                    top: ${p1.y}px;
                    left: ${p1.x}px;
                    height: ${p1.h}px;
                    width: ${p2.x - p1.x}px;
                "></div>`;
            else {
                str += `<div class="${name}" style="
                    top: ${p1.y}px;
                    left: ${p1.x}px;
                    height: ${p.col1 > 0 ? p1.h : p2.y - p1.y}px;
                "></div>`;
                if (p.col1 > 0 && p.line2 - p.line1 > 1) str += `<div class="${name}" style="
                        top: ${p1.y + p1.h}px;
                        left: ${p0.x}px;
                        height: ${p2.y - p1.y - p1.h}px;
                    "></div>`;
                if (p.col2 > 0) str += `<div class="${name}" style="
                        top: ${p2.y}px;
                        left: ${p0.x}px;
                        height: ${p2.h}px;
                        width: ${p2.x - p0.x}px;
                    "></div>`;
            }
        });
        return str ? `<div id="${this.getOverlayId(name)}" data-ymacs-overlay="${name}" class="Ymacs_Overlay ${name}">${str}</div>` : null;
    }
    getOverlaysCount() {
        return this.getOverlaysContainer().childNodes.length - 1; // XXX: subtract the div.content; we need to revisit this if we add new elements.
    }
    _on_bufferOverlayChange(name, props) {
        let html = this.getOverlayHTML(name, Array.isArray(props) ? props : [
            props
        ]);
        if (html) {
            let div = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(html);
            let old = document.getElementById(this.getOverlayId(name));
            old ? old.replaceWith(div) : this.getOverlaysContainer().appendChild(div);
            this._setOverlayClasses();
        } else this._on_bufferOverlayDelete(name);
    }
    _on_bufferOverlayDelete(name) {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).trash(document.getElementById(this.getOverlayId(name)));
        this._setOverlayClasses();
    }
    _setOverlayClasses() {
        let names = [
            ...this.getOverlaysContainer().querySelectorAll(":scope > [data-ymacs-overlay]")
        ].map((el)=>el.dataset.ymacsOverlay);
        this.condClass(names.length > 0, "Ymacs_Frame-hasOverlays");
        this.getElement().dataset.ymacsOverlays = names.join(" ");
    }
    /* -----[ self events ]----- */ _on_destroy() {
        this.setBuffer(null);
    }
    _on_focus() {
        this.ymacs.setActiveFrame(this, true);
        if (!this.o.isMinibuffer) this.buffer.cmd("goto_char", this.caretMarker.getPosition());
        this.buffer.addEventListener(this._moreBufferEvents);
    }
    _on_blur() {
        if (!this.o.isMinibuffer) this.caretMarker.setPosition(this.buffer.caretMarker.getPosition());
        this.buffer.removeEventListener(this._moreBufferEvents);
    }
    _dragSelect_onMouseDown(ev) {
        if (ev.ctrlKey && ev.shiftKey) return;
        if (ev.button != 0) {
            setTimeout(()=>this.focus());
            return;
        }
        this.focus();
        ev.stopPropagation();
        ev.preventDefault();
        clearTimeout($86e983c67b3f6085$var$CLICK_COUNT_TIMER);
        $86e983c67b3f6085$var$CLICK_COUNT++;
        $86e983c67b3f6085$var$CLICK_COUNT_TIMER = setTimeout($86e983c67b3f6085$var$CLEAR_CLICK_COUNT, $86e983c67b3f6085$var$DBL_CLICK_SPEED);
        let pos = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        let buf = this.buffer;
        setTimeout(()=>{
            buf.clearTransientMark();
            buf.cmd("goto_char", buf._rowColToPosition(rc.row, rc.col));
            buf.callInteractively("keyboard_quit");
            if ($86e983c67b3f6085$var$CLICK_COUNT == 1) {
                buf.ensureTransientMark();
                (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(window, this._dragSelectHandlers);
            } else if ($86e983c67b3f6085$var$CLICK_COUNT == 2) {
                buf.cmd("forward_word");
                buf.cmd("backward_word");
                buf.cmd("forward_word_mark");
            } else if ($86e983c67b3f6085$var$CLICK_COUNT == 3) {
                buf.cmd("beginning_of_line");
                buf.cmd("end_of_line_mark");
            } else if ($86e983c67b3f6085$var$CLICK_COUNT == 4) {
                buf.cmd("backward_paragraph");
                buf.cmd("forward_whitespace");
                buf.cmd("beginning_of_line");
                buf.cmd("forward_paragraph_mark");
            }
        });
    }
    _dragSelect_onMouseMove(ev) {
        let pos = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).mousePos(ev, this.getContentElement());
        let rc = this.coordinatesToRowCol(pos.x, pos.y);
        this.buffer.cmd("goto_char", this.buffer._rowColToPosition(rc.row, rc.col));
        this.buffer.ensureTransientMark();
        this.ensureCaretVisible();
    }
    _dragSelect_onMouseUp(ev) {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).off(window, this._dragSelectHandlers);
        let buf = this.buffer;
        if (+buf.transientMarker === buf.point()) buf.clearTransientMark();
    }
    _on_keyDown(ev) {
        if (!$86e983c67b3f6085$var$isModifier(ev.key)) {
            if (this.ymacs.processKeyEvent(ev)) {
                ev.preventDefault();
                ev.stopPropagation();
            }
        }
    }
    _on_keyUp(ev) {}
    _on_scroll() {
        this.redrawModelineWithTimer();
    }
    _on_mouseWheel(ev) {
        ev.preventDefault();
        this.buffer._handleKeyEvent(ev);
    }
    firstLineVisible() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(1, div.scrollTop + 1).row;
    }
    lastLineVisible() {
        var div = this.getOverlaysContainer();
        return this.coordinatesToRowCol(div.clientWidth - 2, div.scrollTop + div.clientHeight - 2).row;
    }
    scrollUp(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.max(this.firstLineVisible() - lines, 0);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    }
    scrollDown(lines) {
        var div = this.getOverlaysContainer();
        var line = Math.min(this.firstLineVisible() + lines, this.buffer.code.length - 1);
        line = this.getLineDivElement(line);
        div.scrollTop = line.offsetTop;
        this.__ensureCaretVisible = false;
    }
}
function $86e983c67b3f6085$var$isModifier(key) {
    return /^(?:Alt|AltGraph|CapsLock|Control|Fn|FnLock|Hyper|Meta|NumLock|ScrollLock|Shift|Super|Symbol|SymbolLock)$/.test(key);
}
class $86e983c67b3f6085$export$7b0e4d18ade0c4c9 extends (0, $ca727f7f34cfa7f3$export$a829527ff4e4114a) {
    static options = {
        horiz: false
    };
    #activeElement = null;
    initClassName() {
        return "Ymacs_SplitCont " + (this.o.horiz ? "horiz" : "vert");
    }
    createElement() {
        super.createElement();
        this._dragHandlers = {
            mousemove: this._onMouseMove.bind(this),
            mouseup: this._onMouseUp.bind(this)
        };
        this._rb = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="bar" tabindex="0"></div>`);
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(this._rb, "mousedown", this._onMouseDown.bind(this));
    }
    setSplit(a, b, frac) {
        this.add(a);
        this.add(this._rb);
        this.add(b);
        if (frac != null) {
            let cont = this.getElement();
            if (this.o.horiz) cont.style.gridTemplateRows = `${frac}fr auto ${1 - frac}fr`;
            else cont.style.gridTemplateColumns = `${frac}fr auto ${1 - frac}fr`;
        }
    }
    _onMouseDown(ev) {
        this.#activeElement = document.activeElement;
        if (ev.target === this._rb) {
            let first = this.getContentElement().children[0];
            this._start = this.o.horiz ? ev.clientY : ev.clientX;
            this._orig_sz = this.o.horiz ? first.offsetHeight : first.offsetWidth;
            this.addClass("dragging");
            ev.stopPropagation();
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(window, this._dragHandlers);
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).overlayOn(this.o.horiz ? "Ymacs_Resize_horiz" : "Ymacs_Resize_vert");
        } else if (this.#activeElement) this.#activeElement.focus();
    }
    _onMouseUp(ev) {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).off(window, this._dragHandlers);
        this.delClass("dragging");
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).overlayOff();
        if (this.#activeElement) this.#activeElement.focus();
        this.#activeElement = null;
    }
    _onMouseMove(ev) {
        let cont = this.getElement();
        let max = this.o.horiz ? cont.offsetHeight : cont.offsetWidth;
        let diff = (this.o.horiz ? ev.clientY : ev.clientX) - this._start;
        let target = Math.min(max, Math.max(0, this._orig_sz + diff));
        let frac = Math.min(0.9, Math.max(0.1, target / max));
        if (this.o.horiz) cont.style.gridTemplateRows = `${frac}fr auto ${1 - frac}fr`;
        else cont.style.gridTemplateColumns = `${frac}fr auto ${1 - frac}fr`;
    }
}



function $bd0ea8a7a4006f2d$var$minElement(array, f, obj, remove) {
    if (array.length == 0) return null;
    var i = 0, minEl = array[0], minValue = f.call(obj, minEl), minIndex = 0, tmp;
    while(++i < array.length)if ((tmp = f.call(obj, array[i])) < minValue) {
        minValue = tmp;
        minIndex = i;
        minEl = array[i];
    }
    if (remove) array.splice(minIndex, 1);
    return minEl;
}
function $bd0ea8a7a4006f2d$var$selectClosestFrameX(byx, pos) {
    if (byx.length > 0) {
        var x = byx.at(-1).getBox().left, a = [
            byx.pop()
        ];
        while(byx.length > 0 && byx.at(-1).getBox().left == x)a.push(byx.pop());
        return $bd0ea8a7a4006f2d$var$minElement(a, function(f) {
            return Math.abs(pos.top - f.getBox().top - f.getBox().height / 2);
        });
    }
}
function $bd0ea8a7a4006f2d$var$selectClosestFrameY(byy, pos) {
    if (byy.length > 0) {
        var y = byy.at(-1).getBox().top, a = [
            byy.pop()
        ];
        while(byy.length > 0 && byy.at(-1).getBox().top == y)a.push(byy.pop());
        return $bd0ea8a7a4006f2d$var$minElement(a, function(f) {
            return Math.abs(pos.left - f.getBox().left - f.getBox().width / 2);
        });
    }
}
class $bd0ea8a7a4006f2d$export$e2cc12c8ba83ca65 extends (0, $ca727f7f34cfa7f3$export$a829527ff4e4114a) {
    static options = {
        buffers: [],
        frames: [],
        cf_frameStyle: Object.create(null),
        ls_keyName: ".ymacs"
    };
    static raw(str) {
        return (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlSafe(str);
    }
    constructor(...args){
        super(...args);
        this.buffers = [
            ...this.o.buffers
        ];
        this.frames = [
            ...this.o.frames
        ];
        this.registers = Object.create(null);
        this.cf_frameStyle = {
            ...this.o.cf_frameStyle
        };
        this.buffers.forEach((b)=>{
            b.ymacs = this;
            this._addBufferListeners(b);
        });
        /* -----[ variables ]----- */ this.killRing = [];
        this.killMasterOfRings = [];
        /* -----[ macro vars ]----- */ // If present, keystrokes are stored in this list.
        this.__macro_recording = null;
        // This is the macro executed by C-x e and named by
        // name-last-kbd-macro.
        this.__macro_finished = null;
        // Set when any buffer does signalError.  Tells us when to abort
        // running a macro.
        this.__error_thrown = false;
        // A list if we're executing a macro.
        this.__running_macro = null;
        // A number of times to execute the current macro.
        this.__macro_times = 0;
        // Macro current step
        this.__macro_step = 0;
        // Timer for the macro
        this.__macro_timer = null;
        // Unbiased active frame
        this.__input_frame = null;
        /* -----[ minibuffer ]----- */ this.minibuffer = new (0, $a49159f89f9c9e7a$export$36a0d48d1b18d0fa)({
            ymacs: this
        });
        this.minibuffer.cmd("minibuffer_mode");
        this.minibuffer_frame = this.createFrame({
            isMinibuffer: true,
            buffer: this.minibuffer,
            hidden: true
        });
        /* -----[ main content ]----- */ if (this.buffers.length == 0) this.createBuffer();
        var frame = this.createFrame({
            buffer: this.buffers[0]
        });
        this.add(frame);
        this.add(this.minibuffer_frame);
        this.setActiveFrame(frame);
        frame.redrawCaret();
    }
    initClassName() {
        return "Ymacs Ymacs-cursor-block";
    }
    _addBufferListeners(buf) {
        buf.addEventListener("onDestroy", ()=>{
            var fr = this.getActiveFrame();
            this.getBufferFrames(buf).forEach((f)=>{
                if (f !== fr) this.deleteFrame(f);
            });
            (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.buffers, buf);
            if (this.getActiveBuffer() === buf) this.nextHiddenBuffer(buf);
        });
    }
    pushToKillRing(text, prepend) {
        prepend ? this.killRing.unshift(text) : this.killRing.push(text);
    }
    killRingToMaster() {
        if (this.killRing.length && (this.killMasterOfRings.length == 0 || this.killMasterOfRings.at(-1).join("") != this.killRing.join(""))) this.killMasterOfRings.push(this.killRing);
        this.killRing = [];
    }
    killRingText() {
        return this.killRing.join("");
    }
    rotateKillRing(push) {
        if (push) {
            this.killMasterOfRings.push(this.killRing);
            this.killRing = this.killMasterOfRings.shift();
        } else {
            this.killMasterOfRings.unshift(this.killRing);
            this.killRing = this.killMasterOfRings.pop();
        }
    }
    getBuffer(buf) {
        if (!(buf instanceof (0, $a49159f89f9c9e7a$export$df331bdfc76955b4))) buf = this.buffers.find((b)=>b.name == buf);
        return buf;
    }
    killBuffer(buf) {
        buf = this.getBuffer(buf);
        this.callHooks("onDeleteBuffer", buf);
        buf.destroy();
    }
    renameBuffer(buf, name) {
        buf = this.getBuffer(buf);
        buf.name = name;
        buf.callHooks("onModelineChange");
    }
    _do_switchToBuffer(buf) {
        this.getActiveFrame().setBuffer(buf);
        this.callHooks("onBufferSwitch", buf);
    }
    switchToBuffer(maybeName) {
        var buf = this.getBuffer(maybeName), a = this.buffers;
        if (!buf) // create new buffer
        buf = this.createBuffer({
            name: maybeName
        });
        (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(a, buf);
        a.unshift(buf);
        this._do_switchToBuffer(buf);
        return buf;
    }
    nextHiddenBuffer(cur) {
        var a = this.buffers.filter((buf)=>{
            if (buf === cur) return false;
            var hidden = true;
            buf.forAllFrames(()=>hidden = false);
            return hidden;
        });
        if (a.length > 0) {
            var buf = a[0];
            (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.buffers, buf);
            this.buffers.push(buf);
            this._do_switchToBuffer(buf);
        } else this.switchToBuffer("*scratch*");
    }
    switchToNextBuffer() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.shift();
            a.push(buf);
            this._do_switchToBuffer(a[0]);
        }
    }
    switchToPreviousBuffer() {
        var a = this.buffers;
        if (a.length > 1) {
            var buf = a.pop();
            a.unshift(buf);
            this._do_switchToBuffer(buf);
        }
    }
    getNextBuffer(buf, n) {
        if (n == null) n = 1;
        var a = this.buffers;
        return a[(a.indexOf(buf) + n) % a.length];
    }
    getPrevBuffer(buf, n) {
        if (n == null) n = 1;
        return this.getNextBuffer(buf, -n);
    }
    getBufferFrames(buf) {
        buf = this.getBuffer(buf);
        return this.frames.filter((f)=>f.buffer === buf);
    }
    createBuffer(args = {}) {
        var buf = new (0, $a49159f89f9c9e7a$export$df331bdfc76955b4)({
            ...args,
            ymacs: this
        });
        this._addBufferListeners(buf);
        if (!args.hidden) this.buffers.push(buf);
        this.callHooks("onCreateBuffer", buf);
        return buf;
    }
    createFrame(args = {}) {
        var frame = new (0, $86e983c67b3f6085$export$3230e5a51f22d844)({
            ...args,
            ymacs: this
        });
        if (!args.hidden) this.frames.unshift(frame);
        frame.setStyle(this.cf_frameStyle);
        return frame;
    }
    setFrameStyle(style) {
        [
            this.minibuffer_frame,
            ...this.frames
        ].forEach((frame)=>frame.setStyle(style));
    }
    keepOnlyFrame(frame) {
        if (this.frames.length > 1) {
            let el = frame.getElement();
            while(el.parentNode != this.getContentElement())el = el.parentNode;
            if (el !== frame) {
                el.replaceWith(frame.getElement());
                this.setActiveFrame(frame);
                frame.centerOnCaret();
                this.frames = [
                    frame
                ];
            }
        }
    }
    deleteFrame(frame) {
        if (this.frames.length > 1) {
            (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.frames, frame);
            let parent = frame.getElement().parentNode;
            let other = [
                ...parent.children
            ].find((el)=>{
                let obj = el._ymacs_object;
                return obj instanceof (0, $86e983c67b3f6085$export$7b0e4d18ade0c4c9) || obj instanceof (0, $86e983c67b3f6085$export$3230e5a51f22d844) && obj !== frame;
            });
            parent.replaceWith(other);
            if (!(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).hasClass(other, "Ymacs_Frame")) other = other.querySelector(".Ymacs_Frame");
            other = other._ymacs_object;
            this.setActiveFrame(other);
            other.centerOnCaret();
        }
    }
    focusOtherFrame() {
        this.setActiveFrame(this.frames[0]);
    }
    getMainElement() {
        const selector = `:scope > .Ymacs_Frame:not(.Ymacs_Minibuffer), :scope > .Ymacs_SplitCont`;
        return this.getElement().querySelector(selector);
    }
    getFrameConfig() {
        let dig = (el)=>{
            if ((0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).hasClass(el, "Ymacs_Frame")) {
                let frame = el._ymacs_object;
                let div = frame.getOverlaysContainer();
                return {
                    buffer: frame.buffer.name,
                    point: +frame.caretMarker,
                    scroll: div.scrollTop / div.scrollHeight,
                    active: frame === this.getActiveFrame()
                };
            } else {
                let layout = el._ymacs_object;
                let horiz = layout.o.horiz;
                let style = window.getComputedStyle(el);
                let rx = /^\s*(\d*(?:\.\d+)?)px\s+(?:\d*(?:\.\d+)?)px\s+(\d*(?:\.\d+)?)px\s*$/;
                let m = rx.exec(horiz ? style.gridTemplateRows : style.gridTemplateColumns);
                let frac = +m[1] / (+m[1] + +m[2]);
                return [
                    horiz,
                    frac,
                    dig(el.children[0]),
                    dig(el.children[2])
                ];
            }
        };
        return dig(this.getMainElement());
    }
    setFrameConfig(config) {
        this.getMainElement()?.remove();
        // 1. figure out if we already have some frames for the set of
        // buffers that we'll need to switch to.
        let buffers = [];
        let frames = [
            ...this.frames
        ];
        (function dig(el) {
            if (Array.isArray(el)) {
                dig(el[2]);
                dig(el[3]);
            } else {
                for(let i = frames.length; --i >= 0;)if (frames[i].buffer?.name == el.buffer) {
                    buffers.push([
                        el.buffer,
                        frames[i]
                    ]);
                    frames.splice(i, 1);
                    break;
                }
            }
        })(config);
        let getBuffer = (bufferName)=>{
            return this.getBuffer(bufferName) || this.createBuffer({
                name: bufferName
            });
        };
        // this will return a frame associated with the given buffer.
        let getFrame = (bufferName, point)=>{
            let i = buffers.findIndex(([buf])=>buf == bufferName);
            if (i < 0) {
                // if we don't have an associated frame...
                if (frames.length > 0) {
                    // there's still one to spare
                    let fr = frames.pop();
                    fr.setBuffer(getBuffer(bufferName, point));
                    return fr;
                } else // otherwise, create new frame.
                return this.createFrame({
                    buffer: getBuffer(bufferName),
                    point: point
                });
            } else {
                // found associated frame; remove it from the list.
                let fr = buffers[i][1];
                buffers.splice(i, 1);
                return fr;
            }
        };
        // 2. dig it again, this time creating widgets.
        let ops = [];
        let active = null;
        let main = function dig(el) {
            if (Array.isArray(el)) {
                let split = new (0, $86e983c67b3f6085$export$7b0e4d18ade0c4c9)({
                    horiz: el[0]
                });
                let frame1 = dig(el[2]);
                let frame2 = dig(el[3]);
                ops.push(()=>split.setSplit(frame1, frame2, el[1]));
                return split;
            } else {
                let frame = getFrame(el.buffer, el.point);
                if (el.active) active = frame;
                ops.push(()=>{
                    let div = frame.getOverlaysContainer();
                    div.scrollTop = el.scroll * div.scrollHeight;
                });
                return frame;
            }
        }(config);
        // at this point, if we still have any frames left (e.g. the
        // editor had more frames than required by the new config),
        // remove/destroy them.
        frames.forEach((fr)=>{
            (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.frames, fr);
            fr.destroy();
        });
        let cont = this.getElement();
        cont.insertBefore(main.getElement(), cont.firstElementChild);
        // ops will contain operations to do after the elements are in
        // the DOM.
        ops.reverse().forEach((f)=>f());
        // done, reset active frame.
        this.setActiveFrame(active || this.frames[0]);
    }
    focus() {
        this.frames.at(-1).focus();
    }
    setInputFrame(frame) {
        this.__input_frame = frame;
    }
    setActiveFrame(frame, nofocus) {
        if (!frame.isMinibuffer) {
            var old = this.getActiveFrame();
            if (old) old.delClass("Ymacs_Frame-active");
            (0, $ca727f7f34cfa7f3$export$cd7f480d6b8286c3)(this.frames, frame);
            this.frames.push(frame);
            frame.addClass("Ymacs_Frame-active");
        }
        this.__input_frame = frame;
        if (!nofocus) frame.focus();
    }
    getActiveFrame() {
        return this.frames.at(-1);
    }
    getActiveBuffer() {
        var frame = this.getActiveFrame();
        return frame ? frame.buffer : this.buffers.at(-1);
    }
    setColorTheme(themes) {
        if (!(themes instanceof Array)) themes = [
            themes
        ];
        let loaded = (0, $ca727f7f34cfa7f3$export$2ceef881d0b4a563)();
        if (themes.every((id)=>loaded.includes(id))) {
            // proceed
            this.delClass(/Ymacs-Theme-[^\s]*/g);
            themes.forEach((themeId)=>{
                this.addClass("Ymacs-Theme-" + themeId);
            });
            return true;
        } else {
            console.warn(`Won't reset theme, some files might not be loaded: ${themes.join(", ")}`);
            return false;
        }
    }
    cursorBar() {
        this.delClass("Ymacs-cursor-block");
        this.addClass("Ymacs-cursor-bar");
    }
    cursorBlock() {
        this.delClass("Ymacs-cursor-bar");
        this.addClass("Ymacs-cursor-block");
    }
    toggleBarCursor() {
        if (this.hasClass("Ymacs-cursor-block")) this.cursorBar();
        else this.cursorBlock();
    }
    getFrameInDirection(dir) {
        let frame = this.getActiveFrame();
        let caret = frame.getCaretElement();
        let box = caret.getBoundingClientRect();
        var byx = [
            ...this.frames
        ].sort((a, b)=>a.getBox().left - b.getBox().left);
        var byy = [
            ...this.frames
        ].sort((a, b)=>a.getBox().top - b.getBox().top);
        switch(dir){
            case "left":
                return this._get_frameInDir_left(byx, byy, box, frame);
            case "right":
                return this._get_frameInDir_right(byx, byy, box, frame);
            case "up":
                return this._get_frameInDir_up(byx, byy, box, frame);
            case "down":
                return this._get_frameInDir_down(byx, byy, box, frame);
        }
        return this["_get_frameInDir_" + dir](byx, byy, box, frame);
    }
    _get_frameInDir_left(byx, byy, pos, frame) {
        byx = byx.filter((f)=>{
            let p = f.getBox();
            return f !== frame && p.left < pos.left && p.top - pos.height <= pos.top && p.top + p.height > pos.top;
        });
        return $bd0ea8a7a4006f2d$var$selectClosestFrameX(byx, pos);
    }
    _get_frameInDir_right(byx, byy, pos, frame) {
        byx.reverse();
        byx = byx.filter((f)=>{
            let p = f.getBox();
            return f !== frame && p.left > pos.left && p.top - pos.height <= pos.top && p.top + p.height > pos.top;
        });
        return $bd0ea8a7a4006f2d$var$selectClosestFrameX(byx, pos);
    }
    _get_frameInDir_up(byx, byy, pos, frame) {
        byy = byy.filter((f)=>{
            let p = f.getBox();
            return f !== frame && p.top < pos.top && p.left - pos.width <= pos.left && p.left + p.width > pos.left;
        });
        return $bd0ea8a7a4006f2d$var$selectClosestFrameY(byy, pos);
    }
    _get_frameInDir_down(byx, byy, pos, frame) {
        byy.reverse();
        byy = byy.filter((f)=>{
            let p = f.getBox();
            return f !== frame && p.top > pos.top && p.left - pos.width <= pos.left && p.left + p.width > pos.left;
        });
        return $bd0ea8a7a4006f2d$var$selectClosestFrameY(byy, pos);
    }
    /* -----[ local storage ]----- */ ls_get() {
        return this.ls_store || (this.ls_store = JSON.parse(localStorage.getItem(this.o.ls_keyName) || "{}"));
    }
    ls_set(src) {
        this.ls_store = src;
        localStorage.setItem(this.o.ls_keyName, JSON.stringify(src));
    }
    ls_getFileContents(name, nothrow) {
        var info = this.ls_getFileDirectory(name), other = info.other, code;
        if (other.length == 1) code = info.dir[other[0]];
        if (code == null && !nothrow) throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("File not found");
        return code;
    }
    ls_setFileContents(name, content) {
        var files = this.ls_getFileDirectory(name, "file");
        files.dir[files.other[0]] = content;
        this.ls_set(files.store);
    }
    ls_getFileDirectory(name, create) {
        var store, dir = store = this.ls_get(), back = [];
        name = name.replace(/^[~\x2f]+/, "").split(/\x2f+/);
        var path = [], other = [];
        while(name.length > 0){
            var part = name.shift();
            if (part == ".") continue;
            if (part == "..") {
                path.pop();
                dir = back.pop();
            } else if (part == "~") {
                path = [];
                other = [];
                back = [];
                dir = store;
            } else if (Object.hasOwn(dir, part) && typeof dir[part] != "string") {
                back.push(dir);
                dir = dir[part];
                path.push(part);
            } else other.push(part);
        }
        if (create) {
            var n = create == "file" ? 1 : 0;
            while(other.length > n)dir = dir[other.shift()] = {};
            this.ls_set(store);
        }
        return {
            store: store,
            dir: dir,
            path: path,
            other: other,
            full: path.concat(other).join("/")
        };
    }
    ls_deleteFile(name) {
        var info = this.ls_getFileDirectory(name);
        delete info.dir[info.other.join("/")];
        this.ls_set(info.store);
    }
    /* -----[ filesystem operations ]----- */ fs_normalizePath(path) {
        path = path.replace(/^[~\x2f]+/, "").split(/\x2f+/);
        var ret = [];
        while(path.length > 0){
            var x = path.shift();
            if (x != ".") {
                if (x == "..") ret.pop();
                else if (x == "~") ret = [];
                else ret.push(x);
            }
        }
        return ret.join("/");
    }
    fs_fileType(name, cont) {
        let { other: other, dir: dir } = this.ls_getFileDirectory(name);
        if (other.length > 0) return cont(!other[0] ? "directory" : typeof dir[other[0]] == "string" ? "file" : "new");
        else return cont("directory");
    }
    fs_getFileContents(name, nothrow, cont) {
        var code = this.ls_getFileContents(name, nothrow);
        cont(code, code); // second parameter is file stamp, on a real fs it should be last modification time
    }
    fs_setFileContents(name, content, stamp, cont) {
        if (stamp && (this.ls_getFileContents(name, true) || "") != stamp) cont(null); // did not change file because stamp is wrong
        else {
            this.ls_setFileContents(name, content);
            cont(content);
        }
    }
    fs_sortDirectoryEntries(dir) {
        return Object.keys(dir).sort((a, b)=>{
            a = a.toLowerCase();
            b = b.toLowerCase();
            let adir = typeof dir[a] != "string";
            let bdir = typeof dir[b] != "string";
            if (adir && !bdir) return -1;
            if (bdir && !adir) return 1;
            if (!adir && !bdir) {
                let aobj = this.fs_isObjectFileName(a);
                let bobj = this.fs_isObjectFileName(b);
                if (aobj && !bobj) return 1;
                if (bobj && !aobj) return -1;
            }
            return a < b ? -1 : a > b ? 1 : 0;
        });
    }
    fs_isObjectFileName(filename) {
        return /\.fasl$/i.test(filename);
    }
    fs_getDirectory(dirname, cont) {
        var info = this.ls_getFileDirectory(dirname, false);
        dirname = info.path.join("/"); // normalized
        if (info) {
            var files = {};
            for (let f of this.fs_sortDirectoryEntries(info.dir))files[f] = {
                name: f,
                path: dirname + "/" + f,
                type: typeof info.dir[f] == "string" ? "file" : "directory"
            };
            cont(files);
        } else cont(null);
    }
    fs_deleteFile(name, cont) {
        this.ls_deleteFile(name);
        cont();
    }
    fs_remapDir(dir, cont) {
        cont(dir);
    }
    isRunningMacro() {
        return !!this.__running_macro;
    }
    isRecordingMacro() {
        return !!this.__macro_recording;
    }
    indicateError() {
        this.__error_thrown = true;
    }
    startMacro(do_append) {
        if (this.isRecordingMacro()) return false;
        if (do_append) {
            this.__macro_recording = this.__macro_finished || [];
            this.__macro_finished = null;
        } else this.__macro_recording = [];
        return true;
    }
    stopMacro() {
        if (this.__macro_recording) {
            this.__macro_finished = this.__macro_recording;
            this.__macro_recording = null;
        }
    }
    getLastMacro() {
        return this.__macro_finished;
    }
    stepMacro() {
        while(true){
            if (this.__macro_step >= this.__running_macro.length) {
                this.__macro_times--;
                this.__macro_step = 0;
            }
            if (this.__macro_times == 0 || this.__error_thrown) {
                this.__macro_times = 0;
                this.__macro_step = 0;
                this.__running_macro = null;
                return;
            }
            var ev = this.__running_macro[this.__macro_step];
            this.processKeyEvent(ev);
            this.__macro_step++;
        }
    }
    runMacro(times, macro) {
        if (this.isRecordingMacro()) return false;
        this.__error_thrown = false;
        this.__running_macro = macro;
        this.__macro_step = 0;
        this.__macro_times = times;
        var self = this;
        setTimeout(function() {
            self.stepMacro();
        }, 0);
        return true;
    }
    processKeyEvent(ev) {
        var frame = this.__input_frame;
        var buffer = frame.buffer;
        if (this.__macro_recording) this.__macro_recording.push(ev);
        return buffer._handleKeyEvent(ev);
    }
    #timer_popupMessage = null;
    popupMessage({ type: type = "info", text: text, isHtml: isHtml = false, atCaret: atCaret = false, anchor: anchor = null, timeout: timeout }) {
        let popup = this.__popup || (this.__popup = new (0, $0c7e6e98ae65431f$export$731251e1d39d8621)());
        let el = popup.getElement();
        popup.condClass(atCaret || anchor, "with-arrow");
        if (!atCaret && !anchor) {
            el.style.removeProperty("left");
            el.style.removeProperty("top");
            el.style.removeProperty("bottom");
            el.style.removeProperty("right");
            el.style.removeProperty("transform");
        }
        if (!isHtml) text = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(text);
        popup.setContent(text);
        if (atCaret) this._popupAtCaret(popup.getElement());
        else if (anchor) this._popupAtAnchor(popup.getElement(), anchor);
        else this.add(popup);
        clearTimeout(this.#timer_popupMessage);
        if (timeout) this.#timer_popupMessage = setTimeout(this.clearPopupMessage.bind(this), timeout);
    }
    clearPopupMessage() {
        if (this.__popup) this.__popup.getElement().remove();
    }
    requestFullScreen() {
        return this.getElement().requestFullscreen();
    }
    _popupAtCaret(el) {
        this._popupAtAnchor(el, this.__input_frame.getCaretElement());
    }
    _popupAtAnchor(el, anchor) {
        el.style.visibility = "hidden";
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).delClass(el, /ppos-[a-z-]+/ig);
        this.add(el);
        let mybox = this.getElement().getBoundingClientRect();
        let cbox = anchor.getBoundingClientRect();
        let cbox_center = {
            x: (cbox.left + cbox.right) / 2,
            y: (cbox.top + cbox.bottom) / 2
        };
        let mybox_center = {
            x: (mybox.left + mybox.right) / 2,
            y: (mybox.top + mybox.bottom) / 2
        };
        if (cbox_center.y > mybox_center.y) {
            if (cbox_center.x < mybox_center.x) {
                (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).addClass(el, "ppos-top-right");
                el.style.transform = `translate(0, calc(-100% - ${cbox.height / 2}px))`;
            } else {
                (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).addClass(el, "ppos-top-left");
                el.style.transform = `translate(-100%, calc(-100% - ${cbox.height / 2}px))`;
            }
        } else if (cbox_center.x < mybox_center.x) {
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).addClass(el, "ppos-bot-right");
            el.style.transform = `translate(0, ${cbox.height / 2}px)`;
        } else {
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).addClass(el, "ppos-bot-left");
            el.style.transform = `translate(-100%, ${cbox.height / 2}px)`;
        }
        el.style.left = cbox_center.x - mybox.left + "px";
        el.style.top = cbox_center.y - mybox.top + "px";
        el.style.removeProperty("visibility");
    }
    jumpToRegister(reg) {
        let value = this.registers[reg];
        if (value == null) return false;
        if (value.frames) {
            this.setFrameConfig(value.frames);
            return true;
        }
    }
    makeDialog(options) {
        let dlg = new $bd0ea8a7a4006f2d$export$d74b4745301088ce({
            ...options,
            ymacs: this
        });
        this.add(dlg);
        return dlg;
    }
}
function $bd0ea8a7a4006f2d$var$cssSize(prop) {
    if (typeof prop == "number") {
        if (prop <= 1 && prop > 0) prop = prop * 100 + "%";
        else prop += "px";
    }
    return prop;
}
class $bd0ea8a7a4006f2d$export$d74b4745301088ce extends (0, $ca727f7f34cfa7f3$export$a829527ff4e4114a) {
    static options = {
        draggable: false,
        resizable: false,
        closable: false,
        content: null,
        centered: false,
        width: "40%",
        height: "50%",
        ymacs: null
    };
    constructor(...args){
        super(...args);
        this.ymacs = this.o.ymacs;
        let cont = this.getElement();
        let content = this.o.content;
        if (typeof content == "function") content = content.call(this, cont, this);
        if (content) cont.appendChild(content);
        if (this.o.resizable) cont.style.resize = "both";
        if (this.o.width) cont.style.width = $bd0ea8a7a4006f2d$var$cssSize(this.o.width);
        if (this.o.height) cont.style.height = $bd0ea8a7a4006f2d$var$cssSize(this.o.height);
        if (this.o.closable) {
            if (this.o.closable === true) {
                let el = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="close-button"></div>`);
                cont.appendChild(el);
                this.o.closable = el;
            }
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(this.o.closable, {
                mousedown: (ev)=>ev.stopPropagation(),
                click: this._closeClick.bind(this)
            });
        }
        if (this.o.draggable) {
            if (this.o.draggable === true) this.o.draggable = cont;
            this._dragHandlers = {
                mousemove: this._dragMouseMove.bind(this),
                mouseup: this._dragMouseUp.bind(this)
            };
            (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(this.o.draggable, "mousedown", this._dragMouseDown.bind(this));
        }
    }
    initClassName() {
        let a = [
            "Ymacs_Dialog"
        ];
        if (this.o.centered) a.push("centered");
        return a.join(" ");
    }
    close() {
        let el = this.getElement();
        let event = document.createEvent("HTMLEvents");
        event.initEvent("close", true, true);
        if (el.dispatchEvent(event)) {
            el.remove();
            this.destroy();
            this.ymacs.focus();
            this.callHooks("onClose");
        }
    }
    _closeClick(ev) {
        ev.stopPropagation();
        this.close();
    }
    _dragMouseDown(ev) {
        ev.stopPropagation();
        let cont = this.getElement();
        this._dragging = {
            orig: (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).relPos(cont),
            start: (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).mousePos(ev, cont.offsetParent),
            focus: document.activeElement
        };
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on(window, this._dragHandlers);
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).overlayOn("Ymacs_Resize_move");
    }
    _dragMouseMove(ev) {
        let cont = this.getElement();
        let mouse = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).mousePos(ev, cont.offsetParent);
        let drg = this._dragging;
        let dx = mouse.x - drg.start.x;
        let dy = mouse.y - drg.start.y;
        cont.style.removeProperty("bottom");
        cont.style.removeProperty("right");
        cont.style.left = drg.orig.x + dx + "px";
        cont.style.top = drg.orig.y + dy + "px";
    }
    _dragMouseUp(ev) {
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).off(window, this._dragHandlers);
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).overlayOff();
        if (this.getElement().tabIndex < 0) this._dragging.focus?.focus();
        this._dragging = null;
    }
}





/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT

const $6fd37b8e17f87e00$var$TOK_DELAY = 10;
class $6fd37b8e17f87e00$export$2f1022027d35d1bb {
    constructor({ buffer: buffer = null, line: line = 0, col: col = 0 } = {}){
        this.buffer = buffer;
        this.line = line;
        this.col = col;
    }
    nextLine() {
        ++this.line;
        this.col = 0;
        return this.line < this.buffer.code.length;
    }
    prevLine() {
        --this.line;
        this.col = 0;
        return this.line >= 0;
    }
    peek(n = 0) {
        if (this.line < this.buffer.code.length) {
            let pos = this.col + n;
            let line = this.buffer.code[this.line];
            if (pos < line.length) return line.charAt(pos);
            if (pos == line.length) return "\n";
        }
    }
    lineText(row = this.line) {
        return this.buffer.code[row];
    }
    lineIndentation(row = this.line) {
        return /^\s*/.exec(this.lineText(row))[0].length;
    }
    lookingAt(what) {
        if (this.line < this.buffer.code.length) {
            var line = this.buffer.code[this.line];
            if (what instanceof RegExp) return what.exec(line.substr(this.col));
            else return line.substr(this.col, what.length) == what ? [
                what
            ] : null;
        }
    }
    textBefore(pos) {
        if (pos == null) pos = this.buffer._rowColToPosition(this.line, this.col);
        return this.buffer.getCode().substr(0, pos);
    }
    textAfter(pos) {
        if (pos == null) pos = this.buffer._rowColToPosition(this.line, this.col);
        return this.buffer.getCode().substr(pos);
    }
    substring(start, end) {
        return this.buffer.getCode().substring(start, end);
    }
    substr(start, end) {
        return this.buffer.getCode().substr(start, end);
    }
    eol() {
        return this.col >= this.buffer.code[this.line].length;
    }
    eof() {
        var n = this.buffer.code.length, l = this.line;
        return l >= n || l == n - 1 && this.eol();
    }
    stopAt(line, col, stop = this.EOF) {
        this.__stop = {
            line: line,
            col: col,
            stop: stop
        };
    }
    length() {
        return this.buffer.code.length;
    }
    lineLength(line) {
        if (line == null) line = this.line;
        return this.buffer.code[line].length;
    }
    save() {
        return {
            buffer: this.buffer,
            line: this.line,
            col: this.col
        };
    }
    restore(state) {
        this.buffer = state.buffer;
        this.line = state.line;
        this.col = state.col;
    }
    checkStop() {
        if (this.eof()) throw this.EOF;
        if (this.eol()) throw this.EOL;
        let stop = this.__stop;
        if (stop) {
            if (this.line > stop.line || this.line == stop.line && this.col >= stop.col) {
                this.__stop = null;
                throw stop.stop ?? this.EOF;
            }
        }
    }
}
$6fd37b8e17f87e00$export$2f1022027d35d1bb.prototype.EOL = {};
$6fd37b8e17f87e00$export$2f1022027d35d1bb.prototype.EOF = {};
class $6fd37b8e17f87e00$export$825fc0b420d392c1 {
    constructor({ buffer: buffer = null, line: line = 0, col: col = 0, pos: pos = null } = {}){
        this.buffer = buffer;
        this.line = line;
        this.col = col;
        this.pos = pos;
        if (this.pos == null) this.pos = this.buffer._rowColToPosition(this.line, this.col);
        else {
            var rc = this.buffer._positionToRowCol(this.pos);
            this.line = rc.row;
            this.col = rc.col;
        }
    }
    static is_whitespace(ch) {
        switch(ch){
            case " ":
            case "\n":
            case "\t":
            case "\x0C":
            case "\u2028":
            case "\u2029":
            case "\xA0":
                return true;
        }
    }
    peek() {
        var a = this.buffer.code;
        var line = a[this.line];
        if (line == null) return null;
        if (this.col == line.length) return this.line == a.length - 1 ? null : "\n";
        return line.charAt(this.col);
    }
    next() {
        var ch = this.peek();
        if (ch) {
            ++this.pos;
            ++this.col;
            if (this.col > this.buffer.code[this.line].length) {
                this.col = 0;
                ++this.line;
            }
        }
        return ch;
    }
    read_while(pred) {
        var ret = "", ch;
        while((ch = this.peek()) && pred(ch))ret += this.next();
        return ret;
    }
    skip_ws() {
        return this.read_while(this.is_whitespace);
    }
    looking_at(what) {
        var line = this.buffer.code[this.line];
        if (what instanceof RegExp) return what.exec(line.substr(this.col));
        else return line.substr(this.col, what.length) == what;
    }
}
$6fd37b8e17f87e00$export$825fc0b420d392c1.prototype.is_whitespace = $6fd37b8e17f87e00$export$825fc0b420d392c1.is_whitespace;
let $6fd37b8e17f87e00$var$LANGUAGES = Object.create(null);
class $6fd37b8e17f87e00$export$3964ae1c660db960 extends (0, $ca727f7f34cfa7f3$export$76712d54f90f7348) {
    static define(name, func) {
        $6fd37b8e17f87e00$var$LANGUAGES[name.toLowerCase()] = func;
    }
    constructor({ buffer: buffer, type: type }){
        super();
        if (typeof type == "string") type = $6fd37b8e17f87e00$var$LANGUAGES[type.toLowerCase()];
        this.buffer = buffer;
        this.type = type;
    }
    reset() {
        this.stream = new $6fd37b8e17f87e00$export$2f1022027d35d1bb({
            buffer: this.buffer
        });
        this.theParser = this.type(this.stream, this);
        this.parsers = [
            this.theParser.copy()
        ];
        this.timerUpdate = null;
        this.start();
    }
    stop() {
        clearTimeout(this.timerUpdate);
    }
    getLanguage(name, options) {
        return $6fd37b8e17f87e00$var$LANGUAGES[name](this.stream, this, options);
    }
    start() {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = a.length - 1;
        stream.col = 0;
        while(!(p = a[stream.line]))stream.prevLine();
        p = p();
        let doit = ()=>{
            this.buffer.preventUpdates();
            let n = 100;
            while(true)try {
                while(true)p.next();
            } catch (ex) {
                if (ex === stream.EOL) {
                    stream.nextLine();
                    a[stream.line] = p.copy();
                    if (--n == 0) {
                        this.buffer.resumeUpdates();
                        this.timerUpdate = setTimeout(doit, $6fd37b8e17f87e00$var$TOK_DELAY);
                        return;
                    }
                } else if (ex === stream.EOF) {
                    this.buffer.resumeUpdates();
                    break;
                } else throw ex;
            }
        };
        doit();
    }
    quickInsertLine(row) {
        this.truncate(row);
    }
    quickDeleteLine(row) {
        this.truncate(row);
    }
    onToken(line, c1, c2, type) {
        this.callHooks("onFoundToken", line, c1, c2, type);
    }
    getParserForLine(row, col = 0) {
        this.stop();
        let stream = this.stream, p, a = this.parsers;
        stream.line = row;
        stream.col = 0;
        while(!(p = a[stream.line]))stream.prevLine();
        if (col != null) stream.stopAt(row, col);
        p = p();
        try {
            this.buffer.preventUpdates();
            while(true){
                if (col == null && stream.line == row) return p;
                try {
                    while(true)p.next();
                } catch (ex) {
                    if (ex === stream.EOL) {
                        stream.nextLine();
                        a[stream.line] = p.copy();
                    } else if (ex === stream.EOF) return p;
                    else throw ex;
                }
            }
        } finally{
            this.buffer.resumeUpdates();
            if (stream.line < stream.length()) this.timerUpdate = setTimeout(this.start.bind(this, stream.line), $6fd37b8e17f87e00$var$TOK_DELAY);
        }
    }
    parseUntil(row, col) {
        return this.getParserForLine(row, col);
    }
    reparseAll() {
        this.truncate(0);
        return this.finishParsing();
    }
    finishParsing() {
        this.getParserForLine(this.stream.length(), null);
        return this.getLastParser();
    }
    getLastParser() {
        return this.parsers.at(-1);
    }
    getIndentation(row, buffer) {
        var p = this.getParserForLine(row, null);
        if (p && p.indentation instanceof Function) return p.indentation(buffer);
    }
    truncate(row) {
        row++;
        if (this.parsers.length > row) this.parsers.length = row;
    }
    getPP() {
        let pp = this.theParser.passedParens;
        if (pp instanceof Function) pp = pp();
        return pp ? [
            ...pp
        ].sort($6fd37b8e17f87e00$export$2b0c642afb0ae4ca) : [];
    }
}
function $6fd37b8e17f87e00$export$2b0c642afb0ae4ca(p1, p2) {
    return (p1.row ?? p1.line) < (p2.row ?? p2.line) ? -1 : (p1.row ?? p1.line) > (p2.row ?? p2.line) ? 1 : (p1.col ?? p1.c1) - (p2.col ?? p2.c1);
}
function $6fd37b8e17f87e00$export$f6a9bcac20db9551(caret, zone) {
    return (p)=>p.closed && zone == "outer" && p.outer ? $6fd37b8e17f87e00$export$2b0c642afb0ae4ca({
            line: p.outer.l1,
            col: p.outer.c1
        }, caret) < 0 && $6fd37b8e17f87e00$export$2b0c642afb0ae4ca({
            line: p.outer.l2,
            col: p.outer.c2
        }, caret) >= 0 : zone == "inner" && p.inner ? $6fd37b8e17f87e00$export$2b0c642afb0ae4ca({
            line: p.inner.l1,
            col: p.inner.c1
        }, caret) <= 0 && $6fd37b8e17f87e00$export$2b0c642afb0ae4ca({
            line: p.inner.l2,
            col: p.inner.c2
        }, caret) >= 0 : $6fd37b8e17f87e00$export$2b0c642afb0ae4ca(p, caret) < 0 && $6fd37b8e17f87e00$export$2b0c642afb0ae4ca(p.closed, caret) >= 0;
}




/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT






function $210e37d01040ed37$var$TMPL_CHAR_INFO({ ch: ch, code: code, codeHex: codeHex, point: point, sizeKB: sizeKB }) {
    return `
<table>
  <tr><td style='text-align: right; font-weight: bold'>Char:</td><td><tt> ${ch} </tt></td></tr>
  ${code != null ? `
    <tr><td style='text-align: right; font-weight: bold'>Char code:</td><td> ${code} / 0x${codeHex} </td></tr>
  ` : ""}
  <tr><td style='text-align: right; font-weight: bold'>Position:</td><td> ${point} </td></tr>
  <tr><td style='text-align: right; font-weight: bold'>Buffer size:</td><td> ${sizeKB} </td></tr>
</table>`;
}
let $210e37d01040ed37$var$help_keymap = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("help", {
    "q": "bury_buffer"
});
let $210e37d01040ed37$var$read_register_keymap = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("read_register", {
    "Escape && C-g": function() {
        this.popKeymap($210e37d01040ed37$var$read_register_keymap);
        this.setMinibuffer("");
    }
});
$210e37d01040ed37$var$read_register_keymap.defaultHandler = [
    function() {
        var ev = this.interactiveEvent();
        var ch = ev.key;
        if (ch.length != 1) this.signalError("Non-character input event");
        else {
            this.getq("read_register_callback")(ch);
            this.setq("read_register_callback");
        }
        this.popKeymap($210e37d01040ed37$var$read_register_keymap);
        this.setMinibuffer("");
        return true;
    }
];
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    forward_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("goto_char", this.point() + x);
    }),
    backward_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("forward_char", -x);
    }),
    forward_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(x) {
        if (x == null) x = 1;
        var rc = this._rowcol;
        if (!/^(forward|backward)_line$/.test(this.previousCommand)) this.setq("line_movement_requested_col", rc.col);
        var ret = this.cmd("goto_char", this._rowColToPosition(rc.row + x, Math.max(rc.col, this.getq("line_movement_requested_col"))));
        if (!ret) this.setq("line_movement_requested_col", rc.col);
        return ret;
    }),
    backward_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(x) {
        if (x == null) x = 1;
        return this.cmd("forward_line", -x);
    }),
    forward_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("P", function(noLine) {
        var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
        if (this.cmd("search_forward_regexp", re)) {
            this.cmd("backward_char");
            return true;
        } else if (!noLine) return this.cmd("end_of_buffer");
    }),
    backward_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("P", function(noLine) {
        var re = noLine ? /[^\x20\t\xA0]/g : /[^\s]/g;
        if (this.cmd("search_backward_regexp", re)) {
            this.cmd("forward_char");
            return true;
        } else if (!noLine) return this.cmd("beginning_of_buffer");
    }),
    beginning_of_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        return this.cmd("goto_char", this._rowColToPosition(this._rowcol.row, 0));
    }),
    back_to_indentation: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var rc = this._rowcol, line = this.code[rc.row], m = /\S/.exec(line);
        if (m) return this.cmd("goto_char", this._rowColToPosition(rc.row, m.index));
    }),
    beginning_of_indentation_or_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        return this.cmd("back_to_indentation") || this.cmd("beginning_of_line");
    }),
    end_of_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var rc = this._rowcol;
        return this.cmd("goto_char", this._rowColToPosition(rc.row, this.code[rc.row].length));
    }),
    beginning_of_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        return this.cmd("goto_char", 0);
    }),
    end_of_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        return this.cmd("goto_char", this.getCodeSize());
    }),
    eob_p: function() {
        return this.point() == this.getCodeSize();
    },
    bob_p: function() {
        return this.point() == 0;
    },
    eol_p: function() {
        var rc = this._positionToRowCol(this.point());
        return rc.col == this.code[rc.line].length;
    },
    bol_p: function() {
        return this._positionToRowCol(this.point()).col == 0;
    },
    backward_delete_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (!this.deleteTransientRegion()) {
            if (n == null) n = 1;
            var pos = this.point();
            if (pos > 0) this._deleteText(pos - n, pos);
        }
    }),
    delete_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (!this.deleteTransientRegion()) {
            if (n == null) n = 1;
            var pos = this.point();
            this._deleteText(pos, pos + n);
        }
    }),
    delete_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^P", function(noLine) {
        if (!this.deleteTransientRegion()) {
            var p = this.point();
            if (this.cmd("forward_whitespace", noLine)) {
                this._deleteText(p, this.point());
                return true;
            }
        }
    }),
    backward_delete_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^P", function(noLine) {
        if (!this.deleteTransientRegion()) {
            var p = this.point();
            if (this.cmd("backward_whitespace", noLine)) {
                this._deleteText(this.point(), p);
                return true;
            }
        }
    }),
    delete_indentation: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("P", function(nextLine) {
        if (nextLine) this.cmd("forward_line");
        this.cmd("back_to_indentation");
        this.cmd("backward_delete_whitespace");
        this.cmd("insert", " ");
    }),
    universal_argument: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.pushKeymap((0, $3dba2f389b913e3b$export$7f1c7eed09402475));
        if (!this.isMinibuffer) this.setMinibuffer("C-u");
    }),
    overwrite_mode: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.resetOverwriteMode();
    }),
    self_insert_command: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(repeat) {
        var ev = this.interactiveEvent();
        var ch = ev.key;
        var rc = this._rowcol;
        if (ch.length == 1 && !ev.altKey && !ev.ctrlKey && !ev.metaKey) {
            this.deleteTransientRegion();
            if (repeat != null) ch = ch.repeat(repeat);
            if (this.overwriteMode) {
                var line = this.code[rc.row], left = line.length - rc.col;
                if (left > 0) this.cmd("delete_char", Math.min(left, repeat || 1));
            }
            this.cmd("insert", ch);
            return true;
        }
        return false;
    }),
    newline: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (n == null) n = 1;
        this.deleteTransientRegion();
        this.cmd("insert", "\n".repeat(n));
    }),
    newline_and_indent: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (n) this.cmd("newline", n);
        else {
            this.cmd("backward_delete_whitespace", true);
            this.cmd("newline");
            this.cmd("indent_line");
        }
    }),
    stupid_indent: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {}),
    indent_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("P", function(noEmpty) {
        if (!this.tokenizer) this.cmd("insert", " ".repeat(this.getq("indent_line")));
        else {
            let indent = this.tokenizer.getIndentation(this._rowcol.row, this);
            if (indent != null) {
                let line = this.getLine();
                if (!noEmpty || /\S/.test(line)) {
                    let rc = this._rowcol;
                    let pos = this.point() - rc.col;
                    let ci = /[^\S\r\n]*/.exec(line)[0].length;
                    if (ci > indent) this._deleteText(pos, pos + ci - indent);
                    else if (indent > ci) this._insertText(" ".repeat(indent - ci), pos);
                    if (rc.col <= ci) this.cmd("goto_char", pos + indent);
                }
            }
        }
    }),
    indent_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        if (end < begin) [begin, end] = [
            end,
            begin
        ];
        this.cmd("save_excursion", function() {
            var m = this.createMarker(end);
            this.cmd("goto_char", begin);
            while(this.point() < m.getPosition()){
                this.cmd("indent_line", true);
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line")) break;
            }
            m.destroy();
        });
    }),
    make_marker: function(pos) {
        return this.createMarker(pos);
    },
    looking_at: function() {
        return this.looking_at.apply(this, arguments);
    },
    looking_back: function() {
        return this.looking_back.apply(this, arguments);
    },
    search_forward: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sSearch: ", function(str, bound) {
        var code = this.getCode(), point = this.point();
        if (this.getq("case_fold_search")) {
            code = code.toLowerCase();
            str = str.toLowerCase();
        }
        var pos = code.indexOf(str, point);
        if (pos >= 0 && (bound == null || pos <= bound)) return this.cmd("goto_char", pos + str.length);
    }),
    search_backward: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sSearch backward: ", function(str, bound) {
        var code = this.getCode(), point = this.point();
        if (this.getq("case_fold_search")) {
            code = code.toLowerCase();
            str = str.toLowerCase();
        }
        var pos = code.lastIndexOf(str, point);
        if (pos == point) pos = code.lastIndexOf(str, point - 1);
        if (pos >= 0 && pos != point && (bound == null || pos >= bound)) return this.cmd("goto_char", pos);
    }),
    make_regexp: function(rx) {
        if (!(rx instanceof RegExp)) {
            var matchCase = !this.getq("case_fold_search");
            try {
                rx = new RegExp(rx, matchCase ? "mug" : "mugi");
            } catch (ex) {
                throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("Invalid regexp");
            }
        }
        return rx;
    },
    search_forward_regexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sRegExp search: ", function(rx) {
        rx = this.cmd("make_regexp", rx);
        let pos = rx.lastIndex = this.point();
        let m = rx.exec(this.getCode());
        if (m && rx.lastIndex != pos) {
            m.after = rx.lastIndex;
            this.matchData = m;
            this.cmd("goto_char", rx.lastIndex);
            return true;
        }
    }),
    search_backward_regexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sBackward RegExp search: ", function(rx) {
        rx = this.cmd("make_regexp", rx);
        var m = this.lastIndexOfRegexp(this.getCode(), rx, this.point());
        if (m && m.index != this.point()) {
            this.cmd("goto_char", m.index);
            return true;
        }
    }),
    forward_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var syntax_word = this.getq("syntax_word"), end = false;
        while(!end && !syntax_word.test(this.charAt()))if (!this.cmd("forward_char")) end = true;
        while(!end && syntax_word.test(this.charAt()))if (!this.cmd("forward_char")) end = true;
    }),
    backward_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var syntax_word = this.getq("syntax_word"), end = false;
        while(!end && !syntax_word.test(this.charAt(-1)))if (!this.cmd("backward_char")) end = true;
        while(!end && syntax_word.test(this.charAt(-1)))if (!this.cmd("backward_char")) end = true;
    }),
    forward_paragraph: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        let rx = this.getq("syntax_paragraph_sep");
        this.cmd("beginning_of_line");
        this.cmd("backward_char");
        if (this.cmd("looking_at", rx)) this.cmd("goto_char", this.cmd("match_end"));
        if (this.cmd("search_forward_regexp", rx)) this.cmd("goto_char", this.cmd("match_beginning") + 1);
        else this.cmd("end_of_buffer");
    }),
    backward_paragraph: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        let point = this.point();
        let rx = this.getq("syntax_paragraph_sep");
        this.cmd("beginning_of_line");
        if (this.cmd("looking_back", rx)) {
            let pos = this.cmd("match_end");
            if (pos == point) pos = this.cmd("match_beginning");
            this.cmd("goto_char", pos);
        }
        if (this.cmd("search_backward_regexp", rx)) this.cmd("goto_char", this.cmd("match_end"));
        else this.cmd("beginning_of_buffer");
    }),
    mark_paragraph: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r", function(begin, end) {
        if (!this.transientMarker) {
            this.cmd("forward_paragraph");
            this.setMark(this.point());
            this.ensureTransientMark();
            this.cmd("backward_paragraph");
        } else this.cmd("save_excursion", function() {
            if (this.transientMarker) this.cmd("goto_char", end);
            this.ensureTransientMark();
            this.cmd("forward_paragraph");
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        });
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),
    transpose_words: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        // if we're in the middle of a word, some
        // weird things happen; better skip it, just
        // like Emacs does.
        this.cmd("backward_char");
        if (this.getq("syntax_word").test(this.charAt())) this.cmd("forward_word");
        var a = [];
        this.cmd("forward_word");
        a.push(this.point());
        this.cmd("backward_word");
        a.push(this.point());
        this.cmd("backward_word");
        a.push(this.point());
        this.cmd("forward_word");
        a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a));
    }),
    transpose_lines: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var a = [];
        this.cmd("backward_line");
        this.cmd("beginning_of_line");
        a.push(this.point());
        this.cmd("end_of_line");
        a.push(this.point());
        this.cmd("forward_char");
        a.push(this.point());
        this.cmd("end_of_line");
        a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a) + 1);
    }),
    transpose_chars: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var pos = this.point();
        if (this.cmd("backward_char")) this.cmd("goto_char", this._swapAreas([
            pos - 1,
            pos,
            pos,
            pos + 1
        ]));
    }),
    kill_word: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(prefix) {
        if (this.transientMarker) {
            var r = this.getRegion();
            this.cmd("kill_region", r.begin, r.end);
            this.clearTransientMark();
        } else {
            var pos = this.point();
            this.cmd("forward_word", prefix == null ? 1 : prefix);
            var pos2 = this.point();
            this._killingAction(pos, pos2, false);
        }
    }),
    backward_kill_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var pos = this.point();
        this.cmd("backward_word");
        var pos2 = this.point();
        this._killingAction(pos, pos2, true);
    }),
    _apply_operation_on_word: function(op, cc) {
        var pos = this.point();
        if (this.getq("syntax_word").test(this.charAt())) {
            var pos2 = this.cmd("save_excursion", function() {
                this.cmd("forward_word");
                return this.point();
            });
            var word = op.call(this._bufferSubstring(pos, pos2));
            this._deleteText(pos, pos2);
            this._insertText(word);
        } else {
            this.cmd("forward_word");
            this.cmd("backward_word");
            if (pos != this.point()) this.cmd(cc);
        }
    },
    capitalize_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this.cmd("_apply_operation_on_word", function() {
            return this.charAt(0).toUpperCase() + this.substr(1).toLowerCase();
        }, "capitalize_word");
    }),
    downcase_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this.cmd("_apply_operation_on_word", String.prototype.toLowerCase, "downcase_word");
    }),
    upcase_word: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this.cmd("_apply_operation_on_word", String.prototype.toUpperCase, "upcase_word");
    }),
    goto_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("NGoto char: ", function(pos) {
        return this._repositionCaret(pos);
    }),
    goto_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("NGoto line: ", function(row) {
        var pos = this._rowColToPosition(row - 1, 0);
        return this.cmd("goto_char", pos);
    }),
    move_to_column: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("NMove to column: ", function(col, force) {
        var rc = this._positionToRowCol(this.point());
        var text = this.code[rc.row];
        if (text.length < col) {
            if (force) {
                this.cmd("end_of_line");
                this.cmd("insert", " ".repeat(col - text.length));
            } else this.cmd("end_of_line");
        } else this.cmd("goto_char", this._rowColToPosition(rc.row, col));
    }),
    delete_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        this._deleteText(begin, end);
    }),
    insert: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sInsert text: ", function(...args) {
        return this._insertText(args.join(""));
    }),
    keyboard_quit: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.clearTransientMark();
        this.setPrefixArg(undefined);
        this.setMinibuffer("");
    }),
    buffer_substring: function(begin, end) {
        if (arguments.length == 0) {
            var r = this.getRegion();
            begin = r.begin;
            end = r.end;
        }
        return this._bufferSubstring(begin, end);
    },
    kill_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(prefix) {
        if (this.transientMarker) {
            var r = this.getRegion();
            this.cmd("kill_region", r.begin, r.end);
            this.clearTransientMark();
        } else if (prefix != null) this._killingAction(this.point(), this.cmd("save_excursion", ()=>{
            this.cmd("forward_line", prefix);
            this.cmd("beginning_of_line");
            return this.point();
        }));
        else {
            var pos = this.point(), rc = this._rowcol, line = this.code[rc.row], end = pos + line.length - rc.col;
            if (rc.row < this.code.length - 1 && this.cmd("looking_at", /\s*$/my)) end++;
            this._killingAction(pos, end);
        }
    }),
    save_excursion: function() {
        return this._saveExcursion.apply(this, arguments);
    },
    prevent_undo: function() {
        return this._disableUndo.apply(this, arguments);
    },
    point: function() {
        return this.caretMarker.getPosition();
    },
    kill_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        this._killingAction(begin, end);
    }),
    copy_region_as_kill: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        this._killingAction(begin, end, false, true);
    }),
    yank: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^P", function(atStart) {
        this.deleteTransientRegion();
        var point = this.point();
        this._insertText(this.ymacs.killRingText());
        this.setMark(point);
        if (atStart) this.caretMarker.swap(this.markMarker);
    }),
    yank_from_operating_system: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(async function() {
        try {
            let code = await navigator.clipboard.readText();
            this._saveKilledText(code);
            this.callInteractively("yank");
        } catch (ex) {
            console.error(ex);
            this.signalError("Cannot read the system clipboard. Error in devtools console.");
        }
    }),
    copy_for_operating_system: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", async function(begin, end) {
        try {
            let code = this.cmd("buffer_substring", begin, end);
            await navigator.clipboard.writeText(code);
        } catch (ex) {
            console.error(ex);
            this.signalError("Cannot write to system clipboard. Error in devtools console.");
        }
    }),
    yank_pop: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        if (/^yank/.test(this.previousCommand)) {
            this.ymacs.rotateKillRing(false);
            this._deleteText(this.caretMarker, this.markMarker);
            this.cmd("yank");
        } else this.signalError("Previous command was not a yank");
    }),
    yank_shift: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        if (/^yank/.test(this.previousCommand)) {
            this.ymacs.rotateKillRing(true);
            this._deleteText(this.caretMarker, this.markMarker);
            this.cmd("yank");
        } else this.signalError("Previous command was not a yank");
    }),
    mark: function() {
        return this.markMarker.getPosition();
    },
    set_mark_command: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("d", function(x) {
        this.clearTransientMark();
        this.setMark(x);
        if (this.currentCommand == "set_mark_command") {
            this.signalInfo("Mark set", null, 1000);
            this.setq("sticky_mark", true);
        }
    }),
    exchange_point_and_mark: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.transientMarker = this.createMarker();
        this.caretMarker.swap(this.markMarker);
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),
    mark_whole_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.clearTransientMark();
        this.cmd("end_of_buffer");
        this.ensureTransientMark();
        this.cmd("beginning_of_buffer");
        this.ensureTransientMark();
    }),
    recenter_top_bottom: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.whenActiveFrame(function(frame) {
            frame.recenterTopBottom(this.sameCommandCount() % 3);
        });
    }),
    recenter: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.whenActiveFrame(function(frame) {
            frame.centerOnCaret();
        });
    }),
    ensure_caret_visible: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenActiveFrame(function(frame) {
            if (frame.ensureCaretVisible()) frame.centerOnCaret();
        });
    }),
    /* -----[ paragraphs ]----- */ get_fill_paragraph_region: function() {
        let r1 = this.cmd("save_excursion", ()=>{
            if (!this.cmd("looking_at", this.getq("syntax_paragraph_sep"))) this.cmd("forward_paragraph");
            let end = this.point() - 1;
            this.cmd("backward_paragraph");
            let begin = this.point();
            return {
                begin: begin,
                end: end
            };
        });
        let r2 = this.cmd("base_limit_fill_paragraph_region") || this.cmd("xml_limit_fill_paragraph_region");
        if (r2) return {
            begin: Math.max(r1.begin, r2.begin),
            end: Math.min(r1.end, r2.end)
        };
        return r1;
    },
    fill_paragraph: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^rP", function(begin, end, noPrefix) {
        this.cmd("save_excursion", function() {
            if (!this.transientMarker) {
                let r = this.cmd("get_fill_paragraph_region");
                begin = r.begin;
                end = r.end;
            } else this.clearTransientMark();
            this.cmd("goto_char", begin);
            var eop = this.createMarker(end);
            // identify the prefix to use for each line
            var prefix = "", del = /\s+/y;
            if (this.cmd("looking_at", /\s*\/\/+\s*/y)) {
                prefix = this.matchData[0];
                del = /\s*\/\/+\s*/y;
            } else if (this.cmd("looking_at", /\s*\/\*\s*/y)) {
                prefix = " ".repeat(this.matchData[0].length);
                del = /\s*\**\s*/y;
            } else if (this.cmd("looking_at", /\s*[#>;\s]+\s*/y)) {
                prefix = this.matchData[0];
                del = /\s*[#>;\s]*\s*/y;
            } else if (this.cmd("looking_at", /\s*([-*]|[0-9]+\.|\(?[a-z][\).])?\s+/iy)) {
                prefix = " ".repeat(this.matchData[0].length);
                del = /\s*[#>;\s]*\s*/y;
            }
            if (noPrefix) {
                this._deleteText(this.point(), this.point() + this.matchData[0].length);
                prefix = "";
            }
            // remove newlines first
            while(true){
                this.cmd("end_of_line");
                this.cmd("backward_delete_whitespace");
                if (this.point() >= eop.getPosition()) break;
                this._replaceText(this.point(), this.point() + 1, " ");
                if (del && this.cmd("looking_at", del)) this._deleteText(this.point(), this.point() + this.matchData[0].length);
            }
            this.cmd("beginning_of_line");
            // main operation
            var bol = this.point(), done = false;
            while(!done){
                var p = this.point();
                if (!this.cmd("search_forward_regexp", /\s/g)) done = true;
                if (this.point() >= eop.getPosition()) {
                    this.cmd("goto_char", eop);
                    done = true;
                }
                if (this._rowcol.col > this.getq("fill_column") + 1) {
                    if (p > bol) this.cmd("goto_char", p);
                    this.cmd("backward_delete_whitespace");
                    this.cmd("newline");
                    this.cmd("insert", prefix);
                    bol = this.point();
                }
            }
            eop.destroy();
        });
    }),
    // this looks at the style of the current paragraph and starts
    // a similar one, i.e. using same indentation level and prefix
    // (list-like prefixes are incremented)
    start_next_paragraph: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("backward_paragraph");
        // identify the prefix to use for each line
        var prefix = "";
        if (this.cmd("looking_at", /(\s*)([0-9]+)(\.\s+)/y)) prefix = this.matchData[1] + (parseInt(this.matchData[2], 10) + 1) + this.matchData[3];
        else if (this.cmd("looking_at", /(\s*\(?)([a-z])([\.\)]\s+)/iy)) prefix = this.matchData[1] + String.fromCharCode(this.matchData[2].charCodeAt(0) + 1) + this.matchData[3];
        else if (this.cmd("looking_at", /\s*[#>;*\s-]+\s*/y)) prefix = this.matchData[0];
        this.cmd("forward_paragraph");
        if (this.cmd("eob_p")) this.cmd("newline");
        this.cmd("insert", "\n", prefix);
        if (!this.cmd("looking_at", /\n\n/y)) {
            this.cmd("newline");
            this.cmd("backward_char");
        }
    }),
    scroll_down_half: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this.whenActiveFrame(function(frame) {
            var hl = frame.heightInLines();
            this.cmd("forward_line", Math.round(hl / 1.33));
            this.cmd("recenter");
        });
    }),
    scroll_up_half: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this.whenActiveFrame(function(frame) {
            var hl = frame.heightInLines();
            this.cmd("backward_line", Math.round(hl / 1.33));
            this.cmd("recenter");
        });
    }),
    scroll_up: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(arg) {
        if (arg == null) arg = 3;
        this.whenActiveFrame(function(frame) {
            frame.scrollUp(arg);
        });
    }),
    scroll_down: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(arg) {
        if (arg == null) arg = 3;
        this.whenActiveFrame(function(frame) {
            frame.scrollDown(arg);
        });
    }),
    nuke_trailing_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("save_excursion", function() {
            this.cmd("goto_char", 0);
            while(this._rowcol.row < this.code.length){
                var line = this.code[this._rowcol.row], m = /\s+$/.exec(line);
                if (m) {
                    this.cmd("beginning_of_line");
                    this._deleteText(this.point() + m.index, this.point() + line.length);
                }
                if (!this.cmd("forward_line")) break;
            }
        });
    }),
    match_string: function(n) {
        return this.matchData[n];
    },
    match_beginning: function() {
        return this.matchData.index;
    },
    match_end: function() {
        return this.matchData.index + this.matchData[0].length;
    },
    undo: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        this._placeUndoBoundary();
        if (!this._playbackUndo()) this.signalError("No further undo information");
    }),
    goto_last_change: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let a = [], pos = this.point();
        this.__undoQueue.forEach((x)=>{
            if (x.type == 3) {
                let m = x.markers.find((m)=>m[0] === this.caretMarker);
                if (m) a.push(m[1]);
            }
        });
        if (a.length) {
            a = a.reverse();
            let i = this.sameCommandCount() % a.length;
            this.cmd("goto_char", a[i]);
        }
    }),
    center_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(n) {
        if (n == null) n = 1;
        for(let i = 0; i < n; ++i){
            if (i > 0) this.cmd("forward_line");
            this.cmd("save_excursion", function() {
                this.cmd("end_of_line");
                this.cmd("backward_delete_whitespace", true);
                this.cmd("beginning_of_line");
                this.cmd("delete_whitespace", true);
                var line = this.code[this._rowcol.row];
                var indent = Math.floor((this.getq("fill_column") - line.length) / 2);
                this.cmd("insert", " ".repeat(indent));
            });
        }
    }),
    /* -----[ dabbrev ]----- */ dabbrev_expand: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        if (this.previousCommand != "dabbrev_expand") this.setq("dabbrev_context", null);
        var ctx = this.getq("dabbrev_context");
        if (!ctx) {
            ctx = this.setq("dabbrev_context", {});
            var p1 = this.cmd("save_excursion", function() {
                this.cmd("bind_variables", {
                    syntax_word: this.getq("syntax_word_dabbrev")
                }, "backward_word");
                return this.point();
            });
            if (p1 == this.point()) return this.signalError("Nothing to expand");
            ctx.search = this.cmd("buffer_substring", p1, this.point());
            ctx.point = p1;
            ctx.length = this.point() - p1;
            ctx.lastSearch = p1;
            ctx.encountered = Object.create(null);
            ctx.forward = false;
            ctx.buffer = this;
            ctx.seenBuffers = [
                this
            ];
            ctx.startBuffer = this;
        }
        var expansion;
        // in the following excursion, *this* is ctx.buffer,
        // not necessarily the currently active buffer.  It's
        // purpose is to determine the next expansion and
        // setup the context so that the next invocation would
        // continue.
        ctx.buffer.cmd("save_excursion", function repeat() {
            var word = this.getq("syntax_word_dabbrev");
            var p1;
            var found = false;
            this.cmd("goto_char", ctx.lastSearch);
            // console.log("last at: %d", ctx.lastSearch);
            if (!ctx.forward) {
                while(this.cmd("search_backward", ctx.search))if (!word.test(this.charAt(-1))) {
                    found = true;
                    break;
                }
                if (found) {
                    p1 = this.point();
                    ctx.lastSearch = p1;
                    this.cmd("goto_char", p1 + ctx.search.length);
                } else {
                    ctx.forward = true;
                    ctx.lastSearch = ctx.point + ctx.length;
                    repeat.call(this);
                    return;
                }
            } else {
                while(this.cmd("search_forward", ctx.search))if (!word.test(this.charAt(-ctx.search.length - 1))) {
                    found = true;
                    break;
                }
                if (found) {
                    ctx.lastSearch = this.point();
                    p1 = this.point() - ctx.search.length;
                } else {
                    ctx.buffer = this.whenYmacs("getNextBuffer", this);
                    if (ctx.seenBuffers.includes(ctx.buffer)) {
                        expansion = ctx.search;
                        ctx.startBuffer.signalError("No more completions");
                        ctx.lastSearch = ctx.point + ctx.length;
                        ctx.startBuffer.setq("dabbrev_context", null);
                        return;
                    } else {
                        ctx.seenBuffers.push(ctx.buffer);
                        ctx.lastSearch = 0;
                        ctx.buffer.cmd("save_excursion", repeat);
                        return;
                    }
                }
            }
            if (p1 != null) {
                // console.log("%s at %d, next from %d", ctx.search, p1, ctx.lastSearch);
                this.cmd("bind_variables", {
                    syntax_word: this.getq("syntax_word_dabbrev")
                }, "forward_word");
                expansion = this.cmd("buffer_substring", p1, this.point());
                if (ctx.encountered[expansion]) repeat.call(this);
            }
        });
        if (expansion != null) {
            this._replaceText(ctx.point, ctx.point + ctx.length, expansion);
            ctx.length = expansion.length;
            ctx.encountered[expansion] = true;
        }
    }),
    /* -----[ frames and buffers ]----- */ split_frame_vertically: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(percent) {
        if (percent == null) percent = "50%";
        else percent += "%";
        this.whenActiveFrame("vsplit", percent);
    }),
    split_frame_horizontally: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(percent) {
        if (percent == null) percent = "50%";
        else percent += "%";
        this.whenActiveFrame("hsplit", percent);
    }),
    delete_other_frames: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenActiveFrame("deleteOtherFrames");
    }),
    delete_frame: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenActiveFrame("deleteFrame");
    }),
    other_frame: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenYmacs("focusOtherFrame");
    }),
    windmove: function(dir) {
        this.whenYmacs(function(ymacs) {
            var f = ymacs.getFrameInDirection(dir);
            if (f) f.focus();
        });
    },
    next_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenYmacs("switchToNextBuffer", this.sameCommandCount() + 1);
    }),
    previous_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenYmacs("switchToPreviousBuffer", this.sameCommandCount() + 1);
    }),
    switch_to_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("BSwitch to buffer: ", function(buffer) {
        this.whenYmacs(function(ymacs) {
            if (!/\S/.test(buffer)) {
                if (ymacs.buffers.length < 2) return;
                buffer = ymacs.buffers[1];
            }
            ymacs.switchToBuffer(buffer);
        });
    }),
    kill_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var self = this;
        function kill() {
            self.whenYmacs(function(ymacs) {
                ymacs.switchToNextBuffer();
                ymacs.killBuffer(self);
            });
        }
        if (self.dirty()) {
            var msg = "Buffer " + self.name + " modified; kill anyway?";
            self.cmd("minibuffer_yn", msg, function(yes) {
                if (yes) kill();
            });
        } else kill();
    }),
    rename_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("sRename current buffer to: ", function(name) {
        this.whenYmacs(function(ymacs) {
            ymacs.renameBuffer(this, name);
        });
    }),
    /* -----[ other ]----- */ delete_region_or_line: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        if (!this.deleteTransientRegion()) {
            this.cmd("beginning_of_line");
            var pos = this.point();
            this.cmd("end_of_line");
            this.cmd("forward_char");
            if (this.point() != pos) {
                this._deleteText(pos, this.point());
                return true;
            }
        }
    }),
    // http://mihai.bazon.net/blog/close-last-xml-tag-emacs
    close_last_xml_tag: (0, $9e418a0e990aea25$export$8c34793404b3efcb)(function() {
        var tag, quote;
        this.cmd("save_excursion", function() {
            var skip = 1;
            while(skip != 0 && this.cmd("search_backward_regexp", /<\x2f?([a-zA-Z0-9:_-]+)/g)){
                tag = this.cmd("match_string", 1);
                if (this.cmd("looking_at", /<\x2f/y)) ++skip;
                else if (!this.cmd("looking_at", /<[^\x2f][^>]*?\x2f>/y)) --skip;
            }
            if (skip != 0) tag = null;
        });
        if (tag) this.cmd("insert", "</", tag, ">");
        else throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("Couldn't find a tag to close");
    }),
    bind_variables: function() {
        return this.withVariables.apply(this, arguments);
    },
    for_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r\nCExecute command within region: ", function(begin, end, func) {
        if (end < begin) {
            var tmp = begin;
            begin = end;
            end = tmp;
        } // MACROS!  I WANT MACROS!  EVAL SUCKS. x-(
        if (!(func instanceof Function)) func = this.COMMANDS[func];
        this.clearTransientMark();
        this.cmd("goto_char", begin);
        begin = this.createMarker(begin, true);
        end = this.createMarker(end);
        this.withCommands({
            goto_char: function(pos) {
                if (pos >= begin.getPosition() && pos <= end.getPosition()) return this._repositionCaret(pos);
                throw "YMACS_RESTRICT";
            }
        }, function() {
            try {
                while(true){
                    var tmp = this.point();
                    func.call(this);
                    if (this.point() == tmp && !this.cmd("forward_line")) break;
                }
            } catch (ex) {
                if (ex !== "YMACS_RESTRICT") throw ex;
            } finally{
                begin.destroy();
                end.destroy();
            }
        });
    }),
    comment_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r", function(begin, end) {
        var cmmt = this.getq("syntax_comment_line") || this.getq("syntax_comment_multi");
        if (!cmmt) return;
        this.clearTransientMark();
        this.cmd("save_excursion", function() {
            end = this.createMarker(end);
            this.cmd("goto_char", begin);
            var min = 100000;
            out: while(this.point() < end.getPosition()){
                while(this.cmd("looking_at", /\s*$/my)){
                    if (!this.cmd("forward_line")) break out;
                }
                var col = this._rowcol.col;
                while(this.cmd("looking_at", /\s/y) && col < min){
                    if (!this.cmd("forward_char")) break out;
                    ++col;
                }
                if (col < min) min = col;
                if (Array.isArray(cmmt.ch)) {
                    this.cmd("insert", cmmt.ch[0], " ");
                    this.cmd("end_of_line");
                    this.cmd("insert", " ", cmmt.ch[1]);
                } else this.cmd("insert", cmmt.ch, " ");
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line")) break out;
            }
            this.cmd("goto_char", end);
            if (this._rowcol.col > 0 && !this.cmd("looking_at", /\s*$/my)) this.cmd("newline_and_indent");
        });
    }),
    uncomment_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        var cmmt1 = this.getq("syntax_comment_line");
        var cmmt2 = this.getq("syntax_comment_multi");
        if (!cmmt1 && !cmmt2) return;
        this.clearTransientMark();
        this.cmd("save_excursion", function() {
            end = this.createMarker(end);
            this.cmd("goto_char", begin);
            while(this.point() < end.getPosition()){
                this.cmd("forward_whitespace");
                if (cmmt1 && this.cmd("looking_at", cmmt1.rx)) this.cmd("delete_char", this.matchData[0].length);
                else if (cmmt2 && this.cmd("looking_at", cmmt2.rx)) this._replaceText(this.point(), this.point() + this.matchData[0].length, this.matchData[1]);
                this.cmd("beginning_of_line");
                if (!this.cmd("forward_line")) break;
            }
        });
    }),
    comment_dwim: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r", function(begin, end) {
        var cmmt1 = this.getq("syntax_comment_line");
        var cmmt2 = this.getq("syntax_comment_multi");
        if (!cmmt1 && !cmmt2) return;
        if (this.transientMarker) this.cmd("save_excursion", function() {
            this.cmd("goto_char", begin);
            var already_comment = cmmt1 && this.cmd("looking_at", cmmt1.rx) || cmmt2 && this.cmd("looking_at", cmmt2.rx);
            if (already_comment) this.cmd("uncomment_region", begin, end);
            else this.cmd("comment_region", begin, end);
        });
        else {
            this.cmd("end_of_line");
            if (cmmt1) this.cmd("insert", " ", cmmt1.ch, " ");
            else {
                this.cmd("insert", " ", cmmt2.ch[0], cmmt2.ch[1]);
                this.cmd("backward_char", cmmt2.ch[1].length);
            }
            this.cmd("indent_line");
        }
    }),
    what_cursor_position: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        var ch = this.charAt(), chname = ch;
        if (ch == null) chname = "EOF";
        else if (ch == " ") chname = "Space";
        else if (ch == "\n") chname = "Newline";
        this.popupMessage({
            isHtml: true,
            atCaret: true,
            text: $210e37d01040ed37$var$TMPL_CHAR_INFO({
                ch: (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(chname),
                code: ch ? ch.charCodeAt(0) : null,
                codeHex: ch ? ch.charCodeAt().toString(16).toUpperCase() : null,
                point: this.point(),
                size: this.getCodeSize(),
                sizeKB: (0, $ca727f7f34cfa7f3$export$e1a3971de07c83b5)(this.getCodeSize(), 2)
            })
        });
    }),
    set_fill_column: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(value) {
        let next = (value)=>{
            let prev = this.getq("fill_column");
            this.setq("fill_column", +value);
            this.signalInfo(`Fill column set to ${value} (was ${prev})`, false, 5000);
        };
        if (value == null) this.whenMinibuffer((mb)=>{
            this.cmd("minibuffer_prompt", "Set fill column to: ");
            mb.setMark();
            mb.transientMarker = mb.createMarker(mb.point(), true);
            mb.cmd("insert", String(this._rowcol.col));
            mb.ensureTransientMark();
            this.cmd("minibuffer_read_number", next);
        });
        else next(value);
    }),
    bury_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ymacs = this.ymacs;
        ymacs.switchToNextBuffer();
    }),
    describe_mode: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ymacs = this.ymacs;
        let buf = ymacs.switchToBuffer("*Help*");
        ymacs.switchToBuffer(buf);
        buf.pushKeymap($210e37d01040ed37$var$help_keymap);
        buf._disableUndo(()=>{
            buf.setCode(`Active keymaps in buffer "${this.name}".\nPress \`q\` to go back.\n\n`);
            buf.cmd("end_of_buffer");
            let dumpKeymap = (prefix, definitions)=>{
                for (let [key, val] of Object.entries(definitions)){
                    let def = (prefix ? prefix + " " : "") + key;
                    if (Array.isArray(val)) {
                        let txt = "  ";
                        txt += def;
                        for(let n = 20 - def.length; n-- >= 0;)txt += " ";
                        txt += " : " + (typeof val[0] == "function" ? "(lambda)" : JSON.stringify(val));
                        buf.cmd("insert", txt);
                        buf.cmd("newline");
                    } else if (val && typeof val == "object") dumpKeymap(def, val);
                }
            };
            [
                ...this.keymap
            ].reverse().forEach((keymap, index)=>{
                if (index > 0) buf.cmd("insert", "\n");
                buf.cmd("insert", `Keymap: ${keymap.name || "(unnamed)"}\n`);
                buf.cmd("newline");
                dumpKeymap("", keymap.definitions);
            });
        });
        buf.dirty(false);
        buf.cmd("goto_char", 0);
    }),
    window_configuration_to_register: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenMinibuffer((mb)=>{
            mb.prompt("Window configuration to register:");
            this.setq("read_register_callback", (reg)=>{
                let ymacs = this.ymacs;
                ymacs.registers[reg] = {
                    frames: ymacs.getFrameConfig()
                };
                this.signalInfo(`Saved to register "${reg}"`, null, 2000);
            });
            this.pushKeymap($210e37d01040ed37$var$read_register_keymap);
        });
    }),
    jump_to_register: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.whenMinibuffer((mb)=>{
            mb.prompt("Jump to register:");
            this.setq("read_register_callback", (reg)=>this.ymacs.jumpToRegister(reg));
            this.pushKeymap($210e37d01040ed37$var$read_register_keymap);
        });
    })
});
/* -----[ rectangle functions (vertical editing) ]----- */ (function() {
    function apply_on_rectangle(buffer, begin, end, func) {
        buffer.cmd("save_excursion", function() {
            var p1 = this._positionToRowCol(begin), p2 = this._positionToRowCol(end), width = Math.abs(p2.col - p1.col);
            for(var line = p1.row; line <= p2.row; ++line){
                this.cmd("goto_char", this._rowColToPosition(line, 0));
                var text = this.code[line], c1 = p1.col, c2 = p2.col, p = this.point(), ws = 0;
                if (c1 > c2) {
                    var tmp = c1;
                    c1 = c2;
                    c2 = tmp;
                }
                if (c1 > text.length) {
                    ws = c1 - text.length;
                    c1 = text.length;
                }
                if (c2 > text.length) c2 = text.length;
                func.call(this, p + c1, p + c2, ws, width);
            }
        }, begin == buffer.point());
    }
    (0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
        string_rectangle: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r\nsString rectangle: ", function(begin, end, string) {
            apply_on_rectangle(this, begin, end, function(c1, c2, ws) {
                if (ws > 0) this._insertText(" ".repeat(ws), c1);
                else this._deleteText(c1, c2);
                this._insertText(string, c1 + ws);
            });
        }),
        kill_rectangle: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
            var text = [];
            apply_on_rectangle(this, begin, end, function(c1, c2, ws, width) {
                var str = this._bufferSubstring(c1, c2);
                if (c2 - c1 < width) str += " ".repeat(width - c2 + c1);
                text.push(str);
                this._deleteText(c1, c2);
            });
            this.setq("killed_rectangle", text);
        }),
        clear_rectangle: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
            this.cmd("string_rectangle", begin, end, " ".repeat(Math.abs(this._positionToRowCol(end).col - this._positionToRowCol(begin).col)));
        }),
        insert_rectangle: function(point, rect) {
            var col = this._positionToRowCol(point).col;
            this.setMark(point);
            rect.forEach((text, i)=>{
                if (i > 0) {
                    if (!this.cmd("forward_line")) {
                        this.cmd("end_of_line");
                        this.cmd("newline");
                    }
                    this.cmd("move_to_column", col, true);
                }
                this.cmd("insert", text);
            });
        },
        yank_rectangle: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("d", function(point) {
            var kr = this.getq("killed_rectangle");
            if (kr == null) throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("No killed rectangle");
            this.cmd("insert_rectangle", point, kr);
        }),
        _next_is_meta: function() {
            if (!this.__nextIsMeta) {
                this.__nextIsMeta = true;
                this.currentKeys.pop();
            }
        }
    });
})();
(function() {
    (0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
        kmacro_start_macro: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(arg) {
            if (this.ymacs.isRunningMacro()) return;
            if (this.ymacs.isRecordingMacro()) {
                this.signalError("Already defining keyboard macro.");
                return;
            }
            this.signalInfo("Defining keyboard macro");
            this.ymacs.startMacro(arg !== null);
        }),
        kmacro_end_macro: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            if (this.ymacs.isRunningMacro()) return;
            if (!this.ymacs.isRecordingMacro()) {
                this.signalInfo("Not defining kbd macro");
                return;
            }
            this.signalInfo("Keyboard macro defined");
            this.ymacs.stopMacro();
        }),
        kmacro_end_and_call_macro: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("p", function(arg) {
            this.ymacs.stopMacro();
            if (arg === null) arg = 1;
            var macro = this.ymacs.getLastMacro();
            this.interactiveEvent(null);
            this.ymacs.runMacro(arg, macro);
        })
    });
})();
/* -----[ transient mark extension commands ]----- */ [
    "forward_char",
    "forward_word",
    "forward_line",
    "forward_paragraph",
    "forward_sexp",
    "beginning_of_line",
    "beginning_of_indentation_or_line",
    "beginning_of_buffer",
    "backward_char",
    "backward_word",
    "backward_line",
    "backward_paragraph",
    "backward_sexp",
    "end_of_line",
    "end_of_buffer"
].forEach(function(cmd) {
    (0, $a49159f89f9c9e7a$export$df331bdfc76955b4).COMMANDS[cmd + "_mark"] = (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.ensureTransientMark();
        this.cmdApply(cmd, arguments);
        this.ensureTransientMark();
    });
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT



/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT





var $40c5782885e50ecf$var$$menu = null, $40c5782885e50ecf$var$$selectedIndex = null, $40c5782885e50ecf$var$$selectedItem = null;
function $40c5782885e50ecf$var$popupCompletionMenu(frame, list, { noError: noError = false, noPrefix: noPrefix = false } = {}) {
    $40c5782885e50ecf$var$killMenu();
    let activeElement = document.activeElement;
    let ymacs = this.ymacs; // `this` is the minibuffer
    $40c5782885e50ecf$var$$menu = new (0, $0c7e6e98ae65431f$export$731251e1d39d8621)();
    $40c5782885e50ecf$var$$menu.addClass("with-arrow");
    list.forEach((label, index)=>{
        let value = label;
        if (typeof label != "string") {
            value = label.value;
            label = label.label;
        }
        let el = (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).fromHTML(`<div class="Ymacs_Menu_Item" data-value="${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(value)}"
                                          data-index="${index}">${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(label)}</div>`);
        $40c5782885e50ecf$var$$menu.add(el);
    });
    (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).on($40c5782885e50ecf$var$$menu.getContentElement(), {
        click: (ev)=>{
            activeElement.focus();
            let item = ev.target;
            if (!(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).hasClass(item, "Ymacs_Menu_Item")) return;
            $40c5782885e50ecf$var$select(+item.dataset.index);
            $40c5782885e50ecf$var$handle_enter.call(this);
        }
    });
    ymacs._popupAtCaret($40c5782885e50ecf$var$$menu.getElement());
    $40c5782885e50ecf$var$select(0);
    // XXX: this is some more aggressive, ivy-like completion, but
    // it's not working very well with filename completion.
    let onChange = this.getq("ivy_completion") && (0, $ca727f7f34cfa7f3$export$ad41d882ec94ba04)(()=>{
        $40c5782885e50ecf$var$killMenu();
        this.cmd("minibuffer_complete", {
            noError: noError,
            noPrefix: noPrefix
        });
    }, 1);
    this.pushKeymap($40c5782885e50ecf$var$KEYMAP_POPUP_ACTIVE);
    if (onChange) this.addEventListener("onChange", onChange);
    $40c5782885e50ecf$var$$menu.addEventListener("onDestroy", ()=>{
        this.popKeymap($40c5782885e50ecf$var$KEYMAP_POPUP_ACTIVE);
        if (onChange) this.removeEventListener("onChange", onChange);
    });
}
function $40c5782885e50ecf$var$killMenu() {
    if ($40c5782885e50ecf$var$$menu) $40c5782885e50ecf$var$$menu.destroy();
    $40c5782885e50ecf$var$$menu = null;
    $40c5782885e50ecf$var$$selectedItem = null;
    $40c5782885e50ecf$var$$selectedIndex = null;
}
function $40c5782885e50ecf$var$select(idx) {
    let cont = $40c5782885e50ecf$var$$menu.getContentElement();
    let items = [
        ...cont.querySelectorAll(".Ymacs_Menu_Item")
    ];
    let n = items.length;
    $40c5782885e50ecf$var$$selectedIndex = (idx % n + n) % n;
    items.forEach((el)=>{
        let current = el.dataset.index == $40c5782885e50ecf$var$$selectedIndex;
        if (current) $40c5782885e50ecf$var$$selectedItem = el;
        (0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).condClass(el, current, "selected");
    });
    $40c5782885e50ecf$var$$selectedItem.scrollIntoView({
        block: "nearest"
    });
}
function $40c5782885e50ecf$export$9cb120f8f6032a76(completions, cont, validate) {
    this.whenMinibuffer(function(mb) {
        let ivy = completions !== $40c5782885e50ecf$var$filename_completion;
        var changed_vars = mb.setq({
            ivy_completion: ivy,
            completion_list: completions,
            minibuffer_validation: (what, cont2)=>{
                if (what == null) what = mb.cmd("minibuffer_contents");
                if (validate) validate.call(this, mb, what, cont2);
                else cont2(true); // accept anything by default
            },
            minibuffer_continuation: (what)=>{
                mb.setq(changed_vars);
                if (cont) cont.call(this, what);
            }
        });
        mb.cmd("minibuffer_complete", {
            noError: true,
            noPrefix: !ivy
        });
    });
}
function $40c5782885e50ecf$var$filename_completion(mb, str, cont, { noPrefix: noPrefix = false } = {}) {
    var self = this;
    var lastslash = str.lastIndexOf("/");
    var dir = str.slice(0, lastslash + 1);
    var partial = str.slice(lastslash + 1);
    self.ymacs.fs_getDirectory(dir, function(files) {
        function add_trailing_slash_to_dir(name) {
            if (files[name].type == "directory") return name + "/";
            else return name;
        }
        if (!files) {
            mb.signalError("Not found");
            cont(null);
        } else {
            var completions = [];
            for (let f of Object.keys(files))if (f.indexOf(partial) == 0) completions.push(add_trailing_slash_to_dir(f));
            if (completions.length == 0) cont([]);
            else {
                var prefix = (0, $ca727f7f34cfa7f3$export$b43564a9f178c38c)(completions);
                if (prefix != partial && !noPrefix) {
                    mb.cmd("minibuffer_replace_input", dir + prefix);
                    cont(completions.map((name)=>({
                            label: name,
                            value: dir + name
                        })));
                } else if (completions.length == 1) cont([
                    str
                ]);
                else cont(completions.map((name)=>({
                        label: name,
                        value: dir + name
                    })));
            }
        }
    });
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    minibuffer_prompt: function(prompt, nofocus) {
        this.whenMinibuffer(function(mb) {
            var f = this.getMinibufferFrame();
            this.ymacs.setInputFrame(f);
            mb.prompt(prompt);
            if (!nofocus) f.focus();
        });
    },
    minibuffer_yn: function(prompt, cont) {
        this.cmd("minibuffer_prompt", prompt + " (yes or no) ");
        this.cmd("minibuffer_read_yn", function(text) {
            cont(text == "yes");
        });
    },
    minibuffer_read_yn: function(cont) {
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, [
            "yes",
            "no"
        ], cont, function(mb, text, cont2) {
            if (text == "yes" || text == "no") cont2(true);
            else mb.signalError("Please enter yes or no");
        });
    },
    minibuffer_read_number: function(cont) {
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, null, cont, function(mb, text, cont2) {
            var n = parseInt(text, 10);
            if (isNaN(n)) mb.signalError("Please enter a number");
            cont2(!isNaN(n));
        });
    },
    minibuffer_read_command: function(cont) {
        var completions = Object.keys(this.COMMANDS).filter((cmd)=>this.COMMANDS[cmd].ymacsInteractive).sort();
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, completions, cont, function(mb, name, cont2) {
            var cmd = this.COMMANDS[name], ret = cmd && cmd.ymacsInteractive;
            if (!ret) mb.signalError("No such command: " + name);
            cont2(ret);
        });
    },
    minibuffer_read_function: function(cont) {
        var commandNames = Array.hashKeys((0, $a49159f89f9c9e7a$export$df331bdfc76955b4).COMMANDS).sort();
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, commandNames, cont, function(mb, name, cont2) {
            var cmd = this.COMMANDS[name], ret = !!cmd;
            if (!ret) mb.signalError("No such function: " + name);
            cont2(ret);
        });
    },
    minibuffer_read_buffer: function(cont) {
        this.whenYmacs(function(ymacs) {
            var bufferNames = ymacs.buffers.map((b)=>b.name);
            bufferNames.push(bufferNames.shift());
            $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, bufferNames, cont);
        //handle_tab.call(this);
        });
    },
    minibuffer_read_string: function(completions, cont, validate) {
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, completions, cont, validate);
    },
    minibuffer_read_variable: function(cont) {
        var tmp = Object.assign({}, this.globalVariables, this.variables);
        var completions = Object.keys(tmp).filter((name)=>!/^\*/.test(name)).sort();
        $40c5782885e50ecf$export$9cb120f8f6032a76.call(this, completions, function(name) {
            let val = this.getq(name);
            this.signalInfo(`Current value of <b>${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(name)}</b> = <b>${(0, $ca727f7f34cfa7f3$export$cb0933279c36a66b).htmlEscape(val)}</b>`, true, 3000);
            return cont.apply(this, arguments);
        });
    },
    minibuffer_read_existing_file: function(cont) {
        var self = this;
        self.cmd("minibuffer_replace_input_by_current_dir", function() {
            $40c5782885e50ecf$export$9cb120f8f6032a76.call(self, $40c5782885e50ecf$var$filename_completion, cont, function(mb, name, cont2) {
                self.ymacs.fs_fileType(name, function(type) {
                    if (type != "file") {
                        mb.signalError("No such file: " + name);
                        cont2(false);
                    } else cont2(true);
                });
            });
        });
    },
    minibuffer_read_file: function(cont) {
        var self = this;
        self.cmd("minibuffer_replace_input_by_current_dir", function() {
            $40c5782885e50ecf$export$9cb120f8f6032a76.call(self, $40c5782885e50ecf$var$filename_completion, cont, function(mb, name, cont2) {
                self.ymacs.fs_fileType(name, function(type) {
                    cont2(type != "directory");
                });
            });
        });
    },
    minibuffer_read_file_or_directory: function(cont) {
        var self = this;
        self.cmd("minibuffer_replace_input_by_current_dir", function() {
            $40c5782885e50ecf$export$9cb120f8f6032a76.call(self, $40c5782885e50ecf$var$filename_completion, cont);
        });
    },
    minibuffer_read_directory: function(cont) {
        var self = this;
        self.cmd("minibuffer_replace_input_by_current_dir", function() {
            $40c5782885e50ecf$export$9cb120f8f6032a76.call(self, $40c5782885e50ecf$var$filename_completion, cont);
        });
    },
    minibuffer_prompt_end: function() {
        return this.whenMinibuffer(function(mb) {
            return mb.promptMarker.getPosition();
        });
    },
    minibuffer_contents: function() {
        return this.whenMinibuffer(function(mb) {
            return mb._bufferSubstring(mb.promptMarker);
        });
    },
    minibuffer_replace_input: function(value) {
        this.whenMinibuffer(function(mb) {
            mb._replaceText(mb.promptMarker, mb.getCodeSize(), value);
            this.getMinibufferFrame().redrawCaret(true);
        });
    },
    minibuffer_replace_input_by_current_dir: function(cont) {
        this.whenYmacs(function(ymacs) {
            var self = this;
            var name = ymacs.getActiveBuffer().name;
            var dir = name.slice(0, name.lastIndexOf("/") + 1);
            ymacs.fs_getDirectory(dir, function(info) {
                if (info && Object.keys(info).length > 0) ymacs.fs_remapDir(dir, function(dir) {
                    self.cmd("minibuffer_replace_input", dir);
                    self.cmd("minibuffer_complete", {
                        noError: true,
                        noPrefix: true
                    });
                    cont();
                });
                else {
                    self.cmd("minibuffer_replace_input", "");
                    cont();
                }
            });
        });
    },
    minibuffer_complete: function({ noError: noError = false, noPrefix: noPrefix = false } = {}) {
        var self = this;
        self.whenMinibuffer(function(mb) {
            let str = mb.cmd("minibuffer_contents");
            let a = mb.getq("completion_list");
            function complete(a) {
                if (!a || a.length == 0) {
                    if (!noError) mb.signalError("No completions", false, 2000);
                } else $40c5782885e50ecf$var$popupCompletionMenu.call(mb, self.getMinibufferFrame(), a, {
                    noError: noError,
                    noPrefix: noPrefix
                });
            }
            if (a instanceof Function) a.call(self, mb, str, function(a) {
                if (a) complete(a);
            }, {
                noPrefix: noPrefix
            });
            else if (a && a.length > 0) complete((0, $ca727f7f34cfa7f3$export$acebde4da3d957e6)(a, str));
            else complete(a);
        });
    },
    minibuffer_complete_and_exit: function() {
        this.whenMinibuffer((mb)=>{
            mb.getq("minibuffer_validation").call(mb, null, (valid)=>{
                if (valid) mb.cmd("minibuffer_keyboard_quit", this.getq("minibuffer_continuation"));
                else mb.cmd("minibuffer_complete");
            });
        });
    },
    minibuffer_keyboard_quit: function(cont) {
        this.whenMinibuffer(function(mb) {
            var text = this.cmd("minibuffer_contents");
            mb.setCode("");
            this.ymacs.setInputFrame(this.ymacs.getActiveFrame());
            this.ymacs.getActiveFrame().focus();
            if (!cont) mb.callHooks("abort");
            setTimeout(()=>{
                if (cont) cont.call(this, text);
                this.getPrefixArg();
            }, 1);
        });
        $40c5782885e50ecf$var$killMenu();
    }
});
function $40c5782885e50ecf$var$handle_arrow_down() {
    if ($40c5782885e50ecf$var$$menu) $40c5782885e50ecf$var$select($40c5782885e50ecf$var$$selectedIndex + 1);
}
function $40c5782885e50ecf$var$handle_arrow_up() {
    if ($40c5782885e50ecf$var$$menu) $40c5782885e50ecf$var$select($40c5782885e50ecf$var$$selectedIndex - 1);
}
function $40c5782885e50ecf$var$handle_popup_home() {
    if ($40c5782885e50ecf$var$$menu) $40c5782885e50ecf$var$select(0);
}
function $40c5782885e50ecf$var$handle_popup_end() {
    if ($40c5782885e50ecf$var$$menu) $40c5782885e50ecf$var$select(-1);
}
function $40c5782885e50ecf$var$handle_enter() {
    if ($40c5782885e50ecf$var$$menu) {
        let item = $40c5782885e50ecf$var$$selectedItem;
        if (item) {
            $40c5782885e50ecf$var$killMenu();
            this.cmd("minibuffer_replace_input", item.dataset.value);
            this.cmd("minibuffer_complete_and_exit");
        } else this.signalError("Select something...");
    } else this.cmd("minibuffer_complete_and_exit");
}
function $40c5782885e50ecf$var$handle_tab() {
    if (!$40c5782885e50ecf$var$$menu) this.cmd("minibuffer_complete");
    else $40c5782885e50ecf$var$handle_arrow_down.call(this);
}
function $40c5782885e50ecf$var$handle_s_tab() {
    $40c5782885e50ecf$var$handle_arrow_up.call(this);
}
function $40c5782885e50ecf$var$handle_home() {
    this.cmd("goto_char", this.promptMarker);
}
function $40c5782885e50ecf$var$handle_home_mark() {
    this.ensureTransientMark();
    this.cmd("goto_char", this.promptMarker);
    this.ensureTransientMark();
}
function $40c5782885e50ecf$var$handle_tilde() {
    if (this.getq("completion_list") === $40c5782885e50ecf$var$filename_completion) {
        if (this.cmd("looking_back", "/")) {
            $40c5782885e50ecf$var$killMenu();
            this.cmd("minibuffer_replace_input", "~/");
            this.cmd("minibuffer_complete", {
                noError: true,
                noPrefix: true
            });
            return;
        }
    }
    this.cmd("self_insert_command");
}
var $40c5782885e50ecf$var$DEFAULT_KEYS = {
    "Tab": $40c5782885e50ecf$var$handle_tab,
    "Enter": $40c5782885e50ecf$var$handle_enter,
    "Home && C-a": $40c5782885e50ecf$var$handle_home,
    "~": $40c5782885e50ecf$var$handle_tilde,
    "S-Home && S-C-a": (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", $40c5782885e50ecf$var$handle_home_mark)
};
var $40c5782885e50ecf$var$KEYMAP_MB_ACTIVE = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define(null, Object.assign({
    "C-g && Escape": "minibuffer_keyboard_quit"
}, $40c5782885e50ecf$var$DEFAULT_KEYS));
var $40c5782885e50ecf$var$KEYMAP_POPUP_ACTIVE = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define(null, Object.assign({
    "S-Tab": $40c5782885e50ecf$var$handle_s_tab,
    "ArrowDown && ArrowRight && C-n && C-f": $40c5782885e50ecf$var$handle_arrow_down,
    "ArrowUp && ArrowLeft && C-p && C-b": $40c5782885e50ecf$var$handle_arrow_up,
    "PageDown": $40c5782885e50ecf$var$handle_arrow_down,
    "PageUp": $40c5782885e50ecf$var$handle_arrow_up,
    "C-End && M->": $40c5782885e50ecf$var$handle_popup_end,
    "C-Home && M-<": $40c5782885e50ecf$var$handle_popup_home,
    "Escape": function() {
        $40c5782885e50ecf$var$killMenu();
    }
}, $40c5782885e50ecf$var$DEFAULT_KEYS));
$40c5782885e50ecf$var$KEYMAP_POPUP_ACTIVE.defaultHandler = [
    function() {
        if (!this.getq("ivy_completion")) $40c5782885e50ecf$var$killMenu();
        return false; // say it's not handled though
    }
];
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("minibuffer_mode", function() {
    this.pushKeymap($40c5782885e50ecf$var$KEYMAP_MB_ACTIVE);
    return function() {
        this.popKeymap($40c5782885e50ecf$var$KEYMAP_MB_ACTIVE);
    };
});


(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    get_region: function() {
        return this.getRegion();
    },
    figure_out_mode: function(code) {
        if (!code) code = this.getCode();
        var lines = code.split(/\n/);
        if (lines.length > 4) lines.splice(2, lines.length - 4);
        for (let line of lines){
            let m = /-\*-\s*(.*?)\s*-\*-/i.exec(line);
            if (m) return m[1];
        }
    },
    // TODO: the mapping from extension to mode should be defined
    // with each mode.
    mode_from_name: function(name) {
        if (!name) name = this.name;
        var ext = (/\.[^.]+$/.exec(name) || [
            ""
        ])[0];
        switch(ext){
            case ".css":
                return "css";
            case ".js":
                return "javascript";
            case ".lisp":
            case ".scm":
            case ".el":
                return "lisp";
            case ".md":
                return "markdown";
            case ".xml":
                return "xml";
            case ".html":
                return "html";
            case ".twig":
                return "twig_html";
        }
        return null;
    },
    set_buffer_mode: function(mode) {
        if (!mode) mode = this.cmd("figure_out_mode") || this.cmd("mode_from_name");
        if (mode) {
            if (this.COMMANDS[mode]) this.cmd(mode, true);
            else if (this.COMMANDS[mode + "_mode"]) this.cmd(mode + "_mode", true);
        }
    },
    cperl_lineup: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r", function(begin, end) {
        this.cmd("save_excursion", function() {
            var rcend = this._positionToRowCol(end), max = 0, lines = [];
            this.cmd("goto_char", begin);
            this.cmd("forward_whitespace", true);
            var ch = this.charAt();
            if (ch.toLowerCase() != ch.toUpperCase()) {
                this.signalError("Cannot lineup here");
                return;
            }
            while(this._rowcol.row <= rcend.row){
                var pos = this.getLine().indexOf(ch);
                if (pos >= 0) {
                    if (pos > max) max = pos;
                    lines.push([
                        this._rowcol.row,
                        pos
                    ]);
                }
                if (!this.cmd("forward_line")) break;
            }
            ++max;
            lines.forEach((l)=>{
                this.cmd("goto_char", this._rowColToPosition(l[0], l[1]));
                this.cmd("insert", " ".repeat(max - l[1]));
            });
        });
    }),
    htmlize_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("r\nP", function(begin, end, lineNum) {
        this.tokenizer.finishParsing();
        var row = this._positionToRowCol(begin).row;
        var html = "";
        var line = row;
        var pad;
        if (lineNum && !lineNum.empty) line = parseInt(lineNum, 10);
        end = this._positionToRowCol(end).row;
        pad = String(end).length;
        while(row <= end){
            html += "<div class='line'>";
            if (lineNum) html += "<span class='line-number'>" + (0, $ca727f7f34cfa7f3$export$3cdc770bf8b2ed3d)(line, pad, " ") + "</span>";
            ++line;
            html += this._textProperties.getLineHTML(row, this.code[row], null) + "</div>\n";
            ++row;
        }
        var tmp = this.ymacs.switchToBuffer("*Htmlize*");
        tmp.setCode(html);
        tmp.cmd("xml_mode", true);
    }),
    execute_extended_command: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^CM-x ", function(cmd) {
        this.callInteractively(cmd);
    }),
    set_variable: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("vSet variable: \nsTo value: ", function(variable, value) {
        var tmp = parseFloat(value);
        if (!isNaN(tmp)) value = tmp;
        this.setq(variable, value);
    }),
    eval_string: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^MEval string: ", function(code) {
        try {
            var variables = [
                this,
                this.ymacs // ymacs
            ];
            code = new Function("buffer", "ymacs", code);
            code.apply(this, variables);
            this.clearTransientMark();
        } catch (ex) {
            this.signalError(ex.type + ": " + ex.message);
            if (window.console) console.log(ex);
        }
    }),
    eval_region: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r", function(begin, end) {
        this.cmd("eval_string", this.cmd("buffer_substring", begin, end));
    }),
    eval_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("eval_string", this.getCode());
    }),
    toggle_line_numbers: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function() {
        this.ymacs.toggleClass("Ymacs-line-numbers");
    }),
    save_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("FWrite file: ", function(name) {
        this.ymacs.ls_setFileContents(name, this.getCode());
        this.signalInfo("Saved in local storage");
    }),
    load_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("fFind file: ", function(name) {
        var code = this.ymacs.ls_getFileContents(name);
        var buffer = this.ymacs.createBuffer({
            name: name
        });
        buffer.setCode(code);
        buffer.cmd("set_buffer_mode");
        buffer.cmd("switch_to_buffer", name);
    }),
    find_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("FFind file: ", function(name) {
        var self = this;
        name = self.ymacs.fs_normalizePath(name);
        self.ymacs.fs_fileType(name, function(type) {
            if (type == "directory") self.signalInfo("Can't open directory");
            else self.ymacs.fs_getFileContents(name, true, function(code, stamp) {
                var buffer = self.ymacs.getBuffer(name);
                function find_file() {
                    buffer.setCode(code || "");
                    buffer.stamp = stamp;
                    buffer.dirty(false);
                    buffer.cmd("set_buffer_mode");
                    buffer.cmd("switch_to_buffer", name);
                }
                if (buffer) {
                    if (stamp == null) find_file();
                    else if (buffer.stamp == stamp) buffer.cmd("switch_to_buffer", name);
                    else {
                        var msg = "File " + name + " changed on disk.  " + (buffer.dirty() ? "Discard your edits?" : "Reread from disk?");
                        buffer.cmd("minibuffer_yn", msg, function(yes) {
                            if (yes) find_file();
                        });
                    }
                } else {
                    buffer = self.ymacs.createBuffer({
                        name: name,
                        stamp: stamp
                    });
                    if (code == null) self.signalInfo("New file");
                    find_file();
                }
            });
        });
    }),
    write_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("FWrite file: ", function(name) {
        var self = this;
        function write_file() {
            self.ymacs.fs_setFileContents(name, self.getCode(), null, function(stamp) {
                self.cmd("rename_buffer", name);
                self.dirty(false);
                self.stamp = stamp; // refresh stamp
                self.signalInfo("Wrote " + name);
            });
        }
        var buffer = self.ymacs.getBuffer(name);
        if (!buffer) write_file();
        else {
            var msg = "A buffer is visiting " + name + "; proceed?";
            buffer.cmd("minibuffer_yn", msg, function(yes) {
                if (yes) {
                    self.ymacs.killBuffer(buffer);
                    write_file();
                }
            });
        }
    }),
    save_some_buffers: function() {
        this.cmd("save_some_buffers_with_continuation", true, function() {});
    },
    save_some_buffers_with_continuation: function(ask, cont) {
        var bufs = this.ymacs.buffers.slice(); // get copy of buffers
        function loop(saved) {
            if (bufs.length > 0) bufs.shift().cmd("save_buffer_with_continuation", ask, loop);
            else cont();
        }
        loop(false);
    },
    save_buffer_with_continuation: function(ask, cont) {
        var self = this;
        function did_save(stamp) {
            self.dirty(false);
            self.stamp = stamp; // refresh stamp
            cont(true);
        }
        function do_save() {
            self.ymacs.fs_setFileContents(self.name, self.getCode(), self.stamp, function(stamp) {
                if (stamp != null) did_save(stamp);
                else self.cmd("minibuffer_yn", self.name + " has changed since visited or saved.  Save anyway?", function(yes) {
                    if (!yes) cont(false);
                    else self.ymacs.fs_setFileContents(self.name, self.getCode(), null, function(stamp) {
                        did_save(stamp);
                    });
                });
            });
        }
        if (!self.dirty() || ask && self.name.match(/^\*.*\*$/)) cont(false);
        else if (!ask) do_save();
        else self.cmd("minibuffer_yn", "Save file " + self.name + "?", function(yes) {
            if (!yes) cont(false);
            else do_save();
        });
    },
    save_buffer: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("", function() {
        var self = this;
        if (self.dirty()) self.cmd("save_buffer_with_continuation", false, function(saved) {
            if (saved) self.signalInfo("Wrote " + self.name);
        });
        else self.signalInfo("No changes need to be saved");
    }),
    delete_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("fDelete file: ", function(name) {
        var self = this;
        self.ymacs.fs_deleteFile(name, function() {
            self.signalInfo("Deleted " + name);
        });
    }),
    eval_file: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("fEval file: ", function(name) {
        var self = this;
        self.ymacs.fs_getFileContents(name, false, function(code, stamp) {
            self.cmd("eval_string", code);
        });
    }),
    request_full_screen: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(async function() {
        try {
            this.ymacs.requestFullScreen();
        } catch (ex) {
            console.error(ex);
            this.signalError("Full-screen denied", false, 3000);
        }
    }),
    set_color_theme: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function(theme) {
        if (theme) this.ymacs.setColorTheme(theme);
        else {
            let names = (0, $ca727f7f34cfa7f3$export$2ceef881d0b4a563)().sort();
            this.cmd("minibuffer_prompt", "Color theme: ");
            (0, $40c5782885e50ecf$export$9cb120f8f6032a76).call(this, names, (theme)=>{
                this.ymacs.setColorTheme(theme);
            }, (mb, theme, cont)=>{
                theme = theme.trim();
                if (!theme) mb.cmd("minibuffer_complete");
                else if (names.indexOf(theme) < 0) {
                    mb.signalError("No such theme");
                    return cont(false);
                } else return cont(true);
            });
        }
    }),
    toggle_bar_cursor: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.ymacs.toggleBarCursor();
    })
});



/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT



let $eb5c1275a77cc963$var$Ymacs_Keymap_ISearch = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("isearch", {
    "Escape": [
        "isearch_abort",
        true
    ],
    "C-g": "isearch_reset_or_abort",
    "C-w && C-S-s": "isearch_yank_word_or_char",
    "C-s": "isearch_forward",
    "C-r": "isearch_backward",
    "M-%": "query_replace",
    "C-M-%": "query_replace_regexp",
    "M-s w && M-w": "isearch_toggle_word",
    "M-s c && M-c": "isearch_toggle_case_fold",
    "M-s Space && M-Space": "isearch_toggle_lax_whitespace",
    "Enter": "isearch_abort",
    "C-l": "recenter_top_bottom",
    "Backspace": function() {
        if (this.getMinibuffer().point() > this._isearchContext.mbMark.getPosition()) {
            this.getMinibuffer().cmd("backward_delete_char");
            this.cmd("goto_char", this._isearchContext.livepoint);
            $eb5c1275a77cc963$var$isearchText.call(this);
            $eb5c1275a77cc963$var$updateIsearch.call(this);
        }
    }
});
$eb5c1275a77cc963$var$Ymacs_Keymap_ISearch.defaultHandler = [
    "isearch_printing_char"
];
function $eb5c1275a77cc963$var$initIsearch({ forward: forward = true, regexp: regexp = false, lax: lax = true, word: word = false, case_fold: case_fold = this.getq("case_fold_search"), case_replace: case_replace = this.getq("case_replace"), qreplace: qreplace = false, region: region = null } = {}) {
    if (!this._isearchContext) {
        this._isearchContext = {
            forward: forward,
            regexp: regexp,
            lax: lax,
            word: word,
            case_fold: case_fold,
            case_replace: case_replace,
            livepoint: this.point(),
            origpoint: this.point(),
            current: {
                begin: this.point(),
                end: this.point()
            },
            mbMark: this.getMinibuffer().promptMarker,
            query: "",
            qreplace: qreplace,
            region: region
        };
        if (!qreplace) {
            this.pushKeymap($eb5c1275a77cc963$var$Ymacs_Keymap_ISearch);
            $eb5c1275a77cc963$var$resetPrompt.call(this);
        }
        return true;
    }
}
function $eb5c1275a77cc963$var$isearchText() {
    return this._isearchContext.query = this.getMinibuffer()._bufferSubstring(this._isearchContext.mbMark);
}
function $eb5c1275a77cc963$var$updateIsearch({ forward: forward } = {}) {
    var query = this._isearchContext.query;
    if (!/\S/.test(query) && this.getq("isearch_last_context")) {
        this._isearchContext = this.getq("isearch_last_context");
        query = this._isearchContext.query;
        this.getMinibuffer()._placeUndoBoundary();
        this.getMinibuffer().cmd("insert", query);
        this._isearchContext.origpoint = this.point();
        this._isearchContext.current = {
            begin: this.point(),
            end: this.point()
        };
    }
    this._isearchContext.livepoint = this.point();
    if (forward != null) this._isearchContext.forward = forward;
    return $eb5c1275a77cc963$var$doSearch.call(this);
}
function $eb5c1275a77cc963$var$caseFold() {
    let ctx = this._isearchContext;
    let query = ctx.query;
    return ctx.case_fold ?? (ctx.regexp ? query = query.replace(/\\./g, "") : query) == query.toLowerCase();
}
function $eb5c1275a77cc963$var$lazyHighlight(qrx, prompt) {
    let cursor = this._rowcol;
    let minpos = this._rowColToPosition(cursor.row - 50, 0);
    let maxpos = this._rowColToPosition(cursor.row + 50, Infinity);
    let code = this.getCode();
    let hl = [];
    let ctx = this._isearchContext;
    let point = this.point();
    let count = 0;
    let crnt = 0;
    qrx.lastIndex = ctx.region?.begin || 0;
    while(true){
        let m = qrx.exec(code);
        if (m && m[0].length) {
            if (ctx.region && qrx.lastIndex > ctx.region.end) break;
            count++;
            if (m.index >= minpos && m.index <= maxpos) {
                let p1 = this._positionToRowCol(m.index);
                let p2 = this._positionToRowCol(qrx.lastIndex);
                hl.push({
                    line1: p1.row,
                    col1: p1.col,
                    line2: p2.row,
                    col2: p2.col
                });
            }
            if (point >= m.index && point < qrx.lastIndex || point == qrx.lastIndex && ctx.forward) crnt = count;
        } else break;
    }
    $eb5c1275a77cc963$var$resetPrompt.call(this, count, crnt, prompt);
    this.setOverlay("isearch-lazy", hl);
}
function $eb5c1275a77cc963$var$resetPrompt(count, crnt, prompt) {
    let ctx = this._isearchContext;
    this.whenMinibuffer((mb)=>{
        if (prompt) mb.prompt(prompt.call(this, count, crnt, ctx));
        else {
            let pos = ctx.qreplace && count ? `[${count}] ` : count && crnt ? `[${crnt}/${count}] ` : "";
            mb.prompt(`${pos}${ctx.qreplace ? "Query replace" : "I-search"}${ctx.regexp ? " regexp" : ctx.word ? " word" : ""}${ctx.forward ? "" : " backward"}:`);
        }
        let start = ctx.mbMark;
        let mid = start + ctx.lastFoundQuery?.length;
        if (mid != null) {
            mb.forEachLine((line, c1, c2)=>{
                mb._textProperties.removeLineProps(line, c1, c2, "css");
            }, start, mid);
            mb.forEachLine((line, c1, c2)=>{
                mb._textProperties.addLineProps(line, c1, c2, "css", "isearch-fail");
            }, mid);
        }
    });
}
function $eb5c1275a77cc963$var$doSearch({ forward: forward } = this._isearchContext) {
    let ctx = this._isearchContext;
    let query = ctx.query;
    let found = false;
    try {
        let rx = $eb5c1275a77cc963$var$searchRegExp.call(this);
        found = this.cmd(forward ? "search_forward_regexp" : "search_backward_regexp", rx);
        if (found) {
            ctx.lastFoundQuery = query;
            this.cmd("ensure_caret_visible");
            let p1 = this.point();
            let p2 = p1 + (forward ? -1 : 1) * this.matchData[0].length;
            if (forward) [p1, p2] = [
                p2,
                p1
            ];
            ctx.current = {
                begin: p1,
                end: p2
            };
            let p1rc = this._positionToRowCol(p1);
            let p2rc = this._positionToRowCol(p2);
            this.setOverlay("isearch", {
                line1: p1rc.row,
                col1: p1rc.col,
                line2: p2rc.row,
                col2: p2rc.col
            });
        }
        $eb5c1275a77cc963$var$lazyHighlight.call(this, rx);
    } catch  {}
    return found;
}
function $eb5c1275a77cc963$var$searchRegExp({ query: query = null, regexp: regexp = false, lax: lax = true, case_fold: case_fold = true, word: word = false } = this._isearchContext || {}) {
    let searchRX = query;
    if (!regexp) {
        searchRX = query.replace(/[\]\[\}\{\)\(\*\+\?\.\\\^\$\|]/g, "\\$&");
        if (word) // XXX: I guess it would be nice to use syntax_word / syntax_word_dabbrev
        searchRX = "\\b" + searchRX + "\\b";
    }
    if (lax) searchRX = searchRX.replace(/\s+/g, "\\s+");
    return new RegExp(searchRX, $eb5c1275a77cc963$var$caseFold.call(this) ? "mugi" : "mug");
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    isearch_forward: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let arg = {
            forward: true
        };
        if (!$eb5c1275a77cc963$var$initIsearch.call(this, arg)) {
            if (!$eb5c1275a77cc963$var$updateIsearch.call(this, arg)) this.signalError("No more forward occurrences of the search text");
        }
    }),
    isearch_backward: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let arg = {
            forward: false
        };
        if (!$eb5c1275a77cc963$var$initIsearch.call(this, arg)) {
            if (!$eb5c1275a77cc963$var$updateIsearch.call(this, arg)) this.signalError("No more backward occurrences of the search text");
        }
    }),
    isearch_forward_regexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let arg = {
            forward: true,
            regexp: true
        };
        if (!$eb5c1275a77cc963$var$initIsearch.call(this, arg)) {
            if (!$eb5c1275a77cc963$var$updateIsearch.call(this, arg)) this.signalError("No more forward occurrences of the search text");
        }
    }),
    isearch_backward_regexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let arg = {
            forward: false,
            regexp: true
        };
        if (!$eb5c1275a77cc963$var$initIsearch.call(this, arg)) {
            if (!$eb5c1275a77cc963$var$updateIsearch.call(this, arg)) this.signalError("No more forward occurrences of the search text");
        }
    }),
    isearch_yank_word_or_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        if (!this._isearchContext) $eb5c1275a77cc963$var$initIsearch.call(this, {
            forward: true
        });
        let ctx = this._isearchContext;
        var pos = ctx.current.end;
        var pos2 = this.cmd("save_excursion", function() {
            this.cmd("goto_char", pos);
            this.cmd("forward_word");
            return this.point();
        });
        if (pos2 != pos) {
            var word = this._bufferSubstring(pos, pos2);
            this.getMinibuffer()._placeUndoBoundary();
            this.getMinibuffer().cmd("insert", word.toLowerCase());
            word = $eb5c1275a77cc963$var$isearchText.call(this);
            this.cmd("goto_char", ctx.current.begin);
            $eb5c1275a77cc963$var$doSearch.call(this, {
                forward: true
            });
        }
    }),
    isearch_toggle_word: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ctx = this._isearchContext;
        ctx.word = !ctx.word;
        if (ctx.word) ctx.regexp = false;
        this.signalInfo(`Search word: ${ctx.word ? "ON" : "OFF"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        $eb5c1275a77cc963$var$doSearch.call(this);
    }),
    isearch_toggle_case_fold: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ctx = this._isearchContext;
        if (ctx.case_fold == null) ctx.case_fold = false;
        else if (ctx.case_fold === false) ctx.case_fold = true;
        else if (ctx.case_fold === true) ctx.case_fold = null;
        this.signalInfo(`Case sensitive: ${ctx.case_fold == null ? "AUTO" : ctx.case_fold ? "OFF" : "ON"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        $eb5c1275a77cc963$var$doSearch.call(this);
    }),
    isearch_toggle_lax_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ctx = this._isearchContext;
        ctx.lax = !ctx.lax;
        this.signalInfo(`Loose whitespace: ${ctx.lax ? "ON" : "OFF"}`, false, 2000);
        this.cmd("goto_char", this._isearchContext.current.begin);
        $eb5c1275a77cc963$var$doSearch.call(this);
    }),
    isearch_printing_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ctx = this._isearchContext;
        var ev = this.interactiveEvent();
        if (ev?.key?.length == 1 && !ev.ctrlKey && !ev.altKey) {
            this.whenMinibuffer((mb)=>{
                mb.cmd("self_insert_command");
                this.cmd("goto_char", ctx.livepoint);
                $eb5c1275a77cc963$var$isearchText.call(this);
                $eb5c1275a77cc963$var$doSearch.call(this);
            });
            return true;
        } else {
            this.cmd("isearch_abort");
            return false;
        }
    }),
    isearch_abort: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function(cancelled) {
        if (!cancelled) this.setGlobal("isearch_last_context", this._isearchContext);
        this.setMinibuffer("");
        this.popKeymap($eb5c1275a77cc963$var$Ymacs_Keymap_ISearch);
        if (cancelled) this.cmd("goto_char", this._isearchContext.origpoint);
        else this.markMarker.setPosition(this._isearchContext.origpoint);
        this._isearchContext = null;
        this.deleteOverlay("isearch");
        this.deleteOverlay("isearch-lazy");
        return true;
    }),
    isearch_reset_or_abort: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let ctx = this._isearchContext;
        if (!ctx.lastFoundQuery || ctx.query == ctx.lastFoundQuery) this.cmd("isearch_abort", true);
        else this.whenMinibuffer((mb)=>{
            mb._replaceText(ctx.mbMark, mb.getCode().length, ctx.lastFoundQuery);
            ctx.query = ctx.lastFoundQuery;
            $eb5c1275a77cc963$var$doSearch.call(this);
        });
    })
});
/* -----[ query-replace ]----- */ let $eb5c1275a77cc963$var$Ymacs_Keymap_Query_Replace = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("query_replace", {
    "y && Space": "query_replace_yes_this_occurrence",
    ".": "query_replace_yes_this_occurrence_and_stop",
    "n && Delete && Backspace": "query_replace_no_this_occurrence",
    "u": "query_replace_undo_previous",
    "!": "query_replace_yes_all_occurrences",
    "q && Enter": [
        "query_replace_abort",
        true
    ],
    "C-l": "recenter_top_bottom"
});
$eb5c1275a77cc963$var$Ymacs_Keymap_Query_Replace.defaultHandler = [
    "query_replace_abort"
];
function $eb5c1275a77cc963$var$query_replace_1(args = {}) {
    this.whenMinibuffer((mb)=>{
        $eb5c1275a77cc963$var$initIsearch.call(this, {
            ...args,
            qreplace: true
        });
        let ctx = this._isearchContext;
        this.cmd("minibuffer_prompt", `Query replace${ctx.regexp ? " regexp" : ctx.word ? " word" : ""}: `);
        let hlOrig = ()=>{
            $eb5c1275a77cc963$var$isearchText.call(this);
            try {
                $eb5c1275a77cc963$var$lazyHighlight.call(this, $eb5c1275a77cc963$var$searchRegExp.call(this));
            } catch  {}
        };
        mb.addEventListener("afterInteractiveCommand", hlOrig);
        let onQuit = (continued)=>{
            mb.removeEventListener("afterInteractiveCommand", hlOrig);
            mb.removeEventListener("abort", onQuit);
            if (!continued) {
                this.deleteOverlay("isearch");
                this.deleteOverlay("isearch-lazy");
                this._isearchContext = null;
            }
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, (orig)=>{
            this._isearchContext.query = orig;
            $eb5c1275a77cc963$var$query_replace_2.call(this);
            onQuit(true);
        }, (buf, query, cont)=>{
            ctx.query = query;
            try {
                $eb5c1275a77cc963$var$lazyHighlight.call(this, $eb5c1275a77cc963$var$searchRegExp.call(this));
                cont(query);
            } catch  {
                buf.popupMessage({
                    type: "error",
                    text: "Incomplete regexp",
                    atCaret: true
                });
            }
        });
    });
}
function $eb5c1275a77cc963$var$query_replace_2() {
    let mb = this.getMinibuffer();
    let ctx = this._isearchContext;
    let query = ctx.query;
    let rxorig = $eb5c1275a77cc963$var$searchRegExp.call(this);
    $eb5c1275a77cc963$var$lazyHighlight.call(this, rxorig);
    if (rxorig) {
        this.cmd("minibuffer_prompt", `Replace ${ctx.regexp ? "regexp " : ""}\u{201C}${query}\u{201D} with: `);
        let onQuit = ()=>{
            this.deleteOverlay("isearch");
            this.deleteOverlay("isearch-lazy");
            this._isearchContext = null;
            mb.removeEventListener("abort", onQuit);
        };
        mb.addEventListener("abort", onQuit);
        this.cmd("minibuffer_read_string", null, (replacement)=>{
            this.clearTransientMark();
            $eb5c1275a77cc963$var$query_replace_3.call(this, mb, rxorig, replacement);
        });
    }
}
function $eb5c1275a77cc963$var$similarCase(rplc) {
    let ctx = this._isearchContext;
    if (ctx.case_replace && rplc == rplc.toLowerCase()) {
        let orig = this.matchData[0];
        if (orig == orig.toUpperCase()) return rplc.toUpperCase();
        if (orig == this.capitalize(orig)) return this.capitalize(rplc);
    }
    return rplc;
}
function $eb5c1275a77cc963$var$query_replace_3(mb, rxorig, replacement) {
    let ctx = this._isearchContext;
    this.pushKeymap($eb5c1275a77cc963$var$Ymacs_Keymap_Query_Replace);
    let cmds, stop, curr, count = 0;
    let queue = [];
    let end = ctx.region && this.createMarker(ctx.region.end);
    this.cmd("goto_char", ctx.region ? ctx.region.begin : ctx.current.begin);
    let domatch = (replacement)=>{
        if (ctx.regexp) // look ma, I can write code like this.
        replacement = replacement.replace(/(?:\\([\\\&\#]|\d+|<.+?>))/g, (s, p)=>p == "\\" ? "\\" : p == "#" ? count : p == "&" ? this.matchData[0] : p[0] == "<" ? this.matchData.groups[p.substr(1, p.length - 2)] : this.matchData[+p]);
        return $eb5c1275a77cc963$var$similarCase.call(this, replacement);
    };
    let gotoNext = (all)=>{
        let point = this.point();
        let found = this.cmd("search_forward_regexp", rxorig);
        if (found) {
            let m = this.matchData;
            if (ctx.region && (m.index < ctx.region.begin || m.after > end)) {
                if (!all) this.cmd("goto_char", point);
                return null;
            }
            let begin = this.matchData.index;
            found = {
                begin: begin,
                end: this.point()
            };
            if (!all) {
                this.cmd("ensure_caret_visible");
                let rc_begin = this._positionToRowCol(begin);
                this.setOverlay("isearch", {
                    line1: rc_begin.row,
                    col1: rc_begin.col,
                    line2: this._rowcol.row,
                    col2: this._rowcol.col
                });
            }
        }
        if (!all) $eb5c1275a77cc963$var$lazyHighlight.call(this, rxorig, (count, crnt)=>`[${count}] Replace with \u{201C}${domatch(replacement)}\u{201D}? (y/n/u/!)`);
        return found;
    };
    cmds = this.replaceCommands({
        query_replace_yes_this_occurrence: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(()=>{
            queue.push(curr);
            this._replaceText(curr.begin, curr.end, domatch(replacement));
            count++;
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_yes_this_occurrence_and_stop: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(()=>{
            queue.push(curr);
            this._replaceText(curr.begin, curr.end, domatch(replacement));
            count++;
            stop();
        }),
        query_replace_no_this_occurrence: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(()=>{
            curr = gotoNext();
            if (!curr) stop();
        }),
        query_replace_yes_all_occurrences: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(()=>{
            let last;
            while(curr){
                last = curr;
                this._replaceText(curr.begin, curr.end, domatch(replacement));
                count++;
                curr = gotoNext(true);
            }
            if (last) {
                this.cmd("goto_char", last.begin);
                this.cmd("ensure_caret_visible");
            }
            stop();
        }),
        query_replace_undo_previous: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(()=>{
            let prev = queue.pop();
            if (prev) {
                count--;
                this.cmd("undo");
                // XXX: this is ugly, but oh well.. We'd like to
                // pretend this edit/undo operation never exist.
                let uptr = this.__undoPointer;
                let uq = this.__undoQueue.slice(0, uptr);
                setTimeout(()=>{
                    this.__undoPointer = uptr;
                    this.__undoQueue = uq;
                });
                this.cmd("goto_char", prev.begin);
                curr = gotoNext();
            } else this.signalError("No more undo");
        }),
        query_replace_abort: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)((enter)=>{
            stop();
            return enter;
        })
    });
    stop = ()=>{
        if (end) end.destroy();
        mb.removeEventListener("abort", stop);
        this.newCommands(cmds);
        this.deleteOverlay("isearch");
        this.deleteOverlay("isearch-lazy");
        this._isearchContext = null;
        this.popKeymap($eb5c1275a77cc963$var$Ymacs_Keymap_Query_Replace);
        mb.cmd("minibuffer_keyboard_quit");
        this.signalInfo(`Replaced ${count} occurrences`);
    };
    curr = gotoNext();
    if (!curr) stop();
    else mb.addEventListener("abort", stop);
}
function $eb5c1275a77cc963$var$query_replace(regexp) {
    return (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^P", function(word) {
        let ctx = this._isearchContext;
        let args = {
            qreplace: true,
            regexp: regexp == null && ctx ? ctx.regexp : !!regexp,
            word: ctx ? ctx.word : !!word,
            region: this.transientMarker && this.getRegion()
        };
        if (ctx) {
            this.cmd("isearch_abort");
            this._isearchContext = Object.assign({}, ctx, args);
            $eb5c1275a77cc963$var$query_replace_2.call(this);
        } else $eb5c1275a77cc963$var$query_replace_1.call(this, args);
    });
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    query_replace: $eb5c1275a77cc963$var$query_replace(null),
    query_replace_regexp: $eb5c1275a77cc963$var$query_replace(true)
});




/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT



class $0813b912056c2fc5$export$96ac030efd09f62d {
    _cont = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _inParens = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _parens = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _inComment = null;
    _inString = false;
    _pmeta = null;
    COMMENT = [
        "//",
        [
            "/*",
            "*/",
            "*"
        ]
    ];
    STRING = [
        '"',
        "'"
    ];
    NUMBER = /^[+-]?(?:0x[0-9a-fA-F]+|(?:\d*\.)?\d+(?:[eE][+-]?\d+)?)/u;
    NAME = /^[\p{L}_$][\p{L}0-9_$]*/iu;
    WHITESPACE = /^[ \u00a0\n\r\t\f\u000b\u200b\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\uFEFF]+/;
    OPEN_PAREN = {
        "(": ")",
        "{": "}",
        "[": "]"
    };
    CLOSE_PAREN = {
        ")": "(",
        "}": "{",
        "]": "["
    };
    constructor({ stream: stream, tok: tok }){
        this._stream = stream;
        this._tok = tok;
    }
    next() {
        this._stream.checkStop();
        if (this._cont !== (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b)) this._cont.car.call(this);
        else this.read();
    }
    forgetState() {
        this._cont = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
        this._inParens = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
        this._parens = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
        this._inComment = null;
        this._inString = false;
        this._pmeta = null;
    }
    copy() {
        let _cont = this._cont;
        let _inParens = this._inParens;
        let _parens = this._parens;
        let _inComment = this._inComment;
        let _inString = this._inString;
        let _pmeta = this._pmeta;
        return ()=>{
            this._cont = _cont;
            this._inParens = _inParens;
            this._parens = _parens;
            this._inComment = _inComment;
            this._inString = _inString;
            this._pmeta = _pmeta;
            return this;
        };
    }
    maybeSave() {
        let s = this._stream;
        if (s.eol() && s.nextLine()) {
            this._tok.parsers[s.line] = this.copy();
            return true;
        }
    }
    skipWS(skipLines = true) {
        let s = this._stream, m;
        while(true){
            if (m = s.lookingAt(this.WHITESPACE)) this.t(null, m[0].length);
            if (skipLines) {
                if (!this.maybeSave()) break;
            } else break;
        }
    }
    read() {
        this.readComment() || this.readString() || this.readOpenParen() || this.readCloseParen() || this.readCustom() || this.readTrailingWhitespace() || this.t();
    }
    readComment() {
        for (let syn of this.COMMENT){
            if (Array.isArray(syn) && this.readCommentMulti(...syn)) return true;
            if (this.readCommentLine(syn)) return true;
        }
    }
    readCommentLine(start) {
        let s = this._stream;
        let p = null, end = null;
        while(s.lookingAt(start)){
            if (!p) p = {
                line: s.line,
                col: s.col,
                c1: s.col,
                c2: s.col,
                comment: true,
                type: "",
                inner: {
                    l1: s.line,
                    c1: s.col
                },
                outer: {
                    l1: s.line,
                    c1: s.col
                }
            };
            this.t("comment-starter", start.length);
            this.token({
                line: s.line,
                c1: s.col,
                c2: s.col = s.lineLength()
            }, "comment");
            end = {
                line: s.line,
                col: s.col,
                c1: s.col,
                c2: s.col,
                type: "",
                opened: p
            };
        // XXX: this screws up the parse state after a line comment.
        // this.maybeSave();
        // this.skipWS(false);
        }
        if (p) {
            p.closed = end;
            p.inner.l2 = p.outer.l2 = end.line;
            p.inner.c2 = p.outer.c2 = end.c1;
            this.doneParen(p);
        }
        return p;
    }
    readCommentMulti(start, stop, inner, tok1 = "mcomment-starter", tok2 = "mcomment", tok3 = "mcomment-stopper") {
        let s = this._stream, m;
        if (this._inComment) {
            if (m = s.lookingAt(stop)) {
                let p = this.popInParen(start, m[0].length, tok3);
                p.inner.l2 = p.closed.line;
                p.inner.c2 = p.closed.c1;
                p.outer.l2 = p.closed.line;
                p.outer.c2 = p.closed.c2;
                this.popCont();
                this._inComment = null;
            } else this.t(tok2);
        } else if (s.lookingAt(start)) {
            this._inComment = {
                line: s.line,
                c1: s.col,
                inner: inner
            };
            let p = this.pushInParen(start, tok1);
            p.comment = true;
            p.inner = {
                l1: p.line,
                c1: p.c2
            };
            p.outer = {
                l1: p.line,
                c1: p.c1
            };
            this.pushCont(this.readCommentMulti.bind(this, start, stop, inner, tok1, tok2, tok3));
            return true;
        }
    }
    readString(start, stop, expStart, expStop, tokType) {
        let s = this._stream;
        if (this._inString) {
            if (s.lookingAt("\\")) {
                this.t(tokType);
                this.t(tokType);
            } else if (s.lookingAt(stop)) {
                this.popCont();
                this._inString = false;
                this.popInParen(start, stop.length, `${tokType}-stopper`);
            } else if (expStart && s.lookingAt(expStart)) {
                this._inString = false;
                let op = this.pushInParen(expStart);
                this.pushCont(()=>{
                    if (s.lookingAt(expStop) && this._inParens.car === op) {
                        this.popInParen(expStart, expStop.length);
                        this.popCont();
                        this._inString = true;
                    } else this.read();
                });
            } else this.t(tokType);
        } else for (let syn of this.STRING){
            let start = syn, stop = syn, expStart, expStop, tokType = "string";
            if (Array.isArray(syn)) [start, stop, expStart, expStop, tokType = "string"] = syn;
            if (s.lookingAt(start)) {
                this.pushInParen(start, `${tokType}-starter`);
                this._inString = true;
                this.pushCont(this.readString.bind(this, start, stop, expStart, expStop, tokType));
                return true;
            }
        }
    }
    readCustom() {
        return this.readNumber();
    }
    readName() {
        let s = this._stream, m = s.lookingAt(this.NAME);
        if (m) return {
            line: s.line,
            c1: s.col,
            c2: s.col += m[0].length,
            id: m[0]
        };
    }
    maybeName(type = null) {
        let name = this.readName();
        if (name) this.token(name, name.type = type);
        return name;
    }
    readNumber() {
        let m = this._stream.lookingAt(this.NUMBER);
        if (m) {
            this.t("number", m[0].length);
            return true;
        }
    }
    setParenMeta(info) {
        this._pmeta = info;
    }
    readOpenParen() {
        let ch = this._stream.peek();
        if (this.OPEN_PAREN[ch]) {
            this.pushInParen(ch).meta = this._pmeta;
            this._pmeta = null;
            return true;
        }
    }
    readCloseParen() {
        let type = this.CLOSE_PAREN[this._stream.peek()];
        if (type) {
            this.popInParen(type, 1);
            return true;
        }
    }
    readTrailingWhitespace() {
        let m = this._stream.lookingAt(/^\s+$/);
        if (m) {
            this.t("trailing-whitespace", m[0].length);
            return true;
        }
    }
    t(type = null, len = 1) {
        this.token({
            line: this._stream.line,
            c1: this._stream.col,
            c2: this._stream.col += len
        }, type);
    }
    token(tok, type = tok.type) {
        this._tok.onToken(tok.line, tok.c1, tok.c2, type);
    }
    pushInParen(type, tokType = "open-paren") {
        let s = this._stream;
        let n = type.length;
        let p = {
            line: s.line,
            col: s.col,
            c1: s.col,
            c2: s.col + n,
            type: type
        };
        this._inParens = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(p, this._inParens);
        if (n) this.t(tokType, n);
        return p;
    }
    popInParen(start, n = start.length, tokType = "close-paren") {
        let s = this._stream;
        if (this._inParens !== (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b)) {
            let paren = this._inParens.car;
            this._inParens = this._inParens.cdr;
            if (start != paren.type) //debugger;
            {
                if (n) this.t("error", n);
            } else {
                paren.closed = {
                    line: s.line,
                    col: s.col,
                    c1: s.col,
                    c2: s.col + n,
                    opened: paren
                };
                this.doneParen(paren);
                if (n) this.t(tokType, n);
            }
            return paren;
        } else if (n) this.t("error", n);
    }
    inParen() {
        return this._inParens.car;
    }
    doneParen(p) {
        this._parens = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(p, this._parens);
    }
    pushCont(cont) {
        this._cont = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(cont, this._cont);
    }
    popCont() {
        this._cont = this._cont.cdr;
    }
    // copied from JS mode; should be decent for C-like langs
    indentation() {
        let s = this._stream;
        let row = s.line;
        let currentLine = s.lineText();
        let indent = 0;
        let INDENT_LEVEL = ()=>this._stream.buffer.getq("indent_level");
        if (this._inString) {
            // inside string literal
            if (row > 0 && !/\S/.test(currentLine)) // on an empty line, set indentation from previous line
            return s.lineIndentation(row - 1);
            // otherwise keep existing indentation
            return null;
        }
        if (this._inComment) {
            let commentStartLine = s.lineText(this._inComment.line);
            indent = this._inComment.c1 + 1;
            if (this._inComment.inner == "*" && !/^\s*\*/.test(currentLine)) {
                // align with the first non-whitespace and non-asterisk character in the comment
                let re = /[^\s*]/g;
                re.lastIndex = this._inComment.c1 + 1;
                let m = re.exec(commentStartLine);
                if (m) indent = m.index;
            }
            return indent;
        }
        let p = this._inParens.car;
        if (p) {
            // check if the current line closes the paren
            let re = new RegExp("^\\s*\\" + this.OPEN_PAREN[p.type]);
            let thisLineCloses = re.test(currentLine);
            // Check if there is text after the opening paren.  If so, indent to that column.
            re = /\S/g;
            re.lastIndex = p.col + 1;
            let m = re.exec(s.lineText(p.line));
            if (m) // but if this line closes the paren, better use the column of the open paren
            indent = thisLineCloses ? p.col : m.index;
            else {
                // Otherwise we should indent to one level more than the indentation of the line
                // containing the opening paren. Except that if another paren is closed on that line
                // before `p`, then we'd like to use that paren's opening line instead. Oh well.
                let line = p.line;
                if (this._parens.car?.closed?.line == line) line = this._parens.car.line;
                indent = s.lineIndentation(line) + INDENT_LEVEL();
                // but if this line closes the paren, then back one level
                if (thisLineCloses) indent -= INDENT_LEVEL();
                else if (this.C_STATEMENTS && /^\s*(?:[.:?*=&|]|\+[^+]|-[^-])/.test(currentLine)) indent += INDENT_LEVEL();
            }
        } else {
            let i = row, m;
            while(i-- > 0)if (m = /\S/.exec(s.lineText(i))) {
                let p = this._tok.getParserForLine(i);
                if (!(p._inString || p._inComment)) break;
            }
            if (m) indent = m.index;
        }
        // Some more adjustments for continued statements.  Since we
        // don't really have a rigorous parser, we have to rely on
        // other regexps here, which sucks but will do for now.
        if (row > 0) {
            let before = s.textBefore();
            if (/\)\s*$/.test(before) && this._parens !== (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b)) {
                // Ends in a paren, could be an if, while or for which demands smart
                // indentation on the current line, let's check it out.
                // Note that the passedParen saved for that close paren is actually
                // the opening one, which suits us greatly.
                p = this._parens.car;
                let stmtLine = s.lineText(p.line);
                if (/^\s*(?:if|for|while)\W/.test(stmtLine) && !/^\s*\{/.test(currentLine)) indent += INDENT_LEVEL();
            } else if (/\Welse\s*$/.test(before) && !/^\s*\{/.test(currentLine)) indent += INDENT_LEVEL();
        }
        // switch labels use half the indent level, which is my favorite
        if (/^\s*(?:case|default)\W/.test(currentLine)) indent -= INDENT_LEVEL() / 2;
        return indent;
    }
    get passedParens() {
        return [
            ...this._parens
        ];
    }
    get buffer() {
        return this._stream.buffer;
    }
    get caret() {
        return this.buffer._rowcol;
    }
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    base_limit_fill_paragraph_region: function() {
        if (!this.tokenizer) return;
        let findComment = ()=>this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(this._rowcol)).findLast((p)=>p.comment);
        let comment = findComment() || this.cmd("save_excursion", ()=>{
            this.cmd("end_of_line");
            return findComment();
        });
        if (comment?.closed) {
            let r = {
                begin: this._rowColToPosition(comment.outer.l1, comment.outer.c1),
                end: this._rowColToPosition(comment.outer.l2, comment.outer.c2)
            };
            this.cmd("save_excursion", ()=>{
                this.cmd("goto_char", r.begin);
                this.cmd("backward_whitespace", true);
                r.begin = this.point();
            });
            return r;
        }
    }
});


class $784e6fab20f30624$var$Ymacs_Lang_CSS extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    readCustom() {
        let s = this._stream, m;
        for (let [rx, ...types] of this.CSSRX)if (m = s.lookingAt(rx)) {
            types.forEach((cls, i)=>cls && m[i] && this.t(cls, m[i].length));
            return true;
        }
    }
    CSSRX = [
        [
            /^(-?(?:\d*\.)?\d+)(px|pt|em|ex|in|cm|mm|rem|vw|vh|fr|s|%)?/u,
            ,
            "number",
            "type"
        ],
        [
            /^((?:--+|\$)[\p{L}\p{N}-]+)(:)?/u,
            ,
            "variable-name",
            "operator"
        ],
        [
            /^([\p{L}\p{N}-]+)(:)/u,
            ,
            "keyword",
            "operator"
        ],
        [
            /^\.[\p{L}\p{N}_:-]+/u,
            "function-name"
        ],
        [
            /^#[\p{L}\p{N}_:-]+/u,
            "constant"
        ],
        [
            /^@[\p{L}\p{N}_:-]+/u,
            "builtin"
        ],
        [
            /^(?:url|none|auto|bold|italic|underline|normal|inherit|print|screen|all|important|calc|var)/,
            "builtin"
        ]
    ];
}
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("css", (stream, tok)=>new $784e6fab20f30624$var$Ymacs_Lang_CSS({
        stream: stream,
        tok: tok
    }));
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("css_mode", function() {
    var tok = this.tokenizer;
    this.setTokenizer(new (0, $6fd37b8e17f87e00$export$3964ae1c660db960)({
        buffer: this,
        type: "css"
    }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    var changed_vars = this.setq({
        syntax_paragraph_sep: /\n(?:[ \t\/\*]*\n)+/g,
        syntax_comment_line: {
            rx: /[^\S\r\n]*\/\/+ ?/ygu,
            ch: "//"
        },
        syntax_comment_multi: {
            rx: /[^\S\r\n]*\/\*+(.*?)\*+\//ygu,
            ch: [
                "/*",
                "*/"
            ]
        }
    });
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match) this.cmd("paren_match_mode", false);
        this.setq(changed_vars);
    };
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT





const $ba8e93a76a858c13$var$KEYWORDS = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)(`abstract break case catch class const async await
continue debugger default delete do else enum export final finally for
function goto if implements import in instanceof interface native new package
private protected public return super switch synchronized throw throws
transient try typeof var void let yield volatile while with`);
const $ba8e93a76a858c13$var$KEYWORDS_TYPE = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)(`boolean byte char double float int long short
void Array Date Function Math Number Object RegExp String`);
const $ba8e93a76a858c13$var$KEYWORDS_CONST = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)(`false null undefined Infinity NaN true`);
const $ba8e93a76a858c13$var$KEYWORDS_BUILTIN = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)(`this Packages decodeURI decodeURIComponent
encodeURI encodeURIComponent eval isFinite isNaN parseFloat parseInt window
document alert arguments fetch`);
const $ba8e93a76a858c13$var$ALLOW_REGEXP_AFTER = /[\[({,;+\-*=?&|!:>][\x20\t\n\xa0]*$|(?:return|typeof|case)\s+$/;
class $ba8e93a76a858c13$var$Ymacs_Lang_JS extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    STRING = [
        '"',
        "'",
        [
            "`",
            "`",
            "${",
            "}"
        ]
    ];
    _isProp = null;
    copy() {
        let _super = super.copy();
        let _isProp = this._isProp;
        return ()=>{
            let self = _super();
            self._isProp = _isProp;
            return self;
        };
    }
    readCustom() {
        let s = this._stream;
        if (s.peek() == "/" && $ba8e93a76a858c13$var$ALLOW_REGEXP_AFTER.test(s.textBefore())) {
            let op = {
                line: s.line,
                col: s.col,
                type: "/"
            };
            this.t("regexp-starter");
            this.pushCont(this.readLiteralRegexp.bind(this, op));
            return true;
        }
        if (s.lookingAt("...")) {
            this.t("operator", 3);
            return true;
        }
        let isProp = this._isProp;
        this._isProp = s.peek() == ".";
        let tok = this.readName();
        if (tok) {
            this.skipWS();
            if (isProp) {
                tok.type = s.lookingAt("(") ? "function-name" : null;
                this.token(tok);
            } else if (s.lookingAt(":")) {
                tok.type = tok.id == "default" ? "keyword" : null;
                this.token(tok);
            } else if (!this.parseALittle(tok)) {
                tok.type = tok.id in $ba8e93a76a858c13$var$KEYWORDS ? "keyword" : s.lookingAt("(") ? "function-name" : tok.id in $ba8e93a76a858c13$var$KEYWORDS_TYPE ? "type" : tok.id in $ba8e93a76a858c13$var$KEYWORDS_CONST ? "constant" : tok.id in $ba8e93a76a858c13$var$KEYWORDS_BUILTIN ? "builtin" : null;
                this.token(tok);
            }
            return true;
        }
        return this.readNumber();
    }
    parseALittle(tok) {
        let s = this._stream;
        let paren = this.inParen();
        let name;
        switch(tok.id){
            case "class":
                if (s.lookingAt("{") || (name = this.maybeName("type"))) {
                    this.token(tok, "keyword");
                    this.skipWS();
                    let ext = this.readName();
                    if (ext) {
                        if (ext.id == "extends") {
                            this.token(ext, ext.type = "keyword");
                            this.skipWS();
                            this.maybeName("type");
                        } else this.token(ext, null);
                    }
                    this.setParenMeta({
                        class: tok,
                        name: name
                    });
                }
                return true;
            case "for":
                this.token(tok, "keyword");
                this.setParenMeta({
                    for: tok
                });
                return true;
            case "of":
                if (paren?.meta?.for) {
                    this.token(tok, "keyword");
                    return true;
                }
                break;
            case "function":
                this.token(tok, "keyword");
                this.maybeName("function-name");
                return true;
            case "new":
                this.token(tok, "keyword");
                if (!s.lookingAt(/^class/)) this.maybeName("type");
                return true;
            case "get":
            case "set":
            case "static":
            case "constructor":
                if (paren?.meta?.class) {
                    this.token(tok, "keyword");
                    return true;
                }
                break;
        }
        if (paren?.meta?.class && s.lookingAt(/^\s*\(/)) {
            // method definition
            this.token(tok, "function-name");
            return true;
        }
    }
    readLiteralRegexp(op) {
        let s = this._stream;
        let ch, esc = false, inset = 0, start = s.col;
        while(!s.eol()){
            ch = s.peek();
            if (ch == "[" && !esc && !inset) inset++;
            if (ch == "]" && !esc && inset) inset--;
            if (ch == "/" && !esc && !inset) {
                let c1 = s.col;
                this.popCont();
                this.token({
                    line: s.line,
                    c1: start,
                    c2: s.col
                }, "regexp");
                this.t("regexp-stopper");
                let m = s.lookingAt(/^[dgimsuvy]+/);
                if (m) this.t("regexp-modifier", m[0].length);
                op.closed = {
                    line: s.line,
                    c1: c1,
                    c2: s.col,
                    opened: op
                };
                this.doneParen(op);
                return;
            }
            esc = !esc && ch === "\\";
            ++s.col;
        }
        this.token({
            line: s.line,
            c1: start,
            c2: s.col
        }, "regexp");
    }
}
$ba8e93a76a858c13$var$Ymacs_Lang_JS.prototype.C_STATEMENTS = true;
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("js", (stream, tok)=>new $ba8e93a76a858c13$var$Ymacs_Lang_JS({
        stream: stream,
        tok: tok
    }));
let $ba8e93a76a858c13$var$Ymacs_Keymap_JS = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("js", {
    "`": [
        "paredit_open_pair",
        "`",
        "`",
        /[\`\\]/g
    ],
    "'": [
        "paredit_open_pair",
        "'",
        "'",
        /[\'\\]/g
    ],
    "M-`": [
        "paredit_wrap_round",
        "`",
        "`",
        /[\`\\]/g
    ],
    "M-'": [
        "paredit_wrap_round",
        "'",
        "'",
        /[\'\\]/g
    ]
});
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("javascript_mode", function() {
    let tok = this.tokenizer;
    this.setTokenizer(new (0, $6fd37b8e17f87e00$export$3964ae1c660db960)({
        buffer: this,
        type: "js"
    }));
    let was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap($ba8e93a76a858c13$var$Ymacs_Keymap_JS);
    let changed_vars = this.setq({
        syntax_paragraph_sep: /\n(?:[ \t\/\*]*\n)+/g,
        syntax_comment_line: {
            rx: /[^\S\r\n]*\/\/+ ?/ygu,
            ch: "//"
        },
        syntax_comment_multi: {
            rx: /[^\S\r\n]*\/\*+(.*?)\*+\//ygu,
            ch: [
                "/*",
                "*/"
            ]
        }
    });
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match) this.cmd("paren_match_mode", false);
        this.popKeymap($ba8e93a76a858c13$var$Ymacs_Keymap_JS);
        this.setq(changed_vars);
    };
});


var $7562c6d7dc15aeff$exports = {};

$parcel$export($7562c6d7dc15aeff$exports, "Ymacs_Lang_Lisp", () => $7562c6d7dc15aeff$export$b112841041e97d65);
/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT






(function() {
    function Partial(value) {
        this.value = value;
    }
    function QuickParser(buffer, pos) {
        var input = new (0, $6fd37b8e17f87e00$export$825fc0b420d392c1)({
            buffer: buffer,
            pos: pos
        });
        function peek() {
            return input.peek();
        }
        function next() {
            return input.next();
        }
        function skip_ws() {
            return input.read_while(function(ch) {
                if (!caret_token && caret != null && input.pos == caret) return false;
                return input.is_whitespace(ch);
            });
        }
        function skip(ch) {
            next();
        }
        function read_while(pred) {
            return input.read_while(pred);
        }
        function read_escaped(start, end, inces) {
            skip(start);
            var escaped = false;
            var str = "";
            while(true){
                var ch = next();
                if (!ch) throw new Partial(str);
                if (escaped) {
                    str += ch;
                    escaped = false;
                } else if (ch == "\\") {
                    if (inces) str += ch;
                    escaped = true;
                } else if (ch == end) break;
                else str += ch;
            }
            return str;
        }
        function read_comment() {
            return read_while(function(ch) {
                return ch && ch != "\n";
            });
        }
        function read_multiline_comment() {
            var comment = read_while(function() {
                return !input.looking_at("|#");
            });
            skip();
            skip();
            return comment;
        }
        function read_string() {
            return read_escaped("\"", "\"");
        }
        function read_list(beg, end) {
            var save_list_index = list_index;
            list_index = 0;
            try {
                var ret = [], p;
                skip(beg);
                out: while(true){
                    skip_ws();
                    switch(peek()){
                        case end:
                            break out;
                        case null:
                            throw new Partial(ret);
                        default:
                            ret.push(read_token());
                            ++list_index;
                    }
                }
                skip(end);
                return ret;
            } finally{
                list_index = save_list_index;
            }
        }
        function read_regexp() {
            var str = read_escaped("/", "/", true);
            var mods = read_while(function(ch) {
                if (ch) switch(ch.toLowerCase()){
                    case "y":
                    case "m":
                    case "g":
                    case "i":
                        return true;
                }
            }).toLowerCase();
            return {
                pattern: str,
                modifiers: mods
            };
        }
        function is_symbol_char(ch) {
            switch(ch){
                case null:
                case "(":
                case ")":
                case "{":
                case "}":
                case "[":
                case "]":
                case "#":
                case ";":
                case "`":
                case "'":
                case "\"":
                case "|":
                case " ":
                case "\n":
                case "\t":
                case "\x0C":
                case "\u2028":
                case "\u2029":
                case "\xA0":
                    return false;
            }
            return true;
        }
        function read_symbol() {
            return read_while(is_symbol_char);
        }
        function read_char() {
            return next() + read_while(function(ch) {
                return ch >= "a" && ch <= "z" || ch >= "A" && ch <= "z" || ch >= "0" && ch <= "9" || ch == "-" || ch == "_";
            });
        }
        function read_elisp_char() {
            skip("?");
            if (peek() == "\\") next();
            return next();
        }
        function read_sharp() {
            skip("#");
            switch(peek()){
                case "\\":
                    next();
                    return token("char", read_char);
                case "/":
                    return token("regexp", read_regexp);
                case "(":
                    return token("vector", read_list.bind(null, "(", ")"));
                case "'":
                    next();
                    return token("function", read_token);
                case "|":
                    next();
                    return token("comment", read_multiline_comment);
                default:
                    return token("unknown", read_token);
            }
        }
        function read_token() {
            skip_ws();
            if (!caret_token && caret != null && input.pos == caret && (!parent || parent.type == "list")) return caret_token = token("caret");
            switch(peek()){
                case ";":
                    return token("comment", read_comment);
                case "\"":
                    return token("string", read_string);
                case "(":
                    return token("list", read_list.bind(null, "(", ")"));
                case "{":
                    return token("list", read_list.bind(null, "{", "}"));
                case "[":
                    return token("list", read_list.bind(null, "[", "]"));
                case "#":
                    return token("sharp", read_sharp);
                case "?":
                    return token("char", read_elisp_char);
                case "`":
                    next();
                    return token("qq", read_token, -1);
                case ",":
                    next();
                    if (peek() == "@") {
                        next();
                        return token("splice", read_token, -2);
                    }
                    return token("unquote", read_token, -1);
                case "'":
                    next();
                    return token("quote", read_token, -1);
                case ")":
                    return null;
                case null:
                    return null; // EOF
            }
            return token("symbol", read_symbol);
        }
        function read_all() {
            var ret = [];
            while(peek() != null){
                var tok = read_token();
                if (tok == null) break;
                ret.push(tok);
                ++list_index;
            }
            return ret;
        }
        var caret_token = null;
        var list_index = 0;
        var caret = null;
        var parent = null;
        var cont_exp = null;
        function token(type, reader, adjust_start) {
            if (adjust_start == null) adjust_start = 0;
            var save_parent = parent;
            try {
                var tok = {
                    value: null,
                    index: list_index,
                    type: type,
                    start: input.pos + adjust_start,
                    parent: parent,
                    depth: parent ? parent.depth + 1 : 0,
                    partial: false
                };
                if (type == "list") parent = tok;
                try {
                    if (reader) {
                        tok.value = reader();
                        if (tok.value === "" && type != "string") // couldn't figure this out, but let's not crash the browser.
                        next();
                    }
                } catch (ex) {
                    if (ex instanceof Partial) {
                        tok.value = ex.value;
                        tok.partial = true;
                    } else throw ex;
                }
                tok.end = input.pos;
                if (caret != null) {
                    if (tok.start <= caret && tok.end >= caret) {
                        if (!cont_exp) cont_exp = tok;
                    }
                }
                return tok;
            } finally{
                parent = save_parent;
            }
        }
        return {
            parse: function(pos) {
                caret = pos;
                return token("list", read_all);
            },
            read: function() {
                return read_token();
            },
            prev_exp: function() {
                if (caret_token) return caret_token.parent.value[caret_token.index - 1];
                else if (cont_exp) {
                    if (cont_exp.type == "list" && cont_exp.end == caret + 1) return cont_exp.value.at(-1);
                    return cont_exp.parent?.value[cont_exp.index];
                }
            },
            caret_token: function() {
                return caret_token;
            },
            cont_exp: function() {
                return cont_exp;
            },
            sexp: function() {
                var tok = cont_exp;
                while(tok && (!/^(?:list|string)$/.test(tok.type) || tok.end == caret))tok = tok.parent;
                return tok;
            }
        };
    }
    (0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
        test_lisp_parse: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            try {
                var p = QuickParser(this);
                var ast = p.parse(this.point());
                console.log(ast);
                console.log(p.caret_token());
                console.log(p.cont_exp());
                console.log(p.prev_exp());
            } catch (ex) {
                console.log(ex);
            }
        }),
        lisp_make_quick_parser: function() {
            return QuickParser.apply(this, [
                this,
                ...arguments
            ]);
        },
        lisp_forward_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            var p = QuickParser(this, this.point());
            var tok = p.read();
            if (tok) this.cmd("goto_char", tok.end);
        }),
        lisp_backward_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            var p = QuickParser(this);
            p.parse(this.point());
            var tok = p.prev_exp();
            if (tok) this.cmd("goto_char", tok.start);
        }),
        lisp_backward_up_list: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            var p = QuickParser(this);
            p.parse(this.point());
            var list = p.sexp();
            if (list && list.parent) this.cmd("goto_char", list.start);
        }),
        lisp_in_string: function() {
            var p = QuickParser(this);
            p.parse(this.point());
            var tok = p.prev_exp();
            return tok && tok.type == "string" && this.point() < tok.end;
        },
        lisp_handle_string_quote: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
            if (this.cmd("lisp_in_string")) {
                if (this.cmd("looking_at", /\"/y)) this.cmd("forward_char");
                else if (!this.cmd("looking_back", /\\/g)) this.cmd("insert", "\\\"");
                else this.cmd("insert", "\"");
            } else {
                this.cmd("insert", '""');
                this.cmd("backward_char");
            }
        })
    });
})();
const $7562c6d7dc15aeff$var$SPECIAL_FORMS = (0, $ca727f7f34cfa7f3$export$90e79c6ee773013e)("  define defvar defparameter declaim proclaim declare type inline   optimize speed safety space debug defconstant   deftype defstruct defclass defsetf destructuring-bind   defmacro defun defmethod defgeneric defpackage in-package defreadtable in-readtable   when cond unless [ec]?typecase e?case   lambda \u03BB let load-time-value quote macrolet   progn begin prog1 prog2 progv go flet the   if throw eval-when multiple-value-prog1 multiple-value-bind unwind-protect let\\*   ignore-errors handler-case handler-bind invoke-restart restart-case restart-bind   labels function symbol-macrolet block tagbody catch locally   inc! dec! cons c[ad]{1,4}r list list\\* eq eql equal equalp and or not null null\\?   loop do do\\* dolist dotimes while dotimes   return return-from setq set! set-car! set-cdr! setf multiple-value-call values", "i");
const $7562c6d7dc15aeff$var$LOOP_KEYWORDS = (0, $ca727f7f34cfa7f3$export$90e79c6ee773013e)("\
  for with and = as in on of then across by while until \
  from downfrom upfrom to upto below downto above \
  being each the hash-keys? using hash-values? \
  collect(?:ing)? nconc(?:ing)? sum(?:ming)? append(?:ing)? into \
  minimize minimizing maximize maximizing count counting \
  symbol symbols external-symbol external-symbols present-symbol present-symbols \
  named always never thereis \
  of-type \
  find maximizes minimizes that which \
  repeat finally initially return \
  if else when unless do doing");
const $7562c6d7dc15aeff$var$ERROR_FORMS = (0, $ca727f7f34cfa7f3$export$90e79c6ee773013e)("error(?:[-/]\\w+)? warn(?:[-/]\\w+)? check(?:[-/]\\w+) assert");
const $7562c6d7dc15aeff$var$CONSTANTS = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("t nil");
const $7562c6d7dc15aeff$var$DEFINES_FUNCTION = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("defun defmacro defgeneric defmethod");
const $7562c6d7dc15aeff$var$DEFINES_TYPE = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("deftype defclass defstruct");
const $7562c6d7dc15aeff$var$DEFINES_ERROR = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("define-condition");
const $7562c6d7dc15aeff$var$DEFINES_VARIABLE = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("defvar defparameter defconstant defconst");
const $7562c6d7dc15aeff$var$FORM_ARGS = {
    "if": "3+",
    "aif": "3+",
    "when": "1*",
    "awhen": "1*",
    "once-only": "1*",
    "lambda": "1*",
    "unless": "1*",
    "defun": "2*",
    "defpackage": "1*",
    "deftest": "1*",
    "defgeneric": "2*",
    "defmethod": "2*",
    "defclass": "2*",
    "defstruct": "1*",
    "defmacro": "2*",
    "defparameter": "1*",
    "defvar": "1*",
    "defconstant": "1*",
    "defglobal": "1*",
    "progn": "0+",
    "begin": "0+",
    "prog1": "1*",
    "multiple-value-prog1": "1*",
    "prog2": "2*",
    "let": "1*",
    "labels": "1*",
    "flet": "1*",
    "foreach": "1*",
    "foreach-index": "1*",
    "macrolet": "1*",
    "symbol-macrolet": "1*",
    "unwind-protect": "1*",
    "catch": "1*",
    "case": "1*",
    "ecase": "1*",
    "typecase": "1*",
    "etypecase": "1*",
    "cond": "0+",
    "handler-bind": "1*",
    "handler-case": "1*",
    "restart-bind": "1*",
    "restart-case": "1*",
    "return-from": "1*",
    "block": "1*",
    "dotimes": "1*",
    "dolist": "1*",
    "do": "2*",
    "do*": "2*",
    "destructuring-bind": "2*",
    "multiple-value-bind": "2*",
    ":method": "1*",
    "eval-when": "1*"
};
const $7562c6d7dc15aeff$var$LOCAL_BODYDEF = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)("labels flet macrolet");
class $7562c6d7dc15aeff$export$b112841041e97d65 extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    _formStack = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _formSym = null;
    _formLen = 0;
    _rxSpecial = null;
    COMMENT = [
        ";",
        [
            "#|",
            "|#"
        ]
    ];
    STRING = [
        '"'
    ];
    NUMBER = /^[+-]?(?:#x[0-9a-f]+|#\d+r[0-9a-z]+|#o[0-7]+|#b[01]+|(?:\d*\.)?\d+(?:e[+-]?\d+)?|\d+\/\d+)$/iu;
    NAME = /^[-_$\p{L}0-9|!#$%&*+./:<=>?@\^~]+/iu;
    OPEN_PAREN = {
        "(": ")",
        "{": "}",
        "[": "]",
        "\u2770": "\u2771",
        "\xab": "\xbb"
    };
    CLOSE_PAREN = {
        ")": "(",
        "}": "{",
        "]": "[",
        "\u2771": "\u2770",
        "\xbb": "\xab"
    };
    constructor({ stream: stream, tok: tok, rx_special: rx_special }){
        super({
            stream: stream,
            tok: tok
        });
        this._rxSpecial = rx_special;
    }
    isNameChar(ch) {
        return this.NAME.test(ch) && !this._rxSpecial?.test(ch);
    }
    readName() {
        if (!this._rxSpecial) return super.readName();
        let s = this._stream, name = s.peek();
        if (this.isNameChar(name)) {
            let col = s.col++, ch;
            while(this.isNameChar(ch = s.peek())){
                name += ch;
                s.col++;
            }
            return {
                line: s.line,
                c1: col,
                c2: s.col,
                id: name
            };
        }
    }
    forgetState() {
        super.forgetState();
        this._formStack = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
        this._formSym = null;
        this._formLen = 0;
    }
    copy() {
        let _super = super.copy();
        let _formStack = this._formStack;
        let _formSym = this._formSym;
        let _formLen = this._formLen;
        return ()=>{
            let self = _super();
            self._formStack = _formStack;
            self._formSym = _formSym;
            self._formLen = _formLen;
            return self;
        };
    }
    newArg(arg) {
        if (arg?.id && !this._formLen) this._formSym = arg;
        this._formLen++;
    }
    isForm(form) {
        var f = this._formSym && this._formSym.id;
        if (f) {
            f = f.toLowerCase();
            if (form == null) return f;
            return typeof form == "string" ? f == form : form instanceof RegExp ? form.test(f) : f in form;
        }
    }
    readCustom() {
        let s = this._stream, m;
        if (m = s.lookingAt(/^#\\.[a-z0-9_-]*/i)) {
            this.newArg();
            this.t("constant", m[0].length);
            return true;
        }
        if (s.lookingAt("#:") && this.isNameChar(s.peek(2))) {
            this.newArg();
            this.t("operator", 2);
            this.maybeName("lisp-keyword");
            return true;
        }
        if (s.lookingAt("#'") && this.isNameChar(s.peek(2))) {
            this.newArg();
            this.t("operator", 2);
            let c = this.maybeName("function-name");
            return true;
        }
        if (m = s.lookingAt(/^#\/((?:\\.|[^\/])*)\/([dgimsuvy]+)?/)) {
            this.newArg();
            this.t("regexp-starter", 2);
            this.t("regexp", m[1].length);
            this.t("regexp-stopper");
            if (m[2]) this.t("regexp-modifier", m[2].length);
            return true;
        }
        if (m = s.lookingAt(/^\?(?:\\?.)/u)) {
            // elisp char syntax
            this.newArg();
            this.t("constant", m[0].length);
            return true;
        }
        if (m = this.readName()) {
            let ch = m.id.charAt(0), type = null;
            // message to future me: good luck figuring this out.
            if (/^e?(?:type)?case$/i.test(this._formStack.cdr?.car?.id) && this._formStack.car > 2 && this._formLen == 0) type = "constant";
            else if (/^e?(?:type)?case$/i.test(this._formStack.cdr?.cdr?.cdr?.car?.id) && this._formStack.cdr?.cdr?.car > 2 && this._formStack.car == 1) type = "constant";
            else if (/^(?:flet|labels)$/i.test(this._formStack.cdr?.cdr?.cdr?.car?.id) && this._formStack.cdr?.cdr?.car == 2 && this._formLen == 0) type = "function-name";
            else if (/^(?:flet|labels)$/i.test(this._formStack.cdr?.car?.id) && this._formStack.car == 2) type = "function-name";
            else if (/^(?:tagbody|do\*?|dolist|dotimes|prog\*?|go)$/i.test(this._formSym?.id)) type = "directive";
            if (!type) type = ch == ":" ? "lisp-keyword" : ch == "&" ? "type" : $7562c6d7dc15aeff$var$ERROR_FORMS.test(m.id) ? "error" : m.id in $7562c6d7dc15aeff$var$CONSTANTS ? "constant" : this.NUMBER.test(m.id) ? "number" : null;
            if (!type && this._formStack.car == 2) {
                let pform = this._formStack.cdr?.car?.id;
                if (pform == "define" && this._formLen == 0) type = "function-name";
                else if (/^def(?!un|ine|test)/.test(pform)) type = "function-name";
            }
            if (!type) {
                if (this._formLen == 0 && $7562c6d7dc15aeff$var$SPECIAL_FORMS.test(m.id)) type = "keyword";
                else if (this._formLen == 0 && /^(?::?with(out)?[-\x2f]|def)/i.test(m.id)) type = "keyword";
                else if (this._formLen == 1 && this.isForm($7562c6d7dc15aeff$var$DEFINES_FUNCTION)) type = "function-name";
                else if (this._formLen == 1 && this.isForm($7562c6d7dc15aeff$var$DEFINES_TYPE)) type = "type";
                else if (this._formLen == 1 && this.isForm($7562c6d7dc15aeff$var$DEFINES_ERROR)) type = "error";
                else if (this._formLen == 1 && this.isForm("let")) type = "function-name";
                else if (this._formLen == 1 && this.isForm($7562c6d7dc15aeff$var$DEFINES_VARIABLE)) type = "variable-name";
                else if (this._formLen == 1 && this.isForm(/^def/)) type = "function-name";
                else if (this._formLen == 2 && this.isForm(/^def/)) type = "type";
                else if (this.isForm("loop") && $7562c6d7dc15aeff$var$LOOP_KEYWORDS.test(m.id)) type = "directive";
            }
            m.type = type;
            this.newArg(m);
            this.token(m, type);
            return true;
        }
    }
    readString(...args) {
        if (super.readString(...args)) {
            this.newArg();
            return true;
        }
    }
    readOpenParen() {
        if (super.readOpenParen()) {
            this.newArg();
            this.pushFormStack(this._formSym);
            this.pushFormStack(this._formLen);
            this._formSym = null;
            this._formLen = 0;
            return true;
        }
    }
    readCloseParen() {
        if (super.readCloseParen()) {
            if (this._backList !== (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b)) {
                this._formLen = this.popFormStack();
                this._formSym = this.popFormStack();
            }
            return true;
        }
    }
    pushFormStack(val) {
        this._formStack = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(val, this._formStack);
    }
    popFormStack() {
        let val = this._formStack.car;
        this._formStack = this._formStack.cdr;
        return val;
    }
    indentation() {
        if (this._inString || this._inComment) return super.indentation();
        let s = this._stream;
        let INDENT_LEVEL = ()=>this._stream.buffer.getq("indent_level");
        var indent = 0;
        // XXX: rewrite this mess.
        var p = this._inParens.car;
        if (p) {
            var line = s.lineText(p.line);
            indent = p.col + 1;
            if (/[\#\']/.test(line.charAt(p.col - 1))) return indent;
            var nextNonSpace;
            if (this.isNameChar(line.charAt(indent))) {
                var re = /\s\S/g;
                re.lastIndex = p.col;
                nextNonSpace = re.exec(line);
                if (nextNonSpace) indent = nextNonSpace = nextNonSpace.index + 1;
            }
            if (this._formLen) {
                var currentForm = this.isForm();
                if (currentForm) {
                    currentForm = currentForm.replace(/\*$/, "");
                    var formArgs = $7562c6d7dc15aeff$var$FORM_ARGS[currentForm];
                    if (!formArgs && /^(?:[\w_-]*::?)?(?:with|for-|foreach|do-)/u.test(currentForm)) // "with" macros usually take one argument, then &body
                    formArgs = "0*";
                    if (!formArgs && /^(?:[\w_-]*::?)?def/u.test(currentForm)) {
                        // definitions usually take two arguments, then &body
                        if (nextNonSpace && /[\(\[\{]/.test(line.charAt(nextNonSpace))) formArgs = "1*";
                        else formArgs = "2*";
                    }
                    if (!formArgs) try {
                        if (this._formStack.cdr?.car?.id == "handler-case") formArgs = "1*";
                        else if ($7562c6d7dc15aeff$var$LOCAL_BODYDEF[this._formStack.cdr.cdr.cdr.car.id] && this._formStack.cdr.cdr.car == 2) formArgs = "1*";
                    } catch (ex) {}
                    if (formArgs) {
                        var n = parseInt(formArgs, 10);
                        var hasRest = /\+/.test(formArgs);
                        var hasBody = /\*/.test(formArgs);
                        indent = p.col + INDENT_LEVEL();
                        if (hasRest && nextNonSpace) indent = nextNonSpace;
                        else if (n > 0 && this._formLen - 1 < n) {
                            if (nextNonSpace && /^(?:do\*?)$/i.test(currentForm)) indent = nextNonSpace;
                            else indent += INDENT_LEVEL();
                        }
                    } else if (/^(?:tagbody|do\*?|dolist|dotimes|prog\*?)$/i.test(currentForm)) {
                        if (/^\s*[\(\[\{]/.test(s.lineText(s.line))) indent += INDENT_LEVEL();
                    }
                }
            }
        }
        return indent;
    }
}
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("lisp", (stream, tok, options = {})=>new $7562c6d7dc15aeff$export$b112841041e97d65({
        stream: stream,
        tok: tok,
        ...options
    }));
let $7562c6d7dc15aeff$var$Ymacs_Keymap_LispMode = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("lisp", {
    '"': [
        "lisp_handle_string_quote"
    ]
});
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("lisp_mode", function() {
    var tok = this.tokenizer;
    this.setTokenizer(new (0, $6fd37b8e17f87e00$export$3964ae1c660db960)({
        buffer: this,
        type: "lisp"
    }));
    var changed_vars = this.setq({
        indent_level: 2,
        syntax_paragraph_sep: /\n(?:[ \t;]*\n)+/g,
        syntax_comment_line: {
            rx: /[^\S\r\n]*;+ ?/gu,
            ch: ";;"
        },
        syntax_word_dabbrev: /^[-0-9_*%+/@&$.=~\p{L}]$/u,
        paredit_space_before () {
            return !this.cmd("lisp_in_string") && !this.looking_back(/[\s\(\[\{,.@'`#\\]/g);
        },
        lisp_mode: true
    });
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap($7562c6d7dc15aeff$var$Ymacs_Keymap_LispMode);
    var changed_commands = this.replaceCommands({
        "forward_sexp": "lisp_forward_sexp",
        "backward_sexp": "lisp_backward_sexp",
        "backward_up_list": "lisp_backward_up_list"
    });
    return function() {
        this.setTokenizer(tok);
        this.setq(changed_vars);
        this.newCommands(changed_commands);
        if (!was_paren_match) this.cmd("paren_match_mode", false);
        this.popKeymap($7562c6d7dc15aeff$var$Ymacs_Keymap_LispMode);
    };
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT




let $547f0d0166a70f9f$var$RX_URL = /^\b((?:[a-z][\w-]+:(?:\/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}\/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))/i;
class $547f0d0166a70f9f$var$Ymacs_Lang_Markdown extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    COMMENT = [];
    STRING = [
        [
            "`",
            "`",
            null,
            null,
            "markdown-code"
        ],
        [
            "**",
            "**",
            null,
            null,
            "bold"
        ],
        [
            "__",
            "__",
            null,
            null,
            "bold"
        ],
        [
            "*",
            "*",
            null,
            null,
            "italic"
        ]
    ];
    OPEN_PAREN = {
        "(": ")",
        "{": "}",
        "[": "]",
        "\xab": "\xbb",
        "\u2770": "\u2771",
        "\u201C": "\u201D"
    };
    CLOSE_PAREN = {
        ")": "(",
        "}": "{",
        "]": "[",
        "\xbb": "\xab",
        "\u2771": "\u2770",
        "\u201D": "\u201C"
    };
    _block = null;
    copy() {
        let _super = super.copy();
        let _block = this._block;
        return ()=>{
            let self = _super();
            self._block = _block;
            return self;
        };
    }
    t(type = null, len = 1) {
        if (this._block != null) type = this._block + (type != null ? " " + type : "");
        this.token({
            line: this._stream.line,
            c1: this._stream.col,
            c2: this._stream.col += len
        }, type);
    }
    next() {
        if (this._stream.eol()) this._block = null;
        return super.next();
    }
    read() {
        this._block != "markdown-pre" && this.readInline() || this.readCustom() || this.readOpenParen() || this.readCloseParen() || this.readTrailingWhitespace() || this.t();
    }
    readInline() {
        return this.readString();
    }
    readCustom() {
        let s = this._stream, m;
        if (s.col == 0 && (m = s.lookingAt(/^#+/))) this._block = "heading" + m[0].length;
        else if (s.col == 0 && (m = s.lookingAt(/^    /))) this._block = "markdown-pre";
        else if (s.col == 0 && (m = s.lookingAt(/^>[>\s]*/))) {
            let level = m[0].replace(/\s+/g, "").length - 1;
            level = Math.min(3, level);
            this._block = "markdown-blockquote" + (level > 0 ? level : "");
        } else if (m = s.lookingAt($547f0d0166a70f9f$var$RX_URL)) {
            this.t(`hyperlink :href-${m[0]}`, m[0].length);
            return true;
        } else if (m = s.lookingAt(/^\[[a-zA-Z0-9_-]+\]/)) {
            this.t("markdown-ref", m[0].length);
            return true;
        }
    }
    indentation() {
        let s = this._stream;
        let row = s.line;
        return row > 0 && s.lineIndentation(row - 1) || s.lineIndentation(row);
    }
}
let $547f0d0166a70f9f$var$Ymacs_Keymap_Markdown = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("markdown", {
    "`": [
        "paredit_open_pair",
        "`",
        "`",
        /[\`\\]/g
    ],
    "M-`": [
        "paredit_wrap_round",
        "`",
        "`",
        /[\`\\]/g
    ]
});
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("markdown", (stream, tok)=>new $547f0d0166a70f9f$var$Ymacs_Lang_Markdown({
        stream: stream,
        tok: tok
    }));
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("markdown_mode", function() {
    var tok = this.tokenizer;
    this.setTokenizer(new (0, $6fd37b8e17f87e00$export$3964ae1c660db960)({
        buffer: this,
        type: "markdown"
    }));
    var was_paren_match = this.cmd("paren_match_mode", true);
    this.pushKeymap($547f0d0166a70f9f$var$Ymacs_Keymap_Markdown);
    return function() {
        this.setTokenizer(tok);
        if (!was_paren_match) this.cmd("paren_match_mode", false);
        this.popKeymap($547f0d0166a70f9f$var$Ymacs_Keymap_Markdown);
    };
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT






let $791728ef2d5bad94$var$Ymacs_Keymap_ParenMatch = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("parenmatch", {
    "C-M-x": "goto_matching_paren",
    "C-M-q": "indent_sexp",
    "C-M-f && C-M-n": "forward_sexp",
    "C-M-b && C-M-p": "backward_sexp",
    "C-M-u && M-a && C-M-ArrowUp": "backward_up_list",
    "C-M-a": "beginning_of_defun",
    "C-M-e": "end_of_defun",
    "M-e": "up_list",
    "C-M-ArrowDown": "down_list",
    "M-C-k": "kill_sexp",
    "M-C-Space": "mark_sexp",
    "M-C-t": "transpose_sexps",
    "(": [
        "paredit_open_pair",
        "(",
        ")"
    ],
    "[": [
        "paredit_open_pair",
        "[",
        "]"
    ],
    "{": [
        "paredit_open_pair",
        "{",
        "}"
    ],
    "\u2770": [
        "paredit_open_pair",
        "\u2770",
        "\u2771"
    ],
    "\xab": [
        "paredit_open_pair",
        "\xab",
        "\xbb"
    ],
    "\u201C": [
        "paredit_open_pair",
        "\u201C",
        "\u201D"
    ],
    '"': [
        "paredit_open_pair",
        '"',
        '"',
        /[\"\\]/g
    ],
    ")": [
        "paredit_close_pair",
        "(",
        ")"
    ],
    "]": [
        "paredit_close_pair",
        "[",
        "]"
    ],
    "}": [
        "paredit_close_pair",
        "{",
        "}"
    ],
    "\u2771": [
        "paredit_close_pair",
        "\u2770",
        "\u2771"
    ],
    "\xbb": [
        "paredit_close_pair",
        "\xab",
        "\xbb"
    ],
    "\u201D": [
        "paredit_close_pair",
        "\u201C",
        "\u201D"
    ],
    "M-(": [
        "paredit_wrap_round",
        "(",
        ")"
    ],
    "M-[": [
        "paredit_wrap_round",
        "[",
        "]"
    ],
    "M-{": [
        "paredit_wrap_round",
        "{",
        "}"
    ],
    "M-\u2770": [
        "paredit_wrap_round",
        "\u2770",
        "\u2771"
    ],
    "M-\xab": [
        "paredit_wrap_round",
        "\xab",
        "\xbb"
    ],
    "M-\u201C": [
        "paredit_wrap_round",
        "\u201C",
        "\u201D"
    ],
    'M-"': [
        "paredit_wrap_round",
        '"',
        '"',
        /[\"\\]/g
    ],
    "M-r": "paredit_raise_sexp",
    "M-s": "paredit_splice_sexp",
    "Backspace": "paredit_backward_delete_char",
    //"Delete && C-d"                : "paredit_delete_char",
    "Enter": "paredit_newline_and_indent",
    "; && : && , && .": "paredit_electric_char"
});
var $791728ef2d5bad94$var$PARENS = {
    "(": ")",
    "[": "]",
    "{": "}",
    "\u2770": "\u2771",
    "\xab": "\xbb",
    "\u201C": "\u201D",
    '"': '"',
    "'": "'",
    '`': '`'
};
var $791728ef2d5bad94$var$R_PARENS = {
    ")": "(",
    "]": "[",
    "}": "{",
    "\u2771": "\u2770",
    "\xbb": "\xab",
    "\u201D": "\u201C",
    '"': '"',
    "'": "'",
    '`': '`'
};
function $791728ef2d5bad94$var$ERROR(o) {
    throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("Balanced expression not found");
}
function $791728ef2d5bad94$var$startOf(paren) {
    return paren.c1 ?? paren.col;
}
function $791728ef2d5bad94$var$endOf(paren) {
    return paren.c2 ?? paren.col + 1;
}
function $791728ef2d5bad94$var$typeOf(paren) {
    if (paren.opened) return paren.opened.type || "";
    return paren.type || "";
}
function $791728ef2d5bad94$var$touches(paren, caret) {
    return paren.line == caret.row && $791728ef2d5bad94$var$startOf(paren) <= caret.col && caret.col <= $791728ef2d5bad94$var$endOf(paren);
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    get_paren_at_point () {
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP();
        let a = [];
        for (let op of parens){
            if (!op.type) continue;
            let cp = op.closed;
            if (!cp) continue;
            if ($791728ef2d5bad94$var$touches(op, rc)) a.push(op);
            if ($791728ef2d5bad94$var$touches(cp, rc)) a.push(cp);
        }
        if (!a.length) return null;
        if (a.length == 1) return a[0].opened || a[0];
        if (/\w/.test($791728ef2d5bad94$var$typeOf(a[0])) && !/\w/.test($791728ef2d5bad94$var$typeOf(a[1]))) return a[0].opened || a[0];
        if (/\w/.test($791728ef2d5bad94$var$typeOf(a[1])) && !/\w/.test($791728ef2d5bad94$var$typeOf(a[0]))) return a[1].opened || a[1];
        a = a.sort((0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca));
        a = a[0].opened ? a[0] : a[1];
        return a.opened || a;
    },
    matching_paren () {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.cmd("get_paren_at_point");
        if (p) {
            if ($791728ef2d5bad94$var$touches(p, rc)) return this._rowColToPosition(p.closed.line, $791728ef2d5bad94$var$endOf(p.closed));
            return this._rowColToPosition(p.line, $791728ef2d5bad94$var$startOf(p));
        }
    },
    indent_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var pos = this.cmd("matching_paren");
        if (pos != null) this.cmd("indent_region", this.point(), pos);
        else $791728ef2d5bad94$var$ERROR(this);
    }),
    goto_matching_paren: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var pos = this.cmd("matching_paren");
        if (pos != null) {
            this.cmd("goto_char", pos);
            return true;
        }
    }),
    forward_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let next;
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP();
        for(let i = 0; i < parens.length; ++i){
            let p = parens[i];
            if (p.line > rc.row || p.line == rc.row && $791728ef2d5bad94$var$startOf(p) >= rc.col) {
                next = p;
                break;
            }
        }
        this.withVariables({
            syntax_word: this.getq("syntax_word_sexp")
        }, "forward_word");
        if (next && next.closed && (0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca)(this._rowcol, next) > 0) this.cmd("goto_char", this._rowColToPosition(next.closed.line, $791728ef2d5bad94$var$endOf(next.closed)));
    }),
    backward_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let prev;
        let rc = this._rowcol;
        let parens = this.tokenizer.getPP().filter((p)=>p.closed).map((p)=>p.closed).sort((0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca));
        for(let i = parens.length; --i >= 0;){
            let p = parens[i];
            if (p.line < rc.row || p.line == rc.row && ($791728ef2d5bad94$var$startOf(p) < rc.col || p.c1 == p.c2 && $791728ef2d5bad94$var$startOf(p) == rc.col)) {
                prev = p;
                break;
            }
        }
        this.withVariables({
            syntax_word: this.getq("syntax_word_sexp")
        }, "backward_word");
        if (prev && prev.opened && (0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca)(this._rowcol, {
            line: prev.line,
            col: $791728ef2d5bad94$var$endOf(prev)
        }) < 0) this.cmd("goto_char", this._rowColToPosition(prev.opened.line, $791728ef2d5bad94$var$startOf(prev.opened)));
    }),
    mark_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^r", function(begin, end) {
        this.tokenizer.finishParsing();
        let paren = this.cmd("get_paren_at_point");
        if (paren?.outer) {
            this.cmd("goto_char", this._rowColToPosition(paren.outer.l1, paren.outer.c1));
            this.ensureTransientMark();
            this.cmd("goto_char", this._rowColToPosition(paren.outer.l2, paren.outer.c2));
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        } else this.cmd("save_excursion", function() {
            if (this.transientMarker) this.cmd("goto_char", end);
            this.ensureTransientMark();
            this.cmd("forward_sexp");
            this.setMark(this.point());
            this.transientMarker.swap(this.caretMarker);
        });
        this.ensureTransientMark();
        this.setq("sticky_mark", true);
    }),
    kill_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this._killingAction(this.point(), this.cmd("save_excursion", function() {
            this.cmd("forward_sexp");
            return this.point();
        }));
    }),
    transpose_sexps: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var a = [];
        this.cmd("forward_sexp");
        a.push(this.point());
        this.cmd("backward_sexp");
        a.push(this.point());
        this.cmd("backward_sexp");
        a.push(this.point());
        this.cmd("forward_sexp");
        a.push(this.point());
        this.cmd("goto_char", this._swapAreas(a));
    }),
    down_list: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let lc = {
            line: rc.row,
            col: rc.col
        };
        let p = this.tokenizer.getPP().filter((p)=>p.closed).find((p)=>(0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca)(p, lc) >= 0);
        if (p != null) this.cmd("goto_char", this._rowColToPosition(p.line, $791728ef2d5bad94$var$endOf(p)));
        else $791728ef2d5bad94$var$ERROR(this);
    }),
    backward_up_list: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(rc)).at(-1);
        if (p != null) this.cmd("goto_char", this._rowColToPosition(p.line, $791728ef2d5bad94$var$startOf(p)));
        else $791728ef2d5bad94$var$ERROR(this);
    }),
    up_list: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("backward_up_list");
        this.cmd("forward_sexp");
    }),
    beginning_of_defun: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(rc))[0];
        if (p != null) this.cmd("goto_char", this._rowColToPosition(p.line, $791728ef2d5bad94$var$startOf(p)));
        else this.cmd("backward_sexp");
        if (!this.getq("lisp_mode")) this.cmd("back_to_indentation");
    }),
    end_of_defun: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let rc = this._rowcol;
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(rc))[0];
        if (p != null) {
            p = p.closed;
            this.cmd("goto_char", this._rowColToPosition(p.line, $791728ef2d5bad94$var$endOf(p)));
        } else this.cmd("forward_sexp");
        if (!this.getq("lisp_mode")) this.cmd("end_of_line");
    }),
    paredit_raise_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(this._rowcol, "inner")).at(-1);
        if (!p) {
            this.signalError("No containing expression");
            return;
        }
        this.cmd("forward_sexp");
        this.cmd("backward_sexp");
        var start = this.point();
        this.cmd("forward_sexp");
        var end = this.point();
        this.cmd("backward_up_list");
        var kstart = this.point();
        this.cmd("forward_sexp");
        var kend = this.point();
        var sexp = this.cmd("buffer_substring", start, end);
        this._replaceText(kstart, kend, sexp);
        this.cmd("goto_char", kstart);
        this.cmd("indent_region", kstart, kstart + sexp.length);
    }),
    paredit_splice_sexp: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(this._rowcol, "inner")).at(-1);
        if (!p) {
            this.signalError("No containing expression");
            return;
        }
        if (p.inner && p.outer) this.cmd("save_excursion", function() {
            let outer_begin = this._rowColToPosition(p.outer.l1, p.outer.c1);
            let outer_end = this._rowColToPosition(p.outer.l2, p.outer.c2);
            let inner_begin = this._rowColToPosition(p.inner.l1, p.inner.c1);
            let inner_end = this._rowColToPosition(p.inner.l2, p.inner.c2);
            this.withMarkers((m1, m2)=>{
                this._deleteText(inner_end, outer_end);
                this._deleteText(outer_begin, inner_begin);
                this.cmd("indent_region", m1, m2);
            }, inner_begin, inner_end);
        });
        else this.cmd("save_excursion", function() {
            this.cmd("backward_up_list");
            var start = this.point();
            this.cmd("forward_sexp");
            this.cmd("backward_delete_char");
            var end = this.point();
            this.cmd("goto_char", start);
            this.cmd("delete_char");
            this.cmd("indent_region", start, end - 1);
        });
    }),
    paredit_backward_delete_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (n != null) return this.cmd("backward_delete_char", n);
        if (!this.deleteTransientRegion()) {
            if (this.cmd("looking_back", /[\(\[\{\"\❰\«\`\']/g)) {
                var close = $791728ef2d5bad94$var$PARENS[this.matchData[0]];
                if (close) {
                    var rx = new RegExp("\\s*\\" + close, "my");
                    if (this.cmd("looking_at", rx)) this.cmd("save_excursion", function() {
                        this.cmd("delete_whitespace");
                        this.cmd("delete_char");
                    });
                }
            }
            this.cmd("backward_delete_char");
        }
    }),
    paredit_delete_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^p", function(n) {
        if (n != null) return this.cmd("delete_char", n);
        if (!this.deleteTransientRegion()) {
            if (this.cmd("looking_at", /[\]\}\)\"\❱\»\`\']/y)) {
                var open = $791728ef2d5bad94$var$R_PARENS[this.matchData[0]];
                if (open) {
                    var rx = new RegExp("\\" + open + "\\s*", "mg");
                    if (this.cmd("looking_back", rx)) this.cmd("save_excursion", function() {
                        this.cmd("backward_delete_whitespace");
                        this.cmd("backward_delete_char");
                    });
                }
            }
            this.cmd("delete_char");
        }
    }),
    paredit_open_pair: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function(pair_a, pair_b, backslash) {
        if (this.transientMarker) {
            this.cmd("paredit_wrap_round", pair_a, pair_b, backslash);
            return;
        }
        if (pair_a == pair_b && this.looking_at(pair_a)) // presumably close; it's already there, just skip it
        this.cmd("forward_char");
        else {
            let maybe_space = this.getq("paredit_space_before");
            if (typeof maybe_space == "function") maybe_space = maybe_space.apply(this, arguments);
            if (maybe_space) this.cmd("insert", " ");
            this.cmd("insert", pair_a);
            this.cmd("insert", pair_b);
            this.cmd("backward_char", pair_b.length);
        }
        this.cmd("paredit_maybe_indent");
    }),
    paredit_close_pair: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function(pair_a, pair_b) {
        if (this.transientMarker) {
            this.cmd("paredit_wrap_round", pair_a, pair_b);
            return;
        }
        var re = new RegExp("\\s*\\" + pair_b, "iy");
        if (this.cmd("looking_at", re)) this._deleteText(this.point(), this.matchData.after);
        this.cmd("insert", pair_b);
        this.cmd("paredit_maybe_indent");
    }),
    paredit_wrap_round: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)("^", function(paren, closing, backslash) {
        let r = this.transientMarker ? this.getRegion() : this.cmd("save_excursion", function() {
            let begin = this.point();
            this.cmd("forward_sexp");
            return {
                begin: begin,
                end: this.point()
            };
        });
        let txt = this._bufferSubstring(r.begin, r.end);
        let before = this.point() < r.end;
        if (backslash) txt = txt.replace(backslash, (s)=>"\\" + s);
        let m = this.createMarker(r.end);
        this.cmd("save_excursion", function() {
            this._replaceText(r.begin, r.end, paren + txt + closing);
        }, before);
        if (before) this.cmd("forward_char");
        this.clearTransientMark();
        this.cmd("indent_region", r.begin, m.getPosition());
        m.destroy();
    }),
    paredit_newline_and_indent: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var inparens = this.looking_at(/[ \t]*([\]\}])/y) && this.looking_back(new RegExp("\\" + $791728ef2d5bad94$var$R_PARENS[this.matchData[1]] + "[ \\t]*", "g"));
        this.cmd("newline_and_indent");
        if (inparens) {
            this.cmd("newline_and_indent");
            this.cmd("backward_line");
            this.cmd("indent_line");
        }
    }),
    paredit_maybe_indent: function() {
        if (this.getq("electric_indent")) this.cmd("indent_line");
    },
    paredit_electric_char: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("self_insert_command");
        this.cmd("paredit_maybe_indent");
    })
});
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("paren_match_mode", function() {
    this.pushKeymap($791728ef2d5bad94$var$Ymacs_Keymap_ParenMatch);
    var clearOvl = (function() {
        this.deleteOverlay("match-paren");
    }).bind(this);
    let events = {
        beforeInteractiveCommand: function() {
            clearOvl();
        },
        afterInteractiveCommand: (0, $ca727f7f34cfa7f3$export$ad41d882ec94ba04)(()=>{
            let rc = this._rowcol;
            let hl = [];
            let p = this.cmd("get_paren_at_point");
            if (p) hl.push({
                line1: p.line,
                col1: p.c1,
                line2: p.line,
                col2: p.c2
            }, {
                line1: p.closed.line,
                col1: p.closed.c1,
                line2: p.closed.line,
                col2: p.closed.c2
            });
            this.setOverlay("match-paren", hl);
        }, 100)
    };
    this.addEventListener(events);
    if (this.getq("electric_indent") == null) this.setq("electric_indent", true);
    return function() {
        clearOvl();
        this.popKeymap($791728ef2d5bad94$var$Ymacs_Keymap_ParenMatch);
        this.removeEventListener(events);
    };
});


/// This file is part of Ymacs - www.ymacs.org
/// Copyright (c) 2009-2024 Mihai Bazon <mihai.bazon@gmail.com>
/// License: MIT







const $65f4282842cf92e9$var$RX_EMPTY_TAG = /^(?:area|base|br|col|embed|hr|img|input|link|meta|param|source|track|wbr)$/i;
const $65f4282842cf92e9$var$RX_BLOCK_TAG = /^(?:article|aside|blockquote|body|button|caption|col|dd|div|dl|dt|fieldset|figcaption|figure|footer|form|h[1-6]|header|hgroup|li|ol|p|pre|section|table|tbody|textarea|tfoot|th|thead|td|tr|ul)$/i;
let $65f4282842cf92e9$var$Ymacs_Keymap_XML = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("xml", {
    "C-c /": "xml_close_tag",
    "/": "xml_slash_complete_tag",
    ">": "xml_gt_complete_tag",
    "%": "twig_insert_percent",
    "M--": "twig_block_toggle_whitespace",
    "C-Enter": "xml_zen_expand",
    "Enter": "xml_newline_and_indent"
});
let $65f4282842cf92e9$var$markup_mode = (tok_type)=>function() {
        var tok = this.tokenizer;
        this.setTokenizer(new (0, $6fd37b8e17f87e00$export$3964ae1c660db960)({
            buffer: this,
            type: tok_type
        }));
        var was_paren_match = this.cmd("paren_match_mode", true);
        this.pushKeymap($65f4282842cf92e9$var$Ymacs_Keymap_XML);
        var changed_vars = this.setq({
            indent_level: 2,
            syntax_comment_multi: {
                rx: /[^\S\r\n]*<!--+[^\S\r\n]*(.*?)[^\S\r\n]*-->/ygu,
                ch: [
                    "<!--",
                    "-->"
                ]
            },
            syntax_word_dabbrev: /^[\p{N}_$\p{L}:#-]$/u,
            syntax_word_sexp: /^[\p{N}_$\p{L}:#-]$/u
        });
        return function() {
            if (!was_paren_match) this.cmd("paren_match_mode", false);
            this.popKeymap($65f4282842cf92e9$var$Ymacs_Keymap_XML);
            this.setq(changed_vars);
            this.setTokenizer(tok);
        };
    };
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("xml_mode", $65f4282842cf92e9$var$markup_mode("xml"));
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("html_mode", $65f4282842cf92e9$var$markup_mode("html"));
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newMode("twig_html_mode", $65f4282842cf92e9$var$markup_mode("twig_html"));
class $65f4282842cf92e9$var$Ymacs_Lang_XML extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    _tags = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _inTag = null;
    _inline = 0;
    COMMENT = [
        [
            "<!--",
            "-->",
            "-"
        ],
        [
            "<![CDATA[",
            "]]>",
            null,
            "xml-cdata-starter",
            "xml-cdata",
            "xml-cdata-stopper"
        ]
    ];
    NAME = /^[\p{L}:_$-][\p{L}0-9:_$-]*/iu;
    OPEN_PAREN = {
        "(": ")",
        "{": "}",
        "[": "]",
        "\xab": "\xbb",
        "\u2770": "\u2771",
        "\u201C": "\u201D"
    };
    CLOSE_PAREN = {
        ")": "(",
        "}": "{",
        "]": "[",
        "\xbb": "\xab",
        "\u2771": "\u2770",
        "\u201D": "\u201C"
    };
    constructor({ stream: stream, tok: tok, emptyTags: emptyTags, inline: inline }){
        super({
            stream: stream,
            tok: tok
        });
        this.emptyTags = emptyTags;
        this.inline = inline;
    }
    copy() {
        let _super = super.copy();
        let _tags = this._tags;
        let _inTag = this._inTag;
        let _inline = this._inline;
        return ()=>{
            let self = _super();
            self._tags = _tags;
            self._inTag = _inTag;
            self._inline = _inline;
            return self;
        };
    }
    read() {
        this.readComment() || this.readDeclaration() || this.readCloseTag() || this.readOpenTag() || this.readOpenParen() || this.readCloseParen() || this.readTrailingWhitespace() || this.t(null, 1, this._stream.peek() != " ");
    }
    t(type = null, len = 1, addInline) {
        if (addInline && this.inline) {
            let cls = this.inline.cls(this._inline);
            if (cls != null) type = cls + (type != null ? " " + type : "");
        }
        this.token({
            line: this._stream.line,
            c1: this._stream.col,
            c2: this._stream.col += len
        }, type);
    }
    readDeclaration() {
        let s = this._stream, m = s.lookingAt(/^<[?!]/);
        if (m) {
            let sp = m[0];
            this.pushInParen(sp, "xml-open-bracket");
            this.pushCont(()=>{
                let m = s.lookingAt(/^\??>/);
                if (m) {
                    this.popInParen(sp, m[0].length, "xml-close-bracket");
                    this.popCont();
                } else this.readString() || this.readTrailingWhitespace() || this.t("directive");
            });
            return true;
        }
    }
    readOpenTag() {
        let s = this._stream;
        if (s.lookingAt("<") && this.NAME.test(s.peek(1))) {
            let outer = {
                l1: s.line,
                c1: s.col
            };
            this.pushInParen("<", "xml-open-bracket");
            let tag = this.maybeName("xml-open-tag");
            tag.outer = outer;
            this._inTag = tag;
            this.pushCont(this.contOpenTag);
            return true;
        }
    }
    contOpenTag() {
        let s = this._stream;
        let ch = s.peek(), name;
        if (s.lookingAt("/>")) {
            this.popInParen("<", 2, "xml-close-bracket");
            this.popCont();
            this._inTag = null;
        } else if (ch === ">") {
            this.popInParen("<", 1, "xml-close-bracket");
            this.popCont();
            if (!this.emptyTags?.test(this._inTag.id)) {
                this._inTag.inner = {
                    l1: s.line,
                    c1: s.col
                };
                this.pushTag(this._inTag);
                if (this.inline) this._inline ^= this.inline.code(this._inTag.id);
            }
            this._inTag = null;
        } else if (name = this.maybeName("xml-attribute")) ;
        else if (ch === '"' || ch === "'") this.readString();
        else this.readTrailingWhitespace() || this.t();
    }
    readCloseTag() {
        let s = this._stream;
        if (s.lookingAt("</") && this.NAME.test(s.peek(2))) {
            let otag = this.popTag();
            if (otag) {
                otag.inner.l2 = s.line;
                otag.inner.c2 = s.col;
                if (this.inline) this._inline ^= this.inline.code(otag.id);
            }
            this.pushInParen("</", "xml-open-bracket");
            let ctag = this.readName();
            this.token(ctag, otag?.id == ctag.id ? "xml-close-tag" : "error");
            if (otag) {
                let popen = {
                    line: otag.line,
                    c1: otag.c1,
                    c2: otag.c2,
                    type: otag.id,
                    inner: otag.inner,
                    outer: otag.outer
                };
                let pclose = {
                    line: ctag.line,
                    c1: ctag.c1,
                    c2: ctag.c2,
                    opened: popen
                };
                popen.closed = pclose;
                this.doneParen(popen);
            }
            this.pushCont(this.contCloseTag.bind(this, otag));
            return true;
        }
    }
    contCloseTag(otag) {
        let s = this._stream, m = s.lookingAt(/^([\s\xA0]*)(>?)/);
        if (m && m[0]) {
            if (m[1]) this.t(null, m[1].length);
            if (m[2]) {
                this.popInParen("</", 1, "xml-close-bracket");
                this.popCont();
                if (otag) {
                    otag.outer.l2 = s.line;
                    otag.outer.c2 = s.col;
                }
            }
        } else this.readTrailingWhitespace() || this.t("error");
    }
    indentation() {
        let s = this._stream;
        var indent, lastTag;
        let INDENT_LEVEL = ()=>this._stream.buffer.getq("indent_level");
        if (this._inComment) indent = s.lineIndentation(this._inComment.line) + INDENT_LEVEL();
        else if (this._inTag) {
            var txt = s.lineText(this._inTag.line);
            if (/^\s*$/.test(txt.substr(0, this._inTag.c1 - 1))) indent = this._inTag.c1 + this._inTag.id.length + 1;
            else indent = s.lineIndentation(this._inTag.line);
        } else if (lastTag = this._tags.car) {
            indent = s.lineIndentation(lastTag.line);
            if (!/^\s*<\x2f/.test(s.lineText())) indent += INDENT_LEVEL();
        }
        if (indent == null) {
            let line = s.line;
            while(line > 0 && !/\S/.test(s.lineText(line)))line--;
            indent = s.lineIndentation(line);
        }
        return indent;
    }
    pushTag(tag) {
        this._tags = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(tag, this._tags);
    }
    popTag() {
        let tag = this._tags.car;
        this._tags = this._tags.cdr;
        return tag;
    }
    get tag() {
        return this._tags.car;
    }
}
class $65f4282842cf92e9$var$Ymacs_Lang_HTML extends $65f4282842cf92e9$var$Ymacs_Lang_XML {
    _mode = this;
    _js = this._tok.getLanguage("js");
    _css = this._tok.getLanguage("css");
    get passedParens() {
        return [
            ...super.passedParens,
            ...this._js.passedParens,
            ...this._css.passedParens
        ];
    }
    next() {
        let s = this._stream;
        if (this._mode === this && this.tag) {
            if (this.tag.id == "script") this._mode = this._js;
            if (this.tag.id == "style") this._mode = this._css;
        }
        if (this._mode === this._js && s.lookingAt("</script")) this._mode = this;
        if (this._mode === this._css && s.lookingAt("</style")) this._mode = this;
        this._mode === this ? super.next() : this._mode.next();
    }
    copy() {
        let _super = super.copy();
        let _js = this._js.copy();
        let _css = this._css.copy();
        let _mode = this._mode;
        return ()=>{
            let self = _super();
            self._js = _js();
            self._css = _css();
            self._mode = _mode;
            return self;
        };
    }
    indentation() {
        let s = this._stream;
        let INDENT_LEVEL = ()=>this._stream.buffer.getq("indent_level");
        if (this._mode === this || !this._mode._inString && /^\s*<\//.test(s.lineText())) {
            let indent = super.indentation();
            let tag = this._tags.car;
            if (tag) {
                let txt = s.lineText(tag.outer.l1).substr(0, tag.outer.c1);
                if (/\S/.test(txt)) // there is text before the innermost tag, let's assume
                // it's inline text and back one level
                indent -= INDENT_LEVEL();
            }
            return indent > 0 ? indent : 0;
        } else {
            let tag = this.tag;
            if (tag) {
                let line = s.line;
                while(--line > tag.line && !/\S/.test(s.lineText(line)));
                if (line == tag.line) return s.lineIndentation(line) + this._stream.buffer.getq("indent_level");
            }
            return this._mode.indentation();
        }
    }
}
const $65f4282842cf92e9$var$TWIG_BUILTIN = (0, $ca727f7f34cfa7f3$export$da43f679ba2e9167)(`in or and not is defined constant divisible empty
even iterable null true false odd same from as starts with only ends matches`);
class $65f4282842cf92e9$var$Ymacs_Lang_Twig extends (0, $0813b912056c2fc5$export$96ac030efd09f62d) {
    STRING = [
        "'",
        [
            '"',
            '"',
            "#{",
            "}"
        ]
    ];
    _blocks = (0, $ca727f7f34cfa7f3$export$d5414a779ebfba6b);
    _inBlock = null;
    _alt = this._tok.getLanguage("html");
    _mode = this._alt;
    get passedParens() {
        return [
            ...super.passedParens,
            ...this._alt.passedParens
        ];
    }
    copy() {
        let _super = super.copy();
        let _alt = this._alt.copy();
        let _mode = this._mode;
        let _blocks = this._blocks;
        let _inBlock = this._inBlock;
        return ()=>{
            let self = _super();
            self._alt = _alt();
            self._mode = _mode;
            self._blocks = _blocks;
            self._inBlock = _inBlock;
            return self;
        };
    }
    next() {
        let s = this._stream, m;
        if (this._mode === this._alt) {
            if (m = s.lookingAt(/^\{%-?/)) {
                this._mode = this;
                this.pushCont(this.readBlock.bind(this, m[0]));
            } else if (m = s.lookingAt(/^\{\{-?/)) {
                this._mode = this;
                this.pushInParen(m[0], "exp-starter");
                return;
            } else if (m = s.lookingAt(/^\{#-?/)) {
                this._alt.readCommentMulti(m[0], /^-?#\}/, "#");
                return this.next();
            }
        } else if ((m = s.lookingAt(/^-?\}\}/)) && /^\{\{/.test(this._inParens.car?.type)) {
            this.popInParen(this._inParens.car?.type, m[0].length, "exp-stopper");
            this._mode = this._alt;
            return this.next();
        }
        this._mode === this ? super.next() : this._alt.next();
    }
    readCustom() {
        let s = this._stream;
        let filter = /\|\s*$/.test(s.textBefore());
        let tok = this.readName();
        if (tok) {
            this.skipWS();
            tok.type = filter || s.lookingAt("(") ? "function-name" : tok.id in $65f4282842cf92e9$var$TWIG_BUILTIN ? "builtin" : null;
            this.token(tok);
            return true;
        }
        return this.readNumber();
    }
    readBlock(start) {
        let s = this._stream, m;
        if (this._inBlock) {
            this.skipWS();
            if (m = s.lookingAt(/^-?%\}/)) {
                this.popInParen(start, m[0].length, "block-stopper");
                this._mode = this._alt;
                this.popCont();
                if (this._inBlock.hasBody) {
                    this._inBlock.inner = {
                        l1: s.line,
                        c1: s.col
                    };
                    this.pushBlock(this._inBlock);
                } else if (this._inBlock.outer) {
                    this._inBlock.outer.l2 = s.line;
                    this._inBlock.outer.c2 = s.col;
                }
                this._inBlock = null;
            } else this.read();
        } else {
            let outer = {
                l1: s.line,
                c1: s.col
            };
            this.pushInParen(start, "block-starter");
            this.skipWS();
            let ctag = this.readName();
            if (ctag) {
                let isEndTag = /^end/.test(ctag.id);
                if (isEndTag) {
                    let otag = this.popBlock();
                    let name = ctag.id.substr(3);
                    this.token(ctag, otag?.id == name ? "keyword" : "error");
                    if (otag) {
                        if (otag.inner) {
                            otag.inner.l2 = outer.l1;
                            otag.inner.c2 = outer.c1;
                        }
                        let popen = {
                            line: otag.line,
                            c1: otag.c1,
                            c2: otag.c2,
                            type: otag.id,
                            inner: otag.inner,
                            outer: otag.outer
                        };
                        let pclose = {
                            line: ctag.line,
                            c1: ctag.c1,
                            c2: ctag.c2,
                            opened: popen
                        };
                        popen.closed = pclose;
                        this.doneParen(popen);
                        this._inBlock = otag; // waiting for %}
                        delete otag.hasBody; // no more body to parse
                    }
                    this.skipWS();
                    if (/^(?:macro|block)$/.test(name)) {
                        let fname = this.readName();
                        if (fname) this.token(fname, fname.id == otag?.fname?.id ? "function-name" : "error");
                        this.skipWS();
                    }
                } else {
                    this.token(ctag, "keyword");
                    ctag.outer = outer;
                    this._inBlock = ctag;
                    this.littleParseTag(ctag);
                }
            } else if (m = s.lookingAt(/^-?%\}/)) {
                // incomplete input but auto-completed end paren
                this.popInParen(start, m[0].length, "error");
                this._mode = this._alt;
                this.popCont();
            } else {
                this.t("error");
                this._inBlock = {
                    hasBody: false
                };
            }
        }
    }
    littleParseTag(ctag) {
        let s = this._stream;
        ctag.hasBody = !/^(import|do|from|include|extends|use|else(?:if)?)$/.test(ctag.id);
        this.skipWS();
        if (ctag.id == "set") {
            while(this.maybeName("variable-name")){
                this.skipWS();
                if (s.lookingAt(",")) {
                    s.col++;
                    this.skipWS();
                } else break;
            }
            this.skipWS();
            ctag.hasBody = s.lookingAt(/^-?%\}/);
        } else if (ctag.id == "block" || ctag.id == "macro") ctag.fname = this.maybeName("function-name");
    }
    indentation() {
        let s = this._stream;
        let INDENT_LEVEL = ()=>this._stream.buffer.getq("indent_level");
        if (this._mode === this) return super.indentation();
        else if (this.block) {
            if (this._mode._inTag) return this._mode.indentation();
            let indent = s.lineIndentation(this.block.line);
            let closing = /^\s*\{%-?\s*(?:end|else)/.test(s.lineText());
            if (closing) return indent;
            if (!this._mode.tag || this._mode.tag.line <= this.block.line) return indent + INDENT_LEVEL();
        }
        return this._mode.indentation();
    }
    pushBlock(block) {
        this._blocks = new (0, $ca727f7f34cfa7f3$export$74c44e616647f5c0)(block, this._blocks);
    }
    popBlock() {
        let block = this._blocks.car;
        this._blocks = this._blocks.cdr;
        return block;
    }
    get block() {
        return this._blocks.car;
    }
    get tag() {
        return this._alt.tag;
    }
}
function $65f4282842cf92e9$var$inlineCode(tag) {
    return tag == "i" || tag == "em" ? 1 : tag == "b" || tag == "strong" ? 2 : tag == "a" ? 4 : tag == "h1" ? 8 : tag == "h2" ? 16 : tag == "h3" ? 32 : tag == "h4" ? 64 : 0;
}
function $65f4282842cf92e9$var$inlineCls(inline) {
    let out = "";
    if (inline & 1) out += " italic";
    if (inline & 2) out += " bold";
    if (inline & 4) out += " link";
    if (inline & 8) out += " heading1";
    if (inline & 16) out += " heading2";
    if (inline & 32) out += " heading3";
    if (inline & 64) out += " heading4";
    return out.trim() || null;
}
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("xml", (stream, tok)=>new $65f4282842cf92e9$var$Ymacs_Lang_XML({
        stream: stream,
        tok: tok
    }));
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("html", (stream, tok)=>new $65f4282842cf92e9$var$Ymacs_Lang_HTML({
        stream: stream,
        tok: tok,
        emptyTags: $65f4282842cf92e9$var$RX_EMPTY_TAG,
        inline: {
            code: $65f4282842cf92e9$var$inlineCode,
            cls: $65f4282842cf92e9$var$inlineCls
        }
    }));
(0, $6fd37b8e17f87e00$export$3964ae1c660db960).define("twig_html", (stream, tok)=>new $65f4282842cf92e9$var$Ymacs_Lang_Twig({
        stream: stream,
        tok: tok
    }));
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    xml_limit_fill_paragraph_region: function() {
        // XXX: this function should somehow be a property of the language (for mixed modes),
        // rather than a top-level method. Not yet decided how to best handle this...
        if (!this.tokenizer) return;
        this.tokenizer.finishParsing();
        let blk = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(this._rowcol, "outer")).findLast((p)=>$65f4282842cf92e9$var$RX_BLOCK_TAG.test(p.type));
        if (blk) {
            let r = {
                begin: this._rowColToPosition(blk.inner.l1, blk.inner.c1),
                end: this._rowColToPosition(blk.inner.l2, blk.inner.c2)
            };
            this.cmd("save_excursion", ()=>{
                this.cmd("goto_char", r.begin);
                if (this.cmd("looking_at", /[^\S\r\n]*\n/gy)) {
                    this.cmd("goto_char", this.matchData.after);
                    r.begin = this.point();
                }
                this.cmd("goto_char", r.end);
                if (this.cmd("looking_back", /\n[^\S\r\n]*/g)) {
                    this.cmd("backward_whitespace");
                    r.end = this.point();
                }
            });
            return r;
        }
    },
    xml_close_tag: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let rc = this._rowcol;
        this.tokenizer.parseUntil(rc.row, rc.col);
        let tag = this.tokenizer.theParser.tag; // XML
        let block = this.tokenizer.theParser.block; // Twig
        let closeTwig = ()=>{
            this.cmd("insert", `{% end${block.id} %}`);
            this.cmd("indent_line");
        };
        let closeXML = ()=>{
            this.cmd("insert", `</${tag.id}>`);
            this.cmd("indent_line");
        };
        if (!tag && !block) return;
        if (!tag && block) return closeTwig();
        if (!block && tag) return closeXML();
        if ((0, $6fd37b8e17f87e00$export$2b0c642afb0ae4ca)(block, tag) < 0) return closeXML();
        closeTwig();
    }),
    xml_slash_complete_tag: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("self_insert_command");
        if (this.looking_back("</")) {
            let rc = this._rowcol;
            this.tokenizer.parseUntil(rc.row, rc.col);
            let tag = this.tokenizer.theParser.tag;
            if (tag) {
                this._placeUndoBoundary();
                this.cmd("insert", tag.id, ">");
                this.cmd("indent_line");
            }
        }
    }),
    xml_gt_complete_tag: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("self_insert_command");
        let rc = this._rowcol;
        this.tokenizer.parseUntil(rc.row, rc.col);
        let tag = this.tokenizer.theParser.tag;
        if (tag?.inner?.l1 == rc.row && tag.inner.c1 == rc.col) {
            this._placeUndoBoundary();
            let pos = this.point();
            this.cmd("insert", "</", tag.id, ">");
            this.cmd("goto_char", pos);
        }
    }),
    xml_newline_and_indent: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let inparens = this.looking_at("</") && this.looking_back(">");
        this.cmd("newline_and_indent");
        if (inparens) {
            this.cmd("newline_and_indent");
            this.cmd("backward_line");
            this.cmd("indent_line");
        }
    }),
    twig_insert_percent: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("self_insert_command");
        let rc = this._rowcol;
        this.tokenizer.parseUntil(rc.row, rc.col);
        let mode = this.tokenizer.theParser._mode;
        if (mode instanceof $65f4282842cf92e9$var$Ymacs_Lang_Twig && this.looking_back("{%")) {
            this._placeUndoBoundary();
            this.cmd("insert", "  %");
            this.cmd("backward_char", 2);
        }
    }),
    twig_block_toggle_whitespace: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.tokenizer.finishParsing();
        let p = this.tokenizer.getPP().filter((0, $6fd37b8e17f87e00$export$f6a9bcac20db9551)(this._rowcol)).filter((x)=>/^\{[{%#]/.test(x.type)).at(-1);
        if (p?.closed) {
            let doFront = ()=>{
                if (this.looking_back("-")) this.cmd("backward_delete_char");
                else this.cmd("insert", "-");
            };
            let doBack = ()=>{
                if (this.looking_at("-")) this.cmd("delete_char");
                else this.cmd("insert", "-");
            };
            this.cmd("save_excursion", ()=>{
                this.cmd("goto_char", this._rowColToPosition(p.closed.line, p.closed.c1));
                doBack();
                this.cmd("goto_char", this._rowColToPosition(p.line, p.c2));
                doFront();
            });
        }
    })
});
/*------------- zen mode -------------*/ // not sure "zen mode" is of any interest these days, but let's leave it for now.
let $65f4282842cf92e9$var$Ymacs_Keymap_XML_Zen = (0, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2).define("xml_zen", {
    "Tab": "xml_zen_next_poi",
    "S-Tab": "xml_zen_prev_poi",
    "C-g": "xml_zen_stop"
});
let $65f4282842cf92e9$var$MODE_TYPE = 1, $65f4282842cf92e9$var$MODE_CLASS = 2, $65f4282842cf92e9$var$MODE_ID = 3, $65f4282842cf92e9$var$MODE_REPEAT = 4, $65f4282842cf92e9$var$MODE_ATTR = 5;
function $65f4282842cf92e9$var$zen_render(el, html) {
    var n = el.repeat || 1;
    for(var i = 1; i <= n; ++i){
        if (i > 1) html("\n");
        html("<", el.type);
        if (el.id) html(' id="', el.id.replace(/\$/g, i), '"');
        if (el.klass) html(' class="', el.klass.replace(/\$/g, i), '"');
        if (el.attributes) el.attributes.forEach((attr)=>html(" ", attr, '="|"'));
        html(">");
        if (el.child) {
            html("\n");
            $65f4282842cf92e9$var$zen_render(el.child, html);
            html("\n");
        } else html("|");
        html("</", el.type, ">");
        if (el.next) {
            html("\n");
            $65f4282842cf92e9$var$zen_render(el.next, html);
        }
    }
}
function $65f4282842cf92e9$var$zen_parse(str, i) {
    var el = {
        type: ""
    }, mode = $65f4282842cf92e9$var$MODE_TYPE;
    OUTER: while(i < str.length){
        var ch = str.charAt(i++);
        switch(ch){
            case "#":
                mode = $65f4282842cf92e9$var$MODE_ID;
                el.id = "";
                break;
            case ".":
                mode = $65f4282842cf92e9$var$MODE_CLASS;
                if (el.klass != null) el.klass += " ";
                else el.klass = "";
                break;
            case ":":
                mode = $65f4282842cf92e9$var$MODE_ATTR;
                if (el.attributes == null) el.attributes = [];
                el.attributes.push("");
                break;
            case "*":
                mode = $65f4282842cf92e9$var$MODE_REPEAT;
                el.repeat = "";
                break;
            case ">":
                el.child = $65f4282842cf92e9$var$zen_parse(str, i);
                i = el.child.i;
                break OUTER;
            case "(":
                el.child = $65f4282842cf92e9$var$zen_parse(str, i);
                i = el.child.i;
                break;
            case ")":
                break OUTER;
            case "+":
                el.next = $65f4282842cf92e9$var$zen_parse(str, i);
                i = el.next.i;
                break OUTER;
            default:
                switch(mode){
                    case $65f4282842cf92e9$var$MODE_TYPE:
                        el.type += ch;
                        break;
                    case $65f4282842cf92e9$var$MODE_CLASS:
                        el.klass += ch;
                        break;
                    case $65f4282842cf92e9$var$MODE_ID:
                        el.id += ch;
                        break;
                    case $65f4282842cf92e9$var$MODE_REPEAT:
                        el.repeat = parseInt(String(el.repeat) + ch, 10);
                        break;
                    case $65f4282842cf92e9$var$MODE_ATTR:
                        el.attributes.push(el.attributes.pop() + ch);
                        break;
                }
        }
    }
    el.i = i;
    return el;
}
function $65f4282842cf92e9$var$maybe_stop_zen() {
    var point = this.point(), a = this.getq("xml_zen_markers"), start = a[0], end = a.at(-1);
    if (point < start.getPosition() || point > end.getPosition() || end.getPosition() == a.at(-2).getPosition()) this.cmd("xml_zen_stop");
}
(0, $a49159f89f9c9e7a$export$df331bdfc76955b4).newCommands({
    xml_zen_expand: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        this.cmd("xml_zen_stop");
        var html = "";
        var start = this.cmd("save_excursion", function() {
            this.cmd("backward_whitespace");
            while(!this.cmd("looking_back", /[\x20\xa0\s\t\n;&]/g))if (!this.cmd("backward_char")) break;
            return this.point();
        });
        var point = this.point();
        try {
            $65f4282842cf92e9$var$zen_render($65f4282842cf92e9$var$zen_parse(this.cmd("buffer_substring", start, point).trim(), 0), (...args)=>html += args.join(""));
        } catch (ex) {
            throw new (0, $b8f5514dd71ab3c2$export$c411e7bd03572a3a)("The Zen is not strong today :-/");
        }
        this.cmd("delete_region", start, point);
        this.cmd("insert", html);
        start = this.createMarker(start, false, "xml_zen");
        // locate points of interest
        var end = this.createMarker(this.point(), true, "xml_zen"), markers = [];
        this.cmd("goto_char", start.getPosition());
        while(this.cmd("search_forward", "|", end.getPosition())){
            this.cmd("backward_delete_char");
            markers.push(this.createMarker(this.point(), true, "xml_zen_start"));
            markers.push(this.createMarker(this.point(), false, "xml_zen_end"));
        }
        this.cmd("indent_region", start.getPosition(), end.getPosition());
        var count = markers.length;
        if (count > 0) {
            // move to first POI
            this.cmd("goto_char", markers[0]);
            markers.unshift(start);
            markers.push(end);
            this.setq("xml_zen_markers", markers);
            this.pushKeymap($65f4282842cf92e9$var$Ymacs_Keymap_XML_Zen);
            this.addEventListener("afterInteractiveCommand", $65f4282842cf92e9$var$maybe_stop_zen);
        } else {
            start.destroy();
            end.destroy();
        }
    }),
    xml_zen_stop: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        var tmp = this.getq("xml_zen_markers");
        if (tmp) {
            tmp.map((m)=>m.destroy());
            this.setq("xml_zen_markers", null);
        }
        this.popKeymap($65f4282842cf92e9$var$Ymacs_Keymap_XML_Zen);
        this.removeEventListener("afterInteractiveCommand", $65f4282842cf92e9$var$maybe_stop_zen);
    }),
    xml_zen_next_poi: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let markers = this.getq("xml_zen_markers"), pos = this.point();
        for(let i = 0; i < markers.length; ++i){
            let m = markers[i];
            if (m.getPosition() > pos) {
                this.cmd("goto_char", m.getPosition());
                break;
            }
        }
    }),
    xml_zen_prev_poi: (0, $9e418a0e990aea25$export$1c2a1b3d78098030)(function() {
        let markers = this.getq("xml_zen_markers"), pos = this.point();
        for(let i = markers.length; --i >= 0;){
            let m = markers[i];
            if (m.getPosition() < pos) {
                this.cmd("goto_char", m.getPosition());
                break;
            }
        }
    })
});








export {$bd0ea8a7a4006f2d$export$e2cc12c8ba83ca65 as Ymacs, $a49159f89f9c9e7a$export$df331bdfc76955b4 as Ymacs_Buffer, $43a6e7b7a98f117c$export$6b6e17e3540cb3d2 as Ymacs_Keymap, $3dba2f389b913e3b$export$e144e07a20daa9d as Ymacs_Keymap_Emacs, $6fd37b8e17f87e00$export$3964ae1c660db960 as Ymacs_Tokenizer, $9e418a0e990aea25$export$1c2a1b3d78098030 as Ymacs_Interactive, $b8f5514dd71ab3c2$export$c411e7bd03572a3a as Ymacs_Exception, $7562c6d7dc15aeff$export$b112841041e97d65 as Ymacs_Lang_Lisp};
//# sourceMappingURL=ymacs.mjs.map
