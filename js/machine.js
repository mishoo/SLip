import { LispCons } from "./list.js";
import { LispType, LispSymbol, LispPackage, LispHash, LispProcess, LispMutex, LispStream, LispInputStream, LispOutputStream, LispChar, LispClosure, LispPrimitiveError, LispObject } from "./types.js";
import { repeat_string, pad_string } from "./utils.js";
import { LispStack } from "./stack.js";

let BASE_PACK = LispPackage.get("%");
let KEYWORD_PACK = LispPackage.get("KEYWORD");
let S_QUOTE = LispSymbol.get("QUOTE");
let S_NIL = LispSymbol.get("NIL");
let S_T = LispSymbol.get("T");
let S_ALLOW_OTHER_KEYS = KEYWORD_PACK.intern("ALLOW-OTHER-KEYS");

const OP = {
    NOP: 0,
    LVAR: 1,
    LSET: 2,
    GVAR: 3,
    GSET: 4,
    BIND: 5,
    FGVAR: 6,
    FGSET: 7,
    POP: 8,
    CONST: 9,
    JUMP: 10,
    TJUMP: 11,
    FJUMP: 12,
    BLOCK: 13,
    LJUMP: 14,
    LRET: 15,
    NOT: 16,
    SETCC: 17,
    SAVE: 18,
    RET: 19,
    CALL: 20,
    UPOPEN: 21,
    UPEXIT: 22,
    UPCLOSE: 23,
    CATCH: 24,
    THROW: 25,
    LET: 26,
    ARGS: 27,
    ARG_: 28,
    FRAME: 29,
    VAR: 30,
    VARS: 31,
    UNFR: 32,
    FN: 33,
    PRIM: 34,
    NIL: 35,
    T: 36,
    CONS: 37,
    LIST: 38,
    LIST_: 39,
    CC: 40,
    CAR: 41,
    CDR: 42,
    CAAR: 43,
    CADR: 44,
    CDAR: 45,
    CDDR: 46,
    CAAAR: 47,
    CAADR: 48,
    CADAR: 49,
    CADDR: 50,
    CDAAR: 51,
    CDADR: 52,
    CDDAR: 53,
    CDDDR: 54,
    CAAAAR: 55,
    CAAADR: 56,
    CAADAR: 57,
    CAADDR: 58,
    CADAAR: 59,
    CADADR: 60,
    CADDAR: 61,
    CADDDR: 62,
    CDAAAR: 63,
    CDAADR: 64,
    CDADAR: 65,
    CDADDR: 66,
    CDDAAR: 67,
    CDDADR: 68,
    CDDDAR: 69,
    CDDDDR: 70,
    BLOCK2: 71,
    LRET2: 72,
    LJUMP2: 73,
    XARGS: 74,
    POPLIST: 75,
    EQ: 76,
    POPGLIST: 77,
    TJUMPK: 78,
    FJUMPK: 79,
    VALUES: 80,
    MVB: 81,
    POPBACK: 82,
};

const OP_LEN = [
    0 /* NOP */,
    2 /* LVAR */,
    2 /* LSET */,
    1 /* GVAR */,
    1 /* GSET */,
    2 /* BIND */,
    1 /* FGVAR */,
    1 /* FGSET */,
    0 /* POP */,
    1 /* CONST */,
    1 /* JUMP */,
    1 /* TJUMP */,
    1 /* FJUMP */,
    0 /* BLOCK */,
    1 /* LJUMP */,
    1 /* LRET */,
    0 /* NOT */,
    0 /* SETCC */,
    1 /* SAVE */,
    0 /* RET */,
    1 /* CALL */,
    1 /* UPOPEN */,
    0 /* UPEXIT */,
    0 /* UPCLOSE */,
    1 /* CATCH */,
    0 /* THROW */,
    1 /* LET */,
    1 /* ARGS */,
    1 /* ARG_ */,
    0 /* FRAME */,
    0 /* VAR */,
    1 /* VARS */,
    2 /* UNFR */,
    2 /* FN */,
    2 /* PRIM */,
    0 /* NIL */,
    0 /* T */,
    0 /* CONS */,
    1 /* LIST */,
    1 /* LIST_ */,
    0 /* CC */,
    0 /* CAR */,
    0 /* CDR */,
    0 /* CAAR */,
    0 /* CADR */,
    0 /* CDAR */,
    0 /* CDDR */,
    0 /* CAAAR */,
    0 /* CAADR */,
    0 /* CADAR */,
    0 /* CADDR */,
    0 /* CDAAR */,
    0 /* CDADR */,
    0 /* CDDAR */,
    0 /* CDDDR */,
    0 /* CAAAAR */,
    0 /* CAAADR */,
    0 /* CAADAR */,
    0 /* CAADDR */,
    0 /* CADAAR */,
    0 /* CADADR */,
    0 /* CADDAR */,
    0 /* CADDDR */,
    0 /* CDAAAR */,
    0 /* CDAADR */,
    0 /* CDADAR */,
    0 /* CDADDR */,
    0 /* CDDAAR */,
    0 /* CDDADR */,
    0 /* CDDDAR */,
    0 /* CDDDDR */,
    1 /* BLOCK2 */,
    1 /* LRET2 */,
    2 /* LJUMP2 */,
    5 /* XARGS */,
    2 /* POPLIST */,
    0 /* EQ */,
    1 /* POPGLIST */,
    1 /* TJUMPK */,
    1 /* FJUMPK */,
    1 /* VALUES */,
    1 /* MVB */,
    1 /* POPBACK */,
];

// normal RET context
class LispRet {
    constructor(m, pc) {
        this.f = m.f;
        this.code = m.code;
        this.pc = pc;
        this.env = m.env;
        this.denv = m.denv;
        this.n_args = m.n_args;
        //if (m.trace) this.trace = m.trace.slice();
    }
    run(m) {
        m.f = this.f;
        m.code = this.code;
        m.pc = this.pc;
        m.env = this.env;
        m.denv = this.denv;
        m.n_args = this.n_args;
        //if (this.trace) m.trace = this.trace;
    }
}

class LispCleanup {
    constructor(ret, addr) {
        this.ret = ret;
        this.addr = addr;
    }
    run(m) {
        this.ret.unwind(m, this.addr);
    }
}

// return context for TAGBODY and BLOCK
class LispLongRet {
    static #NO_RET = {};
    constructor(m, exit) {
        this.f = m.f;
        this.code = m.code;
        this.env = m.env;
        this.denv = m.denv;
        this.slen = m.stack.sp;
        this.n_args = m.n_args;
        this.exit = exit;
        //if (m.trace) this.trace = m.trace.slice();
    }
    unwind(m, addr = this.exit) {
        m.f = this.f;
        m.code = this.code;
        m.env = this.env;
        m.denv = this.denv;
        m.stack.sp = this.slen;
        m.pc = addr;
        m.n_args = this.n_args;
        //if (this.trace) m.trace = this.trace;
    }
    run(m, addr = this.exit, val = LispLongRet.#NO_RET) {
        // figure out if we need to execute cleanup hooks
        let doit;
        (doit = () => {
            var p = m.denv;
            while (p && p !== this.denv) {
                var c = p.car;
                if (c instanceof LispCleanup) {
                    m.after_cleanup = doit;
                    c.run(m);
                    return;
                }
                p = p.cdr;
            }
            this.unwind(m, addr);
            if (val !== LispLongRet.#NO_RET) m.push(val);
        })();
    }
}

// continuations
class LispCC {
    constructor(m) {
        this.stack = m.stack.copy();
        this.denv = m.denv;
        //if (m.trace) this.trace = m.trace.slice();
    }
    run(m) {
        m.stack.restore(this.stack);
        m.denv = this.denv;
        //if (this.trace) m.trace = this.trace.slice();
    }
}

// dynamic bindings
class LispBinding {
    constructor(symbol, value) {
        this.symbol = symbol;
        this.value = value;
    }
}

class LispCatch {
    constructor(m, addr, tag) {
        this.ret = new LispLongRet(m);
        this.addr = addr;
        this.tag = tag;
    }
    run(m, retval) {
        this.ret.run(m, this.addr, retval);
    }
}

var optimize = (function(){
    function find_target(code, label) {
        return code.indexOf(label);
    };
    function used_label(code, label) {
        for (var i = code.length; --i >= 0;) {
            var el = code[i];
            if (!(el instanceof LispSymbol)) {
                if (el[1] === label)
                    return true;
                if (el[0] === "FN" && used_label(el[1], label))
                    return true;
            }
        }
    };
    function optimize1(code, i, pass) {
        var el = code[i];
        if (el instanceof LispSymbol) {
            if (!used_label(code, el)) {
                code.splice(i, 1);
                return true;
            }
            return false;
        }
        if (i+2 < code.length && (code[i][0] == "TJUMPK" || code[i][0] == "FJUMPK")
            && code[i+1][0] == "NIL"
            && code[i+2] === code[i][1])
        {
            // [[TF]JUMPK L1] [NIL] L1 -> (nothing)
            code.splice(i, 2);
            return true;
        }
        if (i+2 < code.length && code[i][0] == "TJUMP" && code[i+1][0] == "JUMP" && code[i+2] === code[i][1]) {
            // [TJUMP L1] [JUMP L2] L1 -> [FJUMP L2] L1
            code.splice(i, 2, [ "FJUMP", code[i+1][1] ]);
            return true;
        }
        if (i+2 < code.length && code[i][0] == "TJUMPK" && code[i+1][0] == "JUMP" && code[i+2] === code[i][1]) {
            // [TJUMPK L1] [JUMP L2] L1 -> [FJUMPK L2] L1
            code.splice(i, 2, [ "FJUMPK", code[i+1][1] ]);
            return true;
        }
        if (i+2 < code.length && code[i][0] == "FJUMP" && code[i+1][0] == "JUMP" && code[i+2] === code[i][1]) {
            // [FJUMP L1] [JUMP L2] L1 -> [TJUMP L2] L1
            code.splice(i, 2, [ "TJUMP", code[i+1][1] ]);
            return true;
        }
        if (i+2 < code.length && code[i][0] == "FJUMPK" && code[i+1][0] == "JUMP" && code[i+2] === code[i][1]) {
            // [FJUMPK L1] [JUMP L2] L1 -> [TJUMPK L2] L1
            code.splice(i, 2, [ "TJUMPK", code[i+1][1] ]);
            return true;
        }
        switch (el[0]) {
          case "VARS":
            if (el[1] == 1) {
                code.splice(i, 1, [ "VAR" ]);
                return true;
            }
            break;
          case "JUMP":
          case "TJUMP":
          case "FJUMP":
            for (var j = i + 1; j < code.length && code[j] instanceof LispSymbol; ++j) {
                if (el[1] === code[j]) {
                    if (el[0] == "JUMP") code.splice(i, 1);
                    else code.splice(i, 1, [ "POP" ]);
                    return true;
                }
            }
            break;
          case "LVAR":
            if (i+1 < code.length && code[i+1][0] == "POP") {
                code.splice(i, 2);
                return true;
            }
            if (i+8 < code.length &&
                code[i+1][0] == "LET" && code[i+1][1] == 1 &&
                code[i+2][0] == "LVAR" && code[i+2][1] == 0 && code[i+2][2] == 0 &&
                code[i+3][0] == "CDR" &&
                code[i+4][0] == "LSET" && code[i+4][1] == el[1] + 1 && code[i+4][2] == el[2] &&
                code[i+5][0] == "POP" &&
                code[i+6][0] == "LVAR" && code[i+6][1] == 0 && code[i+6][2] == 0 &&
                code[i+7][0] == "CAR" &&
                code[i+8][0] == "UNFR" && code[i+8][1] == 1 && code[i+8][2] == 0)
            {
                el[0] = "POPLIST";
                code.splice(i + 1, 8);
                return true;
            }
            break;
          case "GVAR":
            if (i+1 < code.length && code[i+1][0] == "POP") {
                code.splice(i, 2);
                return true;
            }
            if (i+8 < code.length &&
                code[i+1][0] == "LET" && code[i+1][1] == 1 &&
                code[i+2][0] == "LVAR" && code[i+2][1] == 0 && code[i+2][2] == 0 &&
                code[i+3][0] == "CDR" &&
                code[i+4][0] == "GSET" && code[i+4][1] == el[1] &&
                code[i+5][0] == "POP" &&
                code[i+6][0] == "LVAR" && code[i+6][1] == 0 && code[i+6][2] == 0 &&
                code[i+7][0] == "CAR" &&
                code[i+8][0] == "UNFR" && code[i+8][1] == 1 && code[i+8][2] == 0)
            {
                el[0] = "POPGLIST";
                code.splice(i + 1, 8);
                return true;
            }
            break;
          case "PRIM":
            if (el[1].pak === BASE_PACK) {
                if (/^C[AD]{1,4}R$/.test(el[1].name) && el[2] == 1) {
                    code.splice(i, 1, [ el[1].name ]);
                    return true;
                }
                switch (el[1].name) {
                  case "CONS":
                    if (el[2] == 2) {
                        code.splice(i, 1, [ "CONS" ]);
                        return true;
                    }
                    break;
                  case "LIST":
                    code.splice(i, 1, [ "LIST", el[2] ]);
                    return true;
                  case "LIST*":
                    code.splice(i, 1, [ "LIST_", el[2] ]);
                    return true;
                  case "EQ":
                  case "EQL":
                    if (el[2] == 2) {
                        code.splice(i, 1, [ "EQ" ]);
                        return true;
                    }
                    break;
                  case "NULL":
                    if (el[2] == 1) {
                        code.splice(i, 1, [ "NOT" ]);
                        return true;
                    }
                    break;
                }
            }
            break;
        }
        switch (el[0]) {
          case "GSET":
          case "GVAR":
            if (i+2 < code.length &&
                code[i+1][0] == "POP" &&
                code[i+2][0] == "GVAR" &&
                code[i+2][1] == el[1]) {
                code.splice(i + 1, 2);
                return true;
            }
            if (pass == 2 &&
                i+2 < code.length &&
                code[i+1][0] == "FJUMP" &&
                code[i+2][0] == "GVAR" &&
                code[i+2][1] == el[1])
            {
                code[i+1][0] = "FJUMPK";
                code.splice(i + 2, 1);
                return true;
            }
            break;
          case "LSET":
          case "LVAR":
            if (i+2 < code.length &&
                code[i+1][0] == "POP" &&
                code[i+2][0] == "LVAR" &&
                code[i+2][1] == el[1] &&
                code[i+2][2] == el[2]) {
                code.splice(i + 1, 2);
                return true;
            }
            if (pass == 2 &&
                i+2 < code.length &&
                code[i+1][0] == "FJUMP" &&
                code[i+2][0] == "LVAR" &&
                code[i+2][1] == el[1] &&
                code[i+2][2] == el[2])
            {
                code[i+1][0] = "FJUMPK";
                code.splice(i + 2, 1);
                return true;
            }
            break;
          case "SAVE":
          case "FJUMP":
          case "TJUMP":
          case "FJUMPK":
          case "TJUMPK":
            // SAVE L1; ... L1: JUMP L2 --> SAVE L2
            var idx = find_target(code, el[1]);
            if (idx >= 0 && idx < code.length - 1 && code[idx + 1][0] == "JUMP") {
                el[1] = code[idx + 1][1];
                return true;
            }
            break;
          case "JUMP":
            var idx = find_target(code, el[1]);
            if (idx >= 0 && idx < code.length - 1 &&
                (code[idx + 1][0] == "JUMP" || code[idx + 1][0] == "RET")) {
                code[i] = code[idx + 1];
                return true;
            }
            break;
          case "CALL":
          case "RET":
          case "LRET":
          case "LRET2":
            for (var j = i; ++j < code.length;) {
                if (code[j] instanceof LispSymbol) {
                    break;
                }
            }
            if (j - i - 1 > 0) {
                code.splice(i + 1, j - i - 1);
                return true;
            }
            if (el[0] == "RET" && i+2 < code.length && code[i+1] instanceof LispSymbol && code[i+2][0] == "RET") {
                // RET; L1; RET --> L1; RET
                code.splice(i, 3, code[i+1], el);
                return true;
            }
            break;
          case "UNFR":
            if (i+1 < code.length) {
                if (code[i+1][0] == "UNFR") {
                    code[i][1] += code[i+1][1];
                    code[i][2] += code[i+1][2];
                    code.splice(i + 1, 1);
                    return true;
                }
                if ([ "RET", "LRET", "LRET2", "LJUMP", "LJUMP2" ].includes(code[i+1][0])) {
                    code.splice(i, 1);
                    return true;
                }
            }
            break;
        }
        if (i+1 < code.length) {
            if ((el[0] == "CONST" && el[1] === null) || el[0] == "NIL") {
                switch (code[i+1][0]) {
                  case "FJUMP":
                  case "FJUMPK":
                    code.splice(i, 2, [ "JUMP", code[i+1][1] ]);
                    return true;
                  case "TJUMP":
                  case "TJUMPK":
                    code.splice(i, 2);
                    return true;
                  case "NOT":
                    code.splice(i, 2, [ "T" ]);
                    return true;
                }
                if (el[0] == "CONST" && el[1] === null) {
                    code.splice(i, 1, [ "NIL" ]);
                    return true;
                }
            }
            if ((el[0] == "CONST" && constantp(el[1])) || el[0] == "T") {
                switch (code[i+1][0]) {
                  case "FJUMP":
                    code.splice(i, 2);
                    return true;
                  case "FJUMPK":
                    code.splice(i + 1, 1);
                    return true;
                  case "TJUMP":
                    code.splice(i, 2, [ "JUMP", code[i+1][1] ]);
                    return true;
                  case "TJUMPK":
                    code.splice(i + 1, 1, [ "JUMP", code[i+1][1] ]);
                    return true;
                  case "NOT":
                    code.splice(i, 2, [ "NIL" ]);
                    return true;
                }
                if (el[0] == "CONST" && el[1] === true) {
                    code.splice(i, 1, [ "T" ]);
                    return true;
                }
            }
        }
        switch (el[0]) {
          case "NIL":
            if (i+1 < code.length) {
                if (code[i+1][0] == "CONS") {
                    code.splice(i, 2, [ "LIST", 1 ]);
                    return true;
                }
            }
            break;
          case "LIST":
          case "LIST_":
            if (i+1 < code.length) {
                if (code[i+1][0] == "CONS") {
                    code.splice(i, 2, [ el[0], el[1] + 1 ]);
                    return true;
                }
                if (code[i+1][0] == "LIST_") {
                    code.splice(i, 2, [ el[0], el[1] + code[i+1][1] - 1 ]);
                    return true;
                }
            }
            break;
          case "CONS":
            if (i < code.length - 1) {
                if (code[i+1][0] == "CONS") {
                    code.splice(i, 2, [ "LIST_", 3 ]);
                    return true;
                }
            }
            break;
        }
    };
    return function optimize(code) {
        while (true) {
            var changed = false;
            for (var i = 0; i < code.length; ++i)
                if (optimize1(code, i, 1)) --i, changed = true;
            if (!changed) break;
        }
        while (true) {
            var changed = false;
            for (var i = 0; i < code.length; ++i)
                if (optimize1(code, i, 2)) --i, changed = true;
            if (!changed) break;
        }
    };
})();

function constantp(x) {
    return x === true
        || x === null
        || typeof x == "number"
        || typeof x == "string"
        || x instanceof RegExp
        || x instanceof LispChar
        || x instanceof LispSymbol
        || x instanceof LispHash
        || Array.isArray(x);
}

function is_jump_instruction(op) {
    switch (op) {
      case OP.JUMP:
      case OP.TJUMP:
      case OP.FJUMP:
      case OP.LRET:
      case OP.LJUMP:
      case OP.UPOPEN:
      case OP.SAVE:
      case OP.CATCH:
      case OP.BLOCK2:
      case OP.LJUMP2:
      case OP.TJUMPK:
      case OP.FJUMPK:
        return true;
    }
}

function assemble(code) {
    optimize(code);
    let ret = [];
    for (let i = 0; i < code.length; ++i) {
        let el = code[i];
        if (el instanceof LispSymbol) {
            el.value = ret.length;
        } else {
            let op = OP[el[0]];
            let args = el.slice(1);
            if (args.length != OP_LEN[op]) {
                console.log("FUCKED UP");
                debugger;
            }
            ret.push(op, ...args);
        }
    }
    for (let i = 0; i < ret.length;) {
        let op = ret[i++];
        switch (op) {
          case OP.FN:
            ret[i] = assemble(ret[i]);
            break;
          default:
            if (is_jump_instruction(op))
                ret[i] = ret[i].value;
        }
        i += OP_LEN[op];
    }
    return ret;
}

function relocate(code, offset) {
    for (let i = 0; i < code.length;) {
        let op = code[i++];
        if (is_jump_instruction(op))
            code[i] += offset;
        i += OP_LEN[op];
    }
    return code;
}

let INDENT_LEVEL = 8;

function indent(level) {
    return repeat_string(' ', level * INDENT_LEVEL);
}

function dump(thing) {
    if (thing === null) return "NIL";
    if (thing === true) return "T";
    if (typeof thing == "string") return JSON.stringify(LispChar.sanitize(thing));
    if (thing instanceof LispCons) {
        if (LispCons.car(thing) === S_QUOTE && LispCons.len(thing) == 2)
            return "'" + dump(LispCons.cadr(thing));
        var ret = "(", first = true;
        while (thing !== null) {
            if (!first) ret += " ";
            else first = false;
            ret += dump(LispCons.car(thing));
            thing = LispCons.cdr(thing);
            if (!LispCons.isList(thing)) {
                ret += " . " + dump(thing);
                break;
            }
        }
        return ret + ")";
    }
    if (Array.isArray(thing)) return `#(${thing.map(dump).join(" ")})`;
    if (thing instanceof LispType) return thing.print();
    return thing + "";
}

const OP_REV = Object.keys(OP);

export function disassemble(code) {
    let lab = 0;
    function disassemble(code, level) {
        let labels = Object.create(null);
        for (let i = 0; i < code.length;) {
            let op = code[i++];
            if (is_jump_instruction(op)) {
                let addr = code[i];
                if (!labels[addr]) {
                    labels[addr] = "L" + (++lab);
                }
            }
            i += OP_LEN[op];
        }
        let output = "";
        for (let i = 0; i < code.length;) {
            let l = labels[i] || "";
            let op = code[i++];
            if (l) l += ":";
            let data;
            let opcode = OP_REV[op];
            switch (op) {
              case OP.FN:
                opcode = "FN " + code[i + 1];
                data = "\n" + disassemble(code[i], level + 1);
                break;
              case OP.PRIM:
                data = code[i] + " " + code[i + 1];
                break;
              case OP.CONST:
                data = dump(code[i]);
                break;
              default:
                if (is_jump_instruction(op)) {
                    data = labels[code[i]];
                    break;
                }
                data = code.slice(i, i + OP_LEN[op]).map(el =>
                    pad_string(dump(el), 8)).join("");
            }
            var line = pad_string(l, INDENT_LEVEL)
                + indent(level)
                + pad_string(opcode, INDENT_LEVEL)
                + data;
            if (output) output += "\n";
            output += line;
            i += OP_LEN[op];
        }
        return output;
    };
    return disassemble(code, 0);
}

function serialize_const(val, cache) {
    return function dump(val) {
        if (val === null || val === true) return val + "";
        if (val instanceof LispSymbol || val instanceof LispPackage || val instanceof LispChar) return val.serialize(cache);
        if (val instanceof RegExp) return val.toString();
        if (val instanceof LispCons) return "l(" + LispCons.toArray(val).map(dump).join(",") + ")";
        if (val instanceof Array) return "[" + val.map(dump).join(",") + "]";
        if (typeof val == "string") return LispChar.sanitize(JSON.stringify(val));
        if (val + "" == "[object Object]") {
            console.error("Unsupported value in bytecode serialization", val);
            error("Unsupported value in bytecode serialization");
        }
        return val + "";
    }(val);
}

function serialize(code, strip, cache) {
    code = code.map(x => serialize_const(x, cache)).join(",");
    return strip ? code : "[" + code + "]";
}

export function unserialize(code) {
    var names = [], values = [], cache = [];
    names.push("s"); values.push(function(name, pak){
        if (arguments.length == 1 && typeof name == "number") {
            return cache[name];
        }
        let sym;
        if (pak !== null) {
            pak = pak instanceof LispPackage ? pak : LispPackage.get(pak);
            sym = LispSymbol.get(name, pak);
        } else {
            sym = new LispSymbol(name);
        }
        cache.push(sym);
        return sym;
    });
    names.push("p"); values.push(function(name){
        if (arguments.length == 1 && typeof name == "number") {
            return cache[name];
        }
        let pak = LispPackage.get(name);
        cache.push(pak);
        return pak;
    });
    names.push("l"); values.push(function(...args){
        return LispCons.fromArray(args);
    });
    names.push("c"); values.push(function(char){
        return LispChar.get(char);
    });
    names.push("DOT"); values.push(LispCons.DOT);
    var func = new Function("return function(" + names.join(",") + "){return [" + code + "]}")();
    code = func.apply(null, values);
    return code;
}

export class LispMachine {

    static XREF = {};            // store per-file cross-reference information
    static assemble = assemble;
    static constantp = constantp;
    static relocate = relocate;
    static disassemble = disassemble;
    static serialize = serialize;
    static unserialize = unserialize;
    static serialize_const = serialize_const;
    static dump = dump;

    constructor(pm) {
        this.code = null;
        this.pc = null;
        this.stack = null;
        this.env = null;
        this.denv = pm ? pm.denv : null;
        this.n_args = null;
        this.status = null;
        this.error = null;
        this.process = null;
        this.f = null;
        this.after_cleanup = null;
        //this.trace = [];
    }

    find_dvar(symbol) {
        if (symbol.special()) {
            var p = this.denv;
            while (p !== null) {
                var el = p.car;
                if (el instanceof LispBinding && el.symbol === symbol)
                    return el;
                p = p.cdr;
            }
        }
        return symbol;
    }

    gvar(symbol) {
        return this.find_dvar(symbol).value;
    }

    gset(symbol, val) {
        this.find_dvar(symbol).value = val;
    }

    dynpush(thing) {
        this.denv = new LispCons(thing, this.denv);
    }

    bind(symbol, i) {
        let frame = this.env.car;
        this.dynpush(new LispBinding(symbol, frame[i]));
        frame[i] = null;
    }

    push(v) {
        this.stack.push(v);
    }

    pop() {
        return this.stack.pop();
    }

    pop_frame(n) {
        return this.stack.pop_frame(n);
    }

    pop_number(error) {
        var n = this.pop();
        if (typeof n == "number") return n;
        return error("Number expected, got " + dump(n), n);
    }

    mkret(pc) {
        return new LispRet(this, pc);
    }

    mkcont() {
        return new LispCC(this);
    }

    top() {
        return this.stack.top();
    }

    loop() {
        while (this.pc < this.code.length) {
            vmrun(this);
            if (this.pc === null) break;
        }
        return this.stack.sp > 0 ? this.pop() : null;
    }

    atomic_call(closure, args) {
        if (!args) args = [];
        // stop the world, call closure, resume the world
        var save_code = this.code;
        var save_env = this.env;
        var save_denv = this.denv;
        var save_stack = this.stack;
        var save_nargs = this.n_args;
        var save_pc = this.pc;
        var save_f = this.f;
        //var save_trace = this.trace;
        this.code = closure.code;
        this.env = closure.env;
        this.stack = new LispStack().restore([ new LispRet(this, null) ].concat(args));
        this.n_args = args.length;
        this.pc = 0;
        this.f = closure;
        //if (this.trace) this.trace = [ closure, args ];
        try {
            return this.loop();
        } finally {
            //this.trace = save_trace;
            this.f = save_f;
            this.pc = save_pc;
            this.n_args = save_nargs;
            this.stack = save_stack;
            this.denv = save_denv;
            this.env = save_env;
            this.code = save_code;
        }
    }

    _exec(code) {
        this.code = code;
        this.env = null;
        this.stack = new LispStack();
        this.pc = 0;
        this.f = null;
        return this.loop();
    }

    _call(closure, args) {
        args = LispCons.toArray(args);
        this.stack = new LispStack().restore([ new LispRet(this, null) ].concat(args));
        this.code = closure.code;
        this.env = closure.env;
        this.n_args = args.length;
        this.pc = 0;
        this.f = closure;
        while (true) {
            if (this.pc === null) return this.pop();
            vmrun(this);
        }
    }

    _callnext(closure, args) {
        //if (this.trace) this.trace.push([ closure, LispCons.toArray(args) ]);
        if (args !== false) {
            this.push(this.mkret(this.pc));
            let n = 0;
            while (args !== null) {
                this.push(args.car);
                args = args.cdr;
                n++;
            }
            this.n_args = n;
        }
        this.code = closure.code;
        this.env = closure.env;
        this.pc = 0;
        this.f = closure;
        return false;
    }

    set_closure(closure, ...args) {
        this.stack = new LispStack().restore([ new LispRet(this, null) ].concat(args));
        this.code = closure.code;
        this.env = closure.env;
        this.n_args = args.length;
        this.pc = 0;
        this.f = closure;
        //if (this.trace) this.trace = [ closure, args ];
    }

    run(quota) {
        var err = null;
        try {
            while (quota-- > 0) {
                if (this.pc === null) {
                    this.status = "finished";
                    break;
                }
                vmrun(this);
                if (this.status != "running")
                    break;
            }
        } catch(ex) {
            if (ex instanceof LispPrimitiveError) {
                var pe = LispSymbol.get("PRIMITIVE-ERROR", LispPackage.get("SL"));
                if (pe && pe.function) {
                    // RETHROW as Lisp error.
                    this._callnext(pe.function, LispCons.fromArray([ "~A", ex.message ]));
                    return null;
                }
            }
            // we fucked up.
            this.status = "halted";
            err = this.error = ex;
        }
        return err;
    }

    dump(expr) {
        return dump(expr);
    }

    error(...args) {
        error(...args);
    }

}

function frame(env, i) {
    while (i-- > 0) env = env.cdr;
    return env.car;
};

function rewind(env, i) {
    while (i-- > 0) env = env.cdr;
    return env;
};

function eq(a, b) {
    if (a === null) return b === null || b === S_NIL ? true : null;
    if (b === null) return a === null || a === S_NIL ? true : null;
    if (a === true) return b === true || b === S_T ? true : null;
    if (b === true) return a === true || a === S_T ? true : null;
    return a === b ? true : null;
};

let CC_CODE = assemble([
    ["ARGS", 1],
    ["LVAR", 1, 0],
    ["SETCC"],
    ["LVAR", 0, 0],
    ["RET"]
]);

function error(msg) {
    throw new LispPrimitiveError(msg);
}

function find_key_arg(item, array, start, end) {
    for (let i = start; i < end; i += 2) {
        if (eq(item, array[i])) return i;
    }
    return null;
}

let OP_RUN = [
    null,
    /*OP.LVAR*/ (m) => {
        let i = m.code[m.pc++];
        let j = m.code[m.pc++];
        m.push(frame(m.env, i)[j]);
    },
    /*OP.LSET*/ (m) => {
        let i = m.code[m.pc++];
        let j = m.code[m.pc++];
        frame(m.env, i)[j] = m.top();
    },
    /*OP.GVAR*/ (m) => {
        let name = m.code[m.pc++];
        m.push(m.gvar(name));
    },
    /*OP.GSET*/ (m) => {
        let name = m.code[m.pc++];
        m.gset(name, m.top());
    },
    /*OP.BIND*/ (m) => {
        let name = m.code[m.pc++];
        let i = m.code[m.pc++];
        m.bind(name, i);
    },
    /*OP.FGVAR*/ (m) => {
        let name = m.code[m.pc++];
        let f = name.function;
        if (!f) {
            //console.error("Undefined function", name);
            error(`Undefined function ${dump(name)}`);
        }
        m.push(f);
    },
    /*OP.FGSET*/ (m) => {
        let name = m.code[m.pc++];
        name.function = m.top();
    },
    /*OP.POP*/ (m) => {
        m.pop();
    },
    /*OP.CONST*/ (m) => {
        let val = m.code[m.pc++];
        m.push(val);
    },
    /*OP.JUMP*/ (m) => {
        let addr = m.code[m.pc++];
        m.pc = addr;
    },
    /*OP.TJUMP*/ (m) => {
        let addr = m.code[m.pc++];
        if (m.pop() !== null) m.pc = addr;
    },
    /*OP.FJUMP*/ (m) => {
        let addr = m.code[m.pc++];
        if (m.pop() === null) m.pc = addr;
    },
    /*OP.BLOCK*/ (m) => {
        // this is moderately tricky: we can't do
        //   m.env = new LispCons([ new LispLongRet(m) ], m.env);
        // I'll let you figure out why.
        let frame = [];
        m.env = new LispCons(frame, m.env);
        frame[0] = new LispLongRet(m);
    },
    /*OP.LJUMP*/ (m) => {
        let addr = m.code[m.pc++];
        m.pop().run(m, addr);
    },
    /*OP.LRET*/ (m) => {
        let noval = m.f.noval;
        let addr = m.code[m.pc++];
        let bret = m.pop(), val = m.pop();
        bret.run(m, addr);
        if (!noval) m.push(val);
    },
    /*OP.NOT*/ (m) => {
        m.push(m.pop() === null ? true : null);
    },
    /*OP.SETCC*/ (m) => {
        m.stack.top().run(m);
    },
    /*OP.SAVE*/ (m) => {
        let addr = m.code[m.pc++];
        m.push(m.mkret(addr));
    },
    /*OP.RET*/ (m) => {
        let noval = m.f.noval;
        // using m.stack directly, rather than m.pop, since we'd like
        // to keep multiple values around for the caller.
        let retval = m.stack.pop();
        let moreval = m.stack.values[m.stack.sp];
        m.stack.pop().run(m);
        if (!noval) {
            m.stack.push(retval);
            m.stack.values[m.stack.sp-1] = moreval;
        }
    },
    /*OP.CALL*/ (m) => {
        let count = m.code[m.pc++];
        let closure = m.pop();
        //if (m.trace) m.trace.push([ closure, m.stack.slice(-count) ]);
        m.n_args = count;
        m.code = closure.code;
        m.env = closure.env;
        m.pc = 0;
        m.f = closure;
    },
    /*OP.UPOPEN*/ (m) => {
        let addr = m.code[m.pc++];
        let c = new LispCleanup(new LispLongRet(m), addr);
        m.dynpush(c);
    },
    /*OP.UPEXIT*/ (m) => {
        // no need to run it, we're already in
        // the right place.  just discard.
        m.denv = m.denv.cdr;
        m.after_cleanup = null;
    },
    /*OP.UPCLOSE*/ (m) => {
        if (m.after_cleanup) m.after_cleanup(m);
    },
    /*OP.CATCH*/ (m) => {
        let addr = m.code[m.pc++];
        let c = new LispCatch(m, addr, m.pop());
        m.dynpush(c);
    },
    /*OP.THROW*/ (m) => {
        let val = m.pop();
        let tag = m.pop();
        let p = m.denv;
        while (p) {
            let el = p.car;
            if (el instanceof LispCatch && eq(el.tag, tag)) {
                el.run(m, val);
                return;
            }
            p = p.cdr;
        }
        error("CATCH tag not found " + dump(tag));
    },
    /*OP.LET*/ (m) => {
        let count = m.code[m.pc++];
        m.env = new LispCons(m.pop_frame(count), m.env);
    },
    /*OP.ARGS*/ (m) => {
        let count = m.code[m.pc++];
        if (count != m.n_args) {
            console.error(m.f);
            error("Wrong number of arguments - expecting " + count + ", got " + m.n_args);
        }
        if (count) m.env = new LispCons(m.pop_frame(count), m.env);
    },
    /*OP.ARG_*/ (m) => {
        let count = m.code[m.pc++];
        let passed = m.n_args;
        if (passed < count) {
            console.error(m.f);
            error("Insufficient number of arguments");
        }
        let p = null;
        while (passed-- > count) p = new LispCons(m.pop(), p);
        let frame = m.pop_frame(count);
        frame.push(p);
        m.env = new LispCons(frame, m.env);
    },
    /*OP.FRAME*/ (m) => {
        m.env = new LispCons([], m.env);
    },
    /*OP.VAR*/ (m) => {
        m.env.car.push(m.pop());
    },
    /*OP.VARS*/ (m) => {
        let count = m.code[m.pc++];
        let a = m.env.car, n = a.length;
        while (--count >= 0) a[n + count] = m.pop();
    },
    /*OP.UNFR*/ (m) => {
        let lex = m.code[m.pc++];
        let spec = m.code[m.pc++];
        if (lex) m.env = rewind(m.env, lex);
        if (spec) m.denv = rewind(m.denv, spec);
    },
    /*OP.FN*/ (m) => {
        let code = m.code[m.pc++];
        let name = m.code[m.pc++];
        m.push(new LispClosure(code, name, m.env));
    },
    /*OP.PRIM*/ (m) => {
        let name = m.code[m.pc++];
        let nargs = m.code[m.pc++];
        if (nargs == -1) nargs = m.n_args;
        let ret = name.primitive(m, nargs);
        if (ret !== false) m.push(ret ?? null);
    },
    /*OP.NIL*/ (m) => {
        m.push(null);
    },
    /*OP.T*/ (m) => {
        m.push(true);
    },
    /*OP.CONS*/ (m) => {
        let b = m.pop(), a = m.pop();
        m.push(new LispCons(a, b));
    },
    /*OP.LIST*/ (m) => {
        let count = m.code[m.pc++];
        let p = null, n = count;
        if (n == -1) n = m.n_args;
        while (n-- > 0) p = new LispCons(m.pop(), p);
        m.push(p);
    },
    /*OP.LIST_*/ (m) => {
        let count = m.code[m.pc++];
        let p = m.pop(), n = count;
        if (n == -1) n = m.n_args;
        while (--n > 0) p = new LispCons(m.pop(), p);
        m.push(p);
    },
    /*OP.CC*/ (m) => {
        m.push(new LispClosure(CC_CODE, null, new LispCons([m.mkcont()])));
    },
    /*OP.CAR*/ (m) => {
        m.push(LispCons.car(m.pop()));
    },
    /*OP.CDR*/ (m) => {
        m.push(LispCons.cdr(m.pop()));
    },
    /*OP.CAAR*/ (m) => {
        m.push(LispCons.caar(m.pop()));
    },
    /*OP.CADR*/ (m) => {
        m.push(LispCons.cadr(m.pop()));
    },
    /*OP.CDAR*/ (m) => {
        m.push(LispCons.cdar(m.pop()));
    },
    /*OP.CDDR*/ (m) => {
        m.push(LispCons.cddr(m.pop()));
    },
    /*OP.CAAAR*/ (m) => {
        m.push(LispCons.caaar(m.pop()));
    },
    /*OP.CAADR*/ (m) => {
        m.push(LispCons.caadr(m.pop()));
    },
    /*OP.CADAR*/ (m) => {
        m.push(LispCons.cadar(m.pop()));
    },
    /*OP.CADDR*/ (m) => {
        m.push(LispCons.caddr(m.pop()));
    },
    /*OP.CDAAR*/ (m) => {
        m.push(LispCons.cdaar(m.pop()));
    },
    /*OP.CDADR*/ (m) => {
        m.push(LispCons.cdadr(m.pop()));
    },
    /*OP.CDDAR*/ (m) => {
        m.push(LispCons.cddar(m.pop()));
    },
    /*OP.CDDDR*/ (m) => {
        m.push(LispCons.cdddr(m.pop()));
    },
    /*OP.CAAAAR*/ (m) => {
        m.push(LispCons.caaaar(m.pop()));
    },
    /*OP.CAAADR*/ (m) => {
        m.push(LispCons.caaadr(m.pop()));
    },
    /*OP.CAADAR*/ (m) => {
        m.push(LispCons.caadar(m.pop()));
    },
    /*OP.CAADDR*/ (m) => {
        m.push(LispCons.caaddr(m.pop()));
    },
    /*OP.CADAAR*/ (m) => {
        m.push(LispCons.cadaar(m.pop()));
    },
    /*OP.CADADR*/ (m) => {
        m.push(LispCons.cadadr(m.pop()));
    },
    /*OP.CADDAR*/ (m) => {
        m.push(LispCons.caddar(m.pop()));
    },
    /*OP.CADDDR*/ (m) => {
        m.push(LispCons.cadddr(m.pop()));
    },
    /*OP.CDAAAR*/ (m) => {
        m.push(LispCons.cdaaar(m.pop()));
    },
    /*OP.CDAADR*/ (m) => {
        m.push(LispCons.cdaadr(m.pop()));
    },
    /*OP.CDADAR*/ (m) => {
        m.push(LispCons.cdadar(m.pop()));
    },
    /*OP.CDADDR*/ (m) => {
        m.push(LispCons.cdaddr(m.pop()));
    },
    /*OP.CDDAAR*/ (m) => {
        m.push(LispCons.cddaar(m.pop()));
    },
    /*OP.CDDADR*/ (m) => {
        m.push(LispCons.cddadr(m.pop()));
    },
    /*OP.CDDDAR*/ (m) => {
        m.push(LispCons.cdddar(m.pop()));
    },
    /*OP.CDDDDR*/ (m) => {
        m.push(LispCons.cddddr(m.pop()));
    },
    /*OP.BLOCK2*/ (m) => {
        let exit = m.code[m.pc++];
        let frame = [];
        m.env = new LispCons(frame, m.env);
        frame[0] = new LispLongRet(m, exit);
    },
    /*OP.LRET2*/ (m) => {
        let noval = m.f.noval;
        let fr = m.code[m.pc++];
        let retval = m.stack.pop();
        let moreval = m.stack.values[m.stack.sp];
        frame(m.env, fr)[0].run(m);
        if (!noval) {
            m.stack.push(retval);
            m.stack.values[m.stack.sp-1] = moreval;
        }
    },
    /*OP.LJUMP2*/ (m) => {
        let addr = m.code[m.pc++];
        let fr = m.code[m.pc++];
        frame(m.env, fr)[0].run(m, addr);
    },
    /*OP.XARGS*/ (m) => {
        let required = m.code[m.pc++];
        let optional = m.code[m.pc++];
        let rest = m.code[m.pc++];
        let key = m.code[m.pc++];
        let allow_other_keys = m.code[m.pc++];
        let kl = key?.length;
        let n = m.n_args;
        let frame_len = required + 2 * optional + rest + 2 * kl;
        let min = required;
        let max = rest || kl ? null : required + optional;
        if (n < required) {
            error(`Expecting at least ${min} arguments`);
        }
        if (max !== null && n > max) {
            error(`Expecting at most ${max} arguments`);
        }
        let frame = new Array(frame_len).fill(null);
        let stack = m.stack.data;
        let maxi = m.stack.sp;
        let i = maxi - n;
        let index = 0;
        while (required-- > 0) {
            frame[index++] = stack[i++];
        }
        while (optional-- > 0 && i < maxi) {
            frame[index++] = true; // argument-passed-p
            frame[index++] = stack[i++];
        }
        if (i < maxi) {
            if (rest) {
                frame[index++] = LispCons.fromArray(stack, i, maxi);
            }
            if (kl) {
                if ((maxi - i) % 2 != 0) {
                    error("Uneven number of &key arguments");
                }
                if (!allow_other_keys) {
                    let pos = find_key_arg(S_ALLOW_OTHER_KEYS, stack, i, maxi);
                    if (pos !== null) {
                        allow_other_keys = stack[pos + 1];
                        stack[pos] = false;
                        if (pos == i) i += 2;
                    }
                }
                for (let k = 0; k < kl; k++, index += 2) {
                    let pos = find_key_arg(key[k], stack, i, maxi);
                    if (pos !== null) {
                        frame[index] = true; // argument-passed-p
                        frame[index + 1] = stack[pos + 1];
                        stack[pos] = false;
                        if (pos == i) i += 2;
                    }
                }
                if (!allow_other_keys) {
                    while (i < maxi) {
                        if (stack[i] !== false) {
                            error(`Unknown keyword argument ${dump(stack[i])}`);
                        }
                        i += 2;
                    }
                }
            }
        }
        m.stack.sp -= n;
        m.env = new LispCons(frame, m.env);
    },
    /*OP.POPLIST*/ (m) => {
        let i = m.code[m.pc++];
        let j = m.code[m.pc++];
        let fr = frame(m.env, i);
        let lst = fr[j];
        fr[j] = LispCons.cdr(lst);
        m.push(LispCons.car(lst));
    },
    /*OP.EQ*/ (m) => {
        m.push(eq(m.pop(), m.pop()));
    },
    /*OP.POPGLIST*/ (m) => {
        let sym = m.code[m.pc++];
        let binding = m.find_dvar(sym);
        let lst = binding.value;
        m.push(LispCons.car(lst));
        binding.value = LispCons.cdr(lst);
    },
    /*OP.TJUMPK*/ (m) => {
        let addr = m.code[m.pc++];
        if (m.top() === null) {
            m.pop();
        } else {
            m.pc = addr;
        }
    },
    /*OP.FJUMPK*/ (m) => {
        let addr = m.code[m.pc++];
        if (m.top() === null) {
            m.pop();
            m.pc = addr;
        }
    },
    /*OP.VALUES*/ (m) => {
        let nargs = m.code[m.pc++];
        m.stack.set_values(nargs);
    },
    /*OP.MVB*/ (m) => {
        let n = m.code[m.pc++];
        let frame = m.stack.pop_values();
        m.env = new LispCons(frame, m.env);
        while (frame.length < n) frame.push(null);
        frame.length = n;
    },
    /*OP.POPBACK*/ (m) => {
        let n = m.code[m.pc++];
        m.stack.replace(-n-1, m.pop());
    },
];

function vmrun(m) {
    OP_RUN[m.code[m.pc++]](m);
}
