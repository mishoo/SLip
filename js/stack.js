import { LispPrimitiveError } from "./error.js";

class Values {
    constructor(vals) {
        this.vals = vals;
    }
    first() {
        return this.vals.length > 0 ? this.vals[0] : false;
    }
}

export function value(el) {
    return el instanceof Values ? el.first() : el;
}

export class LispStack {
    constructor(maxsize = 4096) {
        this.maxsize = maxsize;
        this.sp = 0;
        this.data = new Array(maxsize);
    }
    pop() {
        if (this.sp > 0) {
            return value(this.data[--this.sp]);
        } else {
            throw new LispPrimitiveError("pop() with an empty stack");
        }
    }
    pop_ret() {
        if (this.sp > 0) {
            return this.data[--this.sp];
        } else {
            throw new LispPrimitiveError("pop() with an empty stack");
        }
    }
    top() {
        if (this.sp > 0) {
            return this.data[this.sp - 1] = value(this.data[this.sp - 1]);
        } else {
            throw new LispPrimitiveError("top() with an empty stack");
        }
    }
    at(index) {
        if (index < 0) index += this.sp;
        return value(this.data.at(index));
    }
    replace(index, newval) {
        if (index < 0) index += this.sp;
        let val = value(this.data[index]);
        this.data[index] = newval;
        return val;
    }
    remove(index) {
        if (index < 0) index += this.sp;
        let val = this.data[index];
        this.data.copyWithin(index, index + 1, this.sp--);
        return val;
    }
    push(val) {
        if (this.sp < this.maxsize) {
            this.data[this.sp++] = val;
        } else {
            throw new LispPrimitiveError("Stack overflow");
        }
    }
    pop_frame(len) {
        let sp = this.sp;
        if (sp < len) {
            throw new LispPrimitiveError(`Insufficient stack elements in pop_frame (${this.sp}/${len})`);
        }
        let frame = new Array(len);
        while (len > 0) frame[--len] = value(this.data[--sp]);
        this.sp = sp;
        return frame;
    }
    copy() {
        return this.data.slice(0, this.sp);
    }
    restore(copy) {
        for (var i = copy.length; --i >= 0;) {
            this.data[i] = copy[i];
        }
        this.sp = copy.length;
        return this;
    }
    set_values(len) {
        this.push(new Values(this.pop_frame(len)));
    }
    set_values_array(vals) {
        this.data[this.sp++] = new Values(vals);
    }
    pop_values() {
        let v = this.data[--this.sp];
        return v instanceof Values ? v.vals : [ v ];
    }
}
