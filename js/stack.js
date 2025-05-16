import { LispPrimitiveError } from "./types.js";

export class LispStack {
    constructor(maxsize = 1024) {
        this.maxsize = maxsize;
        this.sp = 0;
        this.data = new Array(maxsize);
        this.values = new Array(maxsize);
    }
    pop() {
        if (this.sp > 0) {
            return this.data[--this.sp];
        } else {
            throw new LispPrimitiveError("pop() with an empty stack");
        }
    }
    top() {
        if (this.sp > 0) {
            return this.data[this.sp - 1];
        } else {
            throw new LispPrimitiveError("top() with an empty stack");
        }
    }
    at(index) {
        if (index < 0) index += this.sp;
        return this.data.at(index);
    }
    replace(index, newval) {
        if (index < 0) index += this.sp;
        let val = this.data[index];
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
            if (this.values[this.sp] !== null)
                this.values[this.sp] = null;
            this.data[this.sp++] = val;
        } else {
            throw new LispPrimitiveError("Stack overflow");
        }
    }
    pop_frame(len) {
        if (this.sp < len) {
            throw new LispPrimitiveError(`Insufficient stack elements in pop_frame (${this.sp}/${len})`);
        }
        let end = this.sp;
        return this.data.slice(this.sp -= len, end);
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
        if (len === 0) {
            this.values[this.sp] = [];
            this.data[this.sp++] = null;
        } else {
            let vals = this.pop_frame(len);
            this.values[this.sp++] = vals;
        }
    }
    set_values_array(vals) {
        this.values[this.sp] = vals;
        this.data[this.sp++] = vals[0];
    }
    pop_values() {
        let vals = this.values[--this.sp] || [ this.data[this.sp] ];
        this.values[this.sp] = null;
        return vals;
    }
}
