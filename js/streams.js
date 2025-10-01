import { LispChar } from "./types.js";

/* -----[ input streams ]----- */

// abstract, not for direct instantiation
export class LispInputStream {
    static type = "input-stream";
    static is(x) { return x instanceof LispInputStream }

    reader = null;
    buffer = null;
    index = 0;
    position = 0;

    _peek() {
        return this.buffer && this.index < this.buffer.length ? this.buffer[this.index] : false;
    }
    _next() {
        return this.buffer && this.index < this.buffer.length ? this.buffer[this.index++] : false;
    }
    peek() { return this.transform(this._peek()) }
    next() { return this.position++, this.transform(this._next()) }
    transform(val) { return val }
    read_sequence(seq, start, end) {
        let index = start;
        while (this._peek() !== false && (end === false || index < end)) {
            seq[index++] = this.next();
        }
        return index;
    }

    set_position() {
        return false;
    }

    // For reader streams. This function should return TRUE only if it
    // makes sense to check asynchronously for additional data, that
    // is: (1) we have an attached reader and EOF wasn't yet seen (on
    // EOF the reader is nullified, see in FETCH below), (2) we don't
    // have an internal buffer (initial case) or (3) we have consumed
    // the internal buffer.
    try_fetch() {
        return this.reader && (!this.buffer || this.index >= this.buffer.length);
    }
    async fetch() {
        let { value, done } = await this.reader.read();
        if (done) this.reader = null;
        this.buffer = value;
        this.index = 0;
    }
}

// abstract, not for direct instantiation
export class LispTextInputStream extends LispInputStream {
    static type = "text-input-stream";
    static is(x) { return x instanceof LispTextInputStream }

    line = 1;
    col = 0;

    transform(val) { return val === false ? false : LispChar.get(val) }
    makeBuffer(value) {
        // splice it only if it has surrogate pairs
        return /[\uD800-\uDFFF]/.test(value) ? [...value] : value;
    }
    _next() {
        if (this.buffer && this.index < this.buffer.length) {
            let ch = this.buffer[this.index++];
            if (ch == "\n") ++this.line, this.col = 0;
            else ++this.col;
            return ch;
        }
        return false;
    }
}

export class LispTextMemoryInputStream extends LispTextInputStream {
    static type = "text-memory-input-stream";
    static is(x) { return x instanceof LispTextMemoryInputStream }
    constructor(text) {
        super();
        this.buffer = this.makeBuffer(text);
        this.index = 0;
    }
}

export class LispTextReaderInputStream extends LispTextInputStream {
    static type = "text-reader-input-stream";
    static is(x) { return x instanceof LispTextReaderInputStream }
    constructor(stream) {
        super();
        this.reader = stream.pipeThrough(new TextDecoderStream()).getReader();
    }
    async fetch() {
        let { value, done } = await this.reader.read();
        if (done) {
            this.reader = this.buffer = null;
        } else {
            this.buffer = this.makeBuffer(value);
        }
        this.index = 0;
    }
}

export class LispReaderInputStream extends LispInputStream {
    static type = "reader-input-stream";
    static is(x) { return x instanceof LispReaderInputStream }
    constructor(stream) {
        super();
        this.reader = stream.getReader();
    }
}

/* -----[ output streams ]----- */

export class LispOutputStream {
    static type = "output-stream";
    static is(x) { return x instanceof LispOutputStream }

    position = 0;
    index = 0;
    buffer = null;
    writer = null;

    set_position() {
        return false;
    }

    onData() {}
}

export class LispTextOutputStream extends LispOutputStream {
    static type = "text-output-stream";

    line = 1;
    col = 0;

    static is(x) { return x instanceof LispTextOutputStream }
}

export class LispTextMemoryOutputStream extends LispTextOutputStream {
    static type = "text-memory-output-stream";
    static is(x) { return x instanceof LispTextMemoryOutputStream }
    constructor(init) {
        super();
        this.buffer = "";
        if (init) this.put(init);
    }
    put(str) {
        if (LispChar.is(str)) str = str.value;
        var lines = str.split(/\r?\n/);
        this.line += lines.length - 1;
        this.col = lines.length > 1
            ? lines[lines.length - 1].length
            : this.col + lines[0].length;
        this.position += str.length;
        this.buffer += str;
        this.onData(this, str);
        return this.buffer;
    }
    get() {
        let data = this.buffer;
        this.buffer = "";
        return data;
    }
}

/* -----[ whatever stream ]----- */

export class LispStream {
    static type = "stream";
    static is(x) {
        return x instanceof LispInputStream
            || x instanceof LispOutputStream;
    }
    static [Symbol.hasInstance](x) {
        return this.is(x);
    }
}

export class LispTextStream {
    static type = "text-stream";
    static is(x) {
        return x instanceof LispTextInputStream
            || x instanceof LispTextOutputStream;
    }
    static [Symbol.hasInstance](x) {
        return this.is(x);
    }
}
