export class LispPrimitiveError {
    static type = "primitive-error";
    static is(x) { return x instanceof LispPrimitiveError }
    constructor(msg) {
        this.message = msg;
    }
}
