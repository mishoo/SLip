import { readFileSync } from "fs";
import { disassemble, unserialize } from "../js/machine.js";

let filename = process.argv[2];
let code = readFileSync(filename, { encoding: "utf-8" });
code = unserialize(code);
console.log(disassemble(code));
