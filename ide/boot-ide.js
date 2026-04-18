import { LispMachine } from "../js/machine.js";
import { LispSymbol, LispPackage, LispProcess } from "../js/types.js";
import { make_desktop } from "./ide.js";
import "../js/primitives.js";

(function (window) {

    Object.assign(LispMachine.prototype, {
        read: function (pak, str) {
            var f = LispSymbol.get("EXEC-READ", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [pak, str]);
        },
        eval: function (expr) {
            var f = LispSymbol.get("EXEC-EVAL", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [expr]);
        },
        eval_string: function (pak, str) {
            var f = LispSymbol.get("EXEC-EVAL-STRING", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [pak, str]);
        },
    });

    function load(url, callback) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", url + "?killCache=" + Date.now(), true);
        xhr.onreadystatechange = function () {
            if (xhr.readyState == 4) {
                callback(xhr.responseText);
            }
        };
        xhr.send(null);
    }

    let machine = new LispMachine();

    function load_fasls(files, cont) {
        var count = files.length;
        var fasls = [];
        files = files.map(filename => filename.replace(/(\.lisp|\.fasl)?$/, ".fasl"));
        files.forEach(function (filename, i) {
            load(filename, function (code) {
                fasls[i] = LispMachine.unserialize(code);
                if (--count == 0) {
                    fasls.forEach(code => machine._exec(code));
                    console.log("Loaded " + files.join(", "));
                    cont();
                }
            });
        });
    }

    function recompile_all() {
        let t1 = performance.now();
        load_fasls(["lisp/compiler.fasl"], function () {
            document.body.innerHTML = "";
            document.body.style.padding = "2em";
            document.body.style.whiteSpace = "pre";
            document.body.style.fontFamily = "monospace";
            [
                LispSymbol.get("*STANDARD-OUTPUT*"),
                LispSymbol.get("*ERROR-OUTPUT*"),
                LispSymbol.get("*TRACE-OUTPUT*"),
            ].forEach(sym => {
                let stream = sym.value;
                stream.onData = (_, str) => {
                    let txt = document.createTextNode(str);
                    document.body.appendChild(txt);
                    window.scrollTo(0, document.body.scrollHeight);
                };
            });
            let proc = new LispProcess(machine, LispSymbol.get("RECOMPILE-EVERYTHING").function);
            proc.watchers.push({
                resume() {
                    let t2 = performance.now();
                    let el = document.createElement("div");
                    el.innerHTML = `<span style='color: green; font-weight: bold;'>That took ${(t2 - t1).toFixed(2)}ms.<br/>Press ENTER to reload, T to run tests.</span>`;
                    document.body.appendChild(el);
                    window.scrollTo(0, document.body.scrollHeight);
                    document.addEventListener("keydown", ev => {
                        if (ev.key == "Enter") {
                            window.location.replace(document.referrer);
                        } else if (ev.key.toLowerCase() == "t") {
                            let p = new LispProcess(machine, LispSymbol.get("LOAD").function, false, "test/all.lisp");
                            p.watchers.push({
                                resume() {
                                    let p = new LispProcess(machine, LispSymbol.get("RUN-TESTS", LispPackage.get("SL-TEST")).function);
                                    p.resume();
                                }
                            });
                            p.resume();
                        }
                    });
                }
            });
            proc.resume();
        });
    }

    var startup_files = [];

    function init() {
        console.time("Boot");

        load_fasls(["slip-bundle.fasl"], done);

        // load_fasls([ "lisp/compiler.lisp" ], function(){
        //     let lisp_files = LispCons.toArray(LispSymbol.get("*CORE-FILES*").value);
        //     load_fasls(lisp_files, done);
        // });

        function done() {
            console.timeEnd("Boot");
            window.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
            [...document.querySelectorAll(".lisp-log")].forEach(el => el.remove());
            let done = make_desktop(startup_files);
            done();
            if (startup_files.length > 0) {
                let p = new LispProcess(machine, LispSymbol.get("LOAD").function, false, startup_files[0]);
                p.resume();
            }
        }
    }

    if (/\?recompile$/.test(window.location)) {
        recompile_all();
    } else {
        let m = /\?load=([^&]+)/.exec(window.location);
        if (m) startup_files = m[1].split(/\s*,\s*/);
        init();
    }

})(window);
