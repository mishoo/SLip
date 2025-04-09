import { LispMachine } from "../js/machine.js";
import { LispSymbol, LispPackage } from "../js/types.js";
import { LispCons } from "../js/list.js";
import { make_desktop } from "./ide.js";
import "../js/primitives.js";

(function(window){

    Object.assign(LispMachine.prototype, {
        read: function(pak, str) {
            var f = LispSymbol.get("EXEC-READ", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [ pak, str ]);
        },
        eval: function(expr) {
            var f = LispSymbol.get("EXEC-EVAL", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [ expr ]);
        },
        eval_string: function(pak, str) {
            var f = LispSymbol.get("EXEC-EVAL-STRING", LispPackage.get("YMACS")).function;
            return this.atomic_call(f, [ pak, str ]);
        },
    });

    function outdated(url) {
        if (/\.fasl$/.test(url)) {
            if (window.SLIP_COMMIT && window.SLIP_COMMIT != localStorage.getItem(".slip_commit")) {
                return true;
            }
        }
    }

    function load(url, callback) {
        var xhr = new XMLHttpRequest();
        if (!/^https?:\/\//i.test(url) && !outdated(url)) {
            // local storage takes priority
            let content = LispMachine.ls_get_file_contents(url);
            if (content != null) {
                setTimeout(() => callback(content), 0);
                return;
            }
        }
        xhr.open("GET", url + "?killCache=" + Date.now(), true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                callback(xhr.responseText);
            }
        };
        xhr.send(null);
    };

    function save(url, content, callback) {
        if (!/^https?:\/\//i.test(url)) {
            // save to localStorage as well
            LispMachine.ls_set_file_contents(url, content);
        }
        var xhr = new XMLHttpRequest();
        xhr.open("PUT", url, true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                if (xhr.status >= 200 && xhr.status < 300) {
                    callback(false);
                } else {
                    callback(true);
                }
            }
        };
        xhr.send(content);
    };

    let machine = new LispMachine();

    function load_fasls(files, cont) {
        var count = files.length;
        var fasls = [];
        files = files.map(filename => filename.replace(/(\.lisp)?$/, ".fasl"));
        files.forEach(function(filename, i){
            log("Loading: " + filename);
            load(filename, function(code){
                fasls[i] = LispMachine.unserialize(code);
                if (--count == 0) {
                    console.time("Boot");
                    fasls.forEach(code => machine._exec(code));
                    console.timeEnd("Boot");
                    console.log("Loaded " + files.join(", "));
                    cont();
                }
            });
        });
    };

    function log(str) {
        var div = document.createElement("div");
        div.innerHTML = str;
        div.className = "lisp-log";
        document.body.appendChild(div);
        div.scrollIntoView();
    };

    function compile(files, cont, nosave) {
        if (files.length == 0) return cont ? cont() : null;
        var filename = files[0];
        if (!nosave) log("Compiling " + filename);
        var bytecode = machine.atomic_call(LispSymbol.get("%LOAD").function, [ filename ]);
        if (!nosave) {
            var fasl = filename.replace(/(\.lisp)?$/, ".fasl");
            save(fasl, bytecode, function(error){
                if (error) {
                    console.log(`Saving ${fasl} via webdav failed; it was saved in localStorage. Continuing...`);
                } else {
                    log("... " + fasl + " saved.");
                }
                compile(files.slice(1), cont);
            });
        }
    };

    function recompile_all() {
        load_fasls([ "lisp/compiler.lisp" ], function(){
            let lisp_files = LispCons.toArray(LispSymbol.get("*CORE-FILES*").value);
            lisp_files.unshift("lisp/compiler.lisp");
            compile(lisp_files, function(){
                log("<span style='color: green'><b>DONE — press ENTER to reload</b></span>");
                document.addEventListener("keydown", ev => {
                    if (ev.key == "Enter") {
                        //window.location.replace((""+window.location).replace(/\?recompile$/, ""));
                        window.location.replace(document.referrer);
                    }
                });
            });
        });
    };

    // recompile_all();

    var startup_files = [];

    function init() {
        load_fasls([ "lisp/compiler.lisp" ], function(){
            let lisp_files = LispCons.toArray(LispSymbol.get("*CORE-FILES*").value);
            load_fasls(lisp_files, function(){
                window.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
                [ ...document.querySelectorAll(".lisp-log") ].forEach(el => el.remove());
                make_desktop(startup_files);
                compile(startup_files, () => {}, true);
            });
        });
    };

    if (/\?recompile$/.test(window.location)) {
        recompile_all();
    } else {
        let m = /\?load=([^&]+)/.exec(window.location);
        if (m) startup_files = m[1].split(/\s*,\s*/);
        init();
    }

})(window);
