import { make_desktop } from "./ide.js";

(function(window){

    var lisp_files = [
        "lisp/compiler.lisp",
        "lisp/init.lisp",
        "lisp/macroexpand.lisp",
        "lisp/tiny-clos.lisp",
        "lisp/printer.lisp",
        "lisp/format.lisp",
        "lisp/ffi.lisp",
        "lisp/conditions.lisp",
        "lisp/loop.lisp",
        "ide/ide.lisp"
    ];

    LispMachine.extend({
        read: function(pak, str) {
            var f = LispSymbol.get("EXEC-READ", LispPackage.get("YMACS")).func();
            return this.atomic_call(f, [ pak, str ]);
        },
        eval: function(expr) {
            var f = LispSymbol.get("EXEC-EVAL", LispPackage.get("YMACS")).func();
            return this.atomic_call(f, [ expr ]);
        },
        eval_string: function(pak, str) {
            var f = LispSymbol.get("EXEC-EVAL-STRING", LispPackage.get("YMACS")).func();
            return this.atomic_call(f, [ pak, str ]);
        }
    });

    function load(url, callback) {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", url, true);
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                callback(xhr.responseText);
            }
        };
        xhr.send(null);
    };

    function save(url, content, callback) {
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

    var machine = new LispMachine();

    function load_fasls(files, cont) {
        var count = files.length;
        var fasls = [];
        files.forEach(function(filename, i){
            filename = filename.replace(/(\.lisp)?$/, ".fasl");
            log("Loading: " + filename);
            load(filename + "?killCache=" + Date.now(), function(code){
                fasls[i] = code;
                if (--count == 0) {
                    fasls.forEach(function(code){
                        machine._exec(LispMachine.unserialize(code));
                    });
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
        document.body.scrollTop = div.offsetTop;
    };

    function compile(files, cont, nosave) {
        if (files.length == 0) return cont ? cont() : null;
        var filename = files[0];
        if (!nosave) log("Compiling " + filename);
        var bytecode = machine.atomic_call(LispSymbol.get("%LOAD").func(), [ filename ]);
        if (!nosave) {
            var fasl = filename.replace(/(\.lisp)?$/, ".fasl");
            save(fasl, bytecode, function(error){
                if (error) throw new Error("Failed to save bytecode");
                log("... " + fasl + " saved.");
                compile(files.slice(1), cont);
            });
        }
    };

    function recompile_all() {
        load_fasls([ "lisp/compiler.lisp" ], function(){
            compile(lisp_files, function(){
                log("<span style='color: green'><b>DONE — I will reload in 3 seconds</b></span>");
                setTimeout(function(){
                    window.location.replace((""+window.location).replace(/\?recompile$/, ""));
                }, 3000);
            });
        });
    };

    // recompile_all();

    var startup_files = [];

    function init() {
        load_fasls(lisp_files, function(){
            window.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
            Array.prototype.slice.call(document.getElementsByTagName("div")).map(function(el){
                if (el.className == "lisp-log")
                    el.parentNode.removeChild(el);
            });
            make_desktop();
            compile(startup_files, () => {}, true);
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
