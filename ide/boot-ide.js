(function(global, window){

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
                if (files.length == 0) cont();
                else {
                        var filename = files[0].replace(/(\.lisp)?$/, ".fasl");
                        load(filename, function(code){
                                machine._exec(LispMachine.unserialize(code));
                                load_fasls(files.slice(1), cont);
                        });
                }
        };

        function compile(files, cont) {
                if (files.length == 0) return cont();
                var filename = files[0];
                console.log("Compiling " + filename);
                var bytecode = machine.atomic_call(LispSymbol.get("%LOAD").func(), [ filename ]);
                var fasl = filename.replace(/(\.lisp)?$/, ".fasl");
                save(fasl, bytecode, function(error){
                        if (error) throw new Error("Failed to save bytecode");
                        console.log(fasl + " saved.");
                        compile(files.slice(1), cont);
                });
        };

        var lisp_files = [
                "../lisp/compiler.lisp",
                "../lisp/init.lisp",
                "../lisp/macroexpand.lisp",
                "../lisp/tiny-clos.lisp",
                "../lisp/printer.lisp",
                "../lisp/format.lisp",
                "../lisp/ffi.lisp",
                "../lisp/conditions.lisp",
                "ide.lisp"
        ];

        function recompile_all() {
                load_fasls([ "../lisp/compiler.lisp"], function(){
                        compile(lisp_files, function(){
                                console.log("DONE");
                        });
                });
        };

        // recompile_all();

        load_fasls(lisp_files, function(){
                //machine.atomic_call(LispSymbol.get("LOAD").func(), [ "ide.lisp" ]);
                global.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
                window.open("ymacs.html", "ss_lisp", "width=800,height=600,menubar=0,toolbar=0,location=0,personalbar=0,status=0,dependent=1,chrome=1");
        });

})(this, window);
