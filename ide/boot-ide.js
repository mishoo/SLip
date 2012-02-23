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

        var machine = new LispMachine();

        function load_fasls(files, cont) {
                if (files.length == 0) cont();
                else load(files[0], function(code){
                        machine._exec(LispMachine.unserialize(code));
                        load_fasls(files.slice(1), cont);
                });
        };

        load_fasls([
                "../lisp/compiler.fasl",
                "../lisp/init.fasl",
                "../lisp/tiny-clos.fasl",
                "../lisp/printer.fasl",
                "ide.fasl"
        ], function(){
                //machine.atomic_call(LispSymbol.get("LOAD").func(), [ "ide.lisp" ]);
                global.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
                window.open("ymacs.html", "ss_lisp", "width=800,height=600,menubar=0,toolbar=0,location=0,personalbar=0,status=0,dependent=1,chrome=1");
        });

})(this, window);
