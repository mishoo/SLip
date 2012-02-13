(function(global){

        LispMachine.extend({
                read: function(str) {
                        var f = LispSymbol.get("READ1-FROM-STRING").func();
                        return this.atomic_call(f, [ str ]);
                },
                eval: function(expr) {
                        var f = LispSymbol.get("EVAL").func();
                        return this.atomic_call(f, [ expr ]);
                },
                eval_string: function(str) {
                        var f = LispSymbol.get("EVAL-STRING").func();
                        return this.atomic_call(f, [ str ]);
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

        // load the compiler
        load("../fasl/compiler.fasl", function(code){
                var machine = new LispMachine();
                machine._exec(LispMachine.unserialize(code));
                load("../fasl/init.fasl", function(code){
                        machine._exec(LispMachine.unserialize(code));
                        machine.eval_string('(load "ide.lisp")');
                        global.MACHINE = LispSymbol.get("*THREAD*", LispPackage.get("YMACS")).value.m;
                        window.open("ymacs.html", "_ss_lisp_");
                });
        });

})(this);
