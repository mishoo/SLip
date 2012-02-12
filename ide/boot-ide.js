(function(global){

        function init_machine() {
                var m = global.machine = new LispMachine();
                m.read = function(str) {
                        var f = LispSymbol.get("READ1-FROM-STRING").func();
                        return this.atomic_call(f, [ str ]);
                };
                m.eval = function(expr) {
                        var f = LispSymbol.get("EVAL").func();
                        return this.atomic_call(f, [ expr ]);
                };
                m.eval_string = function(str) {
                        var f = LispSymbol.get("EVAL-STRING").func();
                        return this.atomic_call(f, [ str ]);
                };
                return m;
        };

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

        var machine = init_machine();

        // load the compiler
        load("../fasl/compiler.fasl", function(code){
                machine._exec(LispMachine.unserialize(code));
                load("../fasl/init.fasl", function(code){
                        machine._exec(LispMachine.unserialize(code));
                        window.open("ymacs.html", "_ss_lisp_");
                });
        });

})(this);
