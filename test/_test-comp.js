(function(){

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

        var m = new LispMachine();
        this.machine = m;

        // load the compiler
        load("test.sslc", function(code){
                code = LispMachine.unserialize(code);
                m.run(code);
                step2();
        });

        var EOF = {};
        function load_lisp(file) {
                var func = LispSymbol.get("LOAD-LISP-FILE").func();
                return m.call(func, LispCons.fromArray([ file ]));
        };

        function step2() {
                time_it("recompile-compiler", function(){
                        console.log(load_lisp("../lisp/compiler.lisp"));
                });
                time_it("init", function(){
                        load_lisp("../lisp/init.lisp");
                });
        };

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(3) + "s");
}
