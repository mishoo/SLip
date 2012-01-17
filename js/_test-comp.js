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
        load("../lisp/test.sslc", function(code){
                code = LispMachine.unserialize(code);
                m.run(code);
                step2();
        });

        var EOF = {};
        function parse(str) {
                var func = LispSymbol.get("LOAD-LISP-FILE").value;
                return m.call(func, LispCons.fromArray([ "../lisp/compiler.lisp" ]));
        };

        function step2() {
                console.log(LispMachine.dump(parse("(foo 'bar baz)")))
        };

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(3) + "s");
}
