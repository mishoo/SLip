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

        animate_something();

        // load the compiler
        load("test.sslc", function(code){
                code = LispMachine.unserialize(code);
                m._exec(code);
                step2();
        });

        function load1_lisp(file) {
                var func = LispSymbol.get("%LOAD").func();
                return m._call(func, LispCons.fromArray([ file ]));
        };
        function load2_lisp(file) {
                var func = LispSymbol.get("LOAD").func();
                return m._call(func, LispCons.fromArray([ file ]));
        };

        function step2() {
                time_it("recompile-compiler", function(){
                        load1_lisp("../lisp/compiler.lisp");
                });
                time_it("init", function(){
                        load1_lisp("../lisp/init.lisp");
                });
                time_it("compile-objects", function(){
                        load2_lisp("../lisp/tiny-clos.lisp");
                });
                time_it("compile-js", function(){
                        load2_lisp("../lisp/javascript.lisp");
                });
                // time_it("wotf", function(){
                //         load2_lisp("../tmp/wotf.lisp");
                // });
                // time_it("messaging", function(){
                //         load2_lisp("tmp.lisp");
                // });
        };

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(3) + "s");
}

function animate_something() {
        var foo = document.getElementById("foo");
        var div = document.createElement("div");
        foo.appendChild(div);
        div.style.width = "10px";
        div.style.height = "10px";
        div.style.background = "red";
        div.style.borderRadius = "4px";
        div.style.position = "absolute";
        div.style.marginTop = "-5px";
        div.style.marginLeft = "-5px";
        var q = 0;
        var start = Date.now();
        var timer = setInterval(function(){
                var x = 200 + 150 * Math.cos(q);
                var y = 200 + 150 * Math.sin(q);
                div.style.left = x + "px";
                div.style.top = y + "px";
                q += Math.PI / 50;
                if (Date.now() - start > 30000) clearInterval(timer);
        }, 25);
}
