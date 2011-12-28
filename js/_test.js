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

        load("test.lisp", function(code){
                var ast = lisp_parse(code);
                var bc = compile(ast);
                console.log(comp_show(bc));
                var m = new LispMachine();
                bc = LispMachine.assemble(bc);
                console.log(LispMachine.serialize(bc));
                time_it("run", function(){
                        console.log(m.run(bc));
                });
        });

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(2) + "s");
}
