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
                //console.log(LispMachine.dump(ast));

                var m = new LispMachine();

                LispCons.forEach(ast, function(ast){
                        ast = new LispCons(ast, null);
                        console.log(LispMachine.dump(ast));
                        var bc = compile(ast);
                        if (bc) {
                                console.log(comp_show(bc));
                                bc = LispMachine.assemble(bc);
                                console.log(LispMachine.serialize(bc));
                                time_it("run", function(){
                                        console.log(LispMachine.dump(m.run(bc)));
                                });
                        }
                        console.log("****************************************************");
                });

        });

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(2) + "s");
}
