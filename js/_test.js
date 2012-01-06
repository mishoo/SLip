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

        function load_files(list) {
                function dofile(code){
                        var ast = lisp_parse(code);
                        console.log(LispMachine.dump(ast));

                        LispCons.forEach(ast, function(ast){
                                //console.log(LispMachine.dump(ast));
                                var bc = compile(new LispCons(ast, null));
                                if (bc) {
                                        console.log(comp_show(bc));
                                        bc = LispMachine.assemble(bc);
                                        console.log(LispMachine.serialize(bc));
                                        time_it("run", function(){
                                                console.log(LispMachine.dump(m.run(bc)));
                                        });
                                }
                                console.log("------------------------------------------------");
                        });

                        load_files(list.splice(1));
                };
                if (list.length > 0) {
                        console.log("****************** " + list[0]);
                        load(list[0], dofile);
                }
        };

        load_files([
                "tmp/t2.lisp"
                //"test.lisp",
                //"tmp/t1.lisp"
        ]);

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(2) + "s");
}
