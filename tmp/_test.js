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
                        window.CURRENT_FILE = code;
                        var reader = lisp_reader(code);
                        while (true) {
                                ast = reader();
                                if (ast === false) break;
                                //console.log(LispMachine.dump(ast));
                                var bc = lisp_compile(new LispCons(ast, null));
                                if (bc) {
                                        bc = LispMachine.assemble(bc);
                                        //console.log(LispMachine.serialize(bc));
                                        console.log(LispMachine.disassemble(bc));
                                        console.log("- - -");
                                        time_it("run", function(){
                                                var ret = LispMachine.dump(m._exec(bc));
                                                //console.log(ret);
                                        });
                                }
                                //console.log("------------------------------------------------");
                        }

                        load_files(list.splice(1));
                };
                if (list.length > 0) {
                        console.log("****************** " + list[0]);
                        load(list[0], dofile);
                }
        };

        load_files([
                //"tmp.lisp"
                "../lisp/compiler.lisp"
                //,"test.lisp"
                //"tmp/t1.lisp"
                //,"tmp/wotf.lisp"
        ]);

})();

function time_it(name, f) {
        var start = Date.now();
        f();
        console.log(name + ": " + ((Date.now() - start) / 1000).toFixed(3) + "s");
}
