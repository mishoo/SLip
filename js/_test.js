(function(){

        var ast = lisp_parse('((lambda (fact) (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 10)) nil)');
        var ast = lisp_parse('((lambda (sum) (set! sum (lambda (n) (if (= n 0) 0 (+ n (sum (- n 1)))))) (sum 100000)) nil)')

//         var ast = lisp_parse('\
// ((lambda (counter) \
//    (set! counter ((lambda (val) \
//                     (lambda () (set! val (+ val 1)))) 0)) \
// (* (+ (counter) (counter)) (+ (counter) (counter)))) nil)');

        //var ast = lisp_parse("'(foo bar baz)")

        console.log(ast);

        var code = compile(ast);
        console.log(JSON.stringify(code));
        console.log(comp_show(code));

        var m = new LispMachine();

        code = LispMachine.assemble(code);

        console.log(LispMachine.serialize(code));

        time_it(function(){
                console.log(m.run(code));
        });

        // console.time("run_threaded");
        // m.run_threaded(code, function(ret){
        //         console.timeEnd("run_threaded");
        //         console.log("threaded run finished");
        //         console.log(ret);
        // });

})();

function time_it(f) {
        var start = Date.now();
        f();
        console.log(((Date.now() - start) / 1000).toFixed(2));
}
