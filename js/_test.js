(function(){

        var ast = lisp_parse('((lambda (fact) (set! fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 10)) nil)');
        var ast = lisp_parse('((lambda (sum) (set! sum (lambda (n) (if (= n 0) 0 (+ n (sum (- n 1)))))) (sum 100000)) nil)')

//         var ast = lisp_parse('\
// ((lambda (counter) \
//    (set! counter ((lambda (val) \
//                     (lambda () (set! val (+ val 1)))) 0)) \
// (* (+ (counter) (counter)) (+ (counter) (counter)))) nil)');

        console.log(ast);

        var code = compile(ast);
        console.log(code);
        console.log(comp_show(code));


        code = assemble(code);
        console.log(JSON.stringify(code));
        //console.log(code);

        console.time("run");
        console.log(run_bytecode(code));
        console.timeEnd("run");

})();
