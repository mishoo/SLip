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
        console.log(JSON.stringify(code));
        console.log(comp_show(code));

        var m = new LispMachine();

        console.time("run");
        code = m.assemble(code);
        console.log(m.run(code));
        console.timeEnd("run");

})();
