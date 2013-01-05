#lang scheme

(define (fib x)
	( if (< x 2) x
		(+ (fib (- x 1)) (fib (- x 2)) )   )
)

(define (fast_fib x)
	(fast_fib_help x 0 1)
)

(define (fast_fib_help x base1 base2)
	(cond   ((zero? x) base1)
		((zero? (- x 1)) base2)
		(else (fast_fib_help (- x 1) base2 (+ base1 base2)  ) )
	)
)


(fib 1);
(fib 2);
(fib 3);
(fib 4);
(fib 5);
(fib 6);
(fib 7);
(fib 8);
(fib 9);
(fib 10);
(fib 11);
(fib 12);
(fib 13);
(fib 14);
(fib 15);
(fib 16);


(fast_fib 100)
(fast_fib 200)
(fast_fib 300)
(fast_fib 400)
(fast_fib 500)
