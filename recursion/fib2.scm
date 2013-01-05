; Alfred Zhong
; Computer Science Department
; Brown University
; Providence, Rhode Island
; www.cs.brown.edu/~az


#lang scheme 

(define (fib2 x) 
	 (if (< x 2) x 
		(+ (fib2 (- x 1)) 
			(fib2 (- x 2)))   )  
)

(define (fast-fib n)
	(fast_fib_help n 0 1)
)

;0 1 2 3 4 5 index
;0 1 1 2 3 5 fib
;0 0 1 2 3 4 # of additions

; 3 0 1
; 2 1 1
; 1 1 2
; =2

(define (fast_fib_help n base_0 base_1)
	(cond   ((zero? n) base_0 ) ;if (n==0) return 0;
		((zero? (- n 1)) base_1 ) ;if (n==1) return 1;
		(else ( fast_fib_help (- n 1) base_1 (+ base_0 base_1)  ) )
	)
)


(fib2 0)
(fib2 1)
(fib2 2)
(fib2 3)
(fib2 4)
(fib2 5)
(fib2 6)
(fib2 7)
(fib2 8)
(fib2 9)
(fib2 10)

(fast-fib 40)
(fast-fib 80)
(fast-fib 200)
(fast-fib 500)
