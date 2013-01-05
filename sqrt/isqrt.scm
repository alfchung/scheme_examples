
#lang scheme


; algorithm
; find_sqrt(x, guess)
; 	if (guess is good enough)
; 		return guess
; 	else 
;		guess = average(x/guess, guess)
;		return find(x, guess)
; 	

(define (square x) (* x x))

(define (diff x y) 
	(abs 
	    (- x y))
)
;test diff
(diff 30 80)

(define (good_enough x guess)
	    (<  (diff x 
		    (square guess)  
		)

		0.001
            )
)

;test good_enough
(good_enough 4 3)


(define (average x y) 
	(/ (float (+ x y) ) 
	   2
	)
)

(define (float x) (* 1.0 x))

(define (isqrt_helper x guess) 
		( if ( good_enough x guess ) 
		     guess
		     (isqrt_helper x
		     		   (average guess    ; new guess
		     		        (/ (float x) guess) 
				   ) 
		     )
		     	
		)
)

(isqrt_helper 4 1)

(define (isqrt x) 
	(if (< x 0)
	    (error "Error: only non-negative numbers have square root")
  	    (isqrt_helper x 1)
	)
)




