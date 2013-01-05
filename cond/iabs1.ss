


;Implementation of absolution value
;To execute this file, in the scheme intepreter 
; > (load "iabs1.ss") 

; example of condition in scheme, like switch
(define (iabs1 x) 
	(cond ((< x 0) (- x) )
	      ((>= x 0) x)
	)
)


; example of if else statement
(define (iabs2 x) 
	(if  (>= x 0)
		x
		(- x)
	)
)


; test cases
; >0 
(iabs1 213124)
(iabs2 213124)

; =0
(iabs2 0)
(iabs1 0)


; <0
(iabs1 -1233218)
(iabs2 -1233218)

