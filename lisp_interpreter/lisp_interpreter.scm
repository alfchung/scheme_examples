
;#lang scheme



;Built in functions used
; * number? 
; * string?
; * symbol?  example: (symbol? 'abc) returns #t


; if an expression is a number or a string
; it is self-evaluating
(define (self-evaluating? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else #f)))

(define variable? symbol?)

; the 2nd member of exp
(define get-second-member cadr)

(define get-third-member caddr)

(define get-forth-member cadddr)

(define get-first-of-second-member caadr)



; don't know what this means yet
(define (text-of-quotation exp) (get-second-member exp))


; checking if the exp is something like (func a b c ...)
(define (tagged-list? exp tag)
	(if (pair? exp) ; pair works for cons, list, quoted expression
		(eq? (car exp) tag)
	    #f 
	)
)


(define (quoted? exp)
	(tagged-list? exp 'quote)	
)


;assignment
(define (assignment? exp)
	(tagged-list? exp 'set!)
)


(define (assignment-variable exp)
	(get-second-member exp)
)


(define (assignment-value exp)
	(get-third-member exp)
)



; Lambda
(define (lambda? exp)
	(tagged-list? exp 'lambda)
)

(define (lambda-parameters exp)
	(cadr exp) ;second
)

(define (lambda-body exp)
	(cddr exp) ;list starts from the third
)

(define (make-lambda parameters body)
	(cons 'lambda (cons parameters body))
)




; definition
(define (definition? exp)
	(tagged-list? exp 'define)
)


(define (definition-variable exp)
	(define second (get-second-member exp))
	(if (symbol? second)
		second   			 ;for something like (define a 123)
		(get-first-of-second-member exp) ;for something like (define (func x) body)
	)
)


;(define (make-lambda parameters body)
;	(cons 'lambda (cons parameters body))
;)


;(define (definition-value exp)
;	(define second (get-second-member exp))
;	(if (symbol? second)
;		(get-third-member exp) ; (define a 123)
;		; (define (func x) (+ 1 2))
;		; get the parameter and body
;		;(make-lambda (cdr (get-second-member exp)) (get-third-member exp))
;		(make-lambda (cdadr exp) (cddr exp))
;	)
;)

(define (definition-value exp)
	(define second (get-second-member exp))
	(define body (cddr exp))

	(if (symbol? second)    ; second element
		body
	  	(make-lambda 
			(cdr second) ; formal parameters (define (func x y z) (+ x y z))  : (x y z)
				     ; get rid of function name here
	   		body  ; body:  list starts from the third
		)              
	)
) 

	   
; if statement
(define (if? exp)
	(tagged-list? exp 'if)
)

(define (if-predicate exp)
	(get-second-member exp)
)

(define (if-consequent exp)
	(get-third-member exp)
)

(define (if-alternative exp)
	(define alternative (cdddr exp)) ;list starts from the forth element
	(if (not (null? alternative))
		alternative
		'false
	)
)


