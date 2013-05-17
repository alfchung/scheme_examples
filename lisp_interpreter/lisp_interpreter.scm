
;#lang scheme



;Built in functions used
; * number? 
; * string?
; * symbol?  example: (symbol? 'abc) returns #t


; !!!! The following are helper functions !!!!


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





; !!!! The following are types of lisp statements !!
; assignment, lambda, definition, if, begin, applicaiton(function call),

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


(define (make-if predicate consequent alternative)
	(list 'if predicate consequent alternative)
)



;begin statement
(define (begin? exp)
	(tagged-list? exp 'begin)
)

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) 
	(null? (cdr seq)) 
)

(define (first-exp seq) (car seq))

(define (rest-exp seq) (cdr seq))

(define (make-begin seq) (cons 'begin seq))

; make-begin takes a list(seq) with more than one member, a single expression, a null
(define (sequence->exp seq)
	(cond ( (null? seq)      seq              )
	      ( (last-exp? seq)  (first-exp seq)    )
	      ( else             (make-begin seq) )
	)
)




;applicaiton, a function call like (f p1 p2 p3 p4)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operand ops) (cdr ops))






; !!! derived expressions !!!
; cond
; for example
;	(cond ((> x 0) x)
;	      ((= x 0) (display 'zero) 0)
;	      (else (- x)))
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond-else-clause? exp) 
	(eq? (cond-predicate exp) 'else)
)

(define (expand-clauses clauses)
	(if (null? clauses)	
		'false
		(let ( (first (car clauses))
		       (rest  (cdr clauses))
		     )

		     (if (cond-else-clause? first) ; is this the last statement?
		     	(if (null? rest) ; the end of cond
				(sequence->exp (cond-actions first)) ;if only one exp, just itself
								     ;if null, just itself
								     ;if multiple expressions, use begin
				(error "Else clause itsn't last -- COND->IF")
			)
			(make-if (cond-predicate first) ;not end of cond yet
				(sequence->exp (cond-actions first))
				(expand-clauses rest)
			)
		     )
		)
	)
)
; question: cond-actions can be a sequence of expression without begin, doesn't seem possible
; yes like  (cond (#f "hello") (#t (+ 3 4) (+ 2 3) ))

(define (cond->if exp)
	(expand-clauses (cond-clauses exp))
)




; the evaluator data structures
(define (true? x) 
	(not (false? x))
)

(define (false? x)
	(eq? x #f)
)




;(define (ieval exp env)
;	(cond ( (self-evaluating? exp) exp )
;	      ( (variable? exp) (lookup-variable-value exp env) )
;	      ( (quoted? exp) (text-of-quotation exp) )
;	)
;)




;(load "lisp_interpreter.scm")


