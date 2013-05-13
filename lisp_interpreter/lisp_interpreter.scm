
#lang scheme


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


(define (assignment? exp)
	(tagged-list? exp 'set!)
)


;-------------------------------------------------------------
; test type check, judge is a function that checks if the exp is some type(s)
(define (assert title judge exp value)
    (cond ( (equal? (judge exp) value) (string-append title " " "Pass") )
          (else (string-append title " " "Fail"))
    )
)


(define (assert-value title input expected)
	(if (equal? input expected)
		(string-append title " " "Pass")
		(string-append title " " "Fail")
		
	)
)




; test of self-evaluating?
(assert "test 1" self-evaluating? 23 #t)
(assert "test 2" self-evaluating? 55 #t)
(assert "test 3" self-evaluating? 5.5 #t)
(assert "test 4" self-evaluating? "Brown University" #t)
(assert "test 5" self-evaluating? "Computer Science" #t)

; test of variable?
(assert "test 6" variable? 'alfred #t)
(assert "test 7" variable? 'dfsa #t)
(assert "test 8" variable? 23.7 #f)
(assert "test 9" variable? 100 #f)

(assert "test 10" text-of-quotation '(1 2 3 4) 2)
(assert "test 11" text-of-quotation (list "a" "b" "c" "d") "b")
(assert "test 12" text-of-quotation (list 'a 'b 'c 'd) 'b)

(assert-value 
	"test 13"
	(tagged-list? '(func a b c d) 'func)
	#t
)

(assert-value 
	"test 14"
	(tagged-list? '(c a b c) 'func)
	#f
)

(assert-value 
	"test 15"
	(tagged-list? 12 'func)
	#f
)

(assert-value 
	"test 16"
	(quoted? 'sabc)
	#f
)

(assert-value 
	"test 17"
	(quoted? '(quote a bc d))
	#t
)

(assert-value
	"test 18"
	(assignment? '(set! a 3))
	#t
)


(assert-value
	"test 19"
	(assignment? '(set! abc jkl))
	#t
)


(assert-value
	"test 20"
	(assignment? '(+ 2 3))
	#f
)

