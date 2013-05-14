

#lang scheme

(include "lisp_interpreter.scm")


; end
;-------------------------------------------------------------
;*************************************************************
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


(assert-value
	"test 21"
	(definition-variable '(define a 123))
	'a
)


(assert-value
	"test 22"
	(definition-variable '(define (func x)))
	'func
)


(assert-value
	"test 23"
	(definition-value '(define a 123))
	123
)


(assert-value
	"test 24"
	(definition-value '(define (double x) (+ x x)))
	'(lambda(x) (+ x x))
)

; this work at command line but not REPL, don't know why yet
; would come back later
; (  (eval  (definition-value '(define (double x) (+ x x))) ) 10)



