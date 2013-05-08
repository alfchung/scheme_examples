
#lang scheme

; if an expression is a number or a string
; it is self-evaluating
(define (self-evaluating? exp)
    (cond ((number? exp) #t)
          ((string? exp) #t)
          (else #f)))




; test type check, judge is a function that checks if the exp is some type(s)
(define (assert title judge exp)
    (cond ( (equal? (judge exp) #t) (string-append title " " "Pass") )
          (else (string-append title " " "Fail"))
    )
)

(assert "test 1" self-evaluating? 23)
(assert "test 2" self-evaluating? 55)
(assert "test 3" self-evaluating? 5.5)
(assert "test 4" self-evaluating? "Brown University")
(assert "test 5" self-evaluating? "Computer Science")


