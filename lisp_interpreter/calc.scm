
#lang scheme

; yin wang's code
(define (calc exp)
        (match exp                                ; 匹配表达式的两种情况
            [(? number? x) x]                       ; 是数字，直接返回
            [`(,op ,e1 ,e2)                         ; 匹配并且提取出操作符 op 和两个操作数 e1, e2
                (let (  [v1 (calc e1)]                   ; 递归调用 calc 自己，得到 e1 的值
                        [v2 (calc e2)] )                  ; 递归调用 calc 自己，得到 e2 的值
                    (match op                            ; 分支：处理操作符 op 的 4 种情况
                        ['+ (+ v1 v2)]                     ; 如果是加号，输出结果为 (+ v1 v2)
                        ['- (- v1 v2)]                     ; 如果是减号，乘号，除号，相似的处理
                        ['* (* v1 v2)]
                        ['/ (/ v1 v2)]
                    )
                )
            ]
        )
    
)


;-----------------------------------------
; tests                                  |
; ----------------------------------------

;match number
(define (is-num? exp)
            (match exp 
                [(? number? x) "number"]
            )
)

(is-num? 23)

(define (is-exp? exp)
    (match exp
        [`(,op, a, b) "expression"] ; this ` is the one in tilda
    )
)
 
(is-exp? '(+ 2 3))
(is-exp? '(+ 2 (+ 2 3)))



(define (calc2 exp)
    (match exp                                
      [ (? number? x)  x ]                    
    )
)

(calc2 2)
(calc2 100)

(define (is-string? exp)
    (match exp
        [(? string? x) x]
    )
)

(is-string? "brown")
(is-string? "cit")




;-----------------------------------------
; tests 2                                |
; ----------------------------------------


(define (reconize exp)
    (match exp
        [(? number? ) "a number" ]
        [`(,op, a, b ) "an expression"]
    )
)


(reconize 23)
(reconize '(+ 2 3))
(reconize '(+ (+ 2 8) 3))




(define (cal3 exp)
    (match exp
        [(? number? ) exp]
        [`(,op, a, b ) 
            (let  ( 
                    [v1 (cal3 a)] 
                    [v2 (cal3 b)] 
                  )
                  (match op
                        ['+ (+ v1 v2)]
                        ['- (- v1 v2)]
                        ['/ (/ v1 v2)]
                        ['* (* v1 v2)]
                  )
            )
        ]
    )
)

(cal3 '(+ (* 2 3) (+ 2 2)))
(cal3 '(+ (* 2 3) (+ 2 (+ 1 2))))
;(cal3 '(+ 1 3 4))
;(cal3 "(+ (* 2 3) (+ 2 2))")





