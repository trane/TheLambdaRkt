#lang racket
(require racket/match)

(define (subst term var value)
  (match term
    ; [x -> s]y -> y = x ? s : y
    [(? symbol?) (if (eq? term var) value term)]
    ; [x -> s]λx.b -> λx.b
    ; [x -> s]λy.b -> λy.[x -> s]b
    [`(λ (,v) ,body) (if (eq? v var) term `(λ (,v) ,(subst body var value)))]
    ; [x -> s](t1 t2) -> [x -> s]t1 [x -> s]t2
    [`(,f ,a) `(,(subst f var value) ,(subst a var value))]))

;(ß-reduce `((λ(x) (x x))(λ(z) u)))
;'(((λ (z) u)) ((λ (z) u)))
;(ß-reduce `((λ(x)(λ(y) (y x)))(λ(z)u)))
;'(λ (y) (y ((λ (z) u))))
(define (ß-reduce term)
  (match term
    [`((λ (,v1) ,b1) ,(and rhs `(λ (,v2) ,b2))) (subst b1 v1 rhs)]))

;> (full-ß-reduce `((λ(y) (y a))((λ(x)x)(λ(z)((λ(u)u)z)))))
;'((λ (z) ((λ (u) u) z)))
;> (full-ß-reduce `((λ(x)(x x))(λ(x)(x x))))
;'(((λ (x) (x x))) ((λ (x) (x x))))
(define (full-ß-reduce term)
  (match term
    [(? symbol?) (error "Cannot reduce value" term )]
    [`(λ (,v) ,v) (term)]
    [`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce term)]
    [`((λ (,v1) ,b1), e) (full-ß-reduce e)]))

(define (alpha-conversion term)
  (match term
    ; if λx.x -> λy.y
    [`(λ (,v) ,v) (define v1 (gensym 'v)) `(λ (,v1) ,v1)]
    ; if λx.λx.x -> λy.λx.x
    [`(λ (,v1) ,b1) (define v2 (gensym 'v1)) 
                    (if (eq? (car b1) 'λ)
                        ; if λx.λx.x -> λx'.λx''.x''
                        `(λ (,v2) ,(alpha-conversion b1))
                        ; if λx.xy -> λx'.x'y
                        `(λ (,v2) ,(subst b1 v1 v2)))]
    ))

(define (a-conv term)
  (match term
    [(? symbol?) term]
    [`(λ (,v1) ,b) (define v2 (gensym v1)) `(λ (,v2) ,(a-conv (subst b v1 v2)))]
    [`(,f ,e) `( ,(a-conv f) ,(a-conv e))]))


(define (cbv-reduce term)
  (match term
    [(? symbol?) (error "Cannot reduce value" term)]
    [`(λ (,v) ,v) (term)]
    [`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce term)]
    [`((λ (,v1), b1), e) (cbv-reduce e)]
    [`(,f ,e) (full-ß-reduce f)]))