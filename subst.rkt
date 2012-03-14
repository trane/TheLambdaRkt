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
(require racket/date)

(define (coin-toss)
  (= 0 (modulo (date->seconds (current-date)) 2)))

(define (reducible term)
  (match term
    [(? symbol?) #f]
    [_ #t]))


(define (full-ß-reduce term)
  (match term
    [(? symbol?) (error "Cannot reduce value" term) ]
    [`(λ (,v) ,v) term]
    [`(λ (,v) ,b) (if (reducible b) (if (coin-toss) `(λ (,v) ,(full-ß-reduce b)) (a-conv `(λ (,v) ,(full-ß-reduce b)))) term)]
    [`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce term)]
    [`((λ (,v1) ,b1), e) (if (reducible e) (full-ß-reduce e) (subst `(λ (,e) ,b1) v1 e))]
    [`(,f ,e) (full-ß-reduce f)]))


(define (a-conv term)
  (match term
    [(? symbol?) term]
    [`(λ (,v1) ,b) (define v2 (gensym v1)) `(λ (,v2) ,(a-conv (subst b v1 v2)))]
    [`(,f ,e) `( ,(a-conv f) ,(a-conv e))]))


(define (cbv-reduce term)
  (match term
    [(? symbol?) (error "Cannot reduce value" term)]
    [`(λ (,v) ,b) term]
    [`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce term)]
    [`((λ (,v1), b1), e) (cbv-reduce `(,(car term),(cbv-reduce e)))]
    ;[`((λ (,v1), b1), e) (cbv-reduce (if (match e (? symbol?)) (subst term b1 e) (cbv-reduce e)))]
    [`(,f ,e) `(,(cbv-reduce f) ,e)]

))

(define (cbvr term)
  (match term
    ; x->x
    [(? symbol?) term]
    ; (λv.b) -> (λv.b)
    [`(λ (,v) ,b) term]
    ; if (e1->(λv.b) e2->e2'  e[e2'/x]->e'): then (e1 e2) -> e'
    [`((λ (,v1) ,b1)(λ (,v2) (,b2))) (ß-reduce term)]
    [`((λ (,v) ,b) ,e2) (subst b v (cbvr e2))]
    ; if (e1->e1' != (λv.b)  e2->e2'): then (e1 e2) -> (e1' e2')
    [`(,e1 ,e2) `(,(cbvr e1) ,e2)])
  )
(define (eval fun term)
  (printf "~s\n" term)
  (let ([term* (fun term)])
    (if (equal? term term*)
        term
        (eval fun term*))))

(define (evaln term)
  (printf "~s\n" term)
  (let ([term* (cbvr term)])
    (if (equal? term term*)
        term
        (evaln term*))))

(define (cbnr term)
  (match term
    ; x->x
    [(? symbol?) term]
    ; (λv.b) -> (λv.b)
    [`(λ(,v),b) term]
    ; if (e1->(λv.b) e[e2/x]->e'): then (e1 e2) -> e'
    [`((λ(,v),b),e2) (subst b v e2)]
    ; if (e1->e1' != (λv.b)): then (e1 e2) -> (e1' e2)
    [`(,e1,e2) `(,(cbnr e1) ,e2)]))

(define (cbn-reduce term)
  (match term
    [(? symbol?) (error "Cannot reduce value" term) ]
    [`(λ (,v) ,v) term]
    [`(λ (,v) ,b) (if (reducible b) (if (coin-toss) `(λ (,v) ,(cbn-reduce b)) (a-conv `(λ (,v) ,(cbn-reduce b)))) term)]
    [`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce term)]
    [`((λ (,v1) ,b1), e) (if (reducible e) (cbn-reduce e) (subst term v1 e))]
    [`(,f ,e) (cbn-reduce f)]))

(define (isLambda term)
  (match term
    [`(λ (,v) ,b) #t]
    [`(λ (,v) ,v) #t]
    [_ #f]))

(define (cbn term)
  (match term
    [(? symbol?) term]
    [`(λ (,v) ,b) (subst term v b)]
    ;[`((λ (,v1) ,b1)(λ (,v2) ,b2)) (ß-reduce (a-conv term))];(define l (a-conv (car term))) (subst l (cdr l) (cdr term))]
    ;[`((λ (,v) ,b) ,e) (define l (a-conv (car term))) (subst l (cdr l) e)]
    ;[`(,f ,e) `(,f ,e)]))
    [`(,f ,e) (if (isLambda (cbn f)) (cbn (subst (cbn f) (cdr (cbn f)) e)) `(,(cbn f) ,e))]))