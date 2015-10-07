#lang plai
;(print-only-errors true)

(require "practica4-base.rkt")


(define (desugar expr)
  ;Funciones auxiliares: getName-Val nos ayudan a poder trabajar con el with*S
  (define (getName lst)
  (cond
    [(empty? lst) empty]
    [else  (type-case Binding (car lst)
        [bind (name val) (cons name (getName (cdr lst)))])]))

  (define (getVal lst)
    (cond
      [(empty? lst) empty]
      [else (type-case Binding (car lst)
          [bind (name val) (cons (desugar val) (getVal (cdr lst)))])]))
  
  (type-case FAES expr
    [numS (n) (num n)]
    [idS (x) (id x)]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body) (app (fun (map (lambda (bind) 
                                            (bind-name bind)) bindings)
                                            (desugar body))
                                (map (lambda (bind)
                                            (desugar (bind-val bind))) bindings))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [with*S (bindings body) (app (fun (getName bindings) (desugar body)) (getVal bindings))]))



(test (desugar  (numS 3)) (num 3))
(test (desugar (idS 'x)) (id 'x))
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x (+ 5 5)}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))
(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "x symbol is not in the env")

(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
 (type-case FAE expr
   [num (n) (numV n)]
   [id (x) (lookup x env)]
   [fun (params f) (closureV params f env)]
   [app (f a)
        (local ([define v (interp f env)])
          (map (lambda (x)
                 (interp x env))
               a))]            
   [binop (op x y) (numV (op (numV-n (interp x env)) (numV-n (interp y env))))]))


(define (rinterp expr)
  (interp expr (mtSub)))

;Funcion auxiliar subst.
(define (subst expr sub-id val)
  (type-case FAE expr
    [num (n) expr]
    [binop (o l r) (o (subst l sub-id val)
                    (subst r sub-id val))]
    [id (v) (if (symbol=? v sub-id) val expr)]
    [fun (p b) (if (symbol=? p sub-id)
                   expr
                   (fun p (subst b sub-id val)))]
    [app (f e) (app (subst f sub-id val)
                    (subst e sub-id val))]))

;Funcion auxiliar: dado un nombre de variable y el ambiente, buscamos la existencia del primero en el segundo.
;Nos auxiliamos de la definicion de Env.
(define (lookup name env)
  (type-case Env env
   [mtSub () (error 'lookup "Variable libre" (symbol->string name))]
   [aSub (nombre valor ambiente)(cond [(symbol=? nombre name) valor]
                                      [else (lookup name ambiente)])]))

(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {+ x x}})) (numV 20))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))
