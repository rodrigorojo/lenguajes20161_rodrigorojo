#lang plai

(require "practica4-base.rkt")

;(print-only-errors true)

;(define (desugar expr)
 ; (cond
 ;   [(numS? expr) (num (numS-n expr))]
  ;  [(idS? expr) (id (idS-name expr))]
   ; [(binopS? expr) (binop (binopS-f expr) (desugar (binopS-l expr)) (desugar (binopS-r expr)))]
    ;[(funS? expr) (fun (funS-params expr) (desugar (funS-body expr)))]
    ;[(appS? expr) (app (desugar (appS-fun expr)) (map (lambda (x)
     ;                                                   (desugar x))
      ;                                                appS-args))]
    
    ;))


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

(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
  ;; Implementar interp
  (error 'interp "Not implemented"))

(define (rinterp expr)
  (interp expr (mtSub)))

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
