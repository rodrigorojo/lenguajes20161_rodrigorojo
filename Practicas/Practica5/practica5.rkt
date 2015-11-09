#lang plai
(print-only-errors true)

(require "practica5-base.rkt")


(define (desugar expr)
  (type-case RCFAELS expr
    [MEmptyS () (MEmpty)]
    [numS (n) (num n)]
    [idS (x) (id x)]
    [boolS (b) (bool b)]
    [ConsS (h t) (Cons (desugar h) (desugar t))]
    [IfS (c t e) (If (desugar c) (desugar t) (desugar e))]
    [Equal?S (x y) (Equal? (desugar x) (desugar y))]
    [opS (f o) (op f (desugar o))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]
    [withS (bindings body) (app (fun (map (lambda (bind) 
                                            (bind-name bind)) bindings)
                                            (desugar body))
                                (map (lambda (bind)
                                            (desugar (bind-val bind))) bindings))]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (map (lambda (arg) (desugar arg)) args))]
    [with*S (bindings body) (matryoshka bindings body)]))

(define (matryoshka bindings body)
  (cond
    [(empty? bindings) (desugar body)]
    [else (app (fun (list (bind-name (car bindings)))
                    (matryoshka (cdr bindings) body))
               (list (desugar (bind-val (car bindings)))))]))


(test (desugar  (numS 3)) (num 3))
(test (desugar (idS 'x)) (id 'x))
(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
(test (desugar (parse '{with {{x (+ 5 5)}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5))) ))

(define (cparse sexp)
  (desugar (parse sexp)))

(define (interp expr env)
  ;auxiliar para sacar el ambiente para app
  (define (aux params args env)
  (cond
    [(or (empty? params) (empty? args)) env]
    [else (aux (cdr params) (cdr args) (aSub (car params) (interp (car args) env) env))]))
  ;auxiliar checa si un argumento esta en el ambiente
  (define (check arg env)
  (cond
    [(num? arg) #t]
    [(binop? arg) (and (check (binop-l arg) env) (check (binop-r arg) env))]
    [(id? arg) (type-case Env env
                  [mtSub () #f]
                  [aSub (name value e)
                        (if (symbol=? name (id-name arg))
                            #t (check arg e))])]
    [(fun? arg) #t]
    [(app? arg) #t]
    [else (#f)]))
  ;auxiliar checa si una lista argumentos estan en el ambiente
  (define (checkAll args env)
  (cond
    [(empty? args) #t]
    [else (and (check (car args) env) (checkAll (cdr args) env))]))
  ;este es interp
 (type-case RCFAEL expr
   [MEmpty () (EmptyV)] 
   [num (n) (numV n)]
   [id (x) (lookup x env)]
   [bool (b) (boolV b)]
   [Cons (h t) (mlistV (interp h env) (interp t env))]
   [If (c t e) (if (equal? (interp c) (boolV true))
                   (interp t) (interp e))]
   [Equal? (x y) (cond
                   [(and (num? x) (num? y)) (if (= (num-n x) (num-n y)) (boolV true) (boolV false))]
                   [(and (bool? x) (bool? y)) (if (equal? (bool-b x) (bool-b y)) (boolV true) (boolV false))]
                   ;[(and (mlist? x) (mlist? y)) ()
                   [else (boolV false)])]
   [fun (params f) (closureV params f env)]
   [app (fun-expr arg-expr) 
         (local ([define fun-val (interp fun-expr env)])
           (if (checkAll arg-expr env)
               (interp (closureV-body fun-val)
                       (aux (closureV-param fun-val) arg-expr (closureV-env fun-val)))
               (error "x symbol is not in the env")))]
   ;este op no es correcto
   [op (f a) (cond
                 [(numV? a) (let ((r (f (numV-n a))))
                   (if (boolean? r)
                       (boolV r)
                       (numV r)))]
                 [(boolV? a) (let ((r (f (boolV-b a))))
                   (if (boolean? r)
                       (boolV r)
                       (numV r)))])]
               
   [binop (op x y) (if (or (equal? op +) (equal? op -) (equal? op *) (equal? op /))
                       (numV (op (numV-n (interp x env)) (numV-n (interp y env))))
                       (boolV (op (numV-n (interp x env)) (numV-n (interp y env)))))]))

(define (rinterp expr)
  (interp expr (mtSub)))
   
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
(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "x symbol is not in the env")

;nuevas pruebas
(test (rinterp (cparse true)) (boolV true))
(test (rinterp (cparse '(equal? 4 5))) (boolV false))
(test (rinterp (cparse '(< 3 4))) (boolV true))
