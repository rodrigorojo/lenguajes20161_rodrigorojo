#lang plai

(define-type Binding
  [bind (name symbol?) (val RCFAELS?)])

(define-type RCFAELS
  [idS (name symbol?)]
  [numS (n number?)]
  [boolS (b boolean?)]
  [mlistS (l MList?)]
  [IfS (Cond RCFAELS?) (Then RCFAELS?) (Else RCFAELS?)]
  [Equal?S (x RCFAELS?) (y RCFAELS?)]
  [opS (f procedure?) (o RCFAELS?)]
  [binopS (f procedure?) (l RCFAELS?) (r RCFAELS?)]
  [withS (bindings (listof bind?))
         (body RCFAELS?)]
  [with*S (bindings (listof bind?))
          (body RCFAELS?)]
  [funS (params (listof symbol?))
        (body RCFAELS?)]
  [appS (fun RCFAELS?)
        (args (listof RCFAELS?))])

;(define-type RCFAEL
 ; [num (n number?)]
  ;[id (name symbol?)]
 ; [fun (params (listof symbol?))
  ;     (body RCFAEL?)]
  ;[app (fun RCFAEL?)
   ;    (args (listof RCFAEL?))]
  ;[binop (f procedure?)
   ;      (l RCFAEL?)
    ;     (r RCFAEL?)])

(define-type RCFAEL
  [id (name symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [mlist (l MList?)]
  [If (Cond RCFAEL?) (Then RCFAEL?) (Else RCFAEL?)]
  [Equal? (x RCFAEL?) (y RCFAEL?)]
  [op (f procedure?) (o RCFAEL?)]
  [binop (f procedure?) (l RCFAEL?) (r RCFAEL?)]
)
(define (any? x) #t)
(define-type MList
  [Empty]
  [Cons (n any?) (l MList?)])

(define-type RCFAEL-Value
  [numV (n number?)]
  [boolV (b boolean?)]
  [mlistV (ml MList?)]
  [closureV (param (listof symbol?))
            (body RCFAEL?)
            (env Env?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) 
        (value RCFAEL-Value?) 
        (env Env?)])

; FUNCIONES AUXILIARES

;; A::= <number>|<symbol>|listof(<A>)
;; B::= (list <symbol> <A>)
;; parse-bindings: listof(B) -> listof(bind?)
;; "Parsea" la lista de bindings lst en sintaxis concreta
;; mientras revisa la lista de id's en busca de repetidos.
;; (define (parse-bindings lst) 
(define (parse-bindings lst allow)
  (let ([bindRep (buscaRepetido lst (lambda (e1 e2) (symbol=? (car e1) (car e2))))])
    (if (or (boolean? bindRep) allow)
        (map (lambda (b) (bind (car b) (parse (cadr b)))) lst)
        (error 'parse-bindings (string-append "El id " (symbol->string (car bindRep)) " está repetido")))))

(define (elige-binop s)
  (case s
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(<) <]
    [(>) >]
    [(<=) <=]
    [(>=) >=]
    [(and) (lambda(and) and)]
    [(or) (lambda(or) or)]
    ))

(define (elige-op x)
  (case x
    [(inc) (lambda(inc) inc)]
    [(dec) (lambda(dec) dec)]
    [(zero?) (lambda(zero) zero)]
    [(num?) (lambda(num?) num?)]
    [(neg) (lambda (neg) neg)]
    [(bool) (lambda(bool) bool)]
    [(first) (lambda(first) first)]
    [(rest) (lambda(rest) rest)]
    [(empty) (lambda(empty) empty)]
    [(list) (lambda(list) list)]))

  
;; buscaRepetido: listof(X) (X X -> boolean) -> X
;; Dada una lista, busca repeticiones dentro de la misma
;; usando el criterio comp. Regresa el primer elemento repetido
;; o falso eoc.
;; (define (buscaRepetido l comp) 
(define (buscaRepetido l comp) 
  (cond
    [(empty? l) #f]
    [(member? (car l) (cdr l) comp) (car l)]
    [else (buscaRepetido (cdr l) comp)]))

;; member?: X listof(Y) (X Y -> boolean) -> boolean
;; Determina si x está en l usando "comparador" para
;; comparar x con cada elemento de la lista.
;; (define (member? x l comparador)
(define (member? x l comparador)
  (cond
    [(empty? l) #f]
    [(comparador (car l) x) #t]
    [else (member? x (cdr l) comparador)]))

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> RCFAELS
(define (parse sexp)
  (cond
    [(symbol? sexp) (idS sexp)]
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(list? sexp)
     (case (car sexp)
       [(if) (IfS (parse (cadr sexp)) (parse(caddr sexp))(parse(cadddr sexp)))]
       [(list) (mlistS (parse (cadr sexp)))]
       [(equal?) (Equal?S (parse (cadr sexp)) (parse (caddr sexp)))]
       [(with) (withS (parse-bindings (cadr sexp) #f) (parse (caddr sexp)))]
       [(with*) (with*S (parse-bindings (cadr sexp) #t) (parse (caddr sexp)))]
       [(fun) (funS (cadr sexp) (parse (caddr sexp)))]
       [(+ - / * > < <= >= and or) (binopS (elige-binop (car sexp)) (parse (cadr sexp)) (parse (caddr sexp)))]
       [(inc dec zero? num? neg bool? first rest empty? list?) (opS (elige-op (cadr sexp)) (parse (caddr sexp)))]
       [else (appS (parse (car sexp)) (map parse (cdr sexp)))])]))
