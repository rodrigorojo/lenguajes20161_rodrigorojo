#lang plai

;Arreglo
(define-type Array
  [MArray (n number?) (l list?)])

;Lista
(define-type MList
  [MEmpty]
  [MCons (n any?) (l MList?)])

;Predicado Any. Ayuda a poder construir MList con cualquier valor
(define (any? x) #t)

;Arboles
(define-type NTree
  [TLEmpty]
  [NodeN (e any?) (l list?)])


;Position
(define-type Position
  [2D-Point (m number?) (n number?)])

;Figure
(define-type Figure
  [Circle (p 2D-Point?) (n number?)]
  [Square  (p 2D-Point?) (l number?)]
  [Rectangle (p 2D-Point?) (b number?) (h number?)])