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