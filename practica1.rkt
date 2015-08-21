#lang plai

(define (pow n m)
  (cond
    [(zero? m) 1]
    [else (* n (pow n (- m 1)))]))


;Funcion auxiliar
;mlength que calcula
;el tamaño de una lista de manera
;Recursiva .

(define(mlength l)
  (cond
    [(empty? l) 0]
    [else(+ 1 (mlength (cdr l)))]))

;Funcion que calcula la suma de elementos de una lista no vacia
;Supondremos que los elementos son enteros.

(define (suma lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (suma (cdr lst)))]))

;Funcion Average: Dada una lista de enteros, calcula el promedio de sus elementos
(define (average l)
  (/ (suma l)(mlength l)))
