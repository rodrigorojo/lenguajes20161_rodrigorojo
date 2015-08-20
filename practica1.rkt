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
