#lang plai

;Funcion auxiliar que eleva un numero n a una potencia m usando recurcion

(define (pow n m)
  (cond
    [(zero? m) 1]
    [else (* n (pow n (- m 1)))]))


;Funcion auxiliar mlength que calculael tamaño de una lista de manera Recursiva .

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
  (cond
    [(empty? l) 0]
    [else(/ (suma l)(mlength l))]))
;Función Zip:
(define (zip l1 l2)
  (cond
  [(empty? l1) '()]
  [(empty? l2) '()]
  [else (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

;Función auxiliar que genera una lista con los dos elementos dados

;Funcion mconcat: Concatena dos listas recibidas

(define (mconcat l m)
 (cond
  [(empty? l) m]
  [(empty? m) l]
  [else (cons (car l) (mconcat (cdr l) m))]
  ) 
 )




;Pruebas
;Power
(test (pow 3 0) 1)
(test (pow 0 3) 0)
(test (pow 4 4) 256)
(test (pow 5 1) 5)
(test (pow 8 3) 512)
;Average
(test (average '()) 0)
(test (average '(9)) 9)
(test (average '(5 8 3 9 4 1)) 5)
(test (average '(2 4 3)) 3)
(test (average '(2 4 3)) 3)
(test (average '(6 4 3 8 9)) 6)
;Zip
(test (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6) ))
(test (zip '(1 2) '()) '())
(test (zip '() '(6 9)) '())
(test (zip '(7 5 3) '(9 6 4)) '((7 9) (5 6) (3 4)))
(test (zip '(1 5 3) '(3 4)) '((1 3) (5 4)))
;Puerbas para funcion auxiliares
;junta
;(test (junta 1 3) '(1 3))
;(test (junta 7 8) '(7 8))
;(test (junta 3 2) '(3 2))
;Suma
(test (suma '(1 2 3 4 5)) 15)
(test (suma '(1 1 1)) 3)
(test (suma '(15 6)) 21)
(test (suma '(1)) 1)
(test (suma '(1 2 3 4 5 6 7 8 9 10)) 55)

;Pruebas mconcat
(test (mconcat '(1 3 4) '()) '(1 3 4))
(test (mconcat '() '(1 3 4)) '(1 3 4))
(test (mconcat '(1 3 4) '(3 1 2)) '(1 3 4 3 1 2))
(test (mconcat '() '()) '())
(test (mconcat '(1 2 3 4 5 6 7 8 9) '(10 11 12 13 14 15)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))