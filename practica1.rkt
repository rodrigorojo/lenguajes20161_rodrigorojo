#lang plai

;Funcion auxiliar que eleva un numero n a una potencia m usando recurcion.

(define (pow n m)
  (cond
    [(zero? m) 1]
    [else (* n (pow n (- m 1)))]))


;Funcion auxiliar mlength que calcula el tamaño de una lista de manera Recursiva.

(define(mlength l)
  (cond
    [(empty? l) 0]
    [else(+ 1 (mlength (cdr l)))]))

;Funcion Average: Dada una lista de enteros, calcula el promedio de sus elementos.

(define (average l)
  (define (suma lst)
  (cond
    [(empty? lst) 0]
    [else (+ (car lst) (suma (cdr lst)))]))
  (cond
    [(empty? l) 0]
    [else(/ (suma l)(mlength l))]))

;Función Zip, dadas dos listas, regresa una lista cuyos elementos son listas de tamaño dos

(define (zip l1 l2)
  (cond
  [(empty? l1) '()]
  [(empty? l2) '()]
  [else (cons (list (car l1) (car l2)) (zip (cdr l1) (cdr l2)))]))

;Funcion mconcat: Concatena dos listas recibidas

(define (mconcat l m)
 (cond
  [(empty? l) m]
  [(empty? m) l]
  [else (cons (car l) (mconcat (cdr l) m))]))

;Funcion mmap: Dados una funcion y una lista, aplica la funcion a los elementos de la lista.
;Casos: Si la lista es vacía, devolvemos la lista vacía
;Sino, definimos la lista de la funcion aplicada a la cabeza, con la llamada recursiva
;sobre el resto de la lista

(define (mmap funcion lista)
  (cond 
    [(empty? lista) '()]
    [else (cons (funcion(car lista)) (mmap funcion(cdr lista)))]))

;Funcion mpowerset: Define la potencia de una lista
(define (mpowerset l)
  (cond
    [(empty? l) '()]))

;Dice si algun elemento de una lista cumple una propiedad
(define (any? a l)
  (cond
    [(empty? l) #f]
    [else (or ((lambda(x) (a x)) (car l)) (any? a (cdr l)))]))

;Dice si todos los elementos de una lista cumplen una propiedad
(define (every? a l)
  (cond
    [(empty? l) #t]
    [else (and ((lambda(x) (a x)) (car l)) (every? a (cdr l)))]))


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

;Pruebas mconcat
(test (mconcat '(1 3 4) '()) '(1 3 4))
(test (mconcat '() '(1 3 4)) '(1 3 4))
(test (mconcat '(1 3 4) '(3 1 2)) '(1 3 4 3 1 2))
(test (mconcat '() '()) '())
(test (mconcat '(1 2 3 4 5 6 7 8 9) '(10 11 12 13 14 15)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
;Pruebas mmap
(test (mmap car '((1 2 3) (4 5 6) (7 8 9))) '(1 4 7))
(test (mmap add1 '(1 2 3)) '(2 3 4))
(test (mmap cdr '((1 2 3) (4 5 6) (7 8 9))) '((2 3) (5 6) (8 9)))
(test (mmap add1 '(11 32 41 2)) '(12 33 42 3))
(test (mmap car '((1 2 3 4 5)(1 2 3))) '(1 1))
;Pruebas any?
(test (any? number? '()) #f)
(test (any? number? '(a b c d 1)) #t)
(test (any? symbol? '(1 2 3 4)) #f)
(test (any? number? '(h i j k l)) #f)
(test (any? symbol? '(2 3 x 4)) #t)
;Pruebas every
(test (every? number? '()) #t)
(test (every? number? '(1 2 3)) #t)
(test (every? number? '(1 2 3 a)) #f)
(test (every? symbol? '(1 2 3 a)) #f)
(test (every? symbol? '(a b c d)) #t)



