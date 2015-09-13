#lang plai

(print-only-errors true)

(require "practica3-base.rkt")

;Seccion 1

;Int -> Int -> List
;Zones - Dado el ritmo cardiaco de descanso y el máximo ritmo cardiaco de una persona se debe regresar la lista de zonas de frecuencia cardiaca
(define (zones rest max)
  (define range (- max rest))
  (define (calculate-min i rest range)
    (let ([x (+ rest (* range (+ 0.5 (* 0.1 i))))]) x))
  (define (calculate-max i rest range)
    (let ([x (- (+ rest (* range (+ 0.5 (* 0.1 (+ i 1))))) 1)]) x))
  (let ([x (resting rest (+(- (* range 0.5) 1) rest))])
    (let ([y (warm-up (calculate-min 0 rest range) (calculate-max 0 rest range))])
      (let ([z (fat-burning (calculate-min 1 rest range) (calculate-max 1 rest range))])
        (let ([w (aerobic (calculate-min 2 rest range) (calculate-max 2 rest range))])
          (let ([n (anaerobic (calculate-min 3 rest range) (calculate-max 3 rest range))])
            (let ([m (maximum (calculate-min 4 rest range) (+ (calculate-max 4 rest range) 1))])
              (list x y z w n m))))))))
                                                            
(define my-zones (zones 50 180))
;Symbol -> List -> elem
;get-zone - Dado un símbolo que es el nombre de una zona y una lista de zonas regresar el tipo de dato correspondiente
(define (get-zone sym lst)
  (cond
    [(empty? lst) (error "Empty list")]
    [(eqv? sym 'resting) (list-ref lst 0)]
    [(eqv? sym 'warm-up) (list-ref lst 1)]
    [(eqv? sym 'fat-burning) (list-ref lst 2)]
    [(eqv? sym 'aerobic) (list-ref lst 3)]
    [(eqv? sym 'anaerobic) (list-ref lst 4)]
    [(eqv? sym 'maximum) (list-ref lst 5)]
    [else (error "Symbol not in list")]))

(define (bpm->zone lst mzones)
  (cond
    [(empty? mzones) empty]))

;Seccion 2

;ninBT - Dado un árbol de tipo BTree, determinar el número de nodos internos que tiene.

(define (ninBT bt)
  (cond
    [(EmptyBT? bt) 0]
    [(and (EmptyBT? (BNode-l bt)) (EmptyBT? (BNode-r bt))) 0]
    [else (+ 1 (ninBT (BNode-l bt)) (ninBT (BNode-r bt)))]))

;nlBT - Dado un árbol de tipo BTree, determinar el número de hojas no vacías.

(define (nlBT bt)
  (cond
    [(EmptyBT? bt) 0]
    [(and (EmptyBT? (BNode-l bt)) (EmptyBT? (BNode-r bt))) 1]
    [else (+ (nlBT (BNode-l bt)) (nlBT (BNode-r bt)))]))

;nnBT - Dado un árbol de tipo BTree, determinar el número de nodos que tiene. Las hojas vacías no cuentan.

(define (nnBT bt)
  (+ (ninBT bt) (nlBT bt)))

;mapBT - Dado una función de aridad 1 y un árbol de tipo BTree, aplicar la función sobre todos los valores de
;los nodos del árbol (las funciones de aridad 1 sólo regresas números).

(define (mapBT f bt)
  (cond
    [(EmptyBT? bt) (EmptyBT)]
    [else (bnn (mapBT f (BNode-l bt)) (f (BNode-e bt)) (mapBT f (BNode-r bt)))]))

;preorderBT - Dado un árbol de tipo BTree, regresar una lista de sus elementos recorridos en preorden.

(define (preorderBT bt)
  (cond
    [(EmptyBT? bt) '()]
    [else (append (list (BNode-e bt)) (preorderBT (BNode-l bt)) (preorderBT (BNode-r bt)))]))

;inorderBT - Dado un árbol de tipo BTree, regresar una lista de sus elementos recorridos en inorden.

(define (inorderBT bt)
  (cond
    [(EmptyBT? bt) '()]
    [else (append (inorderBT (BNode-l bt)) (list (BNode-e bt)) (inorderBT (BNode-r bt)))]))

;posorderBT - Dado un árbol de tipo BTree, regresar una lista de sus elementos recorridos en post-orden.

(define (posorderBT bt)
  (cond
    [(EmptyBT? bt) '()]
    [else (append (posorderBT (BNode-l bt)) (posorderBT (BNode-r bt)) (list (BNode-e bt)))]))

    
;Tests
;Zones
(test (zones 50 180)(list
                     (resting 50 114.0)
                     (warm-up 115.0 127.0)
                     (fat-burning 128.0 140.0)
                     (aerobic 141.0 153.0)
                     (anaerobic 154.0 166.0)
                     (maximum 167.0 180.0)))
;get-zone
(test (get-zone 'resting my-zones) (resting 50 114.0))
(test (get-zone 'warm-up my-zones) (warm-up 115.0 127.0))
(test (get-zone 'fat-burning my-zones) (fat-burning 128.0 140.0))
(test (get-zone 'aerobic my-zones) (aerobic 141.0 153.0))
(test (get-zone 'maximum my-zones) (maximum 167.0 180.0))
;ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT arbol-base) 5)
;nlBT
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT arbol-base) 4)
(test (nlBT (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT)))) 2)
;nnBT
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT arbol-base) 9)
(test (nlBT (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT)))) 2)
;mapBT
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
;preorderBT
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) '(1 3 2))
(test (preorderBT (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT)))) '(1 2 4))
(test (preorderBT (BNode < (BNode < (EmptyBT) 10(EmptyBT)) 12 (BNode < (EmptyBT) 21 (EmptyBT)))) '(12 10 21))
(test (preorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(0 500 210))
;inorderBT
(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (inorderBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) '(3 1 2))
(test (inorderBT (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT)))) '(2 1 4))
(test (inorderBT (BNode < (BNode < (EmptyBT) 10(EmptyBT)) 12 (BNode < (EmptyBT) 21 (EmptyBT)))) '(10 12 21))
(test (inorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(500 0 210))
;posorderBT
(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
(test (posorderBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) '(3 2 1))
(test (posorderBT (BNode < (BNode < (EmptyBT) 2 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT)))) '(2 4 1))
(test (posorderBT (BNode < (BNode < (EmptyBT) 10(EmptyBT)) 12 (BNode < (EmptyBT) 21 (EmptyBT)))) '(10 21 12))
(test (posorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(500 210 0))