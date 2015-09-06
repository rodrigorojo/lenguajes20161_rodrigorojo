#lang plai

;Sección 1

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

;Sección 2

;MArray2MList - convierte un arreglo de tipo MArray a una lista de tipo MList

(define (MArray2MList marr)
  ;Función auxiliar que regresa un arreglo sin la cabeza
  (define (cdrMarray marr)
  (MArray (- (MArray-n marr) 1) (cdr (MArray-l marr))))
  ; esta es MArray2MList
  (cond
    [(empty? (MArray-l marr)) (MEmpty)]
    [else (MCons (car (MArray-l marr)) (MArray2MList (cdrMarray marr)))]))

;LengthML- Calcula la longitud de 1 MList

(define (lengthML mlst)
  (cond
  [(MEmpty? mlst) 0]
  [else (+ 1 (lengthML (MCons-l mlst)))])) ;Con "-" haces referencia a esa seccion del constructor (en este caso el resto de mlst)

;concatML- Concatena 2 MList
(define (concatML ls m)
  (cond
  [(MEmpty? ls) m]
  [(MEmpty? m) ls]
  [else (MCons (MCons-n ls)(concatML (MCons-l ls) m))]))

;mapML - Dada una lista de tipo MLista y una funcion de aridad 1, regresar una lista de tipo MLista con la aplicación de la funcion a cada uno de los elementos de la lista original
(define (mapML funcion lista)
  (cond
    [(MEmpty? lista) (MEmpty)]
    [else (MCons (funcion(MCons-n lista)) (mapML funcion(MCons-l lista)))]))

;filterML - Dada una lista de tipo MLista y un predicado de un argumento, regresar una lista de tipo MLista
;sin los elementos que al aplicar el predicado, regresa falso

(define (filterML f l)
  (cond
    [(MEmpty? l) (MEmpty)]
    [else (if (f(MCons-n l))
              (MCons (MCons-n l) (filterML f (MCons-l l)))
              (filterML f (MCons-l l)))]))

;Tipo coordenadas

(define-type Coordinates
  [GPS (lat number?)
       (long number?)])

;tipo locación

(define-type Location
  [building (name string?)
            (loc GPS?)])

;; Coordenadas GPS

(define gps-satelite (GPS 19.510482 -99.23411900000002))
(define gps-ciencias (GPS 19.3239411016 -99.179806709))
(define gps-zocalo (GPS 19.432721893261117 -99.13332939147949))
(define gps-perisur (GPS 19.304135 -99.19001000000003))

(define plaza-satelite (building "Plaza Satelite" gps-satelite))
(define ciencias (building "Facultad de Ciencias" gps-ciencias))
(define zocalo (building "Zocalo" gps-zocalo))
(define plaza-perisur (building "Plaza Perisur" gps-perisur))

(define plazas (MCons plaza-satelite (MCons plaza-perisur (MEmpty))))

;gps-coordinates dada una lista de locaciones regresa una lista con su ubicación gps

(define (gps-coordinates lst)
  (cond
    [(MEmpty? lst) (MEmpty)]
    [else (MCons (building-loc (MCons-n lst)) (gps-coordinates (MCons-l lst)))]))

;Test
;MArray2MList
(test (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 2 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 5 '(15 4 35 12 1))) (MCons 15 (MCons 4 (MCons 35 (MCons 12 (MCons 1 (MEmpty)))))))
(test (MArray2MList (MArray 4 '("esta" "es" "una" "prueba"))) (MCons "esta" (MCons "es" (MCons "una" (MCons "prueba" (MEmpty))))))
;LengthML
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MEmpty))))) 3)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MCons 4(MEmpty)))))) 4)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MCons 4 (MCons 5(MEmpty))))))) 5)
;concatML
(test (concatML (MEmpty)(MCons 3 (MEmpty))) (MCons 3 (MEmpty)))
(test (concatML (MCons 3 (MEmpty)) (MEmpty)) (MCons 3 (MEmpty)))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 1(MEmpty)) (MCons 2(MEmpty))) (MCons 1(MCons 2(MEmpty))))
(test (concatML (MCons 10(MEmpty)) (MCons 20(MCons 30(MEmpty)))) (MCons 10(MCons 20(MCons 30(MEmpty)))))
;mapML
(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test  (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML add1 (MCons 1(MEmpty))) (MCons 2(MEmpty)))
(test (mapML (lambda (x) (+ x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 20 (MCons 6 (MEmpty))))
(test (mapML (lambda (x) (+ x 2)) (MCons 2 (MCons 3 (MEmpty)))) (MCons 4 (MCons 5 (MEmpty))))
;filterML
;gps-coordinates
(test (gps-coordinates (MEmpty)) (MEmpty))
(test (gps-coordinates plazas) (MCons (GPS 19.510482 -99.23411900000002) (MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))
(test (gps-coordinates (MCons ciencias (MEmpty))) (MCons (GPS 19.3239411016 -99.179806709) (MEmpty)))
(test (gps-coordinates (MCons ciencias (MCons zocalo (MEmpty)))) (MCons (GPS 19.3239411016 -99.179806709) (MCons (GPS 19.432721893261117 -99.13332939147949) (MEmpty))))
(test (gps-coordinates (MCons plaza-satelite (MCons zocalo (MCons ciencias (MEmpty)))))
      (MCons
       (GPS 19.510482 -99.23411900000002)
       (MCons (GPS 19.432721893261117 -99.13332939147949) (MCons (GPS 19.3239411016 -99.179806709) (MEmpty)))))