#lang plai
;Sección 1
(print-only-errors true)

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

;setValueA - Dado un arreglo de tipo Array, una posicion y un valor numerico v, regresar otro arreglo con el valor v intercambiado en la posicion indicada del arreglo original.
(define (setValueA ar x y)
  ;Funcion auxiliar que dada una lista intercambia un valor en cierto indice por un valor dado.
  (define (intercambia lst x y)
    (cond
      [(zero? x) (cons y (cdr lst))]
      [else(cons (car lst) (intercambia (cdr lst) (- x 1) y))]))
  ;Esta es setValueA
  (cond
    [(>= x (MArray-n ar)) (error "setValueA: Out of bounds")]
    [else(MArray (MArray-n ar) (intercambia (MArray-l ar) x y))]))


;MArray2MList - convierte un arreglo de tipo MArray a una lista de tipo MList

(define (MArray2MList marr)
  ;Función auxiliar que regresa un arreglo sin la cabeza
  (define (cdrMarray marr)
  (MArray (- (MArray-n marr) 1) (cdr (MArray-l marr))))
  ; esta es MArray2MList
  (cond
    [(empty? (MArray-l marr)) (MEmpty)]
    [else (MCons (car (MArray-l marr)) (MArray2MList (cdrMarray marr)))]))

;printML - imprime una MList en formato "[v1,v2,v3]"
;Version 1- imprime MList anidadas
(define (printML mlst)
  ;Auxiliar que cierra la escritura de la lista
  (define (printML2 mls)
    (cond
      [(MEmpty?  mls) (~a "]")]
      [else (string-append  ", " (~a (MCons-n mls)) (printML2 (MCons-l mls)))]))  
  (cond
    [(MEmpty?  mlst) (~a "[]" )]
    [else (string-append  "[" (if (MList? (MCons-n mlst)) (printML (MCons-n mlst)) (~a (MCons-n mlst)))
                          (~a (printML2 (MCons-l mlst))))]))

;(printML (MCons (MCons (MCons 1 (MCons 2 (MEmpty)))  (MCons 1 (MCons 2 (MEmpty)))) (MCons 1 (MCons 2 (MEmpty)))))
;concatML- Concatena 2 MList
(define (concatML ls m)
  (cond
  [(MEmpty? ls) m]
  [(MEmpty? m) ls]
  [else (MCons (MCons-n ls)(concatML (MCons-l ls) m))]))

;LengthML- Calcula la longitud de 1 MList

(define (lengthML mlst)
  (cond
  [(MEmpty? mlst) 0]
  [else (+ 1 (lengthML (MCons-l mlst)))])) ;Con "-" haces referencia a esa seccion del constructor (en este caso el resto de mlst)

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

;haversine - Dados dos valores de tipo GPS calcular su distancia usando la formula de haversine.

(define (haversine gps1 gps2)
  (* 12742 (asin (sqrt (+ (expt (sin (/ (- (degrees->radians (GPS-lat gps2)) (degrees->radians (GPS-lat gps1))) 2)) 2)
                              (* (* (cos (degrees->radians (GPS-lat gps1))) (cos (degrees->radians (GPS-lat gps2))))
                                 (expt (sin (/ (- (degrees->radians (GPS-long gps2)) (degrees->radians (GPS-long gps1))) 2)) 2)))))))

;gps-coordinates dada una lista de locaciones regresa una lista con su ubicación gps

(define (gps-coordinates lst)
  (cond
    [(MEmpty? lst) (MEmpty)]
    [else (MCons (building-loc (MCons-n lst)) (gps-coordinates (MCons-l lst)))]))

;closest-building Dado b un valor de tipo building y una lista de tipo MList de buildings, regresar el edificio mas cercano a b.

(define (closest-building b lst)
  (cond
    [(MEmpty? lst) +inf.0]
    [else (min (haversine (building-loc b) (building-loc (MCons-n lst))) (closest-building b (MCons-l lst)))]))

;buildings-at-distance

(define (buildings-at-distance b lst d)
  (cond
    [(MEmpty? lst) (MEmpty)]
    [else (if (<= (haversine (building-loc b) (building-loc (MCons-n lst))) d)
              (MCons (MCons-n lst) (buildings-at-distance b (MCons-l lst) d))
              (buildings-at-distance b (MCons-l lst) d))]))

;area devuelve el area de una figura

(define (area fig)
  (cond
    [(Circle? fig) (* pi (expt (Circle-n fig) 2))]
    [(Square? fig) (expt (Square-l fig) 2)]
    [(Rectangle? fig) (* (Rectangle-b fig) (Rectangle-h fig))]))

;in-figure? devuelve si un punto esta dentro de una figura 

(define (in-figure? fig p)
  (cond
    [(Circle? fig) (>= (expt (Circle-n fig) 2) (+ (expt (- (2D-Point-m (Circle-p fig)) (2D-Point-m p)) 2)
                                                  (expt (- (2D-Point-n (Circle-p fig)) (2D-Point-n p)) 2)))]
    [(Square? fig) (and (>= (2D-Point-m p) (2D-Point-m (Square-p fig))) (>= (2D-Point-n p) (2D-Point-n (Square-p fig)))
                        (<= (2D-Point-m p) (+ (2D-Point-m (Square-p fig)) (Square-l fig))) (<= (2D-Point-n p) (+ (2D-Point-n (Square-p fig)) (Square-l fig))))]
    [(Rectangle? fig) (and (>= (2D-Point-m p) (2D-Point-m (Rectangle-p fig))) (>= (2D-Point-n p) (2D-Point-n (Rectangle-p fig)))
                           (<= (2D-Point-m p) (+ (2D-Point-m (Rectangle-p fig)) (Rectangle-b fig)))
                           (<= (2D-Point-n p) (+ (2D-Point-n (Rectangle-p fig)) (Rectangle-h fig))))]))

;Test
;setValueA
(test (setValueA (MArray 5 '(0 1 2 3 4)) 1 6) (MArray 5 '(0 6 2 3 4)))
(test (setValueA (MArray 4 '(0 1 2 3)) 2 3) (MArray 4 '(0 1 3 3)))
(test (setValueA (MArray 3 '(0 1 2)) 1 9) (MArray 3 '(0 9 2)))
(test (setValueA (MArray 2 '(0 1)) 0 6) (MArray 2 '(6 1)))
(test (setValueA (MArray 1 '(0)) 0 2) (MArray 1 '(2)))
;MArray2MList
(test (MArray2MList (MArray 3 '(1 2 3))) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
(test (MArray2MList (MArray 0 '())) (MEmpty))
(test (MArray2MList (MArray 2 '("a" "b"))) (MCons "a" (MCons "b" (MEmpty))))
(test (MArray2MList (MArray 5 '(15 4 35 12 1))) (MCons 15 (MCons 4 (MCons 35 (MCons 12 (MCons 1 (MEmpty)))))))
(test (MArray2MList (MArray 4 '("esta" "es" "una" "prueba"))) (MCons "esta" (MCons "es" (MCons "una" (MCons "prueba" (MEmpty))))))
;printML
(test (printML (MEmpty)) "[]")
(test (printML (MCons 7 (MEmpty))) "[7]")
(test (printML (MCons 7 (MCons 4 (MEmpty)))) "[7, 4]")
(test (printML (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons 3 (MEmpty)))) "[[1, 2], 3]")
;concatML
(test (concatML (MEmpty)(MCons 3 (MEmpty))) (MCons 3 (MEmpty)))
(test (concatML (MCons 3 (MEmpty)) (MEmpty)) (MCons 3 (MEmpty)))
(test (concatML (MCons 7 (MCons 4 (MEmpty))) (MCons 1 (MEmpty))) (MCons 7 (MCons 4 (MCons 1 (MEmpty)))))
(test (concatML (MCons 1(MEmpty)) (MCons 2(MEmpty))) (MCons 1(MCons 2(MEmpty))))
(test (concatML (MCons 10(MEmpty)) (MCons 20(MCons 30(MEmpty)))) (MCons 10(MCons 20(MCons 30(MEmpty)))))
;LengthML
(test (lengthML (MEmpty)) 0)
(test (lengthML (MCons 7 (MCons 4 (MEmpty)))) 2)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MEmpty))))) 3)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MCons 4(MEmpty)))))) 4)
(test (lengthML (MCons 7 (MCons 4 (MCons 3 (MCons 4 (MCons 5(MEmpty))))))) 5)
;mapML
(test (mapML add1 (MCons 7 (MCons 4 (MEmpty)))) (MCons 8 (MCons 5 (MEmpty))))
(test (mapML (lambda (x) (* x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 100 (MCons 9 (MEmpty))))
(test (mapML add1 (MCons 1(MEmpty))) (MCons 2(MEmpty)))
(test (mapML (lambda (x) (+ x x)) (MCons 10 (MCons 3 (MEmpty)))) (MCons 20 (MCons 6 (MEmpty))))
(test (mapML (lambda (x) (+ x 2)) (MCons 2 (MCons 3 (MEmpty)))) (MCons 4 (MCons 5 (MEmpty))))
;filterML
(test (filterML (lambda (x) (not (zero? x))) (MCons 2 (MCons 0 (MCons 1 (MEmpty))))) (MCons 2 (MCons 1 (MEmpty))))
(test (filterML (lambda (x) (not (zero? x))) (MEmpty)) (MEmpty))
(test (filterML (lambda (x) (eqv? 1 x)) (MCons 7 (MCons 4 (MCons 1 (MCons 2 (MCons 1 (MEmpty))))))) (MCons 1 (MCons 1 (MEmpty))))
(test (filterML (lambda (n) (= (modulo n 2) 0)) (MCons 1 (MCons 2 (MCons 3 (MCons 4 (MCons 5 (MEmpty))))))) (MCons 2 (MCons 4 (MEmpty))))
(test (filterML (lambda (x) (not (zero? x))) (MCons 1 (MCons 2 (MCons 0 (MCons 0 (MCons 5 (MEmpty)))))))(MCons 1 (MCons 2 (MCons 5(MEmpty)))))
;harvesine
(test (haversine gps-ciencias gps-zocalo) 13.033219276117368)
(test (haversine gps-ciencias gps-perisur) 2.44727738966455)
(test (haversine gps-satelite gps-perisur) 23.401736010506026)
(test (haversine gps-ciencias gps-satelite) 21.510202561254264)
(test (haversine gps-zocalo gps-satelite) 13.653182838772619)
;gps-coordinates
(test (gps-coordinates (MEmpty)) (MEmpty))
(test (gps-coordinates plazas) (MCons (GPS 19.510482 -99.23411900000002) (MCons (GPS 19.304135 -99.19001000000003) (MEmpty))))
(test (gps-coordinates (MCons ciencias (MEmpty))) (MCons (GPS 19.3239411016 -99.179806709) (MEmpty)))
(test (gps-coordinates (MCons ciencias (MCons zocalo (MEmpty)))) (MCons (GPS 19.3239411016 -99.179806709) (MCons (GPS 19.432721893261117 -99.13332939147949) (MEmpty))))
(test (gps-coordinates (MCons plaza-satelite (MCons zocalo (MCons ciencias (MEmpty)))))
      (MCons
       (GPS 19.510482 -99.23411900000002)
       (MCons (GPS 19.432721893261117 -99.13332939147949) (MCons (GPS 19.3239411016 -99.179806709) (MEmpty)))))
;area
(test (area (Circle (2D-Point 5 5) 4)) 50.26548245743669)
(test (area (Square (2D-Point 0 0) 20)) 400)
(test (area (Rectangle (2D-Point 3 4) 5 10)) 50)
(test (area (Circle (2D-Point 0 3) 10)) 314.1592653589793)
(test (area (Square (2D-Point 1 10) 12)) 144)
;in-figure?
(test (in-figure? (Circle (2D-Point 5 5) 4) (2D-Point 3 2)) #t)
(test (in-figure? (Rectangle (2D-Point 1 1) 5 2) (2D-Point 7 3)) #f)
(test (in-figure? (Square (2D-Point 6 6) 6) (2D-Point 8 9)) #t)
(test (in-figure? (Circle (2D-Point 2 2) 3) (2D-Point 4 3)) #t)
(test (in-figure? (Rectangle (2D-Point 4 6) 5 3) (2D-Point 6 7)) #t)