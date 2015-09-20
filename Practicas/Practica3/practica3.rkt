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

;create-trackpoints - Dado una lista en la que cada elemento de la lista contiene: un tiempo en formato UNIX,
;una lista con la latitud y longitud y finalmente el ritmo cardiaco. Como segundo parámetro se tiene una lista
;de zonas cardiacas con lo que se tiene que regresar una lista de trackpoints que contengan la información dada.

(define (create-trackpoints lst zc)
  (cond
    [(empty? lst) '()]
    [else
     (define tr-loc (GPS (car (car (cdr (car lst)))) (car (cdr (car (cdr (car lst)))))))
     (define tr-hr (car (cdr (cdr (car lst)))))
     (define tr-zone (get-zone 'resting zc))
     (define tr-time (car (car lst)))
     (cons (trackpoint tr-loc tr-hr tr-zone tr-time) (create-trackpoints (cdr lst) zc))]))

;GPS -> GPS -> N
;haversine - Dados dos valores de tipo GPS calcular su distancia usando la formula de haversine.
(define (haversine gps1 gps2)
  (* 12742 (asin (sqrt (+ (expt (sin (/ (- (degrees->radians (GPS-lat gps2)) (degrees->radians (GPS-lat gps1))) 2)) 2)
                              (* (* (cos (degrees->radians (GPS-lat gps1))) (cos (degrees->radians (GPS-lat gps2))))
                                 (expt (sin (/ (- (degrees->radians (GPS-long gps2)) (degrees->radians (GPS-long gps1))) 2)) 2)))))))

;total-distance - Dada una lista de trackpoints, regresar la distancia total recorrida.
(define (total-distance tkp)
  (cond
    [(empty? tkp) 0]
    [(empty? (cdr tkp)) 0]
    [else
     (define gps1 (type-case Frame (car tkp)
       [trackpoint (loc hr zone unix-time) loc]))
     (define gps2 (type-case Frame (car(cdr tkp))
       [trackpoint (loc hr zone unix-time) loc]))
     (+ (haversine gps1 gps2) (total-distance (cdr tkp)))]))

;average-hr - Dada una lista de trackpoints, regresar el promedio del ritmo cardiaco, el resultado debe ser un entero.
(define (average-hr tkp)
  (cond
    [(empty? tkp) 0]
    [else(round(/ (add-hr tkp) (length tkp)))]))

;Funcion auxiliar para sumar hr de una lista de trackpoints.
(define (add-hr tkp)
  (cond
    [(empty? tkp) 0]
    [else
     (define hr1 (type-case Frame (car tkp)
       [trackpoint (loc hr zone unix-time) hr]))
     (+ hr1 (add-hr (cdr tkp)))]))

;max-hr - Dada una lista de trackpoints, regresar el máximo ritmo cardiaco, el resultado debe ser un entero.
(define (max-hr tkp)
  (cond
    [(empty? tkp) 0]
    [else(apply max (list-hr tkp))]))
;Funcion auxiliar que pasa los hr de una lista de trackpoints a una lista de enteros
(define (list-hr tkp)
  (cond
    [(empty? tkp) empty]
    [else
     (define hr1 (type-case Frame (car tkp)
       [trackpoint (loc hr zone unix-time) hr]))
     (cons hr1 (list-hr (cdr tkp)))]))

;collapse-trackpoints - Dada una lista de trackpoints y un epsilon e, obtener una nueva lista en que se
;agrupen los deltas consecutivos dado que se cumpla lo siguiente: la distancia de un trackpoint al otro trackpoint
;debe ser menor o igual a e y los trackpoints deben tener el mismo ritmo cardiaco.

(define (collapse-trackpoints lst e)
  (cond
    [(empty? lst) '()]
    [(empty? (cdr lst)) (list (car lst))]
    [else
     (define x (car lst))
     (define y (car (cdr lst)))
     (append (if (and (= (trackpoint-hr x) (trackpoint-hr y))
              (< (haversine (trackpoint-loc x) (trackpoint-loc y)) e))
         '() (list (car lst))) (collapse-trackpoints (cdr lst) e))]))

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
;Listas usadas en las pruebas
(define trackpoints0 (create-trackpoints (take raw-data 4) my-zones))
(define trackpoints1 (create-trackpoints (take raw-data 100) my-zones))
(define trackpoints2 (create-trackpoints raw-data my-zones))
(define trackpoints3 (create-trackpoints (take raw-data 50) my-zones))
(define trackpoints4 (create-trackpoints (take raw-data 400) my-zones))
(define trackpoints5 (create-trackpoints (take raw-data 10) my-zones))
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
;create-trackpoints
(test (create-trackpoints '() my-zones) '())
(test (create-trackpoints (take raw-data 1) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))
(test (create-trackpoints (take raw-data 4) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (create-trackpoints (take raw-data 10) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.490702 -99.2413217) 115 (resting 50 114.0) 1425619678)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (resting 50 114.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (resting 50 114.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (resting 50 114.0) 1425619690)))
(test (create-trackpoints (take raw-data 50) my-zones)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.490702 -99.2413217) 115 (resting 50 114.0) 1425619678)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (resting 50 114.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (resting 50 114.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (resting 50 114.0) 1425619690)
       (trackpoint (GPS 19.4906905 -99.2416019) 120 (resting 50 114.0) 1425619693)
       (trackpoint (GPS 19.4907662 -99.2417367) 122 (resting 50 114.0) 1425619702)
       (trackpoint (GPS 19.4908105 -99.2418386) 123 (resting 50 114.0) 1425619709)
       (trackpoint (GPS 19.4908799 -99.2419175) 123 (resting 50 114.0) 1425619713)
       (trackpoint (GPS 19.491003 -99.2419904) 127 (resting 50 114.0) 1425619718)
       (trackpoint (GPS 19.4910947 -99.2420837) 128 (resting 50 114.0) 1425619722)
       (trackpoint (GPS 19.491214 -99.2421806) 128 (resting 50 114.0) 1425619726)
       (trackpoint (GPS 19.4913238 -99.2422883) 128 (resting 50 114.0) 1425619731)
       (trackpoint (GPS 19.4913761 -99.2423724) 130 (resting 50 114.0) 1425619735)
       (trackpoint (GPS 19.4914257 -99.2424697) 131 (resting 50 114.0) 1425619740)
       (trackpoint (GPS 19.491457 -99.2425136) 131 (resting 50 114.0) 1425619742)
       (trackpoint (GPS 19.4915035 -99.2425561) 131 (resting 50 114.0) 1425619744)
       (trackpoint (GPS 19.4916086 -99.2426661) 132 (resting 50 114.0) 1425619750)
       (trackpoint (GPS 19.4916522 -99.2426908) 131 (resting 50 114.0) 1425619752)
       (trackpoint (GPS 19.4916881 -99.2427445) 131 (resting 50 114.0) 1425619755)
       (trackpoint (GPS 19.4917418 -99.2428858) 133 (resting 50 114.0) 1425619763)
       (trackpoint (GPS 19.4918402 -99.2429976) 133 (resting 50 114.0) 1425619770)
       (trackpoint (GPS 19.4919308 -99.2430909) 134 (resting 50 114.0) 1425619775)
       (trackpoint (GPS 19.4919798 -99.2431264) 135 (resting 50 114.0) 1425619777)
       (trackpoint (GPS 19.4920471 -99.2431517) 135 (resting 50 114.0) 1425619780)
       (trackpoint (GPS 19.4921377 -99.2431702) 135 (resting 50 114.0) 1425619784)
       (trackpoint (GPS 19.4922461 -99.2431949) 136 (resting 50 114.0) 1425619790)
       (trackpoint (GPS 19.4923412 -99.2431893) 135 (resting 50 114.0) 1425619794)
       (trackpoint (GPS 19.4923881 -99.2431795) 135 (resting 50 114.0) 1425619796)
       (trackpoint (GPS 19.4924338 -99.2431681) 135 (resting 50 114.0) 1425619798)
       (trackpoint (GPS 19.4924803 -99.2431236) 135 (resting 50 114.0) 1425619801)
       (trackpoint (GPS 19.4925093 -99.243068) 135 (resting 50 114.0) 1425619803)
       (trackpoint (GPS 19.4925622 -99.2429675) 135 (resting 50 114.0) 1425619808)
       (trackpoint (GPS 19.49257 -99.2429101) 135 (resting 50 114.0) 1425619811)
       (trackpoint (GPS 19.4925666 -99.2428421) 135 (resting 50 114.0) 1425619815)
       (trackpoint (GPS 19.4925485 -99.2427959) 134 (resting 50 114.0) 1425619817)
       (trackpoint (GPS 19.4925287 -99.2427514) 134 (resting 50 114.0) 1425619820)
       (trackpoint (GPS 19.4924243 -99.2426998) 134 (resting 50 114.0) 1425619826)
       (trackpoint (GPS 19.4923684 -99.2426722) 135 (resting 50 114.0) 1425619829)
       (trackpoint (GPS 19.4923359 -99.2426345) 135 (resting 50 114.0) 1425619831)
       (trackpoint (GPS 19.4922724 -99.2425199) 136 (resting 50 114.0) 1425619838)
       (trackpoint (GPS 19.4922266 -99.2423829) 135 (resting 50 114.0) 1425619843)
       (trackpoint (GPS 19.4922158 -99.2423196) 134 (resting 50 114.0) 1425619846)
       (trackpoint (GPS 19.4922217 -99.2422634) 134 (resting 50 114.0) 1425619848)
       (trackpoint (GPS 19.4922327 -99.2421506) 135 (resting 50 114.0) 1425619853)))
;total-distance
(test (total-distance '()) 0)
(test (total-distance trackpoints0) 0.007864840450045972)
(test (total-distance trackpoints1) 0.9509291243812747)
(test (total-distance trackpoints2) 5.051934549322941)
(test (total-distance trackpoints3) 0.46100326160491)
;average-hr
(test (average-hr empty) 0)
(test (average-hr trackpoints1) 134)
(test (average-hr trackpoints2) 150)
(test (average-hr trackpoints3) 128)
(test (average-hr trackpoints4) 147)
;max-hr
(test (max-hr empty) 0)
(test (max-hr trackpoints1) 147)
(test (max-hr trackpoints2) 165)
(test (max-hr trackpoints3) 136)
(test (max-hr trackpoints4) 165)
;collapse-trackpoints
(test (collapse-trackpoints '() 100) '())
(test (collapse-trackpoints trackpoints0 0.01)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
(test (collapse-trackpoints trackpoints5 0.1)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (resting 50 114.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (resting 50 114.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (resting 50 114.0) 1425619690)))
(test (collapse-trackpoints trackpoints5 0.001)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.490702 -99.2413217) 115 (resting 50 114.0) 1425619678)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (resting 50 114.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (resting 50 114.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (resting 50 114.0) 1425619690)))
(test (collapse-trackpoints trackpoints3 1)
      (list
       (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)
       (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)
       (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)
       (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671)
       (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675)
       (trackpoint (GPS 19.4906902 -99.2413796) 115 (resting 50 114.0) 1425619681)
       (trackpoint (GPS 19.4906865 -99.241445) 120 (resting 50 114.0) 1425619685)
       (trackpoint (GPS 19.4906861 -99.2415517) 119 (resting 50 114.0) 1425619690)
       (trackpoint (GPS 19.4906905 -99.2416019) 120 (resting 50 114.0) 1425619693)
       (trackpoint (GPS 19.4907662 -99.2417367) 122 (resting 50 114.0) 1425619702)
       (trackpoint (GPS 19.4908799 -99.2419175) 123 (resting 50 114.0) 1425619713)
       (trackpoint (GPS 19.491003 -99.2419904) 127 (resting 50 114.0) 1425619718)
       (trackpoint (GPS 19.4913238 -99.2422883) 128 (resting 50 114.0) 1425619731)
       (trackpoint (GPS 19.4913761 -99.2423724) 130 (resting 50 114.0) 1425619735)
       (trackpoint (GPS 19.4915035 -99.2425561) 131 (resting 50 114.0) 1425619744)
       (trackpoint (GPS 19.4916086 -99.2426661) 132 (resting 50 114.0) 1425619750)
       (trackpoint (GPS 19.4916881 -99.2427445) 131 (resting 50 114.0) 1425619755)
       (trackpoint (GPS 19.4918402 -99.2429976) 133 (resting 50 114.0) 1425619770)
       (trackpoint (GPS 19.4919308 -99.2430909) 134 (resting 50 114.0) 1425619775)
       (trackpoint (GPS 19.4921377 -99.2431702) 135 (resting 50 114.0) 1425619784)
       (trackpoint (GPS 19.4922461 -99.2431949) 136 (resting 50 114.0) 1425619790)
       (trackpoint (GPS 19.4925666 -99.2428421) 135 (resting 50 114.0) 1425619815)
       (trackpoint (GPS 19.4924243 -99.2426998) 134 (resting 50 114.0) 1425619826)
       (trackpoint (GPS 19.4923359 -99.2426345) 135 (resting 50 114.0) 1425619831)
       (trackpoint (GPS 19.4922724 -99.2425199) 136 (resting 50 114.0) 1425619838)
       (trackpoint (GPS 19.4922266 -99.2423829) 135 (resting 50 114.0) 1425619843)
       (trackpoint (GPS 19.4922217 -99.2422634) 134 (resting 50 114.0) 1425619848)
       (trackpoint (GPS 19.4922327 -99.2421506) 135 (resting 50 114.0) 1425619853)))
;ninBT
(test (ninBT (EmptyBT)) 0)
(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)
(test (ninBT arbol-base) 5)
(test (ninBT arb1) 0)
(test (ninBT maxiarb) 9)
;nlBT
(test (nlBT (EmptyBT)) 0)
(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)
(test (nlBT arbol-base) 4)
(test (nlBT arb1) 1)
(test (nlBT maxiarb) 8)
;nnBT
(test (nnBT (EmptyBT)) 0)
(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
(test (nnBT arbol-base) 9)
(test (nnBT arb1) 1)
(test (nnBT maxiarb) 17)
;mapBT
(test (mapBT add1 (EmptyBT)) (EmptyBT))
(test (mapBT add1 (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 3 (EmptyBT))))
(test (mapBT (lambda (x) (* x x)) (BNode < (EmptyBT) 3 (BNode < (EmptyBT) 2 (EmptyBT))))
      (BNode < (EmptyBT) 9 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (+ x x)) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 4 (EmptyBT))))
(test (mapBT (lambda (x) (- 1 x)) (BNode < (EmptyBT) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (EmptyBT) 0 (BNode < (EmptyBT) -1 (EmptyBT))))
;preorderBT
(test (preorderBT arbol-base) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))
(test (preorderBT maxiarb) '(10 1 2 4 5 3 6 7 9 1 2 4 5 3 6 7 9))
(test (preorderBT arb1) '(1))
(test (preorderBT arb4) '(4 3 2 1 1 2 1 1 3 2 1 1 2 1 1))
(test (preorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(0 500 210))
;inorderBT
(test (inorderBT arbol-base) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))
(test (inorderBT maxiarb) '(4 2 5 1 7 6 9 3 10 4 2 5 1 7 6 9 3))
(test (inorderBT arb1) '(1))
(test (inorderBT arb4) '(1 2 1 3 1 2 1 4 1 2 1 3 1 2 1))
(test (inorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(500 0 210))
;posorderBT
(test (posorderBT arbol-base) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
(test (posorderBT maxiarb) '(4 5 2 7 9 6 3 1 4 5 2 7 9 6 3 1 10))
(test (posorderBT arb1) '(1))
(test (posorderBT arb4) '(1 1 2 1 1 2 3 1 1 2 1 1 2 3 4))
(test (posorderBT (BNode < (BNode < (EmptyBT) 500(EmptyBT)) 0 (BNode < (EmptyBT) 210 (EmptyBT)))) '(500 210 0))