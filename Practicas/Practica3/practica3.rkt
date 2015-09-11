#lang plai

(print-only-errors true)

(require "practica3-base.rkt")
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
