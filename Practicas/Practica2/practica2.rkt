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


;Position
(define-type Position
  [2D-Point (m number?) (n number?)])

;Figure
(define-type Figure
  [Circle (p 2D-Point?) (n number?)]
  [Square  (p 2D-Point?) (l number?)]
  [Rectangle (p 2D-Point?) (b number?) (h number?)])

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


;Test
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
