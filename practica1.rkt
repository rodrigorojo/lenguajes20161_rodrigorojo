#lang plai

(define (pow n m)
  (cond
    [(zero? m) 1]
    [else (* n (pow n (- m 1)))]))

    
