#lang racket

(define G1 '((S) 
             (a)
             (((S) (a S) ()))
             S))

(define G2 '((S A)
             (a b c) 
             (((S) (a A) ()) 
              ((a A) (b S) (c)))
             S))

(define G3 '((S A B C)
             (a b c d e f) 
             (((S) (A a b c B d e f C) ())  
              ((A) (a B) (e C) (a))
              ((B) (b C) (d))
              ((C) (d) ())) 
             S))
;case where there are no variables?
(define (get-variables G)
  (car G))
;test cases
(get-variables G3)
(get-variables G2)

