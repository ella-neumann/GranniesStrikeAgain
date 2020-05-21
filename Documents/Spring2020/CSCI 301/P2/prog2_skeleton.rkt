#lang racket

; prog2.rkt
; Jessica Stillwell and Ella Neumann
; Spring 2020
;
; This program deciphers wheither a grammer is formal, context free, or regular grammar with the help of other functions.

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

(define G4 '((S A)
             (a b c) 
             (a)
             S))

; 1. get-variables
; take a grammar as input and returns the set of variables. 
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; A set of variables (i.e. the
; first element in the grammar 4-tuple).
(define (get-variables G)
  (car G))

; 2. get-alphabet
; take a grammar as input and returns the set of terminals.
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; A set of terminals (i.e. the
; second element in the grammar 4-tuple).
(define (get-alphabet G)
  (car(cdr G)))

; 3. get-rules
; take a grammar as input and returns the set of rules. 
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; A set of rules (i.e. the third
; element in the grammar 4-tuple).
(define (get-rules G)
  (car(cdr(cdr G))))

; 4. get-start-symbol
; take a grammar as input and returns the start symbol. 
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; A start symbol (i.e. the
; fourth element of the grammar 4-tuple).
(define (get-start-symbol G)
  (car(cdr(cdr(cdr G)))))

(define (s-in? a A)(define (torf) #f)
  (cond
    [(empty? A)(torf)]
    [(equal? (car A) a)#t]
    [(not(equal? (car A)a))(s-in? a (cdr A))]))

(define (listOfList? G)
  
  (andmap (lambda (n) (andmap (lambda (g) (list? g))G))

(define (s-remove a A)
  (cond
    [(empty? A) '()]
    [(equal? a (car A))(s-remove a (cdr A))]
    [else (cons (car A)(s-remove a (cdr A)))]))

(define (s-intersect A B)
  (s-remove'()(map (lambda (a)(cond
                    [(s-in? a B)a]
                    [else null]))A)))

(define (s-subset? A B)
  (andmap (lambda (a)(s-in? a B))A)) 

; 5. is-formal-grammar?
; takes a grammar as input and returns true if it is a valid
; formal grammar (and returns false otherwise).
; We say the Racket representation G of the
; grammar is valid if all of the following are true
; • G is a list of length four
; • All but the last element of G is itself a list
; • The last element of G is not a list
; • Each of the rules in G is a list of lists
; • Σ ∩ V = ∅
; • S ∈ V
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
;  true if it is a valid
; formal grammar (and returns false otherwise)
(define (is-formal-grammar? G)
  (and(and (and(and (and (equal? 4 (length G))
       (and (and (list? (get-variables G))(list? (get-rules G))) (list? (get-alphabet G))))
       (not(list? (get-start-symbol G))))
       (listOfList? (get-rules G1)))
       (if (empty? (s-intersect (get-alphabet G) (get-variables G))) #t #f))
       (s-in? 'S (get-variables G))))

(is-formal-grammar? G2)
(is-formal-grammar? G1)
(is-formal-grammar? G4)

; 6. is-context-free?
; takes a grammar as input and returns true if it is a formal
; grammar and each rule fits the definition of context free grammar
; in the Formal Grammar Guide 
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; True if it is a formal grammar and each rule fits the definition of
; context free grammar in the Formal Grammar Guide; returns false otherwise.
; (define (is-context-free? G)

; 7. is-regular-grammar?
; takes a grammar as input and returns true if it is a context
; free grammar and each rule fits the definition of right regular
; regular grammar in the Formal Grammar Guide; returns false otherwise.
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; True if it is a context
; free grammar and each rule fits the definition of right regular regular grammar in the
; Formal Grammar Guide; returns false otherwise.
;(define (is-regular-grammar? G) )

; 8. generate-random-string
; takes a context-free grammar G and produces a string in the language, where the string
; is represented as a list of symbols in the alphabet. It should start with the start symbol,
; and always rewrite the left-most non-terminal, randomly picking among the rewrite rules for
; that non-terminal (see (random n)). It only needs to support context-free grammars; if passed
; a grammar that is not context-free, it should return an empty list. 
; Parameters:
; G (context free grammar): (V, Σ, R, S), 4-tuple
; Returns:
; a string in the language, where the string is represented as a list of symbols in the alphabet.
; (define (generate-random-string G)

