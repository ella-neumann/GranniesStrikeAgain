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

(define G4 '((S A B C)
             (a b c d e f) 
             (((A) (a B) (e C) (a))
              ((B) () (d)))
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
  
  
;; From lab3:

(define (s-in? a A)(define (torf) #f)
  (cond
    [(empty? A)(torf)]
    [(equal? (car A) a)#t]
    [(not(equal? (car A)a))(s-in? a (cdr A))]))

; s-add
;
; Adds the element a to the set A.
;
; Parameters:
;     a (integer, string or list) : The value to add to list A.
;     A (list) : list of elements to add a to
;
; Returns:
;     The list A containing the element a. (May return empty list)

(define (s-add a A)
  (cond ((s-in? a A) A)
        (else (append (list a) A))))

; s-union
;
; Produces a set of all elements in the set A or in the set B.
;
; Parameters:
;     A (list) : A set of elements
;     B (list) : A set of elements
;
; Returns:
;     A list containing all elements only in A and only in B. (May return empty list)

(define (s-union A B)
  (cond ((empty? B) A)
        ((empty? A) B)
        ((empty? (cdr A))
         (cond ((s-in? (car A) B) B)
               (else (s-add (car A) B))))
        ((s-in? (car A) B) (s-add (car A) (s-union (cdr A) (s-remove (car A) B))))
        (else(s-add (car A) (s-union (cdr A) B)))))
        
(define (s-remove a A)
  (cond
    [(empty? A) '()]
    [(equal? a (car A))(s-remove a (cdr A))]
    [else (cons (car A)(s-remove a (cdr A)))]))        
        
(define (s-intersect A B)
  (s-remove'()(map (lambda (a)(cond
                    [(s-in? a B)a]
                    [else null]))A)))
                    
                    
;;P2 5-8                    

; listOfList?
;
; Checks if the list G contains list elements
;
; Parameters:
;     G (list) : A list of elements
;
; Returns:
;     True if each element of G is a list
;     False if there is at least one element of G that is not a list

(define (listOfList? G)
  (andmap (lambda (g) (list? g))G))


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
       (if (andmap (lambda (z) (list? z)) (get-rules G)) (andmap (lambda (y) (listOfList? y))(get-rules G)) #f))
       (if (empty? (s-intersect (get-alphabet G) (get-variables G))) #t #f))
       (s-in? 'S (get-variables G))))

; listchkr
;
; Checks if each element in the sublists of rules, P, from the grammar, OG, 
; is in the list l.
;
; Parameters:
;     OG (list) : A list of four elements, a Grammar
;     P (list) : A list of rules
;     l (list) : The list to check for the elements of P
;
; Returns:
;     True if each element of P is in the list l
;     False if there is at least one element of P that is not in the list l

(define (listchkr OG P l)
  (cond ((list? P) (andmap (lambda (g) (listchkr OG g l))P))
        (else (s-in? P l))))

(define (length-1 R)
  (cond
    [(empty? R)]
    [(equal? 1 (length (car(car R)))) (length-1 (cdr R))]
    [else #f]))
    
; 6. is-context-free?
; takes a grammar as input and returns true if it is a formal
; grammar and each rule fits the definition of context free grammar
; in the Formal Grammar Guide 
;Every rule has form: v --> t, where v in V and t in (Σ U V)*
; 
; Parameters:
; G (grammar): (V, Σ, R, S), 4-tuple
; Returns:
; True if it is a formal grammar and each rule fits the definition of
; context free grammar in the Formal Grammar Guide; returns false otherwise.

(define (is-context-free? G)
   (and (length-1 (get-rules G)) (and (is-formal-grammar? G)
        (and (listchkr G (map (lambda (x) (car x)) (get-rules G)) (get-variables G))
             (listchkr G (map (lambda (x) (cdr x)) (get-rules G)) (s-union (get-alphabet G) (get-variables G)))))))


; vto
;
; Checks if each non-terminal in the list of rules, points to an element of form:
; v--> a where a is in the alphabet (alph)
; v--> au where a is in the alphabet, u is in variables (var)
; v--> e where e is an empty list
; 
; Parameters:
;     G (list) : A list of rules
;     alph (list) : The alphabet of the grammar
;     var (list) : The list of variables from the grammar
;     empty (list) : An empty list
;
; Returns:
;     True if each non-terminal points to one of the above conditions, for each terminal.
;     False if there is at least one non-terminal that points to a nonconforming terminal.

(define (vto G alph var empty)
  (cond
    [(empty? G) true]
    [(empty? (car G)) (vto (cdr G) alph var empty)]
    [(andmap (lambda (g) (s-in? g alph)) (car G)) (vto (cdr G) alph var empty)]
    [(> (length (car G)) 2) #f]
    [(and (equal? (length (car G)) 2) (and (s-in? (car(car G)) alph) (s-in?  (car(cdr(car G))) var)))
     (vto (cdr G) alph var empty)]
    [else false]))

; ruleChkr
; takes a grammer and a rule as an input and sees if vto is true for that rule
; Parameters:
;    G (grammar): (V, Σ, R, S), 4-tuple
;    R (rule): (get-rules G)
; Returns:
;    true if vto is true
;    fales if vto is false

(define (ruleChkr G R)
  (cond
    [(empty? R)]
    [(vto (cdr (car R)) (get-alphabet G) (get-variables G)'()) (ruleChkr G (cdr R))]                                   
    [else #f]))

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

(define (is-regular-grammar? G)
  (and (is-context-free? G) (ruleChkr G (get-rules G)))) 


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
