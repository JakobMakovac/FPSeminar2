#lang racket
(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct complex (a b) #:transparent)
(struct :: (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct if-then-else (p e1 e2) #:transparent)
(struct add (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct gt (e1 e2) #:transparent)
(struct both (e1 e2) #:transparent)
(struct any (e1 e2) #:transparent)
(struct ! (e1) #:transparent)
(struct real (c) #:transparent)
(struct imaginary (c) #:transparent)
(struct is-int (e) #:transparent)
(struct is-bool (e) #:transparent)
(struct is-complex (e) #:transparent)
(struct is-list (e) #:transparent)



(define (mi izraz okolje)
  (cond [(true? izraz) izraz]
        [(false? izraz) izraz]
        [(empty? izraz) izraz]
        [(int? izraz) (if (integer? (int-n izraz)) izraz (displayln "Int mora biti celoštevilska vrednost"))]
        [(complex? izraz) (if (and (integer? (complex-a izraz)) (integer? (complex-b izraz))) izraz (displayln "A in B pri complex morata biti celoštevilski vrednosti."))]
                
        [(if-then-else? izraz)
         (let ([v1 (mi (if-then-else-p izraz) okolje)])
           (if (true? v1)
               (mi (if-then-else-e1 izraz) okolje)
               (mi (if-then-else-e2 izraz) okolje)))]
        
        [(is-int? izraz)
         (let ([v1 (mi (is-int-e izraz) okolje)])
           (if (int? v1)
               (true)
               (false)))]

        [(is-bool? izraz)
         (let ([v1 (mi (is-bool-e izraz) okolje)])
           (if (or (true? v1) (false? v1))
               (true)
               (false)))]

        [(is-complex? izraz)
         (let ([v1 (mi (is-complex-e izraz) okolje)])
           (if (complex? v1)
               (true)
               (false)))]
        
        [(add? izraz)
         (let ([v1 (mi (add-e1 izraz) okolje)]
               [v2 (mi (add-e2 izraz) okolje)])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2)))]
                 [(and (complex? v1) (complex? v2)) (complex (+ (complex-a v1)(complex-a v2)) (+ (complex-b v1) (complex-b v2)))]
                 [#t (displayln "Tipa seštevancev nista enaka")]))]

        [(mul? izraz)
         (let ([v1 (mi (mul-e1 izraz) okolje)]
               [v2 (mi (mul-e2 izraz) okolje)])
           (cond [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2)))]
                 [(and (complex? v1) (complex? v2)) (complex (- (* (complex-a v1) (complex-a v2)) (* (complex-b v1) (complex-b v2))) (+ (* (complex-b v2) (complex-a v1)) (* (complex-a v2) (complex-b v1))))]
                 [#t (displayln "Tipa množencev nista enaka")]))]

        [(gt? izraz)
         (let ([v1 (mi (gt-e1 izraz) okolje)]
               [v2 (mi (gt-e2 izraz) okolje)])
           (if (and (int? v1) (int? v2))
               (if (> (int-n v1) (int-n v2)) (true) (false))
               (displayln "Oba elementa morata biti celoštevilski vrednosti.")))]

        [(both? izraz)
         (let ([v1 (mi (both-e1 izraz) okolje)]
               [v2 (mi (both-e2 izraz) okolje)])
           (if (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
               (if (and (true? v1) (true? v2)) (true) (false))
               (displayln "Eden od izrazov se ne evalvira v true/false.")))]

        [(any? izraz)
         (let ([v1 (mi (any-e1 izraz) okolje)]
               [v2 (mi (any-e2 izraz) okolje)])
           (if (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
               (if (or (true? v1) (true? v2)) (true) (false))
               (displayln "Eden od izrazov se ne evalvira v true/false.")))]

        [(!? izraz)
         (let ([v1 (mi (!-e1 izraz) okolje)])
           (cond [(true? v1) (false)]
                 [(false? v1) (true)]
                 [#t (displayln "Izraz se ne evalvira v true/false.")]))]
        
        [(real? izraz)
         (let ([v1 (mi (real-c izraz) okolje)])
           (if (complex? v1)
               (int (complex-a v1))
               (displayln "Element mora biti kompleksno število")))]

        [(imaginary? izraz)
         (let ([v1 (mi (imaginary-c izraz) okolje)])
           (if (complex? v1)
               (int (complex-b v1))
               (displayln "Element mora biti kompleksno število")))]))

;testi

;(mi (int 2) 0)
;(mi (int "a") 0)
;(mi (true) 0)
;(mi (false) 0)
;(mi (complex 2 3) 0)
;(mi (complex "a" 2) 9)
;(mi (add (int 1) (int 3)) 0)
;(mi (add (complex 2 3) (complex 1 4)) 0)
;(mi (add (complex 2 3) (int 3)) 0)
;(mi (add (int 1) (true)) 0)
;(mi (mul (int 1) (int 3)) 0)
;(mi (mul (complex 2 3) (complex 1 4)) 0)
;(mi (mul (complex 2 3) (int 3)) 0)
;(mi (mul (int 1) (true)) 0)
;(mi (gt (int 1) (int 3)) 0)
;(mi (gt (int 3) (int 1)) 0)
;(mi (gt (true) (int 3)) 0)
;(mi (real (complex 2 3)) 0)
;(mi (imaginary (complex 2 3)) 0)
;(mi (real (int 3)) 0)