#lang racket
(struct int (n) #:transparent)
(struct true () #:transparent)
(struct false () #:transparent)
(struct complex (a b) #:transparent)
(struct :: (e1 e2) #:transparent)
(struct empty () #:transparent)
(struct if-then-else (p e1 e2) #:transparent)
(struct add (e1 e2) #:transparent)
(struct sub (e1 e2) #:transparent)
(struct mul (e1 e2) #:transparent)
(struct gt (e1 e2) #:transparent)
(struct both (e1 e2) #:transparent)
(struct any (e1 e2) #:transparent)
(struct ! (e1) #:transparent)
(struct hd (e) #:transparent)
(struct tl (e) #:transparent)
(struct is-empty (e) #:transparent)
(struct @ (e1 e2) #:transparent)
(struct real (e1) #:transparent)
(struct imaginary (e1) #:transparent)
(struct is-int (e) #:transparent)
(struct is-bool (e) #:transparent)
(struct is-complex (e) #:transparent)
(struct is-list (e) #:transparent)
(struct fun (name fargs body) #:transparent)
(struct proc (name body) #:transparent)
(struct call (e args) #:transparent)
(struct envelope (env f) #:transparent)
(struct var (s e1 e2) #:transparent)
(struct valof (s) #:transparent)



(define (to-complex e1)
  (complex e1 (int 0)))

(define (conj e1)
  e1)

(define (~ e1)
  (sub (int 0) e1))

(define (lt e1 e2)
  (gt e2 e1))

(define (same e1 e2)
  (both (!(lt e1 e2)) (!(gt e1 e2))))

(define (mi izraz okolje)
  (cond [(true? izraz) izraz]
        [(false? izraz) izraz]
        [(empty? izraz) izraz]
        [(envelope? izraz) izraz]
        [(int? izraz) (if (integer? (int-n izraz)) izraz (error "Int mora biti celoštevilska vrednost"))]

        [(complex? izraz)
         (let ([v1 (mi (complex-a izraz) okolje)]
               [v2 (mi (complex-b izraz) okolje)])
           (if (and (int? v1) (int? v2)) (complex v1 v2) (error "A in B morata biti številski konstanti.")))]
        
        [(::? izraz)
         (let ([v1 (mi (::-e1 izraz) okolje)]
               [v2 (mi (::-e2 izraz) okolje)])
           (cond [(or (empty? v1) (::? v1)) (error "Prvi seznam elementa ne more biti seznam ali empty.")]
                 [(::? v2) (:: v1 (mi v2 okolje))]
                 [(empty? v2) (:: v1 (empty))]
                 [#t (error "Napačen seznam")]))]

        [(@? izraz)
         (let ([v1 (mi (@-e1 izraz) okolje)]
               [v2 (mi (@-e2 izraz) okolje)])
           (cond [(empty? v2) v1]
                 [(and (::? v1) (::? v2)) (:: (::-e1 v1) (mi (@ (::-e2 v1) v2) okolje))]
                 [(and (empty? v1) (::? v2)) v2]
                 [#t (error "Napaka s seznami")]
                 ))]
        
        [(if-then-else? izraz)
         (let ([v1 (mi (if-then-else-p izraz) okolje)])
           (cond [(true? v1) (mi (if-then-else-e1 izraz) okolje)]
                 [(false? v1) (mi (if-then-else-e2 izraz) okolje)]
                 [#t (error "Pogoj mora biti logični operator.")]))]
        
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

        [(is-list? izraz)
         (let ([v1 (mi (is-list-e izraz) okolje)])
           (if (or (::? v1) (empty? v1))
               (true)
               (false)))]
        
        [(add? izraz)
         (let ([v1 (mi (add-e1 izraz) okolje)]
               [v2 (mi (add-e2 izraz) okolje)])
           (cond [(and (int? v1) (int? v2)) (int (+ (int-n v1) (int-n v2)))]
                 [(and (complex? v1) (complex? v2)) (complex (mi (add (complex-a v1)(complex-a v2)) okolje) (mi (add (complex-b v1) (complex-b v2)) okolje))]
                 [#t (error "Tipa seštevancev nista enaka")]))]

        [(sub? izraz)
         (let ([v1 (mi (sub-e1 izraz) okolje)]
               [v2 (mi (sub-e2 izraz) okolje)])
           (cond [(and (int? v1) (int? v2)) (int (- (int-n v1) (int-n v2)))]
                 [#t (error "Tipa seštevancev nista enaka")]))]

        [(mul? izraz)
         (let ([v1 (mi (mul-e1 izraz) okolje)]
               [v2 (mi (mul-e2 izraz) okolje)])
           (cond [(and (int? v1) (int? v2)) (int (* (int-n v1) (int-n v2)))]
                 [(and (complex? v1) (complex? v2)) (complex (int (- (* (int-n (complex-a v1)) (int-n (complex-a v2))) (* (int-n (complex-b v1)) (int-n (complex-b v2))))) (int (+ (* (int-n (complex-b v2)) (int-n (complex-a v1))) (* (int-n (complex-a v2)) (int-n (complex-b v1))))))]
                 [#t (error "Tipa množencev nista enaka")]))]

        [(gt? izraz)
         (let ([v1 (mi (gt-e1 izraz) okolje)]
               [v2 (mi (gt-e2 izraz) okolje)])
           (if (and (int? v1) (int? v2))
               (if (> (int-n v1) (int-n v2)) (true) (false))
               (error "Oba elementa morata biti celoštevilski vrednosti.")))]

        [(both? izraz)
         (let ([v1 (mi (both-e1 izraz) okolje)]
               [v2 (mi (both-e2 izraz) okolje)])
           (if (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
               (if (and (true? v1) (true? v2)) (true) (false))
               (error "Eden od izrazov se ne evalvira v true/false.")))]

        [(any? izraz)
         (let ([v1 (mi (any-e1 izraz) okolje)]
               [v2 (mi (any-e2 izraz) okolje)])
           (if (and (or (true? v1) (false? v1)) (or (true? v2) (false? v2)))
               (if (or (true? v1) (true? v2)) (true) (false))
               (error "Eden od izrazov se ne evalvira v true/false.")))]

        [(!? izraz)
         (let ([v1 (mi (!-e1 izraz) okolje)])
           (cond [(true? v1) (false)]
                 [(false? v1) (true)]
                 [#t (error "Izraz se ne evalvira v true/false.")]))]

        [(hd? izraz)
         (let ([v1 (mi (hd-e izraz) okolje)])
           (cond [(::? v1) (::-e1 v1)]
                 [#t (error "Prazen seznam")]))]

        [(tl? izraz)
         (let ([v1 (mi (tl-e izraz) okolje)])
           (cond [(::? v1) (::-e2 v1)]
                 [#t (error "Prazen seznam")]))]

        [(is-empty? izraz)
         (let ([v1 (mi (is-empty-e izraz) okolje)])
           (cond [(empty? v1) (true)]
                 [(::? v1) (false)]
                 [#t (error "Is-empty deluje samo na seznamih")]))]
        
        [(real? izraz)
         (let ([v1 (mi (real-e1 izraz) okolje)])
           (if (complex? v1)
               (complex-a v1)
               (error "Element mora biti kompleksno število")))]

        [(imaginary? izraz)
         (let ([v1 (mi (imaginary-e1 izraz) okolje)])
           (if (complex? v1)
               (complex-b v1)
               (error "Element mora biti kompleksno število")))]

        [(var? izraz)
         (let ([v1 (mi (var-e1 izraz) okolje)])
           (mi (var-e2 izraz) (append (list (cons (var-s izraz) v1)) okolje)))]

        [(valof? izraz)
         (cdr (assoc (valof-s izraz) okolje))]

        [(fun? izraz)
         (envelope okolje izraz)]

        [(proc? izraz)
         izraz]

        [(call? izraz)
         (let ([e1 (mi (call-e izraz) okolje)])
           (cond [(proc? e1) (mi (proc-body e1) (append (list (cons (proc-name e1) e1)) okolje))]
                 [(envelope? e1)
                  (mi (fun-body (envelope-f e1)) (append (append (map (lambda (x) (cons (car x) (mi (cdr x) okolje))) (map cons (fun-fargs (envelope-f e1)) (call-args izraz))) (envelope-env e1)) (list (cons (fun-name (envelope-f e1)) e1))))]
                 [#t (error "Izraz ni funkcija")]))]

        ))



