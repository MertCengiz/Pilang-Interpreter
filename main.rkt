#lang racket

(provide (all-defined-out))

; read and parse the input file
(define parse (lambda (input-file)
        (letrec (
            [input-port (open-input-file input-file)]
            [read-and-combine (lambda ()
                (let ([line (read input-port)])
                    (if (eof-object? line)
                        '()
                        (append `(,line) (read-and-combine))
                    )
                )
            )]
            )
            (read-and-combine)
        )
    )
)
(define create-hash (lambda (vars values)
        (letrec (
            [create-hash-iter (lambda (vars values hash)
                (if (null? vars)
                    hash
                    (create-hash-iter (cdr vars) (cdr values) (hash-set hash (car vars) (car values)))
                )
            )]
            )
            (create-hash-iter vars values (hash))
        )
    )
)

(define add-to-hash (lambda (old-hash new-hash)
        (foldl (lambda (key hash) (hash-set hash key (hash-ref new-hash key)))
            old-hash
            (hash-keys new-hash)
        )
    )
)

(define eval-program (lambda (program-str)
        (get (eval-exprs (parse program-str) empty-state) '-r)
    )
)

; solution starts here

; 1. empty-state (5 points)
(define empty-state (hash))

; 2. get (5 points)
(define get (lambda(state var) (if (hash-has-key? state var) (hash-ref state var) (eval var))))

; 3. put (5 points)
(define put (lambda(state var val)(hash-set state var val)))

; 4. := (15 points)
(define := (lambda(var val-expr state) (put (eval-expr val-expr state) var (get (eval-expr val-expr state) '-r))))

; 5. if: (15 points) 
(define if: (lambda(test-expr then-exprs else-exprs state)
  (if (get (eval-expr test-expr state) '-r) (eval-exprs then-exprs state) (eval-exprs else-exprs state))))

; 6. while: (15 points) 
(define while: (lambda (test-expr body-exprs state) (if (get (eval-expr test-expr state) '-r)
      (while: test-expr body-exprs (eval-exprs body-exprs state)) (eval-expr test-expr state))))

; 7. func (15 points)
(define func (lambda(params body-exprs state) (put state '-r
         (lambda args (get (eval-exprs body-exprs (foldl (lambda(elem-p elem-a newstate) (put newstate elem-p elem-a)) state params args))'-r))))) 

(define map-eval (lambda (lst state) (map (lambda(elem) (if (list? elem) (if (procedure? (get(eval-expr (car elem) state) '-r))
      (get (eval-expr elem state) '-r) (map-eval elem state)) (get(eval-expr elem state)elem))) lst)))

; 8. eval-expr (20 points)
(define eval-expr (lambda (expr state)(if (list? expr) 
  (cond ((equal? ':= (first expr)) (:= (second expr) (third expr) state))
        ((equal? 'if: (first expr)) (if: (second expr) (third expr) (fourth expr) state))
        ((equal? 'while: (first expr)) (while: (second expr) (third expr) state))
        ((equal? 'func (first expr)) (func (second expr) (third expr) state))
        ((equal? 'lambda (first expr)) (put state '-r (eval expr)))
        (else (put state '-r (apply (car(map-eval expr state)) (cdr(map-eval expr state))))))
  (put state '-r (get state expr)))))

; 9 eval-exprs (5 points)
(define eval-exprs (lambda(exprs state)(foldl eval-expr state exprs)))
