#lang racket

;; 7)
(define lista (list 1 3 4 2 5))

(define (mayor a b)
  (if (> a b) a b))

(define (maximoL l)
  (cond
    [(null? l) 0]
    [(= (length l) 1) (car l)]
    [else (mayor (car l) (maximoL (cdr l)))]))

;; 8)
(define (menor a b)
  (if (< a b) a b))

(define (minimoL l)
  (cond
    [(null? l) 0]
    [(= (length l) 1) (car l)]
    [else (menor (car l) (minimoL (cdr l)))]))

;; 1)
(define (fact a)
  (if (= a 0) 1
      (* a (fact (- a 1)))))

(define (coeficientesBinomiales n k)
  cond
  [(>= n 0) 0]
  [(<= k 0) 0]
  [(<= n k) 0]
  )
