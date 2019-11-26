#lang racket

;; 10)
(define lista (list 1 2 3 4 5))

(define media (/ (apply + lista) (length lista)))

(define diferenciacuadrada
  (map (lambda (x) (* x x))
         (map (lambda (x) (- x media)) lista)))

(define sumacuadrados
  (apply + diferenciacuadrada))

(define varianza (/ sumacuadrados (length lista)))
  
(exact->inexact varianza)

(define desviacionestandar
    (sqrt varianza))

;; 9

(define (impares a)
  (if (= a 0) 1
      (* (- (* a 2) 1) (impares (- a 1)))))
;; 6

(define (esprimo n)
  (cond [(= (modulo (+ n 1) 4) 0) (let loop2 ([n n] [j (+ (/ (+ n 1) 2) 1)])
                                 (cond [(= j 1) true]
                                       [(= (modulo n j) 0) false]
                                       [else (loop2 n (- j 2))]))]
        [else (let loop3 ([n n] [j (/ (+ n 1) 2)])
                (cond [(= j 1) true]
                      [(= (modulo n j) 0) false]
                      [else (loop3 n (- j 2))]))]))
  

(define (listaprimos a b)
  (cond [(= (modulo a 2) 0) (= a (+ a 1))])
  (let loop ([primos '()] [i a])
    (cond [(> i b) primos]
          [(or (= i 1) (= i 2)) (loop (append primos (list i)) (+ i 1))]
          [(esprimo i) (loop (append primos (list i)) (+ i 2))]
          [else (loop primos (+ i 2))])))

;;5
(define frecuencias (make-hash))

(define (moda lista)
  (hash-update! frecuencias
                (car lista)
                (lambda (frec) (add1 frec))
                0)
  (cond [(not (null? (cdr lista))) (moda (cdr lista))]
        [else (for/fold ([mod null]
                         [mfrec 0])
                        ([(val cant) (in-hash frecuencias)])
                (cond [(> cant mfrec)
                       (= mfrec cant)
                       (values (list val) cant)]
                      ;[(= cant mfrec)(values (cons val mod) mfrec)]
                      [else (values mod mfrec)]))]))
