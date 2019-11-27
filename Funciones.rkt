#lang racket

;; Funcion para pasar una lista de numeros a un string
(define (slist->string slst)
  (cond ((empty? slst) "")
        ((empty? (rest slst)) (number->string (first slst)))
        (else (string-append (number->string (first slst))
                             " "
                             (slist->string (rest slst))))))

;; 1)
(define (fact a)
  (if (= a 0) 1
      (* a (fact (- a 1)))))

(define (coefBin n k)
  (cond
  [(< n 0) 0]
  [(< k 0) 0]
  [(<= n k) 0]
  [else (/ (fact n) (* (fact (- n k)) (fact k)))]))

;; 2)
(define (combinaciones n k)
  (cond [(= k n) 1]
        [(= k 0) 1]
        [(> k n) 0]
        [else (+ (combinaciones (- n 1) (- k 1)) (combinaciones (- n 1) k))]))

;; ideamos dos formas de resolver el algoritmo
;; 3)
(define (mergelistas l1 l2)
   (cond
    [(null? l1) l2]
    [(null? l2) l1]
    [(< (car l1) (car l2)) (cons (car l1) (mergelistas (cdr l1) l2))]
    [else (cons (car l2) (mergelistas l1 (cdr l2)))]))

(define (mergesort lista)
  (cond [(or (null? lista) (null? (cdr lista))) lista]
        [(null? (cdr (cdr lista))) (mergelistas (list (car lista)) (cdr lista))]
        [else (let ([x (floor (/ (length lista) 2))])
                (mergelistas (mergesort (take lista x)) (mergesort (drop lista x))))]))

;; 3)
(define (UnirListas L M)
    (if (null? L) M
		(if (null? M) L
			(if (< (car L) (car M))
				(cons (car L) (UnirListas (cdr L) M))
				(cons (car M) (UnirListas (cdr M) L))))))
(define (ListaImpar L)
	(if (null? L) '()
		(if (null? (cdr L)) (list (car L))
			(cons (car L) (ListaImpar (cddr L))))))
(define (ListaPar L)
	(if (null? L) '()
		(if (null? (cdr L)) '()
			(cons (cadr L) (ListaPar (cddr L))))))
(define (Separar L)
	(cons (ListaImpar L) (cons (ListaPar L) `())))

(define (MergeSort L)
	(if (null? L) L
		(if (null? (cdr L)) L
			(UnirListas
				(MergeSort (car (Separar L)))
				(MergeSort (cadr (Separar L)))))))

;; 4)
(define (mcd a b)
  (cond [(< a 0) (let ([a (* a -1)]
                       [b (* b -1)])
                   (mcd a b))]
        [(< b a) (mcd b (- a b))]
        [(< a b) (mcd a (- b a))]
        [else a]))


;; 5)
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

;; 6)
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


;; 7)
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

;; 9)
(define (impares a)
  (if (= a 0) 0
      (+ (- (* a 2) 1) (impares (- a 1)))))

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

;; Enviar datos 
(provide (all-defined-out))
