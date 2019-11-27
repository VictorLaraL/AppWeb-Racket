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
;; (MergeSort'(10 2 4 12 0 45 32 1 7))
(define (MergeSort L)
	(if (null? L) L
		(if (null? (cdr L)) L
			(UnirListas
				(MergeSort (car (Separar L)))
				(MergeSort (cadr (Separar L)))))))

;; 4)
(define (MCD X Y)
  (if (= Y 0) X
      (MCD Y (remainder X Y))))