;;2
(define (combinaciones n k)
  (cond [(= k n) 1]
        [(= k 0) 1]
        [else (+ (combinaciones (- n 1) (- k 1)) (combinaciones (- n 1) k))]))

;;3
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

;;4

(define (mcd a b)
  (cond [(< a 0) (let ([a (* a -1)]
                       [b (* b -1)])
                   (mcd a b))]
        [(< b a) (mcd b (- a b))]
        [(< a b) (mcd a (- b a))]
        [else a]))
