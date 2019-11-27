#lang web-server/insta

(require "funciones.rkt")


(struct post (n k) #:transparent)


(define RESULTS
  (list ))


(define (start request)
  (define a-calculate
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) RESULTS)]
          [else RESULTS]))
  (render-page a-calculate request))


(define (render-page results request)
  (response/xexpr
   `(html (head (title "Racket Web App")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
          (body (h1 "Numeros primos en un rango"), (render-results results)
                (form
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (esprimo n)")
                (p "(cond [(= (modulo (+ n 1) 4) 0) (let loop2 ([n n] [j (+ (/ (+ n 1) 2) 1)])")
                (p "(cond [(= j 1) true]")
                (p "[(= (modulo n j) 0) false]")
                (p "[else (loop2 n (- j 2))]))]")
                (p "[else (let loop3 ([n n] [j (/ (+ n 1) 2)])")
                (p "(cond [(= j 1) true]")
                (p "[(= (modulo n j) 0) false]")
                (p "[else (loop3 n (- j 2))]))]))")
  
                (p "(define (listaprimos a b)")
                (p "(cond [(= (modulo a 2) 0) (= a (+ a 1))])")
                (p "(let loop ([primos '()] [i a])")
                (p "(cond [(> i b) primos]")
                (p "[(or (= i 1) (= i 2)) (loop (append primos (list i)) (+ i 1))]")
                (p "[(esprimo i) (loop (append primos (list i)) (+ i 2))]")
                (p "[else (loop primos (+ i 2))])))")))))

(static-files-path "htdocs")


(define (can-parse-post? bindings)
  (and (exists-binding? 'calculate-n bindings)
       (exists-binding? 'calculate-k bindings)))

(define (parse-post bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))
        (string->number
   (extract-binding/single 'calculate-k bindings))))


(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(slist->string
          (listaprimos (post-n result) (post-k result)))))
