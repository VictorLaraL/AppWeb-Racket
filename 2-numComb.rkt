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
          (body (h1 "Calculo del numero combinatorio C(n,k)"), (render-results results)
                (form
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (combinaciones n k)")
                (p "(cond [(= k n) 1]")
                (p "[(= k 0) 1]")
                (p "[(> k n) 0]")
                (p "[else (+ (combinaciones (- n 1) (- k 1)) (combinaciones (- n 1) k))]))")))))

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
        ,(number->string
          (combinaciones (post-n result) (post-k result)))))