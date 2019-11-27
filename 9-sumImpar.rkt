#lang web-server/insta

(require "funciones.rkt")


(struct post (n) #:transparent)


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
          (body (h1 "Suma de los primeros n numeros pares"), (render-results results)
                (form
                 (input ((name "calculate-n")))
                 (input ((type "submit")))
                 (h2 "Algoritmo")
                 (p "(define (impares a)")
                 (p "(if (= a 0) 0")
                 (p "(+ (- (* a 2) 1) (impares (- a 1)))))"))))))

(static-files-path "htdocs")


(define (can-parse-post? bindings)
  (exists-binding? 'calculate-n bindings))

(define (parse-post bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))))


(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(number->string
          (impares (post-n result)))))