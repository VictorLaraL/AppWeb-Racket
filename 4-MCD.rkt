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
          (body (h1 "MCD de dos enteros negativos (n,k)"), (render-results results)
                (form
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (mcd a b)")
                (p "(cond [(< a 0) (let ([a (* a -1)]")
                (p "[b (* b -1)])")
                (p "(mcd a b))]")
                (p "[(< b a) (mcd b (- a b))]")
                (p "[(< a b) (mcd a (- b a))]")
                (p "[else a]))")))))

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
          (mcd (post-n result) (post-k result)))))