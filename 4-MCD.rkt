#lang web-server/insta

(require "funciones.rkt")

;; Estructura del post
(struct post (n k) #:transparent)

;; Lista de resultados
(define RESULTS
  (list ))

;; Arrancamos la pagina y hacemos una peticion para corroborar si existen datos para ingresar
(define (start request)
  (define a-calculate
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) RESULTS)]
          [else RESULTS]))
  (render-page a-calculate request))

;; Render of the page
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

;; Check of the input information
(define (can-parse-post? bindings)
  (and (exists-binding? 'calculate-n bindings)
       (exists-binding? 'calculate-k bindings)))

(define (parse-post bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))
        (string->number
   (extract-binding/single 'calculate-k bindings))))


;; Render of the results
(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(number->string
          (mcd (post-n result) (post-k result)))))