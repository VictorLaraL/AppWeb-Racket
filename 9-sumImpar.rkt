#lang web-server/insta

(require "funciones.rkt")

;; Estructura del post
(struct post (n) #:transparent)

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
          (body (h1 "Suma de los primeros n numeros pares"), (render-results results)
                (form
                 (input ((name "calculate-n")))
                 (input ((type "submit")))
                 )))))

(static-files-path "htdocs")

;; Check of the input information
(define (can-parse-post? bindings)
  (exists-binding? 'calculate-n bindings))

(define (parse-post bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))))

;; Render of the results
(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(number->string
          (impares (post-n result)))))