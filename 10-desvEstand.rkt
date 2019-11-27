#lang web-server/insta

(require "funciones.rkt")


(define ELEMENTS
  (list ))


(define (start request)
  (render-page7 ELEMENTS request))


(define (render-page7 listElements request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Desviacion Estandar de una lista"), (render-result listElements)
                (form ((action , (embed/url insert-post-handler)))
                 (input ((name "elemento")))
                 (input ((type "submit")))
                 ),
                (render-list listElements)
                  (h2 "Algoritmo")
                  (p "(define (media lista)")
                  (p "(/ (apply + lista) (length lista)))")

                  (p "(define (diferenciacuadrada lista)")
                  (p "(map (lambda (x) (* x x))")
                  (p "(map (lambda (x) (- x (media lista))) lista)))")

                  (p "(define (sumacuadrados lista)")
                  (p "(apply + (diferenciacuadrada lista)))")

                  (p "(define (varianza lista)")
                  (p "(/ (sumacuadrados lista) (length lista)))")

                  (p "(define (desviacionestandar lista)")
                  (p "(sqrt (varianza lista)))")))))
    
    (define (insert-post-handler request)
      (render-page7
       (cons (parse-post (request-bindings request))
             listElements)
       request))
    (send/suspend/dispatch response-generator))

(static-files-path "htdocs") 


(define (parse-post bindings)
  (string->number
   (extract-binding/single 'elemento bindings)))


(define (render-list listElements)
  `(div ((class "list"))
        ,(slist->string listElements)))


(define (render-result listElements)
  `(div ((class "result"))
        ,(number->string
          (desviacionestandar listElements))))
