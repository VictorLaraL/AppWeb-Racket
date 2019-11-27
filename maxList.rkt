#lang web-server/insta

(require "funciones.rkt")

; start: request -> response
(define (start request)
  (home request))
 
; home: request -> response
(define (home request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Home Page")
                  (link ((rel "stylesheet");; Estilos para la pagina
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "algoritmo 1")
                  (a ((href ,(embed/url render-page7 )))
                     "Coeficientes Binomiales")))))
  (send/suspend/dispatch response-generator))
 
;; Algoritmo 7
(define (render-page7 request)
  (define listElements
  (list ))
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet");; Estilos para la pagina
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Max en lista"), (render-result listElements);; imprimimos el resultado
                (form ((action , (embed/url insert-post-handler)));; Formulario para ingresar los datos
                 (input ((name "elemento")))
                 (input ((type "submit")));; Boton para ingresar los datos (a travez de un request)
                 ),
                (render-list listElements)))))
    
    (define (insert-post-handler request)
      (cons (parse-post (request-bindings request))
             listElements)
      (render-page7 request))
    (send/suspend/dispatch response-generator))

(static-files-path "htdocs") 
;; Extraccion del dato y casteo a numero
(define (parse-post bindings)
  (string->number
   (extract-binding/single 'elemento bindings)))

;; Impresion de la lista de elementos
(define (render-list listElements)
  `(div ((class "list"))
        ,@(map render-element listElements)))

(define (render-element element)
  `(div ((class "element"))
        ,(number->string element)))

;; Impresion de el resultado del algorithmo
(define (render-result listElements)
  `(div ((class "result"))
        ,(number->string
          (maximoL listElements))))