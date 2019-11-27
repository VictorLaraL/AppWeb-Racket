#lang web-server/insta

(require "funciones.rkt")

;; Lista de datos ingresados
(define ELEMENTS
  (list ))

;; Arrancamos la pagina y hacemos una peticion para corroborar si existen datos para ingresar
(define (start request)
  (render-page7 ELEMENTS request))

;; Render of the page
(define (render-page7 listElements request)
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
      (render-page7
       (cons (parse-post (request-bindings request))
             listElements)
       request))
    (send/suspend/dispatch response-generator))

(static-files-path "htdocs") 

;; Extraccion del dato y casteo a numero
(define (parse-post bindings)
  (string->number
   (extract-binding/single 'elemento bindings)))

;; Impresion de la lista de elementos
(define (render-list listElements)
  `(div ((class "list"))
        ,(slist->string listElements)))


;; Impresion de el resultado del algorithmo
(define (render-result listElements)
  `(div ((class "result"))
        ,(number->string
          (maximoL listElements))))
