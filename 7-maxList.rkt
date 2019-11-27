#lang web-server/insta

(require "funciones.rkt")

;; Lista de datos ingresados
(define ELEMENTS
  (list ))

;; Arrancamos la pagina, esta vez sin corroborar los datos
(define (start request)
  (render-page ELEMENTS request))

;; Renderizado de la pagina
(define (render-page listElements request)
  (define (response-generator embed/url);; esta funcion nos va a permitir no reiniciar la pagina y solo enviar y recibir datos
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet");; Estilos para la pagina
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Max en lista"), (render-result listElements);; Imprimimos el resultado
                (form ((action , (embed/url insert-post-handler)));; Formulario para ingresar los datos
                 (input ((name "elemento")))
                 (input ((type "submit")));; Boton para ingresar los datos (a travez de un request)
                 ),
                (render-list listElements);; Impresion de la lista generada
                  (h2 "Algoritmo");; Algoritmo renderizado en la pagina
                  (p "(define (mayor a b)")
                  (p "(if (> a b) a b))")

                  (p "(define (maximoL l)")
                  (p "(cond")
                  (p "[(null? l) 0]")
                  (p "[(= (length l) 1) (car l)]")
                  (p "[else (mayor (car l) (maximoL (cdr l)))]))")))))
    
    (define (insert-post-handler request) 
      (render-page
       (cons (parse-post (request-bindings request))
             listElements)
       request))
    (send/suspend/dispatch response-generator));; Utilizamos este apartado para complementar los embed/url

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
