#lang web-server/insta

(require "funciones.rkt")

;; Lista de datos ingresados
(define ELEMENTS
  (list ))

;; Arrancamos la pagina y hacemos una peticion para corroborar si existen datos para ingresar
(define (start request)
  (define listElements
    (cond [(can-parse-post? (request-bindings request)) ;; Verifica que exista un dato
           (cons (parse-post (request-bindings request)) ELEMENTS)] ;; Agrega el dato a la lista ELEMENTS
          [else ELEMENTS]))
  (render-page listElements request))

;; Render of the page
(define (render-page listElements request)
  (response/xexpr
   `(html (head (title "Racket Web App")
                (link ((rel "stylesheet");; Estilos para la pagina
                       (href "/test-static.css")
                       (type "text/css"))))
          (body (h1 "Max en lista"), (render-result listElements);; imprimimos el resultado
                (form ;; Formulario para ingresar los datos
                 (input ((name "elemento")))
                 (input ((type "submit")));; Boton para ingresar los datos (a travez de un request)
                 ),
                (render-list listElements)))));; Impresion de la lista de elementos

(static-files-path "htdocs")

;; Verificacion de que exsta un dato a ingresar
(define (can-parse-post? bindings)
 (exists-binding? 'elemento bindings))

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