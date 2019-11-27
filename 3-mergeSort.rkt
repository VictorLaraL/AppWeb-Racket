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
            (body (h1 "Merge Sort"), (render-result listElements);; imprimimos el resultado
                (form ((action , (embed/url insert-post-handler)));; Formulario para ingresar los datos
                 (input ((name "elemento")))
                 (input ((type "submit")));; Boton para ingresar los datos (a travez de un request)
                 ),
                (render-list listElements)
                  (h2 "Algoritmo")
                  (p "(define (mergelistas l1 l2)")
                  (p " (cond")
                  (p "  [(null? l1) l2]")
                  (p "  [(null? l2) l1]")
                  (p "  [(< (car l1) (car l2)) (cons (car l1) (mergelistas (cdr l1) l2))]")
                  (p "  [else (cons (car l2) (mergelistas l1 (cdr l2)))]))")

                  (p "(define (mergesort lista)")
                  (p " (cond [(or (null? lista) (null? (cdr lista))) lista]")
                  (p "  [(null? (cdr (cdr lista))) (mergelistas (list (car lista)) (cdr lista))]")
                  (p "  [else (let ([x (floor (/ (length lista) 2))])")
                  (p "    (mergelistas (mergesort (take lista x)) (mergesort (drop lista x))))]))")))))
    
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
        ,(slist->string
          (MergeSort listElements))))
