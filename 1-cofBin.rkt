#lang web-server/insta

(require "funciones.rkt");; Importamos el fichero donde almacenamos nuestras funciones

;; Estructura del post
(struct post (n k) #:transparent)

;; Lista de resultados
(define RESULTS
  (list ))

;; Arrancamos la pagina y hacemos una peticion para corroborar si existen datos para ingresar
(define (start request)
  (define a-calculate
    (cond [(can-parse-post? (request-bindings request));; Condicional que corrobora si existen datos en el request para agregar a la lista
           (cons (parse-post (request-bindings request)) RESULTS)]
          [else RESULTS]))
  (render-page a-calculate request));; Enviamos la lista y el request

;; Renderizamos la pagina
(define (render-page results request)
  (response/xexpr
   `(html (head (title "Racket Web App")
                (link ((rel "stylesheet");; Estilos para la pagina (CSS)
                       (href "/test-static.css")
                       (type "text/css"))))
          (body (h1 "Coeficientes binomiales (n,k)"), (render-results results);; Llamamos a la funcion que renderiza los resultados
                (form
                 (input ((name "calculate-n")));; Datos de entrada
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo");; Algoritmo en la pagina
                (p "(define (fact a)")
                (p "(if (= a 0) 1")
                (p "(* a (fact (- a 1)))))")
                (p " ")
                (p "(define (coefBin n k)")
                (p "(cond")
                (p "[(< n 0) 0]")
                (p "[(< k 0) 0]")
                (p "[(<= n k) 0]")
                (p "[else (/ (fact n) (* (fact (- n k)) (fact k)))]))")))))

(static-files-path "htdocs");; Importamos la carpeta de los estilos

;; Verificacion de datos de entrada 
(define (can-parse-post? bindings)
  (and (exists-binding? 'calculate-n bindings)
       (exists-binding? 'calculate-k bindings)))

;; Obtencion y casteo de los datos (los regresamos como un post de numeros n,k)
(define (parse-post bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))
        (string->number
   (extract-binding/single 'calculate-k bindings))))


;; Renderizado de los resultados 
(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(number->string
          (coefBin (post-n result) (post-k result)))))
