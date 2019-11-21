#lang web-server/insta
;; Estructura de cada post en l apagina
(struct post (title body))

;; Lista que resguarda el contenido de cada post
(define BLOG
  (list (post "Max of a list" "algorithm")
        (post "Min of a list" "algorithm")))

;; Lanzamos la aplicacion en forma local y hacemos una peticion con request
(define (start request) 
    (render-blog-page BLOG request))

;; Desplegamos la pagina con el contenido haciendo llamado a las demas funciones
(define (render-blog-page blog request)
  (local [(define (response-generator make-url)
    (response/xexpr
     `(html
       (head (title "Racket Web App"))
       (body (h1 "Recursive Algorithms"),
             (render-posts blog)
             (form ((action
                     ,(make-url insert-post-handler)))
                   (input ((name "title")) ;; Ingresamos datos a travez de la pagina
                   (input ((name "body")))
                   (input ((type "submit")))))))))
            
            (define (insert-post-handler request)
              (render-blog-page
               (cons (parse-post (request-bindings request))
                     blog)
               request))]
          (send/suspend/dispatch response-generator)))
     
 

;; Validacion para crear nuevos post en el blog
(define (can-parse-post? bindings)
  (and (exists-binding? 'title bindings)
       (exists-binding? 'body bindings)))

(define (parse-post bindings)
  (post (extract-binding/single 'title bindings)
        (extract-binding/single 'body bindings)))



;; Llama a la funcion render-post para ejecutar uno a uno los post almacenados en la lista
(define (render-posts blog)
  `(div ((class "posts"))
        ,@(map render-post blog)))

(define (render-post a-post)
  `(div ((class "a-post"))
        ,(post-title a-post)
        (p ,(post-body a-post))
        ))


;; Funciones para desplegar el indice de los algoritmos 
(define (render-as-itemized-list fragments)
  '(ul ,@ (map render-as-item fragments)))

(define (render-as-item fragments)
  '(li , fragment))