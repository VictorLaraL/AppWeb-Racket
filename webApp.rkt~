#lang web-server/insta

;; Cuerpo de la estructura que tendra cada algoritmo en un post diferente
(struct post (title algorithm explication code))


(define BLOG
  (list (post "Max of a list" "algorithm" "this algorithm fin the max number in a simple list" "code")
        (post "Min of a list" "algorithm")))

;; Lanzamos la aplicacion en forma local y hacemos una peticion con request
(define (start request)
  (response/xexpr
   '(html
     (head (title "My first web in raquet"))
     (body (h2 "My blog is done"))
     )
   )
  )

;; Funciones para desplegar el indice de los algoritmos 
(define (render-as-itemized-list fragments)
  '(ul ,@ (map render-as-item fragments)))

(define (render-as-item fragments)
  '(li , fragment))