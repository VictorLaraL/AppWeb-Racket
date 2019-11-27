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
            (body (h1 "Merge Sort"), (render-result listElements)
                (form ((action , (embed/url insert-post-handler)))
                 (input ((name "elemento")))
                 (input ((type "submit")))
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


(define (parse-post bindings)
  (string->number
   (extract-binding/single 'elemento bindings)))


(define (render-list listElements)
  `(div ((class "list"))
        ,(slist->string listElements)))


(define (render-result listElements)
  `(div ((class "result"))
        ,(slist->string
          (moda listElements))))
