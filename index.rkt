#lang web-server/insta

(require "functions.rkt");; Importamos el fichero donde almacenamos nuestras funciones

;; Estructura del input de los algoritmos: 1, 2, 4, 6
(struct post (n k) #:transparent)

;; Estructura del input del algoritmo 9
(struct postSI (n) #:transparent)

;; start: request -> response
(define (start request)
  (home request))
 
;; ---------------------------------------------- Home Page --------------------------------------------------- 
(define (home request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
                (link ((rel "stylesheet");; Estilos para la pagina (CSS)
                       (href "/test-static.css")
                       (type "text/css"))))
       (body (h1 "Home Page")
             (p (a ([href ,(embed/url render-page-CB)])
                "Coeficientes Binomiales"))
             (p (a ((href ,(embed/url render-page-NC)))
                "Numeros Combinatorios"))
             (p (a ([href ,(embed/url render-page-MS)])
                "Merge Sort"))
             (p (a ([href ,(embed/url render-page-MCD)])
                "MCD"))
             (p (a ([href ,(embed/url render-page-PR)])
                "Numeros Primos"))
             (p (a ([href ,(embed/url render-page-MX)])
                "Maximo en lista"))
             (p (a ([href ,(embed/url render-page-MN)])
                "Minimo en lista"))
             (p (a ([href ,(embed/url render-page-SI)])
                "Suma de numeros impares"))
             (p (a ([href ,(embed/url render-page-DE)])
                "Desviacion estandar"))))))
  
    (send/suspend/dispatch response-generator))

; ---------------------------------- 1) Coeficientes Binomiales ---------------------------------------------

(define (render-page-CB request [results '()])
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (title "Racket Web App")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
          (body (h1 "Coeficientes binomiales (n,k)"), (render-results-CB results)
                (form ((action ,(embed/url insert-post-handler))) 
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (fact a)")
                (p "(if (= a 0) 1")
                (p "(* a (fact (- a 1)))))")
                (p " ")
                (p "(define (coefBin n k)")
                (p "(cond")
                (p "[(< n 0) 0]")
                (p "[(< k 0) 0]")
                (p "[(<= n k) 0]")
                (p "[else (/ (fact n) (* (fact (- n k)) (fact k)))]))")
                (p (a ([href ,(embed/url home)])
                "Home"))))))  
    (define (insert-post-handler request)
      (render-page-CB  request (cons (parse-posts (request-bindings request))
             results)))
  (send/suspend/dispatch response-generator))

;; Renderizado de los resultados: alg 1
(define (render-results-CB results)
  `(div ((class "results"))
        ,@(map render-result-CB results)))

(define (render-result-CB result)
  `(div ((class "result"))
        ,(number->string
          (coefBin (post-n result) (post-k result)))))


; --------------------------------- 2) Numeros Combinatorios -------------------------------------------------

(define (render-page-NC request [results '()])
    (define (response-generator embed/url)
      (response/xexpr
       `(html (head (title "Racket Web App")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
              (body (h1 "Calculo del numero combinatorio C(n,k)"), (render-results-NC results)
                (form ((action ,(embed/url insert-post-handler)))
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit"))))
                (h2 "Algoritmo")
                (p "(define (combinaciones n k)")
                (p "(cond [(= k n) 1]")
                (p "[(= k 0) 1]")
                (p "[(> k n) 0]")
                (p "[else (+ (combinaciones (- n 1) (- k 1)) (combinaciones (- n 1) k))]))")
                (p (a ([href ,(embed/url home)])
                "Home"))))))
    (define (insert-post-handler request)
      (render-page-NC request (cons (parse-posts (request-bindings request))
             results)))
 (send/suspend/dispatch response-generator))

;; Renderizado de los resultados: alg 2
(define (render-results-NC results)
  `(div ((class "results"))
        ,@(map render-result-NC results)))

(define (render-result-NC result)
  `(div ((class "result"))
        ,(number->string
          (combinaciones (post-n result) (post-k result)))))

;-------------------------------------------- 3) Merge Sort -------------------------------------------------

(define (render-page-MS request [listElements '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Merge Sort"), (render-result-MS listElements)
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
                  (p "    (mergelistas (mergesort (take lista x)) (mergesort (drop lista x))))]))")
                  (p (a ([href ,(embed/url home)])
                        "Home"))))))
    (define (insert-post-handler request)
      (render-page-MS request (cons (parse-post (request-bindings request))
             listElements)))
    (send/suspend/dispatch response-generator))

;; Renderizado de la lista de resultados: alg 3
(define (render-result-MS listElements)
  `(div ((class "result"))
        ,(slist->string
          (MergeSort listElements))))

;------------------------------------------------ 4) MCD ----------------------------------------------------

(define (render-page-MCD request [results '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "MCD de dos enteros negativos (n,k)"), (render-results-MCD results)
                (form ((action ,(embed/url insert-post-handler)))
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (mcd a b)")
                (p "(cond [(< a 0) (let ([a (* a -1)]")
                (p "[b (* b -1)])")
                (p "(mcd a b))]")
                (p "[(< b a) (mcd b (- a b))]")
                (p "[(< a b) (mcd a (- b a))]")
                (p "[else a]))")
                (p (a ([href ,(embed/url home)])
                "Home"))))))
  (define (insert-post-handler request)
      (render-page-MCD request (cons (parse-posts (request-bindings request))
             results)))
 (send/suspend/dispatch response-generator))

;; Renderizado de los resultados: alg 4 
(define (render-results-MCD results)
  `(div ((class "results"))
        ,@(map render-result-MCD results)))

(define (render-result-MCD result)
  `(div ((class "result"))
        ,(number->string
          (mcd (post-n result) (post-k result)))))

;----------------------------------------- 6) Numeros Primos ------------------------------------------------

(define (render-page-PR request [results '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
                 (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Numeros primos en un rango"), (render-results-PR results)
                (form ((action ,(embed/url insert-post-handler)))
                 (input ((name "calculate-n")))
                 (input ((name "calculate-k")))
                 (input ((type "submit")))
                 )
                (h2 "Algoritmo")
                (p "(define (esprimo n)")
                (p "(cond [(= (modulo (+ n 1) 4) 0) (let loop2 ([n n] [j (+ (/ (+ n 1) 2) 1)])")
                (p "(cond [(= j 1) true]")
                (p "[(= (modulo n j) 0) false]")
                (p "[else (loop2 n (- j 2))]))]")
                (p "[else (let loop3 ([n n] [j (/ (+ n 1) 2)])")
                (p "(cond [(= j 1) true]")
                (p "[(= (modulo n j) 0) false]")
                (p "[else (loop3 n (- j 2))]))]))")
  
                (p "(define (listaprimos a b)")
                (p "(cond [(= (modulo a 2) 0) (= a (+ a 1))])")
                (p "(let loop ([primos '()] [i a])")
                (p "(cond [(> i b) primos]")
                (p "[(or (= i 1) (= i 2)) (loop (append primos (list i)) (+ i 1))]")
                (p "[(esprimo i) (loop (append primos (list i)) (+ i 2))]")
                (p "[else (loop primos (+ i 2))])))")
                (p (a ([href ,(embed/url home)])
                "Home"))))))
  (define (insert-post-handler request)
      (render-page-PR request (cons (parse-posts (request-bindings request))
             results)))
 (send/suspend/dispatch response-generator))

;; Renderizamos los resultados: alg 6
(define (render-results-PR algorithms)
  `(div ((class "results"))
        ,@(map render-result-PR algorithms)))

(define (render-result-PR result)
  `(div ((class "result"))
        ,(slist->string
          (listaprimos (post-n result) (post-k result)))))


; ---------------------------------------- 7) Max en lista --------------------------------------------------

(define (render-page-MX request [listElements '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Max en lista"), (render-result-MX listElements)
                (form ((action , (embed/url insert-post-handler)))
                 (input ((name "elemento")))
                 (input ((type "submit")))
                 ),
                (render-list listElements)
                  (h2 "Algoritmo")
                  (p "(define (mayor a b)")
                  (p "(if (> a b) a b))")

                  (p "(define (maximoL l)")
                  (p "(cond")
                  (p "[(null? l) 0]")
                  (p "[(= (length l) 1) (car l)]")
                  (p "[else (mayor (car l) (maximoL (cdr l)))]))")
                  (p (a ([href ,(embed/url home)])
                "Home"))))))
    (define (insert-post-handler request) 
      (render-page-MX request (cons (parse-post (request-bindings request))
             listElements)))
    (send/suspend/dispatch response-generator))

;; Renderizado de el resultado: alg 7
(define (render-result-MX listElements)
  `(div ((class "result"))
        ,(number->string
          (maximoL listElements))))

; ---------------------------------------- 8) Min en lista --------------------------------------------------

(define (render-page-MN request [listElements '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Min en lista"), (render-result-MN listElements)
                (form ((action , (embed/url insert-post-handler)))
                 (input ((name "elemento")))
                 (input ((type "submit")))
                 ),
                (render-list listElements)
                  (h2 "Algoritmo")
                  (p "(define (menor a b)")
                  (p "(if (< a b) a b))")

                  (p "(define (minimoL l)")
                  (p "(cond")
                  (p "[(null? l) 0]")
                  (p "[(= (length l) 1) (car l)]")
                  (p "[else (mayor (car l) (maximoL (cdr l)))]))")
                  (p (a ([href ,(embed/url home)])
                "Home"))))))
    (define (insert-post-handler request)
      (render-page-MN request (cons (parse-post (request-bindings request))
             listElements)))
    (send/suspend/dispatch response-generator))

;; Renderizado del resultado: alg 8
(define (render-result-MN listElements)
  `(div ((class "result"))
        ,(number->string
          (minimoL listElements))))

; ------------------------------------ 9) Suma de numeros Impar ----------------------------------------------

(define (render-page-SI request [results '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Suma de los primeros n numeros pares"), (render-results-SI results)
                (form ((action ,(embed/url insert-post-handler))) 
                 (input ((name "calculate-n")))
                 (input ((type "submit")))
                 (h2 "Algoritmo")
                 (p "(define (impares a)")
                 (p "(if (= a 0) 0")
                 (p "(+ (- (* a 2) 1) (impares (- a 1)))))")
                 (p (a ([href ,(embed/url home)])
                "Home")))))))
  (define (insert-post-handler request)
      (render-page-SI  request (cons (parse-post-SI (request-bindings request))
             results)))
  (send/suspend/dispatch response-generator))

;; Renderizado de los resultados: alg 9 
(define (render-results-SI algorithms)
  `(div ((class "results"))
        ,@(map render-result-SI algorithms)))

(define (render-result-SI result)
  `(div ((class "result"))
        ,(number->string
          (impares (postSI-n result)))))

; ------------------------------------ 10) Desviacion Estandar ----------------------------------------------

(define (render-page-DE request [listElements '()])
  (define (response-generator embed/url)
    (response/xexpr
     `(html (head (title "Racket Web App")
            (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))
            (body (h1 "Desviacion Estandar de una lista"), (render-result-DE listElements)
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
                  (p "(sqrt (varianza lista)))")
                  (p (a ([href ,(embed/url home)])
                "Home"))))))
    (define (insert-post-handler request)
      (render-page-DE  request
       (cons (parse-post (request-bindings request))
             listElements)))
    (send/suspend/dispatch response-generator))

;; Renderizado del resutado: alg 10
(define (render-result-DE listElements)
  `(div ((class "result"))
        ,(number->string
          (desviacionestandar listElements))))

; ----------------------------------------------------------------------------------------------------------

(static-files-path "htdocs");; Importamos la carpeta de los estilos

;; Renderizado de la lista de elementos
(define (render-list listElements)
  `(div ((class "list"))
        ,(slist->string listElements)))

;; Obtencion y casteo de los datos (los regresamos como un post de numeros n,k)
(define (parse-posts bindings)
  (post (string->number
   (extract-binding/single 'calculate-n bindings))
        (string->number
   (extract-binding/single 'calculate-k bindings))))

;; Obtencion y casteo del dato (lo regresamos como un post de n)
(define (parse-post-SI bindings)
  (postSI (string->number
   (extract-binding/single 'calculate-n bindings))))

;; Obtencion y caesto del dato (se envia como un dato a la lista de elementos numericos)
(define (parse-post bindings)
  (string->number
   (extract-binding/single 'elemento bindings)))