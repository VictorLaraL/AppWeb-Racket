#lang web-server/insta
;; Struct of a post of an algorithm
(struct post (result) #:transparent)

(define ALGORITHMS
  (list (post 1)))

;; Start the index page  
(define (start request)
  (define a-calculate
    (cond [(can-parse-post? (request-bindings request))
           (cons (parse-post (request-bindings request)) ALGORITHMS)]
          [else ALGORITHMS]))
  (render-page a-calculate request))

;; Render of the page
(define (render-page algorithms request)
  (response/xexpr
   `(html (head (title "Racket Web App"))
          (body (h1 "Max of a list"), (render-results algorithms)
                (form
                 (input ((name "calculate")))
                 (input ((type "submit")))
                 )))))

;; Check of the input information
(define (can-parse-post? bindings)
  (exists-binding? 'calculate bindings))

(define (parse-post bindings)
  (post (get-number bindings)))

(define (get-number bindings)
  (string->number
   (extract-binding/single 'calculate bindings)))

;; Render of the results
(define (render-results algorithms)
  `(div ((class "results"))
        ,@(map render-result algorithms)))

(define (render-result result)
  `(div ((class "result"))
        ,(fact(post-result result))))

;; Algorithm
(define (fact a)
  (if (= a 0) 1
      (* a (fact (- a 1)))))