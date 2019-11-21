#lang web-server/insta
;; Struct of a post of an algorithm
(struct post (result))

(define ALGORITHMS
  (post "empty"))

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
          (body (h1 "Max of a list"), (render-result algorithms)
                (form
                 (input ((name "calculate")))
                 (input ((type "submit")))
                 )))))

;; Check of the input information
(define (can-parse-post? bindings)
  (exists-binding? 'calculate bindings))

(define (parse-post bindings)
  (post (extract-binding/single 'calculate bindings)))

;; Render of the result
(define (render-result algorithms)
  `(div ((class "result"))
        ,(post-result algorithms)))