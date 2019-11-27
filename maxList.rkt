#lang web-server/insta
; start: request -> response
(define (start request)
  (home request))
 
; home: request -> response
(define (phase-1 request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (body (h1 "algoritmo 1 (coeficientes binomiales)")
             (a ((href ,(embed/url phase-2)))
                "click me!")))))
  (send/suspend/dispatch response-generator))
 
; 1) request -> response
(define (coefBin request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (body (h1 "Phase 2")
             (a ((href ,(embed/url phase-1)))
                "click me!")))))
  (send/suspend/dispatch response-generator))

; 7) request -> response
(define (maxList request)
  (define (response-generator embed/url)
    (response/xexpr
     `(html
       (body (h1 "Phase 2")
             (a ((href ,(embed/url phase-1)))
                "click me!")))))
  (send/suspend/dispatch response-generator))