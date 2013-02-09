;; Updated!
(define-module (stay-alive weight)
               #:export (tons pounds ounces kilos grams))

(define (tons t) (pounds (* 2000 t)))
(define (pounds lb) (ounces (* 16 lb)))
(define (ounces oz) (grams (* oz 28.35)))
(define (kilos k) (grams (* k 1000)))
(define (grams g) g)
