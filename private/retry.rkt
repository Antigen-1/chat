#lang hasket
(require racket/port racket/string "error.rkt")
(provide (rename-out (retry/raise retry))
         Left
         Right)

;; Yv combinator
(define (Yv t)
  (define make-maker (lambda/curry/match #:name make-maker ((m k x) (k (lambda (y) (m m k y)) x))))
  (make-maker make-maker t))

;; Utilities
;; Only the linefeed character is allowed to be used
(define (align-lines s)
  (call-with-input-string
   s
   (lambda (in)
     (string-join
      (for/list ((l (in-lines in)))
        (format "\t~a" l))
      "\n"))))

;; Try for at most n+1 times
;; Retry for at most n times
(define make-retry
  (lambda/curry/match
   #:name make-retry
   ((retry n try)
    (>>>/steps
     ($
      (lambda/curry/match
       #:name retry-limit-checker
       (((errorR (at value position)))
        (if (<= n 0)
            (Left (exn:fail:chat:retry-limit
                   (string-append "make-retry: hit the limit\n"
                                  (format "Depth: ~a\n" (length position))
                                  (format "Its last attempt fails due to:\n~a" (align-lines value)))
                   (current-continuation-marks)))
            (Right #f))))
      ((retry (sub1 n)) try))
     (lambda (_) (try))))))

;; functions
(define retry (Yv make-retry))
(define retry/raise
  (lambda/curry/match
   #:name retry/raise
   ((n try)
    (>>> #f
         ($ (lambda/curry/match #:name retry-raiser (((errorR (at value _))) (raise value))))
         ((retry n) try)))))
