#lang hasket
(require "error.rkt")
(provide (rename-out (report-failure Left)
                     (retry/raise retry))
         Right)

;; Yv combinator
(define (Yv t)
  (define make-maker (lambda/curry/match #:name make-maker ((m k x) (k (lambda (y) (m m k y)) x))))
  (make-maker make-maker t))

;; The log function is called each time fail structure is created
;; The value field is always a string
(define (report-failure (msg #f) (log void))
  (define (message->string msg) (if msg msg "unknown"))
  (define str (message->string msg))
  (log str)
  (Left str))

;; Try for at most n+1 times
;; Retry for at most n times
(define make-retry
  (lambda/curry/match
   #:name make-retry
   ((retry n try)
    (pipeline
     (Right #f)
     ($
      (lambda/curry/match
       #:name retry-limit-checker
       (((errorR (at value position)))
        (if (<= n 0)
            (Left (exn:fail:chat:retry-limit
                   (string-append "make-retry: hit the limit\n"
                                  (format "position: ~a\n" position)
                                  (format "Its last attempt fails due to:~a\n" value))
                   (current-continuation-marks)))
            (Right #f))))
      (lambda (_) ((retry n) try)))
     (lambda (_) (try))))))

;; functions
(define retry (Yv make-retry))
(define retry/raise
  (lambda/curry/match
   #:name retry/raise
   ((n try)
    (>>> #f
         ($ (lambda/curry/match #:name retry-raiser (((errorR (at value _))) (raise value))))
         (lambda (_) ((retry n) try))))))
