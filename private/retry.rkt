#lang hasket
(require hasket/unsafe racket/port racket/string "error.rkt")
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

;; Structures
(struct raised (str dep))
(struct info (dep))

;; Data abstraction
(define (init-info)
  (info 0))
(define update-info
  (lambda/curry/match
   #:name info-updater
   (((info dep))
    (info (add1 dep)))))

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
       (((errorR (at (raised str i) _)))
        (define dep (info-dep i))
        (if (= n dep)
            (raise (exn:fail:chat:retry-limit
                    (string-append "make-retry: hit the limit\n"
                                   (format "Depth: ~a\n" (add1 n))
                                   (format "Its last attempt fails due to:\n~a" (align-lines str)))
                    (current-continuation-marks)))
            (Right (update-info i)))))
      ((retry n) try))
     (lambda (c)
       (define (return v) (if (raised? v) (Left v) (Right v)))
       (return
        (>>>
         #f
         ($ (lambda/curry/match
             #:name left-wrapper
             (((errorR (at value _))) (Right (raised value c)))))
         (lambda (_) (try)))))))))

;; functions
(define retry (Yv make-retry))
(define retry/raise
  (lambda/curry/match
   #:name retry/raise
   ((n try)
    (>>> (init-info) ((retry n) try)))))
