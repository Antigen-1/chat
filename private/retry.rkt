#lang hasket
(require "error.rkt")
(provide retry (rename-out (report-failure fail)))

;; Yv combinator
(define (Yv t)
  (define make-maker (lambda/curry/match #:name make-maker ((m k x) (k (lambda (y) (m m k y)) x))))
  (make-maker make-maker t))

;; Data
;; The log function is called each time fail structure is created
;; The message field is always a string
(struct fail (message) #:constructor-name make-fail)
(define (report-failure (msg #f) (log void))
  (define (message->string msg) (if msg msg "unknown"))
  (define str (message->string msg))
  (log str)
  (make-fail str))

;; Try for at most n+1 times
;; Retry for at most n times
(define make-retry
  (lambda/curry/match
   #:name make-retry
   ((retry n try)
    (let ((result (try)))
      (cond ((and (fail? result) (> n 0)) ((retry (sub1 n)) try))
            ((fail? result)
             (raise (exn:fail:chat:retry-limit
                     (string-append "make-retry: hit the limit\n"
                                    (format "Its last attempt fails due to:~a\n" (fail-message result)))
                     (current-continuation-marks))))
            (else result))))))

;; Exported functions
(define retry (Yv make-retry))
