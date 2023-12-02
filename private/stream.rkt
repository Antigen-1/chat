#lang racket/base
(require racket/stream "error.rkt")
(provide stream-map* stream-map** stream-last (struct-out fail))

;; Y combinator
(define (Y t)
  (define ((make-maker m) t) (t (lambda () ((m m) t))))
  ((make-maker make-maker) t))

;; Try for at most n+1 times
;; Retry for at most n times
(struct fail (message))
(define ((make-retry retry) try n)
  (let ((result (try)))
    (cond ((and (fail? result) (> n 0)) ((retry) try (sub1 n)))
          ((fail? result)
           (raise (exn:fail:chat:retry-limit
                   (format "make-retry: hit the limit~a"
                           (cond ((fail-message result)
                                  =>
                                  (lambda (msg) (format "\nIts last attempt fails due to:~a\n" msg)))
                                 (else "")))
                   (current-continuation-marks))))
          (else result))))
(define retry (Y make-retry))

(define (stream-last s)
  (cond ((stream-empty? (stream-rest s)) (stream-first s))
        (else (stream-last (stream-rest s)))))
(define (stream-map* proc . ss)
  (if (ormap stream-empty? ss)
      empty-stream
      (stream-cons #:eager (apply proc (map stream-first ss))
                   (apply stream-map* proc (map stream-rest ss)))))
(define (stream-map** proc #:retry-limit (limit 0) . ss)
  (define (np . as) (retry (lambda () (apply proc as)) limit))
  (apply stream-map* np ss))
