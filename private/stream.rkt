#lang racket/base
(require racket/stream "error.rkt")
(provide stream-map* stream-last)

(define (stream-last s)
  (cond ((stream-empty? (stream-rest s)) (stream-first s))
        (else (stream-last (stream-rest s)))))
(define (stream-map* proc . ss)
  (if (ormap stream-empty? ss)
      empty-stream
      (stream-cons #:eager (apply proc (map stream-first ss))
                   (apply stream-map* proc (map stream-rest ss)))))
