#lang racket/base
(provide (struct-out exn:fail:chat) (struct-out exn:fail:chat:retry-limit))

(struct exn:fail:chat exn:fail ())
(struct exn:fail:chat:retry-limit exn:fail:chat ())
