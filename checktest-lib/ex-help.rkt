#lang racket/base
(require "main.rkt")
(provide test-list)

(define (test-list xs)
  (test #:name "is list" (check xs list?))
  (test #:name "nonempty" (check (length xs) (lambda (l) (> l 0)))))
