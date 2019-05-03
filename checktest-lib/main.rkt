#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/string
         syntax/srcloc)
(provide test
         tests)

;; This is my bikeshed. There are many like it, but this one is mine.

;; ============================================================
;; Tests

;; A Test is an expression; testing is done by effects.

;; A (test _) expression
;; - catches exceptions (but not continuation escapes, (exit), etc)
;; - is selectively executable: that is, users SHOULD design tests so that
;;   surrounding code does not break if a (test _) form is not evaluated.
;;   For example, the following is bad:
;;     (test .... (open-output-file "the-file.txt") ...)
;;     (delete-file "the-file.txt")
;;   because if the test does not run, the file will not exist and the delete
;;   will fail.

;; TODO:
;; - around-hooks
;;   - built-in around-hook for selective execution
;;   - use around-hook for skipping?
;;   - global (parameter) vs local around-hooks?

(begin-for-syntax
  (define (stx->loc-expr stx)
    #`(quote-syntax #,(datum->syntax #f 'SRCLOC stx)))

  (define-splicing-syntax-class test-name-clause
    (pattern (~seq #:name (~var n (expr/c #'(or/c string? #f)))) ;; FIXME
             #:with name #'n.c))
  (define-splicing-syntax-class test-loc-clause
    (pattern (~seq #:location (~var loc (expr/c #'source-location?)))
             #:with location #'loc.c)
    (pattern (~seq #:location-syntax term)
             #:with location (stx->loc-expr #'term)))
  )

(define-syntax test
  (syntax-parser
    [(_ (~alt (~optional n:test-name-clause)
              (~optional loc:test-loc-clause)) ...
        body:expr ...)
     #`(-test #:name (~? n.name #f)
              #:loc (~? loc.location #,(stx->loc-expr this-syntax))
              (lambda () body ... (void)))]))

(define-syntax tests
  (syntax-parser
    [(_ e:expr ...)
     #`(begin (test e) ...)]))

;; A TestContext is (listof TestFrame)
;; A TestFrame is (hash 'name (U String #f) 'loc (U source-location? #f))
(define current-test-context (make-parameter null))

(define (test-context->string ctx)
  (string-join (reverse
                (for/list ([frame (in-list ctx)])
                  (cond [(hash-ref frame 'name #f)
                         => (lambda (name) name)]
                        [(hash-ref frame 'loc #f)
                         => (lambda (loc) (source-location->string loc))]
                        [else "???"])))
               " > "))

(define (-test proc
               #:loc  loc    ;; (U source-location? #f)
               #:name name)  ;; (U string? 'auto #f)
  (parameterize ((current-test-context
                  (cons (hasheq 'name name 'loc loc)
                        (current-test-context))))
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) (-test-handler e))])
      (proc)
      (-test-success))))

(define (-test-handler e)
  (eprintf "TEST FAILED: ~a\n  exn: ~e\n"
           (test-context->string (current-test-context))
           e)
  (void))

(define (-test-success) (void))
