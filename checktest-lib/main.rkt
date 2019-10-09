#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         racket/match
         racket/contract/base
         racket/struct
         syntax/srcloc
         "private/result.rkt"
         "private/test.rkt"
         "private/check.rkt"
         "private/listener.rkt")
(provide (all-defined-out)
         (all-from-out "private/test.rkt")
         (all-from-out "private/check.rkt")
         (all-from-out "private/listener.rkt"))

;; This is my bikeshed. There are many like it, but this one is mine.

;; Design:

;; A Test is the unit of testing. A Test
;; - returns (void), and catches exceptions (but not other escapes)
;; - arbitrary contextual information can be attached to tests with `with-test-info`
;; - can contain nested sub-tests (ie, no distinction between test-case and test-suite)
;;   - sub-test failure does not abort the enclosing test (make configurable?)
;; - tests (and sub-tests) can be selectively skipped

;; Tests are generally written using `check` expressions:
;;   (check Expr Checker ...) : Expr[Void]
;; A `check` expression
;; - returns (void), but raises an exception on failure to abort the enclosing test
;; - arbitrary contextual information can be attached to checks with `with-check-info`
;; - can contain nested sub-checks
;;   - sub-check failure aborts enclosing check (and thus the enclosing test)
;;   - failure reporting preserves the nesting structure: "the outer
;;     check failed because the inner check failed" rather than just
;;     reporting one or the other (likewise, each check keeps its check
;;     info distinct; it is not flattened)
;; - checks cannot be selectively skipped

;; A Checker verifies properties about the behavior of an expression. A Checker
;; - does not actually control the execution context of the expression
;;   - TODO: allow special checkers to do so? (at most one per `check` expr?)
;; - in general, may receive zero or more values or a caught exception

;; TODO:
;; - allow a RunnerChecker to actually control the execution context of the expression
;;     prep : RunnerChecker -> ((-> Any) -> (-> Any)) x Checker  -- runner and checker can share state

;; TODO:
;; - with-test-info
;; - tests that don't print in vebose mode
;; - tests that don't continue on failure/error
;; - call-with-test-summary (run, passed, failed)
;; - for rackunit/log: count check vs test vs both?

;; TODO:
;; - `current-checker-conversions` for converting other values (eg, expectations) to checkers

;; TODO:
;; - `print-test-summary` print "N tests passed"/"K tests failed, N tests passed"


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
  (define-splicing-syntax-class test-pre-clause
    (pattern (~seq #:pre pre:expr))
    (pattern (~seq #:when c:expr) #:with pre #'(unless c (skip-test))))
  )

(define-syntax test
  (syntax-parser
    [(_ (~alt (~optional n:test-name-clause)
              (~optional loc:test-loc-clause)
              (~optional pre:test-pre-clause)) ...
        body:expr ...)
     #`(run-test #:name (~? n.name #f)
                 #:loc (~? loc.location #,(stx->loc-expr this-syntax))
                 #:pre (~? (lambda () (#%expression pre.pre)) #f)
                 (lambda () body ... (void)))]))

(define-syntax tests
  (syntax-parser
    [(_ e:expr ...)
     #`(begin (test #:location-syntax e e) ...)]))


;; ============================================================
;; Check

(define-syntax check
  (syntax-parser
    [(_ actual:expr (~var checker (expr/c #'checker/c)) ...)
     #`(run-checkers (lambda () (#%expression actual))
                     (list checker.c ...)
                     #,(stx->loc-expr this-syntax))]))

(define checker/c (or/c checker? (-> any/c any)))

(define-syntax-rule (with-info ([k v] ...) . body)
  (with-continuation-mark
    check-context-key
    (list* (~@ k v) ... (current-check-context))
    (let () . body)))

(define-syntax-rule (equal-checker expected)
  (checker:equal (thunk->values-result (lambda () (#%expression expected)))))
(define (raise-checker . pred/rx-list)
  (checker:raise pred/rx-list))
(define (predicate-checker pred #:negate? [negate? #f] . args)
  (checker:predicate pred args (and negate? #t)))

(define-syntax-rule (check-equal actual expected)
  (check actual (equal-checker expected)))
(define-syntax-rule (check-raise actual pred/rx ...)
  (check actual (raise-checker pred/rx ...)))

(define-syntax-rule (check-true e) (check-equal e #t))
(define-syntax-rule (check-false e) (check-equal e #f))
(define-syntax-rule (check-truish e) (check e truish?))

(define (truish? v) (and v #t))

(define-syntax check-equal? (make-rename-transformer #'check-equal))
(define-syntax with-check-info (make-rename-transformer #'with-info))

;; ============================================================

(define ((make-selective-execution-around name-pred) proc)
  ;; FIXME
  (cond [(call/catch (lambda () (name-pred (hash-ref (car (current-test-context)) 'name))))
         (proc)]
        [else (void)]))
