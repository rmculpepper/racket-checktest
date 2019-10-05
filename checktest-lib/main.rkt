#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/list
         racket/match
         racket/contract/base
         racket/pretty
         racket/struct
         racket/string
         racket/format
         racket/path
         syntax/srcloc
         rackunit/log
         "private/result.rkt"
         "private/test.rkt"
         "private/check.rkt")
(provide (all-defined-out)
         (all-from-out "private/test.rkt")
         (all-from-out "private/check.rkt"))

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
    (pattern (~seq #:pre pre:expr)))
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
     #'(run-checkers (lambda () (#%expression actual))
                     (list checker.c ...))]))

(define-syntax-rule (check-equal actual expected)
  (check actual (expect-equal expected)))
(define-syntax-rule (check-raise actual pred/rx ...)
  (check actual (expect-raise pred/rx ...)))

(define checker/c (or/c checker? (-> any/c any)))

(define-syntax-rule (with-info ([k v] ...) . body)
  (with-continuation-mark
    check-context-key
    (list* (~@ k v) ... (current-check-context))
    (let () . body)))

(define-syntax-rule (expect-equal expected)
  (checker:equal (thunk->result (lambda () (#%expression expected)))))
(define (expect-raise . pred/rx-list)
  (checker:raise pred/rx-list #t))
(define (expect-raise* . pred/rx-list)
  (checker:raise pred/rx-list #f))


;; ============================================================

(define KWIDTH 10)
(define INDENT 2)

(define current-test-display-levels (make-parameter #t))

(define (default-test-listener ctx event arg)
  (define levels (current-test-display-levels))
  (define (full-test-name) (test-context->string ctx))
  (define (short-test-name) (test-frame->string (car ctx)))
  (define (prefix s)
    (string-append (if #t s "")
                   (make-string (* 2 (sub1 (length ctx))) #\space)))
  (case event
    [(begin)
     (when levels
       (eprintf "~atesting ~a\n" (prefix "  ") (short-test-name)))]
    [(catch)
     (cond [(skip? arg)
            (when levels
              (eprintf "~askipping ~a\n" (prefix "- ") (short-test-name)))]
           [(check-failure? arg)
            (test-log! #f)
            (eprintf "----------------------------------------\n")
            (eprintf "~a\n" (full-test-name))
            (parameterize ((current-output-port (current-error-port))
                           (in-test-display? #t))
              (print-failure arg))
            (eprintf "----------------------------------------\n")]
           [else
            (test-log! #f)
            (eprintf "----------------------------------------\n")
            (eprintf "~a\n" (full-test-name))
            (eprintf "ERROR\n")
            (parameterize ((current-output-port (current-error-port))
                           (in-test-display? #t))
              (print-error arg))
            (eprintf "----------------------------------------\n")])]
    [(success)
     (test-log! #t)]
    [else (void)]))

(define (test-frame->string frame [can-omit? #f])
  (define (loc->string loc)
    (format "[~a:~a]"
            (let ([src (source-location-source loc)])
              (cond [(path? src) (file-name-from-path src)]
                    [else (or src '?)]))
            (or (source-location-line loc) '?)))
  (cond [(hash-ref frame 'name #f)
         => (lambda (name)
              (cond [(hash-ref frame 'loc #f)
                     => (lambda (loc)
                          (format "~a ~a" name (loc->string loc)))]
                    [else name]))]
        [(hash-ref frame 'loc #f) => loc->string]
        [can-omit? #f]
        [else "???"]))

(define (test-context->string ctx)
  (string-join
   (reverse
    (for/list ([frame (in-list ctx)])
      (test-frame->string frame)))
   " > "))

(define ERROR-CONTEXT-LENGTH 4)

(define (print-error e)
  (define message
    (cond [(exn? e) (exn-message e)]
          [else "value raised was not an exception"]))
  (parameterize ((error-print-context-length ERROR-CONTEXT-LENGTH))
    ((error-display-handler) message e))
  (define ctx (continuation-mark-set-first (exn-continuation-marks e) check-context-key))
  (print-ctx 0 ctx))

(define (print-failure cf)
  (match cf
    [(check-failure why info ctx)
     (printf "~a: ~a\n" (~a #:width KWIDTH "FAILURE") why)
     (define (print-info)
       (let loop ([info info])
         (match info
           [(list* key value rest)
            (print-kv 0 key value)
            (loop rest)]
           ['() (void)])))
     (match ctx
       [(vector actual checker ctx)
        (print-kv 0 "actual" actual)
        (print-info)
        (print-kv 0 "checker" checker)
        (print-ctx 0 ctx)]
       [_
        (print-info)
        (print-ctx 0 ctx)])]))

(define (print-ctx i ctx)
  (when (pair? ctx) (iprintf i "with context info:\n"))
  (let loop ([ctx ctx])
    (match ctx
      [(list* key value rest)
       (print-kv (+ i INDENT) key value)
       (loop rest)]
      [(vector actual checker ctx)
       (iprintf i "within another check:\n")
       (print-kv (+ i INDENT) "actual" actual)
       (print-kv (+ i INDENT) "checker" checker)
       (print-ctx (+ i INDENT) ctx)]
      [#f (void)])))

(define (iprintf indent fmt . args)
  (write-string (make-string indent #\space))
  (apply printf fmt args))

;; pretty-printing trick borrowed from pretty-format pkg by Alex Knauth
(struct formatted (fmt vs)
  #:property prop:custom-write
  (lambda (this out mode) (apply fprintf out (formatted-fmt this) (formatted-vs this))))

(define (print-kv i k v)
  (define space (make-string i #\space))
  (define label (~a #:min-width KWIDTH k))
  (case (current-test-value-style)
    [(pretty)
     (pretty-print (formatted "~a~a: ~v" (list space label v)))
     (when (eq? (pretty-print-columns) 'infinity) (newline))]
    [(full) (printf "~a~a: ~v\n" space label v)]
    [else (printf "~a~a: ~e\n" space label v)]))

;; ============================================================

(define ((make-selective-execution-around name-pred) proc)
  ;; FIXME
  (cond [(call/catch (lambda () (name-pred (hash-ref (car (current-test-context)) 'name))))
         (proc)]
        [else (void)]))

;; ============================================================

(set-default-test-listener! default-test-listener)
