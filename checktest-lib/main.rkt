#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/transformer)
         racket/list
         racket/match
         racket/contract/base
         racket/pretty
         racket/struct
         racket/string
         racket/format
         racket/path
         syntax/srcloc)
(provide (all-defined-out))

;; This is my bikeshed. There are many like it, but this one is mine.

;; ============================================================
;; Results

;; A Result is one of
;; - Any (not result:{values,raised})   -- represents single value other than result:{values,raised}
;; - (result:values (listof Any))       -- multiple values or single result:{values,raised} value
;; - (result:raised Any)                -- raised a value (not necessarily exn!)
(struct result:values (vs) #:transparent
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (v) 'values) (lambda (v) (result:values-vs v))))
(struct result:raised (e) #:transparent
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (v) 'raise) (lambda (v) (list (result:raised-e v)))))

(define (result:single? v)
  (cond [(result:values? v) (and (= (length (result:values-vs v)) 1))]
        [(result:raised? v) #f]
        [else #t]))
(define (make-result:single v)
  (if (or (result:values? v) (result:raised? v)) (result:values (list v)) v))
(define (result:single-value v)
  (cond [(result:values? v) (car (result:values-vs v))]
        [else v]))
(define-match-expander result:single
  (syntax-parser [(_ p) #'(? result:single? (app result:single-value p))])
  (make-variable-like-transformer #'make-result:single))

;; call->result : (-> Any) -> Result
(define (call->result proc)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e) (result:raised e))])
    (call-with-values proc (case-lambda [(v) (result:single v)] [vs (result:values vs)]))))


;; ============================================================
;; Context and communication

;; ----------------------------------------
;; Check-to-test

(struct check-failure (why info ctx))
(struct skip (why))

(define (fail why . info)
  (raise (check-failure why info (current-check-context))))

(define (skip-test [why #f]) (raise (skip why)))

;; ----------------------------------------
;; Check info

;; A CheckContext is one of
;; - #f                                     -- empty context
;; - (vector Result Checker CheckContext)   -- nested check
;; - (list* Symbol Any CheckContext)        -- user context info
(define current-check-context (make-parameter #f))

(define-syntax-rule (with-info ([k v] ...) . body)
  (parameterize ((current-check-context (list* (~@ k v) ... (current-check-context))))
    . body))

;; ----------------------------------------
;; Testing context

;; A TestContext is (listof TestFrame)
;; A TestFrame is (hash 'name (U String #f) 'loc (U source-location? #f))
(define current-test-context (make-parameter null))

;; A TestAround is a procedure ((-> Void) -> Void).
(define current-test-arounds (make-parameter null))

;; A TestListener is (TestContext TestEvent Any -> Void)
;; where TestEvents = 'enter | 'start | 'success | 'catch
(define current-test-listener
  (make-parameter (lambda (c e a) (default-test-listener c e a))))

;; signal : TestContext TestEvent Any -> Void
(define (signal ctx event [arg #f])
  ((current-test-listener) ctx event arg))

;; current-test-value-style : parameter of (U 'short 'full 'pretty)
(define current-test-value-style (make-parameter 'short))


;; ============================================================
;; Checkers

;; An Checker is one of
;; - (checker (Result -> Void))
;; - (Any -> Truth)                -- interpreted as predicate on single-values
(struct checker (proc))
(struct named-checker checker (name args)
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (self) (named-checker-name self))
                                  (lambda (self)
                                    (or (named-checker-args self)
                                        (list (unquoted-printing-string "..."))))))

;; apply-checker : Checker Result -> Void
(define (apply-checker c result)
  (cond [(checker? c) ((checker-proc c) result)]
        [else (apply-pred-checker c result)]))

;; apply-pred-checker : (Any -> Truth) Result -> Void
(define (apply-pred-checker pred result)
  (match result
    [(result:single value)
     (unless (pred (result:single-value result))
       (fail "does not satisfy predicate" 'predicate pred))]
    [_ (fail "not a single value" 'predicate pred)]))

;; equal-checker : Result -> Checker
(define (equal-checker expected-result)
  (named-checker (lambda (actual-result)
                   ;; FIXME: specialize result:raised
                   (unless (equal? actual-result expected-result)
                     (fail "not equal" 'expected expected-result)))
                 'expect-equal #f))

;; mk-raise*-checker : (Listof (U Regexp (-> Any Truth))) -> Checker
(define ((mk-raise*-checker rx/pred-list require-exn?) actual-result)
  (match actual-result
    [(result:raised raised)
     (define raised-msg (if (exn? raised) (exn-message raised) ""))
     (when (and require-exn? (not (exn? raised)))
       (fail "result is not a raised exception"))
     (for ([rx/pred (in-list rx/pred-list)])
       (cond [(regexp? rx/pred)
              (unless (exn? raised)
                (fail "did not raise exception" 'regexp rx/pred))
              (unless (regexp-match? rx/pred raised-msg)
                (fail "exception message does not match regexp" 'regexp rx/pred))]
             [(procedure? rx/pred)
              (unless (rx/pred raised)
                (fail (if require-exn?
                          "exception does not satisfy predicate"
                          "raised value does not satisfy predicate")
                      'predicate rx/pred))]))]
    [_ (fail (cond [require-exn? "did not raise exception"]
                   [else "did not raise value"]))]))

(define-syntax-rule (expect-equal expected)
  (equal-checker (call->result (lambda () (#%expression expected)))))
(define (expect-raise . pred/rx-list)
  (named-checker (mk-raise*-checker pred/rx-list #t) 'expect-raise pred/rx-list))
(define (expect-raise* . pred/rx-list)
  (named-checker (mk-raise*-checker pred/rx-list #f) 'expect-raise* pred/rx-list))


;; ============================================================
;; Check

(define-syntax check
  (syntax-parser
    [(_ actual:expr checker:expr ...)
     #'(-check (lambda () (#%expression actual))
               (list checker ...))]))

(define (-check actual-thunk checkers)
  (define check-ctx (current-check-context))
  (define actual (call->result actual-thunk))
  (for ([checker (in-list checkers)])
    (define ctx (vector actual checker check-ctx))
    (parameterize ((current-check-context ctx))
      (apply-checker checker actual))))

(define-syntax-rule (check-equal actual expected)
  (check actual (expect-equal expected)))
(define-syntax-rule (check-raise actual pred/rx ...)
  (check actual (expect-raise pred/rx ...)))


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
     #`(-test #:name (~? n.name #f)
              #:loc (~? loc.location #,(stx->loc-expr this-syntax))
              #:pre (~? (lambda () (#%expression pre.pre)) #f)
              (lambda () body ... (void)))]))

(define-syntax tests
  (syntax-parser
    [(_ e:expr ...)
     #`(begin (test #:location-syntax e e) ...)]))

(define (-test proc
               #:loc  loc    ;; (U source-location? #f)
               #:name name   ;; (U string? 'auto #f)
               #:pre pre-thunk)
  (define ctx (cons (hasheq 'name name 'loc loc) (current-test-context)))
  (signal ctx 'enter)
  (parameterize ((current-test-context ctx))
    (with-handlers ([(lambda (e) #t)
                     (lambda (e) (signal ctx 'catch e))])
      (let loop ([arounds (current-test-arounds)])
        (match arounds
          [(cons around arounds)
           (around (lambda () (loop arounds)))]
          ['()
           (when pre-thunk (pre-thunk))
           (signal ctx 'begin)
           (proc)]))
      (signal ctx 'success)
      (void))))


;; ============================================================

(define KWIDTH 10)
(define INDENT 2)

(define current-test-display-levels (make-parameter #f))

(define (default-test-listener ctx event arg)
  (define levels (current-test-display-levels))
  (define (full-test-name) (test-context->string ctx))
  (define (short-test-name)
    (cond [(exact-positive-integer? levels)
           (let ([levels (if (exact-positive-integer? levels) levels 1)])
             (define len (- (length ctx) (sub1 levels)))
             (test-context->string (take ctx (max 1 len))))]
          [else (full-test-name)]))
  (define (prefix char)
    (cond [(exact-positive-integer? levels)
           (define s (make-string (add1 levels) #\space))
           (string-set! s (sub1 (min levels (length ctx))) char)
           s]
          [else ""]))
  (case event
    [(begin)
     (when levels
       (eprintf "~arunning ~a\n" (prefix #\+) (short-test-name)))]
    [(catch)
     (cond [(skip? arg)
            (when levels
              (eprintf "~askipping ~a\n" (prefix #\-) (short-test-name)))]
           [(check-failure? arg)
            (eprintf "----------------------------------------\n")
            (eprintf "~a\n" (full-test-name))
            (parameterize ((current-output-port (current-error-port)))
              (print-failure arg))
            (eprintf "----------------------------------------\n")]
           [else
            (eprintf "----------------------------------------\n")
            (eprintf "ERROR\n~e\n" arg)
            (eprintf "----------------------------------------\n")])]
    [else (void)]))

(define (test-context->string ctx)
  (string-join (reverse
                (for/list ([frame (in-list ctx)])
                  (cond [(hash-ref frame 'name #f)
                         => (lambda (name) name)]
                        [(hash-ref frame 'loc #f)
                         => (lambda (loc)
                              (format "~a:~a"
                                      (let ([src (source-location-source loc)])
                                        (cond [(path? src) (file-name-from-path src)]
                                              [else (or src '?)]))
                                      (or (source-location-line loc) '?)))]
                        [else "???"])))
               " > "))

(define (print-failure cf)
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
  (cond [(name-pred (hash-ref (car (current-test-context)) 'name))
         (proc)]
        [else (void)]))
