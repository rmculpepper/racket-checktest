#lang racket/base
(require (for-syntax racket/base syntax/parse)
         racket/contract/base
         racket/struct
         racket/string
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
(define (result:single v)
  (if (or (result:values? v) (result:raised? v)) (result:values (list v)) v))
(define (result:single-value v)
  (cond [(result:values? v) (car (result:values-vs v))]
        [else v]))

;; call->result : (-> Any) -> Result
(define (call->result proc)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e) (result:raised e))])
    (call-with-values proc (case-lambda [(v) (result:single v)] [vs (result:values vs)]))))


;; ============================================================
;; Checkers

;; An Checker is one of
;; - (checker (Result -> Void))
;; - (Any -> Truth)                -- interpreted as predicate on single-values
(struct checker (proc))
(struct named-checker checker (name args)
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (self) (named-checker-name self))
                                  (lambda (self) (named-checker-args self))))

;; apply-checker : Checker Result -> Void
(define (apply-checker c result)
  (cond [(checker? c) ((checker-proc c) result)]
        [else (apply-pred-checker c result)]))

;; apply-pred-checker : (Any -> Truth) Result -> Void
(define (apply-pred-checker pred result)
  (cond [(result:single? result)
         (unless (pred (result:single-value result))
           (fail "actual value does not satisfy predicate"
                 'predicate pred))]
        [else (fail "actual result not a single value"
                    'predicate pred)]))

;; equal-checker : Result -> Checker
(define (equal-checker expected-result)
  (named-checker (lambda (actual-result)
                   ;; FIXME: specialize result:raised
                   (unless (equal? actual-result expected-result)
                     (fail "actual result not equal to expected result"
                           'expected expected-result)))
                 'expect-equal (list expected-result)))

;; mk-raise*-checker : (Listof (U Regexp (-> Any Truth))) -> Checker
(define ((mk-raise*-checker rx/pred-list require-exn?) actual-result)
  (cond [(result:raised? actual-result)
         (define raised (result:raised-e actual-result))
         (define raised-msg (if (exn? raised) (exn-message raised) ""))
         (when (and require-exn? (not (exn? raised)))
           (fail "actual result is not a raised exception"
                 'predicates rx/pred-list))
         (for ([rx/pred (in-list rx/pred-list)])
           (cond [(regexp? rx/pred)
                  (unless (exn? raised)
                    (fail "actual result is raised non-exception value"
                          'regexp rx/pred 'predicates rx/pred-list))
                  (unless (regexp-match? rx/pred raised-msg)
                    (fail "actual exception message does not match regexp"
                          'regexp rx/pred 'predicates rx/pred-list))]
                 [(procedure? rx/pred)
                  (unless (rx/pred raised)
                    (fail (if require-exn?
                              "actual raised exception does not satisfy predicate"
                              "actual raised value does not satisfy predicate")
                          'predicate rx/pred 'predicates rx/pred-list))]))]
        [else (fail (cond [require-exn? "actual result is not a raised exception"]
                          [else "actual result is not a raised value"])
                    'predicates rx/pred-list)]))

(define current-fail
  (make-parameter (lambda (why info) (error 'fail "called outside of a check expression"))))

(define (fail why . info) ((current-fail) why info))

(define-syntax-rule (expect-equal expected)
  (equal-checker (call->result (lambda () (#%expression expected)))))
(define (expect-raise . pred/rx-list)
  (named-checker (mk-raise*-checker pred/rx-list #t) 'expect-raise pred/rx-list))
(define (expect-raise* . pred/rx-list)
  (named-checker (mk-raise*-checker pred/rx-list #f) 'expect-raise* pred/rx-list))

;; ============================================================
;; Check

(define current-check-context (make-parameter #f))
(struct check-failure (why actual info ctx) #:transparent)

(define (print-failure cf)
  (printf "FAILURE: ~a\n" (check-failure-why cf))
  (printf "actual: ~e\n" (check-failure-actual cf))
  (let loop ([info (check-failure-info cf)])
    (when (pair? info)
      (printf "~a: ~e\n" (car info) (cadr info))
      (loop (cddr info))))
  (let loop ([i 0] [ctx (check-failure-ctx cf)])
    (define (iprintf indent fmt . args)
      (write-string (make-string indent #\space))
      (apply printf fmt args))
    (when ctx
      (iprintf i "within another check:\n")
      (let ([i (add1 i)])
        (iprintf i "actual: ~e\n" (vector-ref ctx 0))
        (iprintf i "checker: ~e\n" (vector-ref ctx 1))
        (loop i (vector-ref ctx 2)))))
  (void))

(define-syntax check
  (syntax-parser
    [(_ actual:expr checker:expr ...)
     #'(-check (lambda () (#%expression actual))
               (list checker ...))]))

(define-syntax-rule (check-equal actual expected)
  (check actual (expect-equal expected)))
(define-syntax-rule (check-raise actual pred/rx ...)
  (check actual (expect-raise pred/rx ...)))

(define (-check actual-thunk checkers)
  (define check-ctx (current-check-context))
  (define actual (call->result actual-thunk))
  (define (fail-here why info)
    (raise (check-failure why actual info check-ctx)))
  (for ([checker (in-list checkers)])
    (parameterize ((current-fail fail-here)
                   (current-check-context
                    (vector actual checker check-ctx)))
      (apply-checker checker actual))))

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
  (eprintf "----------------------------------------\n")
  (eprintf "~a\n" (test-context->string (current-test-context)))
  (cond [(check-failure? e)
         (parameterize ((current-output-port (current-error-port)))
           (print-failure e))]
        [else
         (eprintf "ERROR\n~e\n" e)])
  (eprintf "----------------------------------------\n")
  (void))

(define (-test-success) (void))
