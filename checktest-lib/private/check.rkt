#lang racket/base
(require racket/match
         racket/struct
         "test.rkt"
         "result.rkt")
(provide (all-defined-out))

;; ============================================================
;; Check

(define (run-checkers actual-thunk checkers)
  (define check-ctx (current-check-context))
  (define actual (thunk->result actual-thunk))
  (for ([checker (in-list checkers)])
    (define ctx (vector actual checker check-ctx))
    (with-continuation-mark check-context-key ctx
      (apply-checker checker actual))))

;; ----------------------------------------
;; Check info

;; A CheckContext is one of
;; - #f                                     -- empty context
;; - (vector Result Checker CheckContext)   -- nested check
;; - (list* Symbol Any CheckContext)        -- user context info
(define check-context-key (gensym 'check-context))
(define (current-check-context)
  (continuation-mark-set-first #f check-context-key))

(define (fail why . info)
  (raise (check-failure why info (current-check-context))))


;; ============================================================
;; Checkers

;; A Checker is an instance of a struct implementing prop:checker
;; with a value of type (Checker Result -> Void).
;; A predicate is coerced to a Checker via checker:predicate.
(define-values (prop:checker checker? checker-ref)
  (make-struct-type-property 'checker))

(struct checker:predicate (pred)
  #:property prop:custom-write
  (lambda (self port mode)
    (match-define (checker:predicate pred) self)
    (case mode
      ((0 1) (print pred port mode))
      ((#t) (write pred port))
      (else (display pred port))))
  #:property prop:checker
  (lambda (self actual)
    (match-define (checker:predicate pred) self)
    (match actual
      [(result:single value)
       (unless (pred value)
         (fail "does not satisfy predicate" 'predicate pred))]
      [_ (fail "not a single value" 'predicate pred)])))

(struct checker:equal (expected)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'expect-equal)
   (lambda (self)
     (if (in-test-display?)
         ;; redundant with "expected:", so suppress
         (list (unquoted-printing-string "..."))
         (list (checker:equal-expected self)))))
  #:property prop:checker
  (lambda (self actual)
    (match-define (checker:equal expected) self)
    (unless (equal? actual expected)
      (fail "not equal" 'expected expected))))

(struct checker:raise (preds require-exn?)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self)
     (cond [(checker:raise-require-exn? self) 'expect-raise]
           [else 'expect-raise*]))
   (lambda (self) (checker:raise-preds self)))
  #:property prop:checker
  (lambda (self actual)
    (match-define (checker:raise preds require-exn?) self)
    (match actual
      [(result:raised raised)
       (define raised-msg (if (exn? raised) (exn-message raised) ""))
       (when (and require-exn? (not (exn? raised)))
         (fail "result is not a raised exception"))
       (for ([pred (in-list preds)])
         (cond [(regexp? pred)
                (unless (exn? raised)
                  (fail "did not raise exception" 'regexp pred))
                (unless (regexp-match? pred raised-msg)
                  (fail "exception message does not match regexp" 'regexp pred))]
               [(procedure? pred)
                (unless (pred raised)
                  (fail (if require-exn?
                            "exception does not satisfy predicate"
                            "raised value does not satisfy predicate")
                        'predicate pred))]))]
      [_ (fail (cond [require-exn? "did not raise exception"]
                     [else "did not raise value"]))])))

;; apply-checker : Checker Result -> Void
(define (apply-checker c result)
  (let ([c (if (checker? c) c (checker:predicate c))])
    ((checker-ref c) c result)))
