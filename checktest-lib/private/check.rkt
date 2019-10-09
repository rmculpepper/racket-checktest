#lang racket/base
(require racket/match
         racket/struct
         "test.rkt"
         "result.rkt")
(provide (all-defined-out))

;; ============================================================
;; Check

(define (run-checkers actual-thunk checkers [loc #f])
  (define check-ctx (current-check-context))
  (define actual (thunk->result actual-thunk))
  (for ([checker (in-list checkers)])
    (define ctx (vector actual checker loc check-ctx))
    (with-continuation-mark check-context-key ctx
      (apply-checker checker actual))))

;; ----------------------------------------
;; Check info

;; A CheckContext is one of
;; - #f                                       -- empty context
;; - (vector Result Checker Loc CheckContext) -- nested check
;; - (list* Symbol Any CheckContext)          -- user context info
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

(struct checker:predicate (pred args negate?)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'predicate-checker)
   (lambda (self)
     (match-define (checker:predicate pred args negate?) self)
     (append (cons pred args)
             (if negate? (list (unquoted-printing-string "#:negate?") negate?) null))))
  #:property prop:checker
  (lambda (self actual)
    (match-define (checker:predicate pred args negate?) self)
    (match actual
      [(result:single value)
       (unless ((if negate? not values) (apply pred value args))
         (fail "does not satisfy predicate" #| 'predicate pred |#))]
      [_ (fail "not a single value" #| 'predicate pred |#)])))

(struct checker:equal (expected)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'equal-checker)
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

(struct checker:raise (preds)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'raise-checker)
   (lambda (self) (checker:raise-preds self)))
  #:property prop:checker
  (lambda (self actual)
    (match-define (checker:raise preds) self)
    (match actual
      [(result:raised raised)
       (for ([pred (in-list preds)])
         (cond [(regexp? pred)
                (unless (exn? raised)
                  (fail "raised value is not exception" 'regexp pred))
                (unless (regexp-match? pred (exn-message raised))
                  (fail "exception message does not match regexp" 'regexp pred))]
               [(procedure? pred)
                (unless (pred raised)
                  (fail "raised value does not satisfy predicate" 'predicate pred))]))]
      [_ (fail "did not raise exception or other value")])))

;; apply-checker : Checker Result -> Void
(define (apply-checker c result)
  (let ([c (if (checker? c) c (checker:predicate c null #f))])
    ((checker-ref c) c result)))
