#lang racket/base
(require (for-syntax racket/base syntax/parse syntax/transformer)
         racket/match
         racket/struct)
(provide (all-defined-out))

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

;; thunk->result : (-> Any) -> Result
(define (thunk->result proc)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e) (result:raised e))])
    (call-with-values proc (case-lambda [(v) (result:single v)] [vs (result:values vs)]))))
