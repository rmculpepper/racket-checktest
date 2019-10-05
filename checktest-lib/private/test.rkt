#lang racket/base
(require racket/match)
(provide (all-defined-out))

;; run-test : (-> Any) [...] -> Void
(define (run-test proc
                  #:loc  loc    ;; (U source-location? #f)
                  #:name name   ;; (U string? 'auto #f)
                  #:pre pre-thunk)
  (define ctx (cons (hasheq 'name name 'loc loc) (current-test-context)))
  (define nested? (pair? (cdr ctx)))
  (signal ctx 'enter)
  (parameterize ((current-test-context ctx))
    (with-handlers ([(lambda (e) (not (exn:break? e)))
                     (lambda (e)
                       (cond [(caught? e) (raise (if nested? e (caught-v e)))]
                             [else (signal ctx 'catch e)]))])
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

;; wrapper for exceptions in framework code so (test _) doesn't catch them; they
;; should actually escape and halt execution
(struct caught (v))
(define (call/catch proc)
  (with-handlers ([(lambda (e) #t) (lambda (e) (raise (caught e)))]) (proc)))

;; signal : TestContext TestEvent Any -> Void
(define (signal ctx event [arg #f])
  (call/catch (lambda () ((current-test-listener) ctx event arg))))

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

;; current-test-value-style : parameter of (U 'short 'full 'pretty)
(define current-test-value-style (make-parameter 'short))

(define in-test-display? (make-parameter #f))

;; ----------------------------------------
;; Communicating with test

(struct check-failure (why info ctx))
(struct skip (why))

(define (skip-test [why #f]) (raise (skip why)))

;; ----------------------------------------
;; Default Test Listener

(define (default-test-listener c e a)
  (error 'default-test-listener "not initialized"))
(define (set-default-test-listener! listener)
  (set! default-test-listener listener))
