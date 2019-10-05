#lang racket/base
(require racket/match
         racket/list
         racket/path
         racket/string
         racket/format
         racket/pretty
         syntax/srcloc
         rackunit/log
         "test.rkt"
         "check.rkt")
(provide current-test-display-config)

;; ============================================================

(define KWIDTH 10)
(define INDENT 2)

(define current-test-display-config
  (make-parameter #hash((print? . #t) (skipped? . #f))))

(define (default-test-listener ctx event arg)
  (define config (current-test-display-config))
  (define (full-test-name) (test-context->string ctx))
  (define (short-test-name) (test-frame->string (car ctx)))
  (define (prefix s)
    (string-append (if #t s "") (make-prefix-string (sub1 (length ctx)))))
  (define (make-prefix-string n)
    (apply string-append (make-list n "| ")))
  (case event
    [(begin)
     (when (hash-ref config 'print? #f)
       (eprintf "test ~a~a\n" (prefix "") (short-test-name)))]
    [(catch)
     (cond [(skip? arg)
            (when (hash-ref config 'skipped? #f)
              (eprintf "skip ~a~a\n" (prefix "") (short-test-name)))]
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
            (eprintf "ERROR ")
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
     (printf "FAILURE: ~a\n" why)
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

(set-default-test-listener! default-test-listener)
