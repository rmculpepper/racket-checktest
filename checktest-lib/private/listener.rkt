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

(define-logger checker)

;; ============================================================

(define KWIDTH 10)
(define INDENT 2)

(define current-test-display-config
  (make-parameter (hasheq 'print-test? #f)))

(define (config-ref key default)
  (hash-ref (current-test-display-config) key default))
(define (config:print-test? name)
  (and (config-ref 'print-test? #f)
       (regexp-match? (config:print-test-rx) name)))
(define (config:print-test-rx)
  ;; by default, skip with empty or missing names or with names starting with "_"
  (config-ref 'print-test-rx #rx"^[^_]"))
(define (config:print-skipped? name)
  (and (config:print-test? name) (config-ref 'print-skipped? #f)))
(define (config:error-context-length)
  (config-ref 'error-context-length 4))
(define (config:value-style)
  (config-ref 'value-style 'short))

(define (default-test-listener ctx event arg)
  (define (full-test-name) (test-context->string ctx))
  (define (short-test-name) (test-frame->string (car ctx)))
  (define (prefix s)
    (string-append (if #t s "") (make-prefix-string (sub1 (length ctx)))))
  (define (make-prefix-string n)
    (apply string-append (make-list n "| ")))
  (define (immediate-test-name) (or (hash-ref (car ctx) 'name #f) ""))
  (case event
    [(enter)
     (log-checker-debug "enter: ~a" (full-test-name))]
    [(begin)
     (log-checker-info "running: ~a" (full-test-name))
     (when (config:print-test? (immediate-test-name))
       (eprintf "test ~a~a\n" (prefix "") (short-test-name)))]
    [(catch)
     (cond [(skip? arg)
            (log-checker-info "skipping: ~a" (full-test-name))
            (when (config:print-skipped? (immediate-test-name))
              (eprintf "skip ~a~a\n" (prefix "") (short-test-name)))]
           [(check-failure? arg)
            (test-log! #f)
            (log-checker-info "failure: ~a" (full-test-name))
            (eprintf "----------------------------------------\n")
            (eprintf "~a\n" (full-test-name))
            (parameterize ((current-output-port (current-error-port))
                           (in-test-display? #t))
              (print-failure arg))
            (eprintf "----------------------------------------\n")]
           [else
            (test-log! #f)
            (log-checker-info "error: ~a" (full-test-name))
            (eprintf "----------------------------------------\n")
            (eprintf "~a\n" (full-test-name))
            (eprintf "ERROR: ")
            (parameterize ((current-output-port (current-error-port))
                           (in-test-display? #t))
              (print-error arg))
            (eprintf "----------------------------------------\n")])]
    [(exit)
     (log-checker-debug "exit: ~a" (full-test-name))
     (test-log! #t)]
    [else (void)]))

(define (test-frame->string frame [can-omit? #f] #:last-path [last-path #f])
  (define (loc->string loc)
    (format "[~a:~a]"
            (let ([src (source-location-source loc)])
              (cond [(and last-path (equal? last-path src)) ""]
                    [(path? src) (file-name-from-path src)]
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

(define ABBREV-LAST-PATH? #t)

(define (test-context->string ctx)
  (string-join
   (for/fold ([acc null] [last-path #f] #:result (reverse acc))
             ([frame (in-list (reverse ctx))])
     (values (cons (test-frame->string frame #:last-path last-path) acc)
             (and ABBREV-LAST-PATH?
                  (let* ([loc (hash-ref frame 'loc #f)]
                         [src (and loc (source-location-source loc))])
                    (and (path? src) src)))))
   " > "))

(define (print-error e)
  (define message
    (cond [(exn? e) (exn-message e)]
          [else "value raised was not an exception"]))
  (parameterize ((error-print-context-length (config:error-context-length)))
    ((error-display-handler) message e))
  (define ctx (continuation-mark-set-first (exn-continuation-marks e) check-context-key))
  (print-ctx 0 ctx))

(define (print-failure cf)
  (match cf
    [(check-failure why info ctx)
     (printf "FAILURE: ~a\n" why)
     (match ctx
       [(vector actual checker ctx)
        (let ([info (append (list "actual" actual) info (list "checker" checker))])
          (print-info 0 info))
        (print-ctx 0 ctx)]
       [_
        (print-info 0 info)
        (print-ctx 0 ctx)])]))

(define (print-info i info [k void])
  (define w
    (let loop ([info info])
      (match info
        [(list* key value rest)
         (max (string-length (format "~a" key)) (loop rest))]
        [_ 0])))
  (let loop ([info info])
    (match info
      [(list* key value rest)
       (print-kv i w key value)
       (loop rest)]
      [end (k end)])))

(define (print-ctx i ctx)
  (when (pair? ctx) (iprintf i "with context info:\n"))
  (print-info (+ i INDENT) ctx
              (lambda (end)
                (match end
                  [(vector actual checker ctx)
                   (iprintf i "within another check:\n")
                   (print-info (+ i INDENT)
                               (list "actual" actual
                                     "checker" checker))
                   (print-ctx (+ i INDENT) ctx)]
                  [#f (void)]))))

(define (iprintf indent fmt . args)
  (write-string (make-string indent #\space))
  (apply printf fmt args))

;; pretty-printing trick borrowed from pretty-format pkg by Alex Knauth
(struct formatted (fmt vs)
  #:property prop:custom-write
  (lambda (this out mode) (apply fprintf out (formatted-fmt this) (formatted-vs this))))

(define (print-kv i w k0 v)
  (define space (make-string i #\space))
  (define k (format "~a" k0))
  (define kspace (make-string (max 0 (- w (string-length k))) #\space))
  (case (config:value-style)
    [(pretty)
     (pretty-print (formatted "~a~a: ~a~v" (list space k kspace v)))
     (when (eq? (pretty-print-columns) 'infinity) (newline))]
    [(full) (printf "~a~a: ~a~v\n" space k kspace v)]
    [else (printf "~a~a: ~a~e\n" space k kspace v)]))

(set-default-test-listener! default-test-listener)
