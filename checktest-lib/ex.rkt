#lang racket/base
(require "main.rkt" "ex-help.rkt")
(require racket/list)

#;
(current-test-arounds
 (list (make-selective-execution-around
        (lambda (name) (and name (regexp-match? #rx"printing" name))))))

(test
 #:name "here"
 (check 3 (lambda (v)
            (check-equal (values 1 2) (values 2 3)))))

(test
 #:name "there"
 (check 3 even?))

(test
 #:name "nested"
 (test
  #:name "here"
  (with-info (['where "right here"])
    (check 3 (lambda (v)
               (with-info (['why "just because"])
                 (check-equal (values 1 2) (values 2 3)))))))
 (test
  #:name "error w/in check"
  (with-info (['where "over yonder"])
    (check 4 (lambda (v)
               (error 'nope "nope nope\n  expected: yup yup yup"))))))

(test
 #:name "here"
 (with-info (['where "right here"])
   (check-equal 3 4)))

(test (check-raise 'foo exn:fail? #rx"foo"))

(test (check-raise (error 'foo) #rx"snarf"))

(test (fail "didn't work" 'try-again "probably not"))

(test (with-info (['apple "orange"])
        (check 5 (lambda (v)
                   (with-info (['pear 'banana])
                     (fail "nope" 'effort "minimal"))))))

(current-test-display-config (hasheq 'print? #t 'skipped? #t))

(test #:name "suite"
  (tests
   (check-equal 1 1)
   (check-equal 2 2)
   (check-equal 3 3))
  (test
    (test
      (test #:pre (skip-test) (check-equal 4 5))))
  (test #:name "way"
        (test #:name "too"
              (test #:name "nested"
                    (check-equal 'a 'a)))))

(test #:name "printing"
  (check-equal 1
               '(test x #:name "suite"
                  (tests
                   (check-equal 1 1)
                   (check-equal 2 2)
                   (check-equal 3 3)
                   (test #:pre (skip-test) (check-equal 4 5)))
                  (test #:name "way"
                    (test #:name "too"
                      (test #:name "nested"
                        (check-equal 'a 'a)))))))

(test #:name "printer2"
  (define xs (range 1000))
  (check-equal xs (cdr xs)))

#|
(test #:name "sleepy test"
  (test (sleep 2))
  (test (printf "woke up\n")))

(test #:name "sleepy check"
  (test (check-equal (begin (sleep 2) 'ok) 'ok))
  (test (printf "woke up\n")))
|#

(test #:name "real list"
      (test-list '(1 2 3)))
(test #:name "empty list"
      (test-list '()))
(test #:name "improper list"
      (test-list '(a . b)))

(module foo racket/base
  (require "main.rkt")
  (provide subtest)
  (define (subtest)
    (test #:name "in a submod"
          (check 1 even?))))
(require 'foo)

(test #:name "test from submod"
      (subtest))
