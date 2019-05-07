#lang racket/base
(require "main.rkt")

(test
 #:name "here"
 (check 3 (lambda (v)
            (check-equal (values 1 2) (values 2 3)))))

(test
 #:name "here"
 (with-info (['where "right here"])
   (check 3 (lambda (v)
              (with-info (['why "just because"])
                (check-equal (values 1 2) (values 2 3)))))))

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

(current-test-display-levels 3)

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


(require racket/list)
(test #:name "printer2"
  (define xs (range 1000))
  (check-equal xs (cdr xs)))
