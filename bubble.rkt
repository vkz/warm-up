#lang racket

(require rackunit)

(define (bubble-sort ul <)
  (define (insert e sl)
    (cond
      ([empty? sl] (cons e empty))
      ([< e (car sl)] (cons e sl))
      (else (cons (car sl) (insert e (cdr sl))))))
  (cond
    [(empty? ul) ul]
    [else (insert (car ul) (bubble-sort (cdr ul) <))]))

(define (test/< l op)
  (define unsorted (shuffle l))
  (check-equal? (bubble-sort unsorted op) (sort unsorted op))
  (check-equal? (bubble-sort unsorted (compose not op))
                (sort unsorted (compose not op))))

(define test1 (shuffle (build-list 10 identity)))
(define test2 (shuffle (build-list 10 (compose number->string identity))))
(for-each test/< (list test1 test2) (list < string<?))
