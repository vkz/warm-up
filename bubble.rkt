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

(define (test/< op)
  (define unsorted (shuffle (build-list 10 identity)))
  (check-equal? (bubble-sort unsorted op) (sort unsorted op))
  (check-equal? (bubble-sort unsorted (not op)) (sort unsorted (not op))))