#lang racket
(require rackunit)

(define (make-stack)
  '())

(define stack-length length)
(define empty-stack? empty?)
(define stack? list?)
(define push cons)
(define peek car)
(define (pop s)
  (values (car s) (cdr s)))
       


(check-pred stack? (make-stack))
(check-equal? 'a (peek (push 'a (push 'b (make-stack)))))
(let*-values (((s) (push 'a (push 'b (make-stack))))
	      ((e ns) (pop s)))
  (check-equal? 'a e)
  (check-equal? (cdr s) ns))
