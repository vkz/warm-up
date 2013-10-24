#lang racket
(require rackunit)

;; naive implementation
(define (fib/raw n)
  (case n
    ((0 1) 1)
    (else (+ (fib/raw (- n 1))
             (fib/raw (- n 2))))))

(define (fact/raw n)
  (case n
    ((1) 1)
    (else (* (fact/raw (- n 1)) n))))

;; momoize by hand
(define (fact/memo n)
  (define memo (make-hash))
  (define (f n)
    (or (hash-ref memo n #f)
        (begin
          (hash-set! memo n
                     (case n
                       ((1) 1)
                       (else (* (f (- n 1)) n))))
          (hash-ref memo n))))
  (f n))

;; magic
(define-syntax (define/memo stx)
  (syntax-case stx ()
    [(_ (fun arg) body ...)
     (syntax
      (begin
        (define memo (make-hash))
        (define (fun arg)
          (or (hash-ref memo arg #f)
              (begin
                (hash-set! memo arg (begin body ...))
                (hash-ref memo arg))))))]))

;; naive implementation is now linear in n
(define/memo (fact n)
  (case n
    ((1) 1)
    (else (* (fact (- n 1)) n))))

(define/memo (fib n)
  (case n
    ((0 1) 1)
    (else (+ (fib (- n 1))
             (fib (- n 2))))))

;; test
(for ((n (in-range 1 35)))
  (check-equal? (fib/raw n) (fib n))
  (check-equal? (fact/raw n) (fact n)))
