#lang racket

(require data/heap)

(define (skyline input)
  ;; set of active buildings
  (define A (make-heap (lift > cadr)))
  (heap-add! A (list -inf.0 0 +inf.0))

  ;; set of visited buildings (up for deletion)
  (define V (make-hasheq))

  ;; points for drawing
  (define D '())

  (define ((start b) O)
    (heap-add! A b)
    (when (> (cadr b) O)
      (set! D (cons (list (car b) (cadr b)) D))))

  (define ((end b) O)
    (hash-set! V b 1)
    (while (hash-ref V (heap-min A) #f)
      (heap-remove-min! A))
    (define N (cadr (heap-min A)))
    (unless (= O N)
      (set! D (cons (list (caddr b) N) D))))

  (define E (make-heap (lift < car)))
  (for ([b (in-list input)])
    (heap-add! E (cons (car b) (start b)))
    (heap-add! E (cons (caddr b) (end b))))

  (for ([e (in-heap E)])
    ((cdr e) (cadr (heap-min A))))

  (reverse D))

(define (lift < f)
  (lambda (x y) (< (f x) (f y))))

(define-syntax-rule (while C B)
  (let L () (when C B (L))))

(require rackunit racket/list)

(define in
  '(( 1 11  5) ( 2  6  7)
    ( 3 13  9) (12  7 16)
    (14  3 25) (19 18 22)
    (23 13 29) (24  4 28)))

(define out
  '(( 1 11) ( 3 13) ( 9  0)
    (12  7) (16  3) (19 18)
    (22  3) (23 13) (29  0)))

;; basic test
(check-equal? (skyline in) out)

;; complex test
(for* ([i (in-range 10)]
       [j (in-range 1 10)])
  (check-equal?
   (skyline (shuffle (append* (build-list j (λ (_) in)))))
   out))
