#lang racket
;; follows excellent Jay McCarthy's article
;; http://jeapostrophe.github.io/2013-08-19-reverse-post.html

(require rackunit
         racket/string
         racket/list
	 "reverse-string.rkt")

(define reverse-string/slow rev1)
(define reverse-string/med rev4) ;medium in Jay's article
(define reverse-string/really-fast rev5)
(define reverse-string/fast (compose reverse-string/really-fast string-copy))


(define (random-char)
  (if (zero? (random 5))
    #\space
    (integer->char (+ (char->integer #\A) (random 26)))))

(define (random-string N)
  (list->string (build-list N (λ (i) (random-char)))))
 
(define N 40000)

(define ss (build-list N (λ (i) (random-string 1000))))
 
(define-syntax-rule
  (time-exp exp)
  (let-values ([(as cpu real gc) (time-apply (λ () exp) empty)])
    (collect-garbage) (collect-garbage) (collect-garbage) ;why do we gc 3x?
    (values (first as)
            cpu)))
 
(define-values
  (slows st)
  (time-exp
   (for/list ([s (in-list ss)])
     (reverse-string/slow s))))
 
(define-values
  (mediums mt)
  (time-exp
   (for/list ([s (in-list ss)])
     (reverse-string/med s))))
 
(define-values
  (fasts ft)
  (time-exp
   (for/list ([s (in-list ss)])
     (reverse-string/fast s))))
 
(define-values
  (reallys rt)
  (time-exp
   (for/list ([s (in-list ss)])
     (reverse-string/really-fast s))))
 
(for ([s (in-list slows)]
      [l (in-list slows)]
      [m (in-list mediums)]      
      [f (in-list fasts)]
      [r (in-list reallys)])
  (check-equal? m l)
  (check-equal? f l)
  (check-equal? r l))
 
(printf "~a\n" (list st mt ft rt))
