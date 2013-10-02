#lang racket

(provide (all-defined-out))
	 

;; since string literals in Racket are immutable
;; create a mutable string 
(define str (string-copy "reverse me"))

;; the most straightforward way
;; also the most wasteful
;; for a n-long string
;; --- process (* 3 n) chars,
;; --- allocate 1 n-string
;; --- allocate (* 2 n) cons-cells
(define rev1 (compose list->string reverse string->list))

;; direct iteration
;; silly way of doing it
(define (rev2 s)
  (list->string
   (for/list ([i (in-range (sub1 (string-length s)) -1 -1)])
     (string-ref s i))))

;; another direct iteration
;; but without the -1 -1 ugliness
;; accumulating for/fold is more general
;; but very slow (is it append?)
(define (rev3 s)
  (for/fold ([reversed (make-string 0)])
            ([c (in-string s)])
    (string-append (string c) reversed)))

;; faster and less wasteful
;; process each char just once
;; allocate n-string just once
(define (rev4 s)
  (define len (string-length s))
  (build-string len (lambda (i) (string-ref s (- len i 1)))))

;; really fast "swap in place"
;; takes length/2 steps
;; but mutates the string
(define (rev5 s)
  (define len (string-length s))
  (for ([c (in-string s)]
	[i (in-range (quotient len 2))])
    (define ni (- len i 1))
    (define ci (string-ref s ni))
    (string-set! s i ci)
    (string-set! s ni c))
  s)
