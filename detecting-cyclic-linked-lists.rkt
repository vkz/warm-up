#lang racket/base
;; follows wonderful post by Jay McCarthy
;; http://jeapostrophe.github.io/2013-09-09-middle-post.html

(require racket/match
	 racket/list
	 rackunit)

(define N 20)
	 

;; conses are immutable in Racket, so we can't just create cycles
;; make-placeholder and make-reader-graph for the rescue
(let* ([ph (make-placeholder #f)]
       [x (cons 1 ph)])
  (placeholder-set! (cdr x) x)
  (make-reader-graph x))

(define (make-cyclic-list N)
  (define cs
    (for/list ([i (in-range N)])
      (cons i (make-placeholder empty))))

  ;; link all conses into a list
  (for ([from (in-list cs)]
	[to (in-list (rest cs))])
    (placeholder-set! (cdr from) to))

  ;; randomly generate the end and beginning of cycle
  (match-define
   (list cycle-start cycle-end)
   (sort (build-list 2 (lambda (i) (random N)))
	 <))

  (placeholder-set!
   (cdr (list-ref cs cycle-end))
   (cons +inf.0 (list-ref cs cycle-start)))

  ;; now make the list with cycle immutable
  (make-reader-graph (first cs)))

(define (test-has-a-cycle? N has-a-cycle?)
  (for ([i (in-range N)])
    (check-false (has-a-cycle? (build-list N add1)))
    (check-true (has-a-cycle? (make-cyclic-list N)))))

;; find cycles by "marking" every cons on the way
;; conses don't have space for a mark, but
;; hasheq that indexes by eq? is effectively like
;; adding a field to any Racket object
;; so lets keep marks in a hash-table indexed by
;; conses themselves
;;
;; keeping such a table is obviously wastful
;; but a good first solution
(define (marking-cycle? l)
  (let loop ([marks (hasheq)] [l l]) ;handy technique - build data by cycling data
    (cond
     [(empty? l) #f]
     [(hash-has-key? marks l) #t]
     [else (loop (hash-set marks l #t)
		 (cdr l))])))

(test-has-a-cycle? N marking-cycle?)

;; tortoise and hare algorithm (due to Floyd)
;; use escape-continuation when we reach the end of the list (no cycle)
(define (non-marking-cycle? l)
  (let/ec esc
    (define (cdr* l)
      (if (empty? l)
	  (esc #f) ;end of list (no cycles)
	  (cdr l)))
    (let loop ([tortoise (cdr* l)]
	       [hare (cdr* (cdr* l))])
      (unless (eq? tortoise hare)
	(loop (cdr* tortoise)
	      (cdr* (cdr* hare)))))
    #t))
      
(test-has-a-cycle? N non-marking-cycle?)	      
	 
