#lang racket
;; reversing doubly-linked lists
;; based on Jay McCarthy's
;; Source: http://jeapostrophe.github.io/2012-03-31-siq-reve-post.html
;; Wiki:   http://en.wikipedia.org/wiki/Doubly_linked_list
(require rackunit
	 racket/list)

;; DLL implementation
;; ==================
(struct node (last element next) #:transparent #:mutable)
(struct dll (head tail) #:transparent #:mutable)
(define (make-dll)
  (dll #f #f))

(define ((make-dll-cons!
          dll-head node set-node-last!
          dll-tail set-dll-tail! set-dll-head!)
         e l)
  (define head (dll-head l))
  (define new (node #f e head))
  (when head (set-node-last! head new))
  (unless (dll-tail l) (set-dll-tail! l head)) ;I think Jay has a mistake here
  (set-dll-head! l new))

(define dll-cons!
  (make-dll-cons!
   dll-head node set-node-last!
   dll-tail set-dll-tail! set-dll-head!))

(define dll-snoc!
  (make-dll-cons!
   dll-tail (lambda (last e next) (node next e last))
   set-node-next! dll-head set-dll-head! set-dll-tail!))

(define (make-dll-fold dll-head node-next)
  (define (dll-fold cons empty node)
    (if node
	(cons (node-element node)
	      (dll-fold cons empty (node-next node)))
	empty))
  (lambda (cons empty list)
    (dll-fold cons empty (dll-head list))))
(define dll-fold (make-dll-fold dll-head node-next))
(define dll-rfold (make-dll-fold dll-tail node-last))

;; Naive dll reversing O(n)
;; ===================
(define (dll-naive-reverse! l)
  (define head (dll-head l))
  (let loop ([last #f] [current (dll-head l)])
    (when last (set-node-last! last current))
    (when current
      (define next (node-next current))
      (set-node-next! current last)
      (loop current next)))
  (set-dll-head! l (dll-tail l))
  (set-dll-tail! l head))

;; delayed dll reversing O(1)
;; =====================
(struct rdll (reversed? dll) #:transparent #:mutable)

;; just reversing the flag
(define (rdll-reverse! l)
  (set-rdll-reversed?!
   l (not (rdll-reversed? l))))

(define (make-rdll)
  (rdll #f (make-dll)))
(define-syntax-rule
  (define-rdll (id arg ... rl) reversed-dll normal-dll)
  (define (id arg ... rl)
    (define l (rdll-dll rl))
    (if (rdll-reversed? rl)
      (reversed-dll arg ... l)
      (normal-dll arg ... l))))
(define-rdll (rdll-cons! e rl) dll-snoc! dll-cons!)
(define-rdll (rdll-snoc! e rl) dll-cons! dll-snoc!)
(define-rdll (rdll-fold cons empty rl) dll-rfold dll-fold)
(define-rdll (rdll-rfold cons empty rl) dll-fold dll-rfold)
  


;; Testing
;; =======
(define (dll-test make-dll dll-cons! dll-snoc!
                  dll-fold dll-rfold dll-reverse!)
  (define c123 (make-dll))
  (dll-cons! 2 c123)
  (dll-cons! 1 c123)
  (dll-snoc! 3 c123)
  (check-equal? (dll-fold cons empty c123)
                '(1 2 3))
  (check-equal? (dll-rfold cons empty c123)
                '(3 2 1))
  (dll-reverse! c123)
  (check-equal? (dll-fold cons empty c123)
                '(3 2 1))
  (check-equal? (dll-rfold cons empty c123)
                '(1 2 3))
  (dll-cons! 4 c123)
  (dll-reverse! c123)
  (dll-cons! 0 c123)
  (check-equal? (dll-fold cons empty c123)
                '(0 1 2 3 4))
  (check-equal? (dll-rfold cons empty c123)
                '(4 3 2 1 0)))

(dll-test make-dll dll-cons! dll-snoc!
          dll-fold dll-rfold dll-naive-reverse!)

(dll-test
 make-rdll rdll-cons! rdll-snoc!
 rdll-fold rdll-rfold rdll-reverse!)

