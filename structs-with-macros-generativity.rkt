#lang racket/base
;; based on Jay McCarthy's struct article
;; http://jeapostrophe.github.io/2013-02-04-generati-post.html

(require rackunit)

;; naive implementation
;; structures are lists
;; field access is O(n) in the number of fields
(let ()
  (define-syntax-rule (vstruct struct struct? (struct-f ...))
    (begin
      (define (struct struct-f ...)
	(list struct-f ...))
      (define (struct? v)
	(and (list? v)
	     (= (length v) (length '(struct-f ...)))))
      (define (struct-f l)
	(for/or ([v (in-list l)]
		 [f (in-list '(struct-f ...))])
	  (and (eq? f 'struct-f)
	       v)))
      ...))
  
  ;; example / test code
  (vstruct posn posn? (posn-x posn-y))
  (define p (posn 1 2))
  (check-true (posn? p))
  (check-equal? 1 (posn-x p))
  (check-equal? 2 (posn-y p))
  )

;; fixing O(n) field lookup
;; by using tables
(let ()
  (define-syntax-rule (vstruct struct struct? (struct-f ...))
    (begin
      (define (struct struct-f ...)
	(make-immutable-hasheq (list (cons 'struct-f struct-f) ...)))
      (define (struct? v)
	(and (hash-eq? v) (immutable? v)
	     (hash-has-key? v 'struct-f)
	     ...))
      (define (struct-f v)
	(hash-ref v 'struct-f))
      ...))

  
  ;; example / test code
  (vstruct posn posn? (posn-x posn-y))
  (define p (posn 1 2))
  (check-true (posn? p))
  (check-equal? 1 (posn-x p))
  (check-equal? 2 (posn-y p))

  ;; hack1
  ;; doesn't catch fake "pretend"-structures
  (define not-a-posn (hasheq 'posn-x #f 'posn-y #t))
  (check-true (posn? not-a-posn))
  )


;; protecting from hack1
;; assume the module implementing vstruct macro does'n export the key
(let ()
  (define vstruct-key (gensym 'quit-hacking-bro))
  (define-syntax-rule (vstruct struct struct? (struct-f ...))
    (begin
      (define (struct struct-f ...)
	(make-immutable-hasheq (list (cons vstruct-key vstruct-key)
				     (cons 'struct-f struct-f) ...)))
      (define (struct? v)
	(and (hash-eq? v) (immutable? v)
	     (eq? (hash-ref v vstruct-key #f) vstruct-key)
	     (hash-has-key? v 'struct-f)
	     ...))
      (define (struct-f v)
	(hash-ref v 'struct-f))
      ...))

  
  ;; example / test code
  (vstruct posn posn? (posn-x posn-y))
  (define p (posn 1 2))
  (check-true (posn? p))
  (check-equal? 1 (posn-x p))
  (check-equal? 2 (posn-y p))

  ;; hack1
  ;; should not work now
  (define not-a-posn (hasheq 'posn-x #f 'posn-y #t))
  (check-false (posn? not-a-posn))

  ;; hack2
  (define a-posn (posn 1 2))
  (define da-key
    (for/or ([v (in-hash-values a-posn)])
      (and (not (number? v))
	   v)))
  (define fake-posn (hasheq da-key da-key 'posn-x #f 'posn-y #t))
  (check-true (posn? fake-posn))
  )

;; The problem is that we don’t have a way to restrict access to the
;; data-structure we use to implement the structures. Many people assume
;; that the feature structures provide is the ability to efficiently
;; represent records and extract their values, etc. This is not the
;; case. Structures provide a way to implement sealing, or tying core
;; data-structures to their creators and only their creators.  
;;                                                     --- Jay McCarthy


;; sealing the struct
(let ()
  (define (make-seal)
    (define unique-key (gensym 'new-struct-key))
    (define (seal v)
      (lambda (key)
	(when (eq? key unique-key)
	  v)))
    (define (seal? sv)
      (and (procedure? sv)
	   (not (void? (unseal sv)))))
    (define (unseal sv)
      (sv unique-key))
    (values seal seal? unseal))
	    
  (define-syntax-rule (vstruct struct struct? (struct-f ...))
    (begin
      (define-values (seal seal? unseal) (make-seal))
      (define (struct struct-f ...)
	(seal (make-immutable-hasheq (list (cons 'struct-f struct-f) ...))))
      (define (struct? v)
	(seal? v))
      (define (struct-f v)
	(hash-ref (unseal v) 'struct-f))
      ...))

  
  ;; example / test code
  (vstruct posn posn? (posn-x posn-y))
  (define p (posn 1 2))
  (check-true (posn? p))
  (check-equal? 1 (posn-x p))
  (check-equal? 2 (posn-y p))

  ;; hack1
  ;; should not work now
  (define not-a-posn (hasheq 'posn-x #f 'posn-y #t))
  (check-false (posn? not-a-posn))

  ;; hack2
  ;; should not work
  (define a-posn (posn 1 2))
  (define not-a-posn2 (hasheq 'posn-x #f 'posn-y #t))
  (check-false (posn? not-a-posn2))
  )
