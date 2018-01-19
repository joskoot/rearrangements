#lang racket

(require "rearrangements.rkt")

(define (test-equal a b)
 (unless (equal? a b) (error 'test-equal "not equal:~n~s~n~s~n" a b)))

(define (tester L (EQ? equal?))
 (define F (make-exact-nonnegative-integer->rearrangement L EQ?))
 (define G (make-rearrangement->exact-nonnegative-integer L EQ?))
 (define N (nr-of-rearrangements L EQ?))
 ; Check that G is the inverse of F.
 (for ((K (in-range 0 N))) (test-equal K (G (F K))))
 (for ((K (in-range 0 N))) (define L (F K)) (test-equal L (F (G L)))))

(tester '())
(tester '(a))
(tester '(a b))
(tester '(a a))
(tester '(a b c d) eq?)
(tester '(a a b c))
(tester '(a b a c))
(tester '(a b c a))
(tester '(a a a b))
(tester '(a a a a))
(tester '(1 1 2 2 3 3 4 4) =)
(tester '(1 1 1 1 1 2 2 2 2 2) =)
(tester '(1 2 1 2 1 2 1 2 1 2) =)
(tester '(1 2 2 3 3 3 4 4 4 4) =)
(tester '(1 2 3 4 2 3 4 3 4 4) =)

(test-equal (nr-of-rearrangements '(1 1 2 2 3 3 4 4) =) 2520)
(test-equal (nr-of-rearrangements '(1 2 1 2 1 2 1 2) =) 70)
(test-equal (nr-of-rearrangements (list (list 'a) (list 'a) (list 'a)) eq?) 6)
(test-equal (nr-of-rearrangements (list (list 'a) (list 'a) (list 'a)) equal?) 1)

(define (sort-Ls Ls) (sort Ls L<?))

(define (L<? L0 L1)
 (and (pair? L0) (pair? L1)
  (let ((E0 (car L0)) (E1 (car L1)))
   (or (symbol<? E0 E1)
    (and (eq? E0 E1)
     (L<? (cdr L0) (cdr L1)))))))

(test-equal
 (sort-Ls (for/list ((k 6)) ((make-exact-nonnegative-integer->rearrangement '(a b c)) k)))
 (sort-Ls '((c b a) (b c a) (a c b) (c a b) (a b c) (b a c))))

(test-equal
 (sort-Ls (for/list ((k 3)) ((make-exact-nonnegative-integer->rearrangement '(a a b)) k)))
 (sort-Ls '((b a a) (a b a) (a a b))))

"All is well"
