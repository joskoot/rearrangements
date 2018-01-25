#lang racket

(require "rearrangements.rkt")

(define (test-equal a b)
 (unless (equal? a b) (error 'test-equal "not equal:~n~s~n~s~n" a b)))

(define (tester L (EQ? equal?))
 (define F (make-N->R L EQ?))
 (define G (make-R->N L EQ?))
 (define N (nr-of-Rs L EQ?))
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
(tester (range 10) (λ (x y) (eq? (even? x) (even? y))))

(test-equal (nr-of-Rs '(1 1 2 2 3 3 4 4) =) 2520)
(test-equal (nr-of-Rs '(1 2 1 2 1 2 1 2) =) 70)
(test-equal (nr-of-Rs (list (list 'a) (list 'a) (list 'a)) eq?) 6)
(test-equal (nr-of-Rs (list (list 'a) (list 'a) (list 'a)) equal?) 1)

(define (sort-Ls Ls) (sort Ls L<?))

(define (L<? L0 L1)
 (and (pair? L0) (pair? L1)
  (let ((E0 (car L0)) (E1 (car L1)))
   (or (symbol<? E0 E1)
    (and (eq? E0 E1)
     (L<? (cdr L0) (cdr L1)))))))

(test-equal
 (sort-Ls (for/list ((k 6)) ((make-N->R '(a b c)) k)))
 (sort-Ls '((c b a) (b c a) (a c b) (c a b) (a b c) (b a c))))

(test-equal
 (sort-Ls (for/list ((k 3)) ((make-N->R '(a a b)) k)))
 (sort-Ls '((b a a) (a b a) (a a b))))

(define x (make-N->R '(a b c)))
(define y (for/list ((k (in-range 6))) (x k)))
(unless (for/and ((y (in-list y))) (R? y '(a b c)))
 (error 'R? "fails"))

"All is well"

