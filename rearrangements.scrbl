#lang scribble/manual

@(require
  scribble/core
  scribble/eval
  racket
  "rearrangements.rkt"
  (for-label "rearrangements.rkt" racket)
  (for-syntax racket))

@title[#:version ""]{Rearrangements}
@author{Jacob J. A. Koot}
@(defmodule "rearrangements.rkt" #:packages ())

@section{Introduction}
A rearrangement of a list is a list of the same elements but in arbitrary order,
the original list included.
Two elements of a list are considered to be the same if they satisfy a given equivalence relation.
Two rearrangements are the same if all corresponding elements are the same.
The number of distinct rearrangements of a list L is:

(/ (factorial (length L)) (factorial M@subscript{0}) ... (factorial M@subscript{n-1}))

where n is the number of distinct elements and M@subscript{i} the number of occurrences
of the i@superscript{th} distinct element. If all elements of the list are distinct,
the number of rearrangements is:

(factorial (length L))

With equivalence relation eq? list (a b c) has 6 rearrangements:

(a b c)@(linebreak)
(a c b)@(linebreak)
(b c a)@(linebreak)
(b a c)@(linebreak)
(c a b)@(linebreak)
(c b a)

With the same equivalence relation list (a b b) has 3 rearrangements:

(a a b)@(linebreak)
(a b a)@(linebreak)
(b a a)

The number of distinct rearrangements can be very large. For example a deck of 52 distinct playing
cards has 80658175170943878571660636856403766975289505440883277824000000000000 distinct
rearrangements. Therefore it often is impossible two compute all rearrangements. In order to
represent the set of all rearrangements, we can use a function that given a natural number K
computes the K-th rearrangement, K<N, where N is the number of distinct rearrangements.

@section{Method}

Let `R' be short for `rearrangement'.

First assume that all elements are distinct. Then all Rs can be found as follows:

If the list is empty, it has one R, namely itself.
If the list is not empty, take all its rotations.
For every rotation r, cons (car r) to every R of (cdr r).
A list of n distinct elements has n rotations.
A list is a rotation of itself. Let this be r@subscript{0}.
Rotation r@subscript{n+1} can be found from rotation r@subscript{n} as
(append (cdr r@subscript{n}) (list (car r@subscript{n}))).

It is not necessary to take true rotations. The first element must be in place,
but the cdr may have another order.
These will be called pseudorotations. If there are n distinct elements, each
collection of n Rs will do provided that they all differ for the first element.
This does disturb the order of the Rs.

A procedure produced by @racket[make-N->R]
does not compute all pseudorotations or Rs.
If index K is less than the number of Rs of the cdr of a rotation,
the car of that rotation is consed to the K-th R of the cdr. If K is greater, then the
next rotation is formed and K is decreased by the number of Rs of the cdr of the skipped
rotation.

Procedures @racket[make-N->R]
accepts an equivalence relation (such as @racket[equal?] and @racket[eq?]).
The produced Rs are guaranteed to differ according to this relation. 

@defproc*[
(((make-N->R (L list?) (EQ? (-> any/c any/c any/c) equal?))
(-> exact-nonnegative-integer? list?))
 (((make-N->R (L list?) (EQ? (-> any/c any/c any/c) equal?))
   (K exact-nonnegative-integer?)) list?))]{
@racket[EQ?] must be an equivalence relation.
Procedure @racket[make-N->R]
takes a list @racket[L] and returns a procedure.
Let N be the number of distinct Rs of @racket[L].
The returned procedure takes an index @racket[K] less than
the number of distinct Rs of @racket[L].
It returns the @racket[K]@superscript{th} distinct R of @racket[L].
Results are unpredictable when @racket[K] is equal to or greater than
the number of distinct Rs of @racket[L].}

@interaction[
(require "rearrangements.rkt" racket)
(code:line)
(define x (make-N->R '(a b c)))
(for/list ((k (in-range 6))) (x k))
(code:line)
(define y (make-N->R '(a a b)))
(for/list ((k (in-range 3))) (y k))
(code:line)
(define z (make-N->R (range 1000)))
(for ((z (in-list (z (expt 10 2500)))) (k (in-cycle (range 20))))
 (when (= k 0) (newline))
 (printf "~a " (~s #:min-width 3 #:align 'right z)))]

@defproc*[
(((make-R->N (L list?) (EQ? (-> any/c any/c any/c) equal?))
(-> list? exact-nonnegative-integer?))
 (((make-R->N (L list?) (EQ? (-> any/c any/c any/c) equal?)) (lst list?))
  exact-nonnegative-integer?))]{
@racket[EQ?] must be an equivalence relation.
Procedure @racket[make-R->N] produces the inverse
of the procedure returned by procedure @racket[make-N->R].
Results are unpredictable if @racket[lst] is not an R of @racket[L].
Let's check that @racket[make-R->N] produces the inverse of
the procedure returned by @racket[make-N->R].}

@interaction[
(require "rearrangements.rkt")
(code:line)
(define (test-equal a b)
 (unless (equal? a b) (error 'test-equal "not equal:~n~s~n~s~n" a b))
 #t)
(code:line)
(define (tester L (EQ? equal?))
 (define F (make-N->R L EQ?))
 (define G (make-R->N L EQ?))
 (define N (nr-of-Rs L EQ?))
 ; Check that G is the inverse of F.
 (and
  (for/and ((K (in-range 0 N))) (test-equal K (G (F K))))
  (for/and ((K (in-range 0 N))) (define L (F K)) (test-equal L (F (G L))))))
(code:line)
(and
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
 (tester '(1 2 3 4 2 3 4 3 4 4) =))]

@defproc[(nr-of-Rs (L list?) (EQ? (-> any/c any/c any/c) equal?))
         exact-positive-integer?]{
@racket[EQ?] must be an equivalence relation.
@racket[nr-of-Rs] returns the number of distinct Rs of @racket[L].}
Examples:

@interaction[
(require "rearrangements.rkt")
(nr-of-Rs '())
(nr-of-Rs '(x))
(nr-of-Rs '(a b c d))
(nr-of-Rs '(a a b b))
(nr-of-Rs (list (list 'a) (list 'a) (list 'a)) eq?)
(nr-of-Rs (list (list 'a) (list 'a) (list 'a)) equal?)]

