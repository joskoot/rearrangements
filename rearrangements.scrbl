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
the order of the original list included.
Two elements of a list are considered to be the same if they satisfy a given equivalence relation.
Two rearrangements are the same if all corresponding elements are the same.
The number of distinct rearrangements of a list L is:

(/ (factorial (length L)) (* (factorial M@subscript{0}) ... (factorial M@subscript{n-1})))

where n is the number of distinct elements and M@subscript{i} the number of occurrences
of the i-th distinct element. If all elements of the list are distinct, the
number of rearrangements is:

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

First assume that all elements are distinct. Then all rearrangements can be found as follows:

If the list is empty, it has one rearrangement, namely itself.
If the list is not empty, take all its rotations.
For every rotation R, cons (car R) to every rearrangement of (cdr R).
A list of n distinct elements has n rotations.
A list is a rotation of itself. Let this be R0.
Rotation Rn+1 can be found from rotation Rn as
(append (cdr Rn) (list (car Rn))).

It is not necessary to take true rotations. The first element must be in place,
but the cdr may have another order.
These will be called pseudorotations. If there are n distinct elements, each
collection of n rearrangements will do provided that they all differ for the first element.

A procedure produced by @racket[make-exact-nonnegative-integer->rearrangement]
does not compute all pseudorotations or rearrangements.
If index K is less than the number of rearrangements of the cdr of a rotation,
the car of that rotation is consed to the K-th rearrangement of the cdr. If K is greater, then the
next rotation is formed and K is decreased by the number of rearrangements of the cdr of the skipped
rotation.

@defproc[
(make-exact-nonnegative-integer->rearrangement (L list?) (EQ? (-> any/c any/c any/c) equal?))
(-> exact-nonnegative-integer? list?)]{
Procedure @racket[make-exact-nonnegative-integer->rearrangement]
takes a list @racket[L] and returns a procedure.
Let N be the number of distinct rearrangements of @racket[L].
The returned procedure takes an index K less than N
and returns the K@superscript{th} distinct rearrangement of @racket[L].}

@defproc[
(make-rearrangement->exact-nonnegative-integer 
(L (listof any/c)) (EQ? (-> any/c any/c any/c) equal?))
(-> exact-nonnegative-integer? (listof any/c))]{
Procedure @racket[make-rearrangement->exact-nonnegative-integer] produces the inverse
of the procedure returned by procedure @racket[make-exact-nonnegative-integer->rearrangement].}

