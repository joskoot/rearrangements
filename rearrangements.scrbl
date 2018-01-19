#lang scribble/manual

@(require
  scribble/core
  scribble/eval
  racket
  scribble/html-properties
  "rearrangements.rkt"
  (for-label "rearrangements.rkt"
             racket (only-in typed/racket Setof Exact-Nonnegative-Integer Sequenceof))
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

(a b c)
(a c b)
(b c a)
(b a c)
(c a b)
(c b a)

With the same equivalence relation list (a b b) has 3 rearrangements:

(a a b)
(a b a)
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
Rotation Rn+1 can be found from rotation Rn as (append (cdr Rn) (list (car Rn))).

It is not necessary to take true rotations. The first element must be in place, but the cdr may have
another order. These will be called pseudorotations. If there are n distinct elements, each
collection of n rearrangements will do provided that they all differ for the first element.

A proc produced by make-natural->rearrangement does not compute all pseudorotations or 
rearrangements. If the index K is less than the number of rearrangements of the cdr of a rotation,
the car of that rotation is consed to the K-th rearrangement of the cdr. If K is greater, then the
next rotation is formed and K is decreased by the number of rearrangements of the cdr of the skipped
rotation.

In the following a natural number is an exact nonnegative integer number.

====================================================================================================
Proc: ((make-natural->rearrangement L (EQ? equal?)) K) -> R
L   : list of E
EQ? : (-> E E boolean?) : equivalence relation on the elements of L
E   : any element of L
K   : natural number
R   : rearrangement of L

Proc make-natural->rearrangement takes a list L and returns a proc.
Let N be the number of distinct rearrangements of L.
The returned proc takes an index K less than N and returns the K-th distinct rearrangement of L. |#


#|==================================================================================================
Proc: ((make-rearrangement->natural L (EQ? equal?)) R) -> K
L : list
K : natural number
R : rearrangement of L

Proc make-rearrangement->natural produces the inverse of the proc returned by proc
make-natural->rearrangement. Mark the similarity between make-natural->rearrangement and
make-rearrangement->natural. |#

