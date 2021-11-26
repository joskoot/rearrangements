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
@(defmodule "rearrangements/rearrangements.rkt" #:packages ())
@(define ↓ subscript)
@(define ↑ superscript)

@section{Introduction}
A rearrangement of a list is a list of the same elements but in arbitrary order,
the original order included.
Two elements of a list are considered to be the same if they satisfy a given equivalence relation.
Two rearrangements are the same if all corresponding elements are the same.
The number of distinct rearrangements of a list @tt{L} is:

@tt{(/ (factorial (length L)) (factorial M@↓{0}) ... (factorial M@↓{n-1}))}

where @tt{n} is the number of distinct elements and @tt{M@↓{i}} the number of occurrences
of the @tt{i@↑{th}} distinct element. If all elements of the list are distinct,
the number of rearrangements is:

@tt{(factorial (length L))}

With equivalence relation @racket[eq?] list @tt{(a b c)} has 6 rearrangements:

@tt{(a b c)@(linebreak)
(a c b)@(linebreak)
(b c a)@(linebreak)
(b a c)@(linebreak)
(c a b)@(linebreak)
(c b a)}

With the same equivalence relation list @tt{(a b b)} has 3 rearrangements:

@tt{(a a b)@(linebreak)
(a b a)@(linebreak)
(b a a)}

The number of distinct rearrangements can be very large.
For example a deck of 52 distinct playing cards has about 8.065818×10@↑{67} distinct rearrangements.
Therefore it often is impossible two compute all rearrangements.
In order to represent the set of all rearrangements,
we can use a function that given a natural number K computes the K@↑{th} rearrangement,
K<N, where N is the number of distinct rearrangements.

@section{Method}

First assume that all elements are distinct. Then all rearrangements can be found as follows:

If the list is empty, it has one rearrangement, namely itself.
If the list is not empty, take all its rotations.
For every rotation @tt{r}, cons @tt{(car r)} to every rearrangement of @tt{(cdr r)}.
A list of n distinct elements has n rotations.
A list is a rotation of itself. Let this be @tt{r@↓{0}}.
Rotation @tt{r@↓{k+1}} can be found from rotation @tt{r@↓{k}} as
@tt{(append (cdr r@↓{k}) (list (car r@↓{k}}))).

It is not necessary to take true rotations. The first element must be in place,
but the cdr may have another order.
These will be called pseudorotations. If there are n distinct elements, each
collection of n rearrangements will do provided that they all differ for the first element.
This does affect the order of the rearrangements, of course.

A procedure produced by @racket[make-N->R]
does not compute all pseudorotations or rearrangements.
If index K is less than the number of rearrangements of the cdr of a rotation,
the car of that rotation is consed to the K@↑{th} rearrangement of the cdr. If K is greater,
then the next rotation is formed and K is decreased
by the number of rearrangements of the cdr of the skipped
rotation.

Procedures @racket[make-N->R] and @racket[make-R->N]
accept an equivalence relation (such as @racket[equal?] and @racket[eq?],
but any procedure defining an equivalence on the set of elements of the list being rearranged
will do).
The produced rearrangements are guaranteed to differ according to this relation. 

@defproc*[
(((make-N->R (L list?) (EQ? (-> any/c any/c any/c) equal?))
(-> exact-nonnegative-integer? list?))
 (((make-N->R (L list?) (EQ? (-> any/c any/c any/c) equal?))
   (K exact-nonnegative-integer?)) list?))]{
@racket[EQ?] must be an equivalence relation for the elements of @racket[L],
every value returned by @racket[EQ? ] other than @racket[#f] being interpreted as true.
Procedure @racket[make-N->R]
takes a list @racket[L] and returns a procedure.
The returned procedure takes an index @racket[K] less than
the number of distinct rearrangements of @racket[L].
It returns the @racket[K]@↑{th} rearrangement of @racket[L].
Results are unpredictable when @racket[K] is equal to or greater than
the number of distinct rearrangements of @racket[L].}

@interaction[
(require "rearrangements.rkt" racket)
(code:comment " ")
(define x (make-N->R '(a b c)))
(for/list ((k (in-range 6))) (x k))
(code:comment " ")
(define y (make-N->R '(a a b)))
(for/list ((k (in-range 3))) (y k))]

An example with a list of 1000 elements, all distinct:

@interaction[
(require racket "rearrangements.rkt")
(require (only-in math/number-theory factorial))
(require (only-in math/base random-natural))
(code:comment " ")
(define n 1000)
(random-seed 0)
(define m (factorial n))
(define k (random-natural m))
(list (~r #:notation 'exponential m) (~r #:notation 'exponential k))
(define lst (range n))
(define z (make-N->R lst =))
(define r (time (z k)))
(code:comment #,(list "This is significantly slower than procedure " @racket[shuffle] "."))
(void (time (shuffle lst)))
(code:comment #,(list "But " @racket[shuffle] " does not guarantee distinct results"))
(code:comment "when called repeatedly:")
(random-seed 0)
(check-duplicates (for/list ((k (in-range 6))) (shuffle '(a b c))))
(code:comment " ")
(equal? (sort r <) lst)
(for ((e (in-list r)) (i (in-cycle (range 20))))
 (when (= i 0) (newline))
 (printf "~a " (~s #:min-width 3 #:align 'right e)))]

@defproc*[
(((make-R->N (L list?) (EQ? (-> any/c any/c any/c) equal?))
(-> list? exact-nonnegative-integer?))
 (((make-R->N (L list?) (EQ? (-> any/c any/c any/c) equal?)) (lst list?))
  exact-nonnegative-integer?))]{
@racket[EQ?] must be an equivalence relation for the elements of @racket[L],
every value returned by @racket[EQ? ] other than @racket[#f] being interpreted as true.
Procedure @racket[make-R->N] produces the inverse
of the procedure returned by procedure @racket[make-N->R].
Results are unpredictable if @racket[lst] is not an rearrangement of @racket[L].
Let's check that @racket[make-R->N] produces the inverse of
the procedure returned by @racket[make-N->R].}

@interaction[
(require "rearrangements.rkt")
(code:comment " ")
(define (test-equal a b)
 (unless (equal? a b) (error 'test-equal "not equal:~n~s~n~s~n" a b))
 #t)
(code:comment " ")
(define (tester L (EQ? equal?))
 (define F (make-N->R L EQ?))
 (define G (make-R->N L EQ?))
 (define N (nr-of-Rs L EQ?))
 ; Check that G is the inverse of F.
 (and
  (for/and ((K (in-range 0 N))) (test-equal K (G (F K))))
  (for/and ((K (in-range 0 N))) (define L (F K)) (test-equal L (F (G L))))))
(code:comment " ")
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
@racket[EQ?] must be an equivalence relation for the elements of @racket[L].
@racket[nr-of-Rs] returns the number of distinct rearrangements of @racket[L].}
Examples:

@interaction[
(require "rearrangements.rkt")
(nr-of-Rs '())
(nr-of-Rs '(x))
(nr-of-Rs '(a b c d))
(nr-of-Rs '(a a b b))
(nr-of-Rs (list (list 'a) (list 'a) (list 'a)) eq?)
(nr-of-Rs (list (list 'a) (list 'a) (list 'a)) equal?)]

@defproc[(R? (x list?) (y list?) (EQ? (-> any/c any/c any/c) equal?)) boolean?]{
Returns @racket[#t] iff @racket[x] and @racket[y] are rearrangements of each other
according to equivalence relation @racket[EQ?].} Examples:

@interaction[
(require "rearrangements.rkt" racket)
(R? '(a b c) '(c b a))
(R? '(a b c) '(d e f))
(R? '(a b c) '(d e f) (λ (x y) #t))
(R? '(a b c) '(a b c d) (λ (x y) #t))]
