# rearrangements
Rearrangements of a list (often called permutations)

For a long list it is not possible to produce all rearrangements,
but it is possible to produce a representation in terms of a function
that given index k returns the k-th rearrangement.

An inverse function is included.
Given a rearrangement, it returns the index.

The two procedures accept an equivalence relation applying to
the elements of the list being rearranged.
Two rearrangements are considered to be the same
if all corresponding elements are equivalent.
