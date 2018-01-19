#lang racket

(require (only-in math/number-theory multinomial))

(provide
 make-exact-nonnegative-integer->rearrangement
 make-rearrangement->exact-nonnegative-integer
 nr-of-rearrangements)

(define (make-exact-nonnegative-integer->rearrangement L (EQ? equal?))
 (let ((N (nr-of-rearrangements L EQ?)))
  (λ (K)
   (let rearrange ((L L) (K K) (N N) (result '()))
    (if (null? L) result
     (let pseudo-rotate ((H L) (T '()) (K K))
      (let ((E (car H)) (H (cdr H)))
       (if (member E T EQ?)
        (pseudo-rotate H (cons E T) K)
        (let ((M (/ (* N (count-occurrences E L EQ?)) (length L))))
         (if (< K M)
          (rearrange (append H T) K M (cons E result))
          (pseudo-rotate H (cons E T) (- K M))))))))))))

(define (make-rearrangement->exact-nonnegative-integer L (EQ? equal?))
 (let ((N (nr-of-rearrangements L EQ?)))
  (λ (R)
   (let rearrange ((L L) (R (reverse R)) (K 0) (N N))
    (if (null? L) K
     (let pseudo-rotate ((H L) (T '()) (K K))
      (let ((E (car H)) (H (cdr H)))
       (if (member E T EQ?) (pseudo-rotate H (cons E T) K)
        (let ((M (/ (* N (count-occurrences E L EQ?)) (length L))))
         (if (EQ? E (car R)) (rearrange (append H T) (cdr R) K M)
          (pseudo-rotate H (cons E T) (+ K M))))))))))))

(define (nr-of-rearrangements L (EQ? equal?))
 (multinomial (length L) (occurrences L EQ?)))

(define (occurrences L (EQ? equal?))
 (map (λ (E) (count-occurrences E L EQ?)) (remove-duplicates L EQ?)))

(define (count-occurrences E L (EQ? equal?))
 (let loop ((L L) (n 0))
  (cond
   ((null? L) n)
   ((EQ? E (car L)) (loop (cdr L) (add1 n)))
   (else (loop (cdr L) n)))))

