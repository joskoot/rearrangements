#lang racket

(require (only-in math/number-theory multinomial))

(provide
 make-N->R
 make-R->N
 nr-of-Rs
 R?)

(define (make-N->R L (EQ? equal?))
 (let ((N (nr-of-Rs L EQ?)))
  (λ (K)
   (reverse
    (let rearrange ((L L) (K K) (N N) (result '()))
     (if (null? L) result
      (let pseudo-rotate ((H L) (T '()) (K K))
       (let ((E (car H)) (H (cdr H)))
        (if (member E T EQ?)
         (pseudo-rotate H (cons E T) K)
         (let ((M (/ (* N (count-occurrences E L EQ?)) (length L))))
          (if (< K M)
           (rearrange (append H T) K M (cons E result))
           (pseudo-rotate H (cons E T) (- K M)))))))))))))

(define (make-R->N L (EQ? equal?))
 (let ((N (nr-of-Rs L EQ?)))
  (λ (R)
   (let rearrange ((L L) (R R) (K 0) (N N))
    (if (null? L) K
     (let pseudo-rotate ((H L) (T '()) (K K))
      (let ((E (car H)) (H (cdr H)))
       (if (member E T EQ?) (pseudo-rotate H (cons E T) K)
        (let ((M (/ (* N (count-occurrences E L EQ?)) (length L))))
         (if (EQ? E (car R)) (rearrange (append H T) (cdr R) K M)
          (pseudo-rotate H (cons E T) (+ K M))))))))))))

(define (nr-of-Rs L (EQ? equal?))
 (multinomial (length L) (occurrences L EQ?)))

(define (occurrences L (EQ? equal?))
 (map (λ (E) (count-occurrences E L EQ?)) (remove-duplicates L EQ?)))

(define (count-occurrences E L (EQ? equal?))
 (define (eq x) (EQ? x E))
 (count eq L))

(define (R? a b (EQ? equal?))
 (and (= (length a) (length b))
  (let loop1 ((a a) (b b))
   (cond
    ((null? a))
    (else
     (define e (car a))
     (let loop2 ((b b) (r '()))
      (cond
       ((null? b) #f)
       ((EQ? e (car b)) (loop1 (cdr a) (append (cdr b) r)))
       (else (loop2 (cdr b) (cons (car b) r))))))))))

