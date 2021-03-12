#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Functional Combinators

(define (first-compose f g)
  (lambda args
    (f (apply g args))))

(define (second-compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)

(define (simple-compose f g)
  (assert (= (get-arity f) 1))
  (define (the-composition . args)
    (f (apply g args)))
  (restrict-arity the-composition (get-arity g)))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  (restrict-arity the-composition (get-arity g)))

(define (first-parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(define (simple-parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  (let ((n1 (get-arity f))
        (n2 (get-arity g)))
    (assert (= n1 n2))
    (restrict-arity the-combination n1)))

(define (parallel-apply f g)
  (define (the-combination . args)
    (let-values ((fv (apply f args))
                 (gv (apply g args)))
      (apply values (append fv gv))))
  (let ((nf (get-arity f))
        (ng (get-arity g)))
    (assert (= nf ng))
    (restrict-arity the-combination nf)))

(define (parallel-combine h f g)
  (compose h (parallel-apply f g)))

(define (simple-spread-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (spread-apply f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    (let ((m (+ (get-arity f) 1)))
      (define (the-combination . args)
        (assert (= (length args) m))
        (apply f (list-remove args i)))
      (assert (< i m))
      (restrict-arity the-combination m))))

(define ((curry-argument i) . args)
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))

#|
(define (curry-argument i)
  (lambda (f)
    (let ((m (- (get-arity f) 1)))
      (define (the-combination . args)
        (assert (= (length args) m))
        (lambda (x)
          (apply f (list-insert args i x))))
      (restrict-arity the-combination m))))
|#

(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec)))
    (lambda (f)
      (define (the-combination . args)
        (apply f (permute args)))
      (let ((n (get-arity f)))
        (assert (= n (length permspec)))
        (restrict-arity the-combination n)))))

(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (assert (pair? lst))
    (if (= index 0)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cons value lst)
        (begin
          (assert (pair? lst))
          (cons (car lst) (lp (cdr lst) (- index 1)))))))

;;; Given a permutation (represented as
;;; a list of numbers), and a list to be
;;; permuted, construct the list so
;;; permuted.

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p))
         permspec))
  the-permuter)
