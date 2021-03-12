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

;;;; Support sets

(define (%make-support-set elements)
  (cons 'support-set elements))

(define (support-set? object)
  (and (pair? object)
       (eq? 'support-set (car object))
       (list? (cdr object))))
(register-predicate! support-set? 'support-set)

(define (support-set-elements support-set)
  (cdr support-set))

(define (make-support-set elements)
  (guarantee list? elements 'make-support-set)
  (if (null? elements)
      %empty-support-set
      (%make-support-set (delete-duplicates elements))))

(define (support-set . elements)
  (if (null? elements)
      %empty-support-set
      (%make-support-set (delete-duplicates elements))))

(define %empty-support-set
  (%make-support-set '()))

(define (support-set-empty? s)
  (null? (support-set-elements s)))

(define (support-set-adjoin set . elts)
  (make-support-set
   (apply lset-adjoin eqv? (support-set-elements set) elts)))

(define (support-set-union . sets)
  (make-support-set
   (apply lset-union
          eqv?
          (map support-set-elements sets))))

(define (support-set= set1 set2)
  (lset= eqv?
         (support-set-elements set1)
         (support-set-elements set2)))

(define (support-set<= set1 set2)
  (lset<= eqv?
          (support-set-elements set1)
          (support-set-elements set2)))

(define (support-set< set1 set2)
  (and (not (support-set= set1 set2))
       (support-set<= set1 set2)))

(define (support-set-remove set . elts)
  (make-support-set
   (lset-difference eqv?
                    (support-set-elements set)
                    elts)))

(define (support-set-filter predicate . sets)
  (make-support-set
   (apply filter
          predicate
          (map support-set-elements sets))))

(define (support-set-every predicate . sets)
  (apply every
         predicate
         (map support-set-elements sets)))

(define (support-set-any predicate . sets)
  (apply any
         predicate
         (map support-set-elements sets)))