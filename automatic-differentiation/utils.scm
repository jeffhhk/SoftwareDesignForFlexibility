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

;;; Termlists are sorted

(define (ordered-union <-? l1 l2)
  (let lp ((l1 l1) (l2 l2))
    (cond ((null? l1) l2)
          ((null? l2) l1)
          ((eqv? (car l1) (car l2))
           (cons (car l1) (lp (cdr l1) (cdr l2))))
          ((<-? (car l1) (car l2))
           (cons (car l1) (lp (cdr l1) l2)))
          ((<-? (car l2) (car l1))
           (cons (car l2) (lp l1 (cdr l2))))
          (else (error "Union")))))

(define (ordered-intersect <-? l1 l2)
  (let lp ((l1 l1) (l2 l2))
    (cond ((null? l1) '())
          ((null? l2) '())
          ((eqv? (car l1) (car l2))
           (cons (car l1) (lp (cdr l1) (cdr l2))))
          ((<-? (car l1) (car l2))
           (lp (cdr l1) l2))
          ((<-? (car l2) (car l1))
           (lp l1 (cdr l2)))
          (else (error "Intersect")))))


;;; Need this to make n-ary literal functions

(define (combinations lst p)
  (cond ((= p 0) '(()))
        ((null? lst) '())
        (else (append (map (lambda (rest)
                             (cons (car lst) rest))
                           (combinations (cdr lst) (- p 1)))
                      (combinations (cdr lst) p)))))

(define (all-combinations lst)
  (append-map (lambda (i)
                (combinations lst i))
              (iota (length lst) 1)))

;;; Nice versions of arithmetic to simplify stuff.

(define (numerical-simplifier-wrapper base-arithmetic)
  (make-arithmetic 'numerical-simplifier
                   (arithmetic-domain-predicate base-arithmetic)
                   (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (lambda (operator base-operation)
      (case operator
        ((+) (wrap-simplifier simplify:+ base-operation))
        ((*) (wrap-simplifier simplify:* base-operation))
        ((expt) (wrap-simplifier simplify:expt base-operation))
        (else base-operation)))))

(define ((simplify:+ base+) x y)
  (cond ((eqv? x 0) y)
        ((eqv? y 0) x)
        (else (base+ x y))))

(define ((simplify:* base*) x y)
  (cond ((or (eqv? x 0) (eqv? y 0)) 0)
        ((eqv? x 1) y)
        ((eqv? y 1) x)
        (else (base* x y))))

(define ((simplify:expt base-expt) x y)
  (cond ((eqv? x 0)
         (if (and (number? y) (< y 0))
             (error "undefined EXPT" x y))
         0)
        ((or (eqv? x 1) (eqv? y 0)) 1)
        ((eqv? y 1) x)
        (else (base-expt x y))))

(define (wrap-simplifier wrapper base-operation)
  (let ((wrapped
         (transform-operation-procedure wrapper base-operation)))
    (hash-table-set! numeric-simplifier-forwarding
                     (operation-procedure wrapped)
                     (operation-procedure base-operation))
    wrapped))

(define-generic-procedure-extractor 'numerical-simplifier
  (lambda (object)
    (hash-table-ref/default numeric-simplifier-forwarding
                            object
                            #f)))

(define numeric-simplifier-forwarding
  (make-key-weak-eqv-hash-table))

(define full-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
      (symbolic-extender numeric-arithmetic))
    (numerical-simplifier-wrapper g)))

(install-arithmetic! full-arithmetic)

(define (differential finite-part infinitesimal-part dx)
  (make-differential
   (cons (make-diff-term infinitesimal-part (list dx))
         (real->diff-terms finite-part))))

(define (real->diff-terms x)
  (list (make-diff-term x '())))

;;; gets overwritten if we have a simplifier.
(define (simplify x) x)
