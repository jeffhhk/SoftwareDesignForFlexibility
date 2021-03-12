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

;;; The following must be in the AmbScheme evaluator environment

(define (one-of lst)
  (if (null? lst)
      (amb)
      (amb (car lst)
           (one-of (cdr lst)))))

(define (distinct l)
  (cond ((null? l) true)
        ((null? (cdr l)) true)
        ((member (car l) (cdr l)) false)
        (else (distinct (cdr l)))))

(define (require p)
  (if (not p) (amb)))

(define (map f l)
  (if (null? l)
      '()
      (cons (f (car l))
            (map f (cdr l)))))

(define (to-right n)
  (modulo (+ n 1) 6))

(define (better-hand? position1 position2 values)
  (> (cdr (assv position1 values))
     (cdr (assv position2 values))))

(define (except set exceptions)
  (one-of (lset-difference eqv? set exceptions)))

(define (assignments places values)
  (if (null? places)
      '()
      (let ((choice (one-of values)))
        (cons (cons (car places) choice)
              (assignments (cdr places)
                           (delv choice values))))))