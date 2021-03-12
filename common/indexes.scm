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

;;;; Indexes: sequence masks

(define (index-limit length)
  (n:expt 2 length))

(define (index-all length)
  (n:- (index-limit length) 1))

(define (index-predicate length)
  (lambda (object)
    (and (n:exact-nonnegative-integer? object)
         (n:< object (index-limit length)))))

(define (index->booleans index length)
  (let loop ((i 0) (index index) (choices '()))
    (if (n:< i length)
        (loop (n:+ i 1)
              (quotient index 2)
              (cons (odd? index) choices))
        choices)))