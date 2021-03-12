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

;;;; Boolean arithmetic

(define boolean-arithmetic
  (make-arithmetic 'boolean boolean? '()
    (lambda (name)
      (case name
        ((additive-identity) #f)
        ((multiplicative-identity) #t)
        (else (default-object))))
    (lambda (operator)
      (let ((procedure
             (case operator
               ((+) (lambda (b1 b2) (or b1 b2)))
               ((*) (lambda (b1 b2) (and b1 b2)))
               ((-) (lambda (b1 b2) 
                      (error "Boolean binary - not defined"
                             b1 b2)))
               ((<) (lambda (b1 b2) (and (not b1) b2)))
               ((<=) (lambda (b1 b2) (or (not b1) b2)))
               ((=) (lambda (b1 b2) (eq? b1 b2)))
               ((>=) (lambda (b1 b2) (or b1 (not b2))))
               ((>) (lambda (b1 b2) (and b1 (not b2))))
               ((positive?) (lambda (b) b))
               ((zero?) (lambda (b) (not b)))
               ((max) (lambda (b1 b2) (or b1 b2)))
               ((min) (lambda (b1 b2) (and b1 b2)))
               ((negate) (lambda (b) (not b)))
               (else (lambda args
                       (error "Operator undefined in Boolean"
                              operator args))))))
        (and procedure
             (simple-operation operator boolean? procedure))))))
