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

(define (require-distinct cells)
  (compound-propagator cells '()
    (lambda ()
      (for-each-distinct-pair
       (lambda (c1 c2)
         (let-cells (p)
           (p:= c1 c2 p)
           (abhor p)))
       cells))
    'require-distinct))

(define (one-of values output-cell)
  (compound-propagator '() (list output-cell)
    (lambda ()
      (let ((cells
             (map (lambda (value)
                    (let-cells ((cell value))
                      cell))
                  values)))
        (one-of-the-cells cells output-cell)))
    'one-of))

(define (one-of-the-cells input-cells output-cell)
  (cond ((n:= (length input-cells) 2)
         (let-cells (p)
           (p:conditional p
                          (car input-cells)
                          (cadr input-cells)
                          output-cell)
           (binary-amb p)))
        ((n:> (length input-cells) 2)
         (let-cells (link p)
           (one-of-the-cells (cdr input-cells) link)
           (p:conditional p
                          (car input-cells)
                          link
                          output-cell)
           (binary-amb p)))
        (else
         (error "Inadequate choices for one-of-the-cells"
                input-cells output-cell))))