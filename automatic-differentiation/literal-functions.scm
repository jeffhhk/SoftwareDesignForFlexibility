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

;;; This is the literal function mechanism

(define (literal-function fexp)
  (define (the-function . args)
    (if (any differential? args)
        (let ((n (length args))
              (factor (apply maximal-factor args)))
          (let ((realargs
                 (map (lambda (arg)
                        (finite-part arg factor))
                      args))
                (deltargs
                 (map (lambda (arg)
                        (infinitesimal-part arg factor))
                      args)))
            (let ((fxs (apply the-function realargs))
                  (partials
                   (map (lambda (i)
                          (apply (literal-function
                                  (deriv-expr i n fexp))
                                 realargs))
                        (iota n))))
              (fold d:+ fxs
                (map d:* partials deltargs)))))
        `(,fexp ,@args)))
  the-function)

(define (deriv-expr i n fexp)
  (if (= n 1)
      `(derivative ,fexp)
      `((partial ,i) ,fexp)))
