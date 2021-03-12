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

;;;; Alternative function extenders

(define (pure-function-extender codomain-arithmetic)
  (make-arithmetic 'pure-function
                   function?
                   (list codomain-arithmetic)
    (lambda (name codomain-constant)
      (lambda args
        codomain-constant))
    (lambda (operator codomain-operation)
      (simple-operation operator function?
        (lambda functions
          (lambda args
            (apply-operation codomain-operation
                             (map (lambda (function)
                                    (apply function args))
                                  functions))))))))

(define (function-extender-with-coercion codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function-with-coercion
                     (disjoin codomain-predicate function?)
                     (list
                      (function-extender codomain-arithmetic))
      (lambda (name function-constant)
        function-constant)
      (lambda (operator function-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (apply-operation function-operation
                             (map (lambda (thing)
                                    (if (function? thing)
                                        thing
                                        (lambda args thing)))
                                  things))))))))
