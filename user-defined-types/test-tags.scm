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

(define-test 'built-in-predicates
  (lambda ()
    (test-built-in-predicate exact-integer? 'exact-integer 0)
    (test-built-in-predicate exact-integer? 'exact-integer (expt 2 400))
    (test-built-in-predicate exact-rational? 'exact-rational 1/2)
    (test-built-in-predicate inexact-real? 'inexact-real 1.0)
    (test-built-in-predicate complex? 'complex 1+3i)
    (test-built-in-predicate symbol? 'symbol 'a)
    (test-built-in-predicate symbol? 'symbol (generate-uninterned-symbol "foo"))))

(define (test-built-in-predicate predicate name object)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv predicate (tag->predicate tag))
    (assert-equal name (predicate-name predicate))
    (assert-equal name (tag-name tag))
    (assert-eqv predicate (get-predicate object))
    (assert-eqv tag (get-tag object))
    (assert-eqv object (get-data object))))