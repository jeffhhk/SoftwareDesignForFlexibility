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

(define-test 'generic-basic
  (lambda ()
    (define foo
      (simple-generic-procedure 'foo 0 #f))

    (define-generic-procedure-handler foo (match-args)
      (lambda ()
        'foobar))

    (assert-equal 'foobar (foo))))

(define-test 'generic-example1
  (lambda ()
    (define foo
      (simple-generic-procedure 'foo 2 #f))

    (define-generic-procedure-handler foo (match-args number? number?)
      (lambda (a b)
        (+ a b)))

    (define-generic-procedure-handler foo (any-arg 2 symbol? number?)
      (lambda (a b)
        (list '+ a b)))

    (assert-equal 3 (foo 1 2))
    (assert-equal '(+ 1 a) (foo 1 'a))
    (assert-equal '(+ a 2) (foo 'a 2))
    (assert-equal '(+ a b) (foo 'a 'b))))