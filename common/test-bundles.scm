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

(define-test 'empty
  (lambda ()
    (define foo? (make-bundle-predicate 'foo))
    (define foo (bundle foo?))

    (assert-true (foo? foo))

    (assert-equal '() (bundle-names foo))

    (assert-false (bundle-ref foo 'operator1 #f))

    (assert-error (lambda () (bundle-ref foo 'operator1)))

    (assert-equal '() (bundle-alist foo))
    ))

(define-test 'simple
  (lambda ()

    (define (operator0) 2)
    (define (operator1 a) (+ a 3))
    (define (operator2 a b) (* a b))

    (define foo? (make-bundle-predicate 'foo))
    (define foo (bundle foo? operator0 operator1 operator2))

    (assert-true (foo? foo))

    (assert-lset= eq?
                  '(operator0 operator1 operator2)
                  (bundle-names foo))

    (assert-eqv operator0 (bundle-ref foo 'operator0))
    (assert-eqv operator1 (bundle-ref foo 'operator1))
    (assert-eqv operator2 (bundle-ref foo 'operator2))
    (assert-error (lambda () (bundle-ref foo 'operator3)))

    (assert-lset= equal?
                  (list (cons 'operator0 operator0)
                        (cons 'operator1 operator1)
                        (cons 'operator2 operator2))
                  (bundle-alist foo))

    (assert-eqv 2 (foo 'operator0))
    (assert-eqv 8 (foo 'operator1 5))
    (assert-eqv 10 (foo 'operator1 7))
    (assert-eqv 6 (foo 'operator2 2 3))
    (assert-eqv 35 (foo 'operator2 5 7))
    (assert-error (lambda () (foo 'operator3)))
    (assert-error (lambda () (foo 'operator3 2)))
    (assert-error (lambda () (foo 'operator3 3 5)))
    ))