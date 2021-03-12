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

(define-test 'rules
  (lambda ()
    (assert-equal '(+ (+ (* x y) (* x z)) (* w x))
                  (algebra-1 '(* (+ y (+ z w)) x)))

    (assert-equal '(+ (* w x) (* x y) (* x z))
                  (algebra-2 '(* (+ y (+ z w)) x)))

    (assert-equal '(* 3 x)
                  (algebra-2 '(+ (* 3 (+ x 1)) -3)))

    (assert-equal '(+ 3 a b c)
                  (algebra-2 '(+ (+ (+ a b) 1 2) c)))

    (assert-equal '(+ a b)
                  (algebra-2 '(+ 0 a b 0)))

    (assert-equal '(+ a b)
                  (algebra-2 '(+ (* 1 a) b)))

    (assert-equal '(+ (* a b d) (* a c d))
                  (algebra-2 '(* a (+ b c) d)))

    (assert-equal '(+ 6 a b c)
                  (algebra-2 '(+ a b c 1 2 3)))

    (assert-equal '(* 6 a b c)
                  (algebra-2 '(* 1 c 2 b 3 a)))

    (assert-equal '(+ (* a a b c)
                      (* a b w x)
                      (* a c y z)
                      (* w x y z))
                  (algebra-2
                   '(* (+ (* a b) (* y z)) (+ (* w x) (* a c)))))
    ))

(define-test 'symfib
  (lambda ()

    (define (symfib x)
      (if (< x 2)
          x
          `(+ ,(symfib (- x 1))
              ,(symfib (- x 2)))))

    (assert-equal 21
                  (algebra-2 (symfib 8)))))

(define-test 'symfact
  (lambda ()

    (define (symfact n)
      (letrec ((countup
                (lambda (m)
                  (if (> m 0)
                      `(+ ,(countup (- m 1)) 1)
                      0))))
            (if (> n 0)
                `(* ,(symfact (- n 1))
                    ,(countup n))
                1)))

    (assert-equal 120
                  (algebra-2 (symfact 5)))))
