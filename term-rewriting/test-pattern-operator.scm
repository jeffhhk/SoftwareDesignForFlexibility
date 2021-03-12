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

(define-test 'factorial-1
  (lambda ()

    (define factorial
      (make-pattern-operator
       (rule '(0) 1)
       (rule `((? n ,positive?))
             (* n (factorial (- n 1))))))

    (assert-equal (factorial 10)
                  3628800)))

(define-test 'factorial-2
  (lambda ()
    (define factorial (make-pattern-operator))

    (attach-rule! factorial (rule '(0) 1))

    (attach-rule! factorial
     (rule `((? n ,positive?))
           (* n (factorial (- n 1)))))

    (assert-equal (factorial 10)
                  3628800)))

(define-test 'quad
  (lambda ()

    (define quad
      (make-pattern-operator
       (rule
        `((? a) (? b) (? c) (? x))
        (+ (* a (expt x 2))
           (* b x)
           c))

       (rule
        `((? a) (? x) (? x) + (? b) (? x) + (? c))
        (+ (* a (expt x 2))
           (* b x)
           c))))

    (assert-equal (quad 1 2 3 4) 27)
    (assert-equal (quad 1 4 4 '+ 2 4 '+ 3) 27)))

(define-test 'frob
  (lambda ()

    (define frob
      (make-pattern-operator))

    (attach-rule! frob
     (rule
      '(a (?? x) (?? y) (?? x) c)
      (and (<= (length y) 2)
           y)))

    (assert-equal (apply frob '(a b b b b b b c))
                  '(b b))))