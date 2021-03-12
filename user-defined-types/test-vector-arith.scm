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

(define numeric+vector
  (make-vector-arithmetic numeric-arithmetic))

(define numeric+symbolic
  (make-symbolic-arithmetic numeric-arithmetic))

(define numeric+symbolic+vector
  (make-vector-arithmetic numeric+symbolic))

(define numeric-vector
  (vector-constructor number?))

(define literal-number
  (symbolic-constructor number?))

(define literal-vector
  (vector-constructor (arithmetic-domain-predicate numeric+symbolic)))

(define-arith-test 'numeric
  (lambda () numeric+vector)
  (lambda ()
    (let ((assert= (make-equality-asserter =)))
      (assert= (numeric-vector 5 7 9)
               (+ (numeric-vector 1 2 3)
                  (numeric-vector 4 5 6)))

      (assert= (numeric-vector 2 2 2)
               (- (numeric-vector 4 5 6)
                  (numeric-vector 1 1 1)
                  (numeric-vector 1 2 3)))
      (assert= 14
               (* (numeric-vector 1 2 3)
                  (numeric-vector 1 2 3)))
      (assert= (numeric-vector 3 6 9)
               (* 3 (numeric-vector 1 2 3)))
      (assert= (numeric-vector 3 6 9)
               (* (numeric-vector 1 2 3) 3))
      (assert= 5
               (magnitude (numeric-vector 3 4)))
      )))

(define-arith-test 'symbolic
  (lambda () numeric+symbolic+vector)
  (lambda ()
    (let ((assert=
           (lambda (expected actual)
             (assert-equal expected (rewrite-tags actual)))))
      (assert= '(tagged-data (vector (disjoin number (symbolic number)))
                             #(5 (+ 2 a) 9))
               (+ (literal-vector 1 2 3)
                  (literal-vector 4 (literal-number 'a) 6)))
      (assert= '(tagged-data (symbolic number)
                             (sqrt (+ 5 (* a a))))
               (magnitude (literal-vector 1 2 (literal-number 'a))))
      )))

#|
((magnitude (vector square square)) 'a)
;Value: (sqrt (+ (* (* a a) (* a a)) (* (* a a) (* a a))))

;;; Stormer works!

(define ss0
  (make-initial-history 0 .01
                        (vector (sin 0) (sin 0))
                        (vector (sin -.01) (sin -.01))
                        (vector (sin -.02) (sin -.02))))


(define (F t x) (- x))

(x 0 ((evolver F .01 stormer-2) ss0 100))
;Value: #(.8414709493275624 .8414709493275624)
|#