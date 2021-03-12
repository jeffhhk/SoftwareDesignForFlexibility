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

;;; x'' = -x

(define (F t x) (- x))

(define numeric-s0
  (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))

(define (make-symbolic-s0)
  (make-initial-history 't 'h 'xt 'xt-h 'xt-2h))

(define-arith-test 'stormer-numeric-arithmetic
  (lambda ()
    numeric-arithmetic)
  (lambda ()
    (numerical-tests)))

(define (numerical-tests)
  (assert-equal .8414709493275624
                (x 0 ((evolver F .01 stormer-2) numeric-s0 100))))

(define-arith-test 'stormer-symbolic
  (lambda ()
    (symbolic-extender numeric-arithmetic))
  (lambda ()
    (pure-symbolic-tests)))

(define (pure-symbolic-tests)
  (assert-equal '(+ (+ (* 2 0)
                       (* -1 -9.999833334166664e-3))
                    (* (/ (expt .01 2) 12)
                       (+ (+ (* 13 (negate 0))
                             (* -2 (negate -9.999833334166664e-3)))
                          (negate -.01999866669333308))))
                (x 0
                   ((evolver F .01 stormer-2)
                    numeric-s0
                    1)))
  (assert-equal '(+ (+ (* 2 xt)
                       (* -1 xt-h))
                    (* (/ (expt h 2) 12)
                       (+ (+ (* 13 (negate xt))
                             (* -2 (negate xt-h)))
                          (negate xt-2h))))
                (x 0
                   ((evolver F 'h stormer-2)
                    (make-symbolic-s0)
                    1))))

(define-arith-test 'stormer-combined
  (lambda ()
    (extend-arithmetic symbolic-extender
                       numeric-arithmetic))
  (lambda ()
    (stormer-combined-tests)))

(define stormer-combined-tests
  (lambda ()
    (assert-equal 0 (+))
    (assert-equal 6 (+ 1 2 3))
    (assert-equal '(+ (+ 1 a) 3) (+ 1 'a 3))
    (assert-equal .8414709493275624
                  (x 0
                     ((evolver F .01 stormer-2)
                      numeric-s0
                      100)))
    (assert-equal '(+ (+ (* 2 xt)
                         (* -1 xt-h))
                      (* (/ (expt h 2) 12)
                         (+ (+ (* 13 (negate xt))
                               (* -2 (negate xt-h)))
                            (negate xt-2h))))
                  (x 0
                     ((evolver F 'h stormer-2)
                      (make-symbolic-s0)
                      1)))
    (assert-equal '(+ (+ 8 (* -1 xt-h))
                      (* (/ (expt h 2) 12)
                         (+ (+ -52 (* -2 (negate xt-h)))
                            (negate xt-2h))))
                  (x 0
                     ((evolver F 'h stormer-2)
                      (make-initial-history 't 'h 4 'xt-h 'xt-2h)
                      1)))
    (assert-equal '(+ 9.999833334166664e-3
                      (* (/ (expt h 2) 12)
                         -9.999750002487318e-7))
                  (x 0
                     ((evolver F 'h stormer-2)
                      numeric-s0
                      1)))))