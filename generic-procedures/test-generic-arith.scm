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

;; This tests a generic arithmetic that has symbolic operations, but
;; not purely numerical operations.
(define-arith-test 'generic+symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g
        (symbolic-extender numeric-arithmetic))
      g))
  (lambda ()
    ;; Symbolic arithmetic needs at least one argument.
    (assert-eqv 0 (+))
    ;; Symbolic arithmetic needs at least one symbolic argument.
    (assert-error (lambda () (+ 1 2 3)))
    (assert-equal '(+ (+ 1 a) 3) (+ 1 'a 3))))

;; This tests a generic arithmetic that has symbolic operations, but
;; not purely numerical operations.
(define-arith-test 'generic++numeric+symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g
        (symbolic-extender numeric-arithmetic))
      g))
  (lambda ()
    (assert-equal 0 (+))
    (assert-equal '(+ (+ 1 a) 3) (+ 1 'a 3))))

(define-arith-test 'combined
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g combined-arithmetic)
      g))
  (lambda ()
    (combined)))

(define-arith-test 'combined-flat
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g
        (symbolic-extender numeric-arithmetic))
      g))
  (lambda ()
    (combined)))

(define (combined)
  ;; these tests require mixed numeric/symbolic:
  (assert-equal 0 (+))
  (assert-equal 6 (+ 1 2 3))
  (assert-equal '(+ (+ 1 a) 3) (+ 1 'a 3)))

(define-arith-test 'combined-plus-function-over-numeric
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g combined-arithmetic)
      (add-to-generic-arithmetic! g
        (function-extender numeric-arithmetic))
      g))
  (lambda ()
    (combined-plus-function-over-numeric)))

(define-arith-test 'combined-plus-function-over-numeric-flat-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g
        (symbolic-extender numeric-arithmetic))
      (add-to-generic-arithmetic! g
        (function-extender numeric-arithmetic))
      g))
  (lambda ()
    (combined-plus-function-over-numeric)))

(define-arith-test 'combined-plus-function-over-numeric-flat-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g
        (function-extender numeric-arithmetic))
      (add-to-generic-arithmetic! g
        (symbolic-extender numeric-arithmetic))
      g))
  (lambda ()
    (combined-plus-function-over-numeric)))

(define (combined-plus-function-over-numeric)
  (combined)
  ;; this test requires numeric, symbolic over numeric, and function
  ;; over numeric:
  (assert-equal '(+ a -.8488724885405782)
                (+ 'a ((+ cos sin) 3))))

(define-arith-test 'combined-plus-function-over-combined
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g combined-arithmetic)
      (add-to-generic-arithmetic! g
                                  (function-extender combined-arithmetic))
      g))
  (lambda ()
    (combined-plus-function-over-combined)))

(define (combined-plus-function-over-combined)
  (combined-plus-function-over-numeric)
  ;; These tests require symbolic over numeric, and function over both
  ;; symbolic and numeric:
  (assert-equal '(* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))
                (* 'b ((+ 4 cos sin) (+ 3 'a))))
  (assert-equal '(+ a (+ (cos b) (sin b)))
                (+ 'a ((+ cos sin) 'b)))
  (assert-equal '(+ a (+ (+ 3 (cos b)) (sin b)))
                (+ 'a ((+ 3 cos sin) 'b)))
  (assert-equal '(+ a (+ 3 (cos (sin b))))
                (+ 'a ((+ 3 (cos sin)) 'b)))
  (literal-function-tests))

(define (literal-function-tests)
  ;; Tests that a trivial literal function is sufficient to mark named
  ;; functions.
  (assert-equal '(+ a (+ (+ (c b) (cos b)) (sin b)))
                (+ 'a ((+ (literal-function 'c) cos sin) 'b)))
  (assert-equal '(+ (+ (* 2 xt) (* -1 xt-h))
                    (* (/ (expt h 2) 12)
                       (+ (+ (* 13 (f t xt))
                             (* -2 (f (- t h) xt-h)))
                          (f (- t (* 2 h))
                             xt-2h))))
                (x 0
                   ((evolver (literal-function 'F)
                             'h
                             stormer-2)
                    (make-initial-history 't 'h 'xt 'xt-h 'xt-2h)
                    1))))

(define-arith-test 'combined-plus-function-over-combined-flat-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g
                                  (symbolic-extender numeric-arithmetic))
      (extend-generic-arithmetic! g function-extender)
      g))
  (lambda ()
    (combined-plus-function-over-combined-flat)))

(define-arith-test 'combined-plus-function-over-combined-flat-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (extend-generic-arithmetic! g function-extender)
      (add-to-generic-arithmetic! g
                                  (symbolic-extender numeric-arithmetic))
      g))
  (lambda ()
    (combined-plus-function-over-combined-flat)))

(define (combined-plus-function-over-combined-flat)
  (lambda ()
    (combined-plus-function-over-combined)
    ;; Works regardless of order because symbols represent only numbers:
    (assert-equal '(+ a (+ (+ c (cos b)) (sin b)))
                  (+ 'a ((+ 'c cos sin) 'b)))
    (assert-equal '(+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))
                  (+ 'a ((+ 'c cos sin) (* 2 'b))))
    (literal-function-tests)))

(define-arith-test 'closed-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (extend-generic-arithmetic! g symbolic-extender)
      (extend-generic-arithmetic! g function-extender)
      g))
  (lambda ()
    (closed)
    ;; Ambiguous whether c is a literal function or a literal number.
    ;; Works with this precedence of function over symbolic.
    (assert-equal '(+ a (+ (+ c (cos b)) (sin b)))
                  (+ 'a ((+ 'c cos sin) 'b)))
    (assert-equal '(+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))
                  (+ 'a ((+ 'c cos sin) (* 2 'b))))))

(define-arith-test 'closed-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (extend-generic-arithmetic! g function-extender)
      (extend-generic-arithmetic! g symbolic-extender)
      g))
  (lambda ()
    (closed)
    ;; Ambiguous whether c is a literal function or a literal number.
    ;; Doesn't work with this precedence of symbolic over function.
    (assert-error (lambda () (+ 'a ((+ 'c cos sin) 'b))))))

(define (closed)
  (combined-plus-function-over-combined)
  ;; This test requires function over function (closure):
  (assert-equal '(+ (3 . 4) (4 . 3))
                (((+ (lambda (x) (lambda (y) (cons x y)))
                     (lambda (x) (lambda (y) (cons y x))))
                  3)
                 4)))