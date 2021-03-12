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

(define numeric+symbolic
  (make-symbolic-arithmetic numeric-arithmetic))

(define numeric+functional
  (make-endo-function-arithmetic numeric-arithmetic))

(define numeric+symbolic+functional
  (make-endo-function-arithmetic numeric+symbolic))

(define literal-number?
  (make-symbolic-predicate number?))

(define literal-number
  (predicate-constructor literal-number?))

(define literal-function?
  (make-function-predicate (list (arithmetic-domain-predicate numeric+symbolic))
                           literal-number?))

(define (literal-function expr)
  (make-simple-function expr
                        literal-function?
                        (lambda args
                          (literal-number (cons expr args)))))

(define (assert-literal-number expected-expr value)
  (assert-equal `(tagged-data (symbolic number) ,expected-expr)
                (rewrite-tags value)))

(define function-over-number?
  (make-endo-function-predicate number?))

(define-arith-test 'mixed-numeric-function
  (lambda ()
    numeric+functional)
  (lambda ()
    (assert-equal -.8488724885405782 ((+ cos sin) 3))
    (assert-equal -1.6977449770811563 (* 2 ((+ cos sin) 3)))
    (assert-equal .3022550229188436 (* 2 ((+ 1 cos sin) 3)))))

(define-arith-test 'mixed-symbolic-function
  (lambda ()
    numeric+symbolic+functional)
  (lambda ()
    (assert-equal -.6536436208636119 (cos 4))
    (assert-literal-number '(f 4)
                           ((literal-function 'f) 4))
    (assert-literal-number '(+ -.6536436208636119 (f 4))
                           ((+ cos (literal-function 'f)) 4))
    (assert-literal-number '(+ (cos a) (f a))
                           ((+ cos (literal-function 'f)) (literal-number 'a)))
    (assert-literal-number '(* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))
                           (* (literal-number 'b)
                              ((+ 4 cos sin) (+ 3 (literal-number 'a)))))
    (assert-literal-number '(+ a (+ (cos b) (sin b)))
                           (+ (literal-number 'a)
                              ((+ cos sin) (literal-number 'b))))
    (assert-literal-number '(+ a -.8488724885405782)
                           (+ (literal-number 'a)
                              ((+ cos sin) 3)))
    (assert-literal-number '(+ a (+ (+ 3 (cos b)) (sin b)))
                           (+ (literal-number 'a)
                              ((+ 3 cos sin) (literal-number 'b))))
    (assert-literal-number '(+ a (+ (+ c (cos b)) (sin b)))
                           (+ (literal-number 'a)
                              ((+ (literal-number 'c) cos sin) (literal-number 'b))))
    (assert-literal-number '(+ a (+ (+ (c b) (cos b)) (sin b)))
                           (+ (literal-number 'a)
                              ((+ (literal-function 'c) cos sin) (literal-number 'b))))))

#|
;; Most powerful but has issues.  Doesn't work at present.

(define-arith-test 'closed-function
  (lambda ()
    (extend-and-close-arithmetic numeric+symbolic
                                 function-extender))
  (lambda ()
    (assert-equal '(+ a (+ 3 (cos (sin b))))
                  (+ 'a ((+ 3 (cos sin)) 'b)))
    (assert-equal '(+ (3 . 4) (4 . 3))
                  (((+ (lambda (x) (lambda (y) (cons x y)))
                       (lambda (x) (lambda (y) (cons y x))))
                    3)
                   4))
    ;; Works when extend-and-close-environment prioritizes the closure
    ;; over the base, fails otherwise.
    (assert-equal '(+ a (+ c (cos b) (sin b)))
                  (+ 'a ((+ 'c cos sin) 'b)))))
|#

(define-arith-test 'combined-flat
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric+symbolic)
      g))
  (lambda ()
    (assert-eqv 0 (+))
    (combined)))

(define (combined)
  ;; these tests require mixed numeric/symbolic:
  (assert-equal 6 (+ 1 2 3))
  (assert-literal-number '(+ (+ 1 a) 3) (+ 1 (literal-number 'a) 3)))

(define-arith-test 'combined-plus-function-over-numeric-flat-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric+symbolic)
      (add-to-generic-arithmetic! g numeric+functional)
      g))
  (lambda ()
    (assert-eqv 0 (+))
    (combined-plus-function-over-numeric function-over-number?)))

(define-arith-test 'combined-plus-function-over-numeric-flat-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric+functional)
      (add-to-generic-arithmetic! g numeric+symbolic)
      g))
  (lambda ()
    (assert-eqv 0 (+))
    (combined-plus-function-over-numeric function-over-number?)))

(define (combined-plus-function-over-numeric function?)
  (combined)
  ;; this test requires numeric, symbolic over numeric, and function
  ;; over numeric:
  (assert-literal-number '(+ a -.8488724885405782)
                       (+ (literal-number 'a)
                          ((+ (make-simple-function 'cos function? cos)
                              (make-simple-function 'sin function? sin))
                           3))))

(define (combined-plus-function-over-combined function?)
  (combined-plus-function-over-numeric function?)
  ;; These tests require symbolic over numeric, and function over both
  ;; symbolic and numeric:
  (assert-literal-number
   '(* b (+ (+ 4 (cos (+ 3 a))) (sin (+ 3 a))))
   (* (literal-number 'b)
      ((+ 4
          (make-simple-function 'cos function? cos)
          (make-simple-function 'sin function? sin))
       (+ 3 (literal-number 'a)))))
  (assert-literal-number
   '(+ a (+ (cos b) (sin b)))
   (+ (literal-number 'a)
      ((+ (make-simple-function 'cos function? cos) (make-simple-function 'sin function? sin))
       (literal-number 'b))))
  (assert-literal-number
   '(+ a (+ (+ 3 (cos b)) (sin b)))
   (+ (literal-number 'a)
      ((+ 3 (make-simple-function 'cos function? cos) (make-simple-function 'sin function? sin))
       (literal-number 'b))))
  (assert-literal-number
   '(+ a (+ 3 (cos (sin b))))
   (+ (literal-number 'a)
      ((+ 3 (cos (make-simple-function 'sin function? sin))) (literal-number 'b))))
  (literal-function-tests function?))

(define (literal-function-tests function?)
  ;; Tests that a trivial literal function is sufficient to mark named
  ;; functions.
  (assert-literal-number
   '(+ a (+ (+ (c b) (cos b)) (sin b)))
   (+ (literal-number 'a)
      ((+ (literal-function function? 'c)
          (make-simple-function 'cos function? cos)
          (make-simple-function 'sin function? sin))
       (literal-number 'b))))
  (assert-literal-number
   '(+ (+ (* 2 xt) (* -1 xt-h))
       (* (/ (expt h 2) 12)
          (+ (+ (* 13 (f t xt))
                (* -2 (f (- t h) xt-h)))
             (f (- t (* 2 h))
                xt-2h))))
   (x 0
      ((evolver (literal-function function? 'F)
                (literal-number 'h)
                stormer-2)
       (make-initial-history (literal-number 't)
                             (literal-number 'h)
                             (literal-number 'xt)
                             (literal-number 'xt-h)
                             (literal-number 'xt-2h))
       1))))

(define-arith-test 'combined-plus-function-over-combined-flat-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric+symbolic)
      (add-to-generic-arithmetic! g (make-endo-function-arithmetic g))
      g))
  (lambda ()
    (assert-eqv 0 (+))
    (combined-plus-function-over-combined-flat
     (make-function-predicate (list any-object?)
                              any-object?))))

(define-arith-test 'combined-plus-function-over-combined-flat-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g (make-endo-function-arithmetic g))
      (add-to-generic-arithmetic! g numeric+symbolic)
      g))
  (lambda ()
    (assert-eqv 0 (+))
    (combined-plus-function-over-combined-flat
     (make-function-predicate (list any-object?)
                              any-object?))))

(define (combined-plus-function-over-combined-flat function?)
  (lambda ()
    (combined-plus-function-over-combined function?)
    ;; Works regardless of order because symbols represent only numbers:
    (assert-literal-number
     '(+ a (+ (+ c (cos b)) (sin b)))
     (+ (literal-number 'a)
        ((+ (literal-number 'c) cos sin) (literal-number 'b))))
    (assert-literal-number
     '(+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))
     (+ (literal-number 'a)
        ((+ (literal-number 'c) cos sin)
         (* 2 (literal-number 'b)))))
    (literal-function-tests)))

#|
;;; Temporarily disabled because extensions over generic arithmetic
;;; need predicate algebra to match properly.

(define-arith-test 'closed-prefer-function
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g (make-symbolic-arithmetic g))
      (add-to-generic-arithmetic! g (make-endo-function-arithmetic g))
      g))
  (lambda ()
    (closed (make-function-predicate (list any-object?) any-object?))
    ;; Ambiguous whether c is a literal function or a literal number.
    ;; Works with this precedence of function over symbolic.
    (assert-literal-number
     '(+ a (+ (+ c (cos b)) (sin b)))
     (+ (literal-number 'a)
        ((+ (literal-number 'c) cos sin) (literal-number 'b))))
    (assert-literal-number
     '(+ a (+ (+ c (cos (* 2 b))) (sin (* 2 b))))
     (+ (literal-number 'a)
        ((+ (literal-number 'c) cos sin)
         (* 2 (literal-number 'b)))))))

(define-arith-test 'closed-prefer-symbolic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (add-to-generic-arithmetic! g (make-endo-function-arithmetic g))
      (add-to-generic-arithmetic! g (make-symbolic-arithmetic g))
      g))
  (lambda ()
    (closed (make-function-predicate (list any-object?) any-object?))
    ;; Ambiguous whether c is a literal function or a literal number.
    ;; Doesn't work with this precedence of symbolic over function.
    (assert-error
     (lambda ()
       (+ (literal-number 'a)
          ((+ (literal-number 'c) cos sin) (literal-number 'b)))))))

(define (closed function?)
  (combined-plus-function-over-combined function?)
  ;; This test requires function over function (closure):
  ;; This (might) work *only* because FUNCTION? is function over anything.
  (assert-equal '(+ (3 . 4) (4 . 3))
                (((+ (make-simple-function #f
                                    function?
                                    (lambda (x)
                                      (make-simple-function #f
                                                     function?
                                                     (lambda (y) (cons x y)))))
                     (make-simple-function #f
                                    function?
                                    (lambda (x)
                                      (make-simple-function #f
                                                     function?
                                                     (lambda (y) (cons y x))))))
                  3)
                 4)))
|#