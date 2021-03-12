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

(define test-arith
  (extend-arithmetic layered-extender
                     numeric-arithmetic))
(install-arithmetic! test-arith)


(pp (layered-datum 3 support-layer (support-set 'a)))
'expect-description:
'((base-layer 3)
  (support-layer (support-set a)))

(define one-half
  (layered-datum 1/2 support-layer (support-set 'Newton)))

(define (KE m v)
  (* one-half m (square v)))

(pp (KE (layered-datum 2 support-layer (support-set 'cph))
         (layered-datum 3 support-layer (support-set 'gjs))))
'expect-description:
'((base-layer 9)
  (support-layer (support-set gjs cph newton)))

(define (KE m v)
  (* 1/2 m (square v)))

(pp (KE (layered-datum 2 support-layer (support-set 'cph))
         (layered-datum 3 support-layer (support-set 'gjs))))
'expect-description:
'((base-layer 9)
  (support-layer (support-set gjs cph)))

;;; All together now!

(define (generic-symbolic)
  (let ((g (make-generic-arithmetic make-default-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (extend-generic-arithmetic! g symbolic-extender)
    g))

(define generic-with-layers
  (let ((g (generic-symbolic)))
    (extend-generic-arithmetic! g layered-extender)
    g))

(install-arithmetic! generic-with-layers)

(define (KE m v)
  (* 1/2 m (square v)))

;; coderef: ke-1
(pp (KE (layered-datum 'm
                        unit-layer (unit 'kilogram 1)
                        support-layer (support-set 'cph))
         (layered-datum 'v
                        unit-layer (unit 'meter 1 'second -1)
                        support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set gjs cph)))

;; coderef: ke-2
(pp (KE (layered-datum 0
                        unit-layer (unit 'kilogram 1)
                        support-layer (support-set 'jems))
         (layered-datum 'v
                        unit-layer (unit 'meter 1 'second -1)
                        support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* 0 (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set jems)))


(define one-half
  (layered-datum 1/2
                 unit-layer (unit)
                 support-layer (support-set 'Newton)))

(define (KE m v)
  (* one-half m (square v)))

(pp (KE (layered-datum 'm
                        unit-layer (unit 'kilogram 1)
                        support-layer (support-set 'cph))
         (layered-datum 'v
                        unit-layer (unit 'meter 1 'second -1)
                        support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set gjs cph newton)))

(pp (KE (layered-datum 0
                        unit-layer (unit 'kilogram 1)
                        support-layer (support-set 'jems))
         (layered-datum 'v
                        unit-layer (unit 'meter 1 'second -1)
                        support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* 0 (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set jems)))

(define (KE m v)
  (* 1/2 m (square v)))

(pp (KE (layered-datum 'm
                       unit-layer (unit 'kilogram 1)
                       support-layer (support-set 'cph))
        (layered-datum 'v
                       unit-layer (unit 'meter 1 'second -1)
                       support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set gjs cph)))

(pp (KE (layered-datum 0
                       unit-layer (unit 'kilogram 1)
                       support-layer (support-set 'jems))
        (layered-datum 5
                       unit-layer (unit 'meter 1 'second -1)
                       support-layer (support-set 'gjs))))
'expect-description:
'((base-layer 0)
  (unit-layer (unit kilogram 1 meter 2 second -2))
  (support-layer (support-set jems)))

(define layered-ke
  (make-layered-procedure 'ke 2 ke))

(pp
 (layered-ke (layered-datum 'm
                            unit-layer (unit 'kilogram 1)
                            support-layer (support-set 'cph))
             (layered-datum 'v
                            unit-layer (unit 'meter 1 'second -1)
                            support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (support-layer (support-set gjs cph)))

(pp
 (layered-ke (layered-datum 0
                            unit-layer (unit 'kilogram 1)
                            support-layer (support-set 'jems))
             (layered-datum 'v
                            unit-layer (unit 'meter 1 'second -1)
                            support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* 0 (square v)))
  (support-layer (support-set gjs jems)))

(define-layered-procedure-handler layered-ke support-layer
  (lambda (base-value m v)
    (declare (ignore base-value))
    (support-set-adjoin
     (support-set-union (support-layer-value m)
                        (support-layer-value v))
     'newton)))

(pp
 (layered-ke (layered-datum 'm
                            unit-layer (unit 'kilogram 1)
                            support-layer (support-set 'cph))
             (layered-datum 'v
                            unit-layer (unit 'meter 1 'second -1)
                            support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* (* 1/2 m) (square v)))
  (support-layer (support-set newton gjs cph)))

(pp
 (layered-ke (layered-datum 0
                            unit-layer (unit 'kilogram 1)
                            support-layer (support-set 'jems))
             (layered-datum 'v
                            unit-layer (unit 'meter 1 'second -1)
                            support-layer (support-set 'gjs))))
'expect-description:
'((base-layer (* 0 (square v)))
  (support-layer (support-set newton gjs jems)))

(define (F m1 m2 r)
  (/ (* G m1 m2)
     (square r)))

(define G
  (layered-datum 6.67408e-11
    unit-layer (unit 'meter 3 'kilogram -1 'second -2)
    support-layer (support-set 'CODATA-2018)))

(define M-Earth
  (layered-datum 5.9722e24
                 unit-layer (unit 'kilogram 1)
                 support-layer
                 (support-set 'Astronomical-Almanac-2016)))

(define M-Moon
  (layered-datum 7.342e22
                 unit-layer (unit 'kilogram 1)
                 support-layer (support-set 'NASA-2006)))

(define a-Moon
  (layered-datum 384399e3
                 unit-layer (unit 'meter 1)
                 support-layer (support-set 'Wieczorek-2006)))

(pp (F M-earth M-Moon a-moon))
'expect-description:
'((base-layer 1.9805035857209e20)
  (unit-layer (unit kilogram 1 meter 1 second -2))
  (support-layer
   (support-set Wieczorek-2006
                NASA-2006
                Astronomical-Almanac-2016
                CODATA-2018)))