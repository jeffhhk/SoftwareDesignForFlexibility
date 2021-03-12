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

(define (get-unit-operation operator base-operation)
  (let ((uniform
         (lambda (procedure)
           (simple-operation operator unit-conversion?
                             procedure))))
    (case operator
      ((*) (uniform unit:*))
      ((/) (uniform unit:/))
      ((invert) (uniform unit:invert))
      ((expt)
       (make-operation operator
                       (match-args unit-conversion?
                                   exact-integer?)
                       unit:expt))
      (else
       (uniform
        (lambda args
          (error "Undefined unit operator:" operator)))))))

(define (unit-arithmetic base-arithmetic)
  (make-arithmetic 'units
                   unit-conversion?
                   (list base-arithmetic)
                   (lambda (name base-constant)
                     (declare (ignore name base-constant))
                     identity-unit-conversion)
                   get-unit-operation))

(define-arith-test 'units-basic
  (lambda ()
    (let ((g (make-generic-arithmetic make-default-dispatch-store)))
      (add-to-generic-arithmetic! g numeric-arithmetic)
      (extend-generic-arithmetic! g unit-arithmetic)
      g))
  (lambda ()
    (assert-equal 3 (+ 1 2))
    (assert-error (lambda ()
                    (+ unit-conversion-1
                       fahrenheit-to-celsius)))
    (assert-equal 0
                  ((* fahrenheit-to-celsius
                      unit-conversion-1)
                   32))
    (assert-equal 32
                  ((/ (* fahrenheit-to-celsius
                         unit-conversion-1))
                   0))))

(define unit-conversion-1
  (make-unit-conversion (lambda (x) (* x 2))
                        (lambda (x) (/ x 2))))

(define-test 'units-gas-law-1
  (lambda ()
    (assert-close .02404848592336704 1e-4
                  (gas-law-volume (psi-to-nsm 14.7)
                                  ((compose celsius-to-kelvin
                                            fahrenheit-to-celsius)
                                   68)
                                  1))
    (assert-close 7.049624635839811 1e-4
                  ((unit:invert inch-to-meter)
                   (sphere-radius
                    (gas-law-volume (psi-to-nsm 14.7)
                                    ((compose celsius-to-kelvin
                                              fahrenheit-to-celsius)
                                     68)
                                    1))))
    (assert-close 1467.5286508533222 1e-4
                  ((unit:expt (unit:invert inch-to-meter) 3)
                   (gas-law-volume (psi-to-nsm 14.7)
                                   ((compose celsius-to-kelvin
                                             fahrenheit-to-celsius)
                                    68)
                                   1)))
    (assert-close 1467.5286508533222 1e-4
                  (let ((specializer
                         (unit-specializer gas-law-volume
                                           '(expt meter 3)
                                           '(/ newton (expt meter 2))
                                           'kelvin
                                           'mole)))
                    (let ((specialized
                           (specializer '(expt inch 3)
                                        '(/ pound (expt inch 2))
                                        'fahrenheit
                                        'mole)))
                      (specialized 14.7 68 1))))))

(define (gas-law-volume pressure temperature amount)
  (/ (* amount gas-constant temperature) pressure))

(define gas-constant 8.3144621)         ;J/(K*mol)

(define (sphere-radius volume)
  (expt (/ volume (* 4/3 pi)) 1/3))

(define pi (* 4 (atan 1 1)))

(define psi-to-nsm
  (compose pound-to-newton
           (unit:invert inch-to-meter)
           (unit:invert inch-to-meter)))
