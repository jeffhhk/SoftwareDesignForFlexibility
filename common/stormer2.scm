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

(define (make-initial-history t h xt xt-h xt-2h)
  (list (cons t xt)
        (cons (- t h) xt-h)
        (cons (- t (* 2 h)) xt-2h)))

(define (extend-history t+h xt+h history)
  (cons (cons t+h xt+h) history))

(define (t index history)
  (car (list-ref history index)))

(define (x index history)
  (cdr (list-ref history index)))

(define (stormer-2 F h)
  (lambda (history)
    (+ (* 2 (x 0 history))
       (* -1 (x 1 history))
       (* (/ (expt h 2) 12)
          (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))))))

(define (stepper h integrator)
  (lambda (history)
    (extend-history (+ (t 0 history) h)
                    (integrator history)
                    history)))

(define (evolver F h make-integrator)
  (let ((integrator (make-integrator F h)))
    (let ((step (stepper h integrator)))
      (define (evolve history n-steps)
        (if (n:> n-steps 0)
            (evolve (step history) (n:- n-steps 1))
            history))
      evolve)))

#|
;;; Here are some simple examples.

(install-arithmetic! numeric-arithmetic)

;;; x'' = -x
(define (F t x) (- x))

(define s0
  (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))

(x 0 ((evolver F .01 stormer-2) s0 100))
;Value: .8414709493275624

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(install-arithmetic! combined-arithmetic)

(+)
;Value: 0

(+ 1 2 3)
;Value: 6

(+ 1 'a 3)
;Value: (+ (+ 1 a) 3)

(x 0
   ((evolver F .01 stormer-2)
    s0 100))
;Value: .8414709493275624

(pp (x 0
       ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 'xt 'xt-h 'xt-2h)
        1)))
#|
(+ (+ (* 2 xt)
      (* -1 xt-h))
   (* (/ (expt h 2) 12)
      (+ (+ (* 13 (negate xt))
            (* -2 (negate xt-h)))
         (negate xt-2h))))
|#

(pp (x 0
       ((evolver F 'h stormer-2)
        (make-initial-history 't 'h 4 'xt-h 'xt-2h)
        1)))
#|
(+ (+ 8 (* -1 xt-h))
   (* (/ (expt h 2) 12)
      (+ (+ -52 (* -2 (negate xt-h)))
         (negate xt-2h))))
|#

(pp (x 0
       ((evolver F 'h stormer-2)
        s0
        1)))
#|
(+ 9.999833334166664e-3
   (* (/ (expt h 2) 12)
      -9.999750002487318e-7))
|#
|#
