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

(install-core-propagators!
 merge
 numeric-arithmetic
 simple-propagator-projector)
(initialize-scheduler)

(define-cell x)
(define-cell guess)
(define-cell better-guess)

(heron-step x guess better-guess)

(tell! x 2)
(tell! guess 1.4)

(cell-strongest better-guess)
'expect-value: 1.4142857142857141

(initialize-scheduler)

(define-cell x)
(define-cell answer)

(sqrt-network x answer)

(tell! x 2)

(cell-strongest answer)
'expect-value: 1.4142135623746899

(install-arithmetic! layered-arith)        ;debugging
(install-core-propagators! merge-layered
                           layered-arith
                           layered-propagator-projector)
(initialize-scheduler)

(define-cell x)
(define-cell guess)
(define-cell better-guess)

(heron-step x guess better-guess)

(tell! x 2 'gjs1)
(tell! guess 1.4 'gjs2)

(what-is-in better-guess)
'expect-write: '
;; cell: (better-guess)
(in 1.4142857142857141
    (gjs1 gjs2)
    ((p:/ heron-step) (g+x/g 2.8285714285714283) (two 2)))

(initialize-scheduler)

(define-cell x)
(define-cell answer)

(sqrt-network x answer)

(tell! x 2 'gjs1)

(what-is-in answer)
'expect-write: '
;; cell: (answer)
(in
 1.4142135623746899
 (gjs1)
 (in
  #t
  (gjs1)
  ((p:< good-enuf?
        sqrt-iter
        sqrt-iter
        sqrt-iter
        sqrt-iter
        sqrt-iter
        sqrt-network)
   (ax-g^2 4.510614104447086e-12)
   (eps .00000001)))
 (in
  1.4142135623746899
  (gjs1)
  ((p:/ heron-step
        sqrt-iter
        sqrt-iter
        sqrt-iter
        sqrt-iter
        sqrt-network)
   (g+x/g 2.8284271247493797)
   (two 2))))

(install-arithmetic! layered-arith)     ;debugging
;; coderef: install-layered-propagator-projector
(install-core-propagators! merge-value-sets
                           layered-arith
                           layered-propagator-projector)

(initialize-scheduler)

(define-cell x)
(define-cell guess)
(define-cell better-guess)

(heron-step x guess better-guess)

(tell! x 2 'gjs1)
(tell! guess 1.4 'gjs2)

(what-is-in better-guess)
'expect-write: '
;; cell: (better-guess)
(in 1.4142857142857141
    (gjs1 gjs2)
    ((p:/ heron-step) (g+x/g 2.8285714285714283) (two 2)))

(retract! 'gjs1)
(what-is-in better-guess)
'expect-write: '
;; cell: (better-guess)
(out 1.4142857142857141
     (gjs1 gjs2)
     ((p:/ heron-step) (g+x/g 2.8285714285714283) (two 2)))
;; strongest value:
'expect-write: '(the-nothing)

(assert! 'gjs1)
(what-is-in better-guess)
'expect-write: '
;; cell: (better-guess)
(in 1.4142857142857141
    (gjs1 gjs2)
    ((p:/ heron-step) (g+x/g 2.8285714285714283) (two 2)))

(tell! x 3 'gjs3)
'expect-write: '
;; contradiction:
((x)
 (has-value (the-contradiction))
 (depends-on gjs1 gjs3)
 (because
  ((has-value 2) (depends-on gjs1) (because i-told-you-so))
  ((has-value 3) (depends-on gjs3) (because i-told-you-so))))

(what-is-in x)
;; cell: (x)
'expect-write: '(in 3 (gjs3) i-told-you-so)
'expect-write: '(in 2 (gjs1) i-told-you-so)
'expect-write: '
;; strongest value:
(in (the-contradiction)
    (gjs1 gjs3)
    (in 2 (gjs1) i-told-you-so)
    (in 3 (gjs3) i-told-you-so))

(retract! 'gjs3)
(what-is-in x)
;; cell: (x)
'expect-write: '(out 3 (gjs3) i-told-you-so)
'expect-write: '(in 2 (gjs1) i-told-you-so)
;; strongest value:
'expect-write: '(in 2 (gjs1) i-told-you-so)

(what-is-in better-guess)
'expect-write: '
;; cell: (better-guess)
(in 1.4142857142857141
    (gjs1 gjs2)
    ((p:/ heron-step) (g+x/g 2.8285714285714283) (two 2)))

(initialize-scheduler)

(define answers (multiple-dwelling))
(run)
(map (lambda (cell)
       (get-base-value (cell-strongest cell)))
     answers)
'expect-value: '(3 2 4 5 1)

#|
;;; The following is too variable to test
*number-of-calls-to-fail*
'expect-value: 105
|#

#|
(let loop ()
  (run)
  (let ((result
         (map (lambda (cell)
                (get-base-value (cell-strongest cell)))
              answers)))
    (write-line result)
    (if (not (equal? result '(3 2 4 5 1)))
        (begin
          (alert-propagators! (all-propagators))
          (loop)))))
|#

(initialize-scheduler)
(define answers (killer))
(run)
(map (lambda (cell)
       (get-base-value (cell-content cell)))
     answers)
'expect-value: '(2 2 4)

(initialize-scheduler)
(define answers (pythagorean-1))
(run)
(map (lambda (cell)
       (get-base-value (cell-content cell)))
     answers)
'expect-value: '(3 4 5)
*number-of-calls-to-fail*
'expect-value: 124

(initialize-scheduler)
(define answers (pythagorean-2))
(run)
(map (lambda (cell)
       (get-base-value (cell-content cell)))
     answers)
'expect-value: '(3 4 5 9 16 25)
*number-of-calls-to-fail*
'expect-value: 172

(initialize-scheduler)
(define answers (pythagorean-3))
(run)
(map (lambda (cell)
       (get-base-value (cell-content cell)))
     answers)
'expect-value: '(4 3 5 4 3 5 16 9 25)
*number-of-calls-to-fail*
'expect-value: 160

(initialize-scheduler)
(define answers (pythagorean-4))
(run)
(map (lambda (cell)
       (get-base-value (cell-content cell)))
     answers)
'expect-value: '(4 3 5 4 3 5 16 9 25)
*number-of-calls-to-fail*
'expect-value: 160



