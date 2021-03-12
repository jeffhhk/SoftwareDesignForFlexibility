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

(define (heron-step x g h)
  (compound-propagator (list x g)       ;inputs
                       (list h)         ;outputs
    (lambda ()                          ;how to build
      (let-cells (x/g g+x/g (two 2))
        (p:/ x g x/g)
        (p:+ g x/g g+x/g)
        (p:/ g+x/g two h)))
    ;; given name for the compound propagator
    'heron-step))

(define (sqrt-network x answer)
  (compound-propagator (list x) (list answer)
    (lambda ()
      (let-cells ((one 1.))
        (sqrt-iter x one answer)))
    'sqrt-network))

(define (sqrt-iter x g answer)
  (compound-propagator (list x g) (list answer)
    (lambda ()
      (let-cells (done
                  not-done
                  x-if-not-done
                  g-if-not-done
                  new-g
                  ;;better-answer
                  )
        (good-enuf? g x done)
        (p:spst-switch done g answer)
        ;;(p:spdt-switch done g better-answer answer)
        (p:not done not-done)
        (p:spst-switch not-done x x-if-not-done)
        (p:spst-switch not-done g g-if-not-done)
        (heron-step x-if-not-done g-if-not-done new-g)
        (sqrt-iter x-if-not-done new-g answer)
        ;;(sqrt-iter x-if-not-done new-g better-answer)
        ))
    'sqrt-iter))

(define (good-enuf? g x done)
  (compound-propagator (list g x) (list done)
    (lambda ()
      (let-cells (g^2 (eps 0.00000001) x-g^2 ax-g^2)
        (p:* g g g^2)
        (p:- x g^2 x-g^2)
        (p:abs x-g^2 ax-g^2)
        (p:< ax-g^2 eps done)))
    'good-enuf?))

(define layered-arith
  (extend-arithmetic layered-extender
                     numeric-arithmetic))

;;; From scmutils
(define :2pi (n:* 8 (n:atan 1)))
(define :pi (n:* 4 (n:atan 1)))
(define :pi/2 (n:* 2 (n:atan 1)))
(define :-pi/2 (n:- :pi/2))

#|
;; Using binary-amb.

(define (multiple-dwelling)
  (let-cells (baker cooper fletcher miller smith)
    (let ((floors '(1 2 3 4 5)))
      (one-of floors baker)       (one-of floors cooper)
      (one-of floors fletcher)    (one-of floors miller)
      (one-of floors smith)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells (b=5        c=1   f=5    f=1
                  m>c        sf    fc     (one 1)
                  (five 5)   s-f   as-f   f-c
                  af-c)
        (p:= five baker b=5)       (abhor b=5)
        (p:= one cooper c=1)       (abhor c=1)
        (p:= five fletcher f=5)    (abhor f=5)
        (p:= one fletcher f=1)     (abhor f=1)
        (p:> miller cooper m>c)    (require m>c)
        (c:+ fletcher s-f smith)
        (c:abs s-f as-f)
        (p:= one as-f sf)          (abhor sf)
        (c:+ cooper f-c fletcher)
        (c:abs f-c af-c)
        (p:= one af-c fc)          (abhor fc)
        (list baker cooper fletcher miller smith)))))
|#
#|
;; Alternative with p:amb.

(define (multiple-dwelling)
  (let-cells (baker cooper fletcher miller smith)
    (let ((floors '(1 2 3 4 5)))
      (p:amb baker floors)
      (p:amb cooper floors)
      (p:amb fletcher floors)
      (p:amb miller floors)
      (p:amb smith floors)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells (b=5        c=1   f=5    f=1
                  m>c        sf    fc     (one 1)
                  (five 5)   s-f   as-f   f-c
                  af-c)
        (p:= five baker b=5)       (abhor b=5)
        (p:= one cooper c=1)       (abhor c=1)
        (p:= five fletcher f=5)    (abhor f=5)
        (p:= one fletcher f=1)     (abhor f=1)
        (p:> miller cooper m>c)    (require m>c)
        (c:+ fletcher s-f smith)
        (c:abs s-f as-f)
        (p:= one as-f sf)          (abhor sf)
        (c:+ cooper f-c fletcher)
        (c:abs f-c af-c)
        (p:= one af-c fc)          (abhor fc)
        (list baker cooper fletcher miller smith)))))
|#

;;; Alternative, flushing abhor and require
;;; (more excess constants!)

(define (multiple-dwelling)
  (let-cells (baker cooper fletcher miller smith)
    (let ((floors '(1 2 3 4 5)))
      (p:amb baker floors)
      (p:amb cooper floors)
      (p:amb fletcher floors)
      (p:amb miller floors)
      (p:amb smith floors)
      (require-distinct
       (list baker cooper fletcher miller smith))
      (let-cells ((b=5 #f)   (c=1 #f)   (f=5 #f)
                  (f=1 #f)   (m>c #t)   (sf #f)
                  (fc  #f)   (one 1)    (five 5)
                  s-f   as-f   f-c    af-c)
        (p:= five baker b=5)
        (p:= one cooper c=1)
        (p:= five fletcher f=5)
        (p:= one fletcher f=1)
        (p:> miller cooper m>c)
        (c:+ fletcher s-f smith)
        (c:abs s-f as-f)
        (p:= one as-f sf)
        (c:+ cooper f-c fletcher)
        (c:abs f-c af-c)
        (p:= one af-c fc)
        (list baker cooper fletcher miller smith)))))

(define (killer)
  (let-cells (x y v)
    (p:amb x '(1 2 3 4 5))
    (p:amb y '(1 2 3 4 5))
    (p:* x y v)
    (p:+ x y v)
    (list x y v)))

(define (pythagorean-1)
  (let-cells (x y z x2 y2 z2)
    (p:amb x '(1 2 3 4 5))
    (p:amb y '(1 2 3 4 5))
    (p:amb z '(1 2 3 4 5))
    (p:* x x x2)
    (p:* y y y2)
    (p:* z z z2)
    (p:+ x2 y2 z2)
    (list x y z)))

(define (pythagorean-2)
  (let-cells (x y z x2 y2 z2)
    (p:amb x '(1 2 3 4 5))
    (p:amb y '(1 2 3 4 5))
    (p:amb z '(1 2 3 4 5))
    (c:* x x x2)
    (c:* y y y2)
    (c:* z z z2)
    (c:+ x2 y2 z2)
    (list x y z x2 y2 z2)))

(define (pythagorean-3)
  (let-cells (x y z xx yy zz x2 y2 z2)
    (p:amb xx '(1 2 3 4 5))
    (p:amb yy '(1 2 3 4 5))
    (p:amb zz '(1 2 3 4 5))
    (p:-> xx x)
    (p:-> yy y)
    (p:-> zz z)
    (c:* x x x2)
    (c:* y y y2)
    (c:* z z z2)
    (c:+ x2 y2 z2)
    (list x y z xx yy zz x2 y2 z2)))

(define (pythagorean-4)
  (let-cells (x y z xx yy zz x2 y2 z2)
    (p:amb xx '(1 2 3 4 5))
    (p:amb yy '(1 2 3 4 5))
    (p:amb zz '(1 2 3 4 5))
    (c:same xx x)
    (c:same yy y)
    (c:same zz z)
    (c:* x x x2)
    (c:* y y y2)
    (c:* z z z2)
    (c:+ x2 y2 z2)
    (list x y z xx yy zz x2 y2 z2)))