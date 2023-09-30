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

;;;

(define (c->f c)
  (+ (* 9/5 c) 32))

(define text-arith
  (extend-arithmetic
   layered-extender
   (extend-arithmetic interval-extender
                      numeric-arithmetic)))

(install-arithmetic! text-arith)
(install-core-propagators!
 merge-value-sets
 text-arith
 layered-propagator-projector)


(initialize-scheduler)

(define-cell C)

(define-cell F)

(define-cell nine-fifths)

(define-cell thirty-two)

(define-cell int)

(c:* C nine-fifths int)

(c:+ int thirty-two F)

(define-cell K)

(define-cell two-seventy-three)

(c:+ K two-seventy-three C)

#|
(tell! nine-fifths 9/5 'definition-1)
(tell! thirty-two 32 'definition-2)

(tell! F 77 'GJS-1)

(inquire C)
#|
((c) (has-value 25)
     (depends-on definition-1 definition-2 gjs-1)
     (because ((p:/ c:*) (int 45) (nine-fifths 9/5))))
|#

(retract! 'GJS-1)

(inquire C)
#| ((c) (has-value (the-nothing)) (because unknown)) |#

(tell! C 25 'gjs-2)

(inquire F)
#|
((f) (has-value 77)
     (depends-on definition-2 definition-1 gjs-2)
     (because ((p:+ c:+) (int 45) (thirty-two 32))))
|#

(inquire int)
#|
((int) (has-value 45)
       (depends-on definition-1 gjs-2)
       (because ((p:* c:*) (c 25) (nine-fifths 9/5))))
|#

(tell! two-seventy-three 273.15 'definition-3)

(inquire k)
#|
((k) (has-value -2.4815e2)
     (depends-on definition-3 gjs-2)
     (because ((p:- c:+) (c 25) (two-seventy-three 2.7315e2))))
|#

;;; Whoops, I made a sign error... Oh well...

(retract! 'definition-3)

(tell! two-seventy-three -273.15 'definition-4)

(inquire k)
#|
((k)
 (has-value 2.9815e2)
 (depends-on definition-4 gjs-2)
 (because ((p:- c:+) (c 25) (two-seventy-three -2.7315e2))))
|#

(retract! 'gjs-2)

(tell! K 300 'GJS-3)

(inquire F)
#|
((f) (has-value 8.033e1)
     (depends-on definition-2 definition-1 definition-4 gjs-3)
     (because ((p:+ c:+) (int 4.833e1) (thirty-two 32))))
|#

;;; -------------------------------------------------------

(what-is-in F)
;; cell: (f)
#|
(in 80.33000000000004
    (definition-2 definition-1 definition-4 gjs-3)
    ((p:+ c:+) (int 48.33000000000004) (thirty-two 32)))
|#
#|
(out 77
     (definition-2 definition-1 gjs-2)
     ((p:+ c:+) (int 45) (thirty-two 32)))
|#
#| (out 77 (gjs-1) i-told-you-so) |#
;; strongest value:
#|
(in 80.33000000000004
    (definition-2 definition-1 definition-4 gjs-3)
    ((p:+ c:+) (int 48.33000000000004) (thirty-two 32)))
|#


(what-is-in int)
;; cell: (int)
#|
(in 48.33000000000004
    (definition-1 definition-4 gjs-3)
    ((p:* c:*) (c 26.850000000000023) (nine-fifths 9/5)))
|#
#|
(out 45
     (definition-1 gjs-2)
     ((p:* c:*) (c 25) (nine-fifths 9/5)))
|#
#|
(out 45
     (definition-2 gjs-1)
     ((p:- c:+) (f 77) (thirty-two 32)))
|#
;; strongest value:
#|
(in 48.33000000000004
    (definition-1 definition-4 gjs-3)
    ((p:* c:*) (c 26.850000000000023) (nine-fifths 9/5)))
|#

(what-is-in C)
;; cell: (c)
#|
(in 26.850000000000023
    (definition-4 gjs-3)
    ((p:+ c:+) (k 300) (two-seventy-three -273.15)))
|#
#| (out 25 (gjs-2) i-told-you-so) |#
#|
(out 25
     (definition-1 definition-2 gjs-1)
     ((p:/ c:*) (int 45) (nine-fifths 9/5)))
|#
;; strongest value:
#|
(in 26.850000000000023
    (definition-4 gjs-3)
    ((p:+ c:+) (k 300) (two-seventy-three -273.15)))
|#

(what-is-in K)
;; cell: (k)
#| (in 300 (gjs-3) i-told-you-so) |#
#|
(out 298.15
     (definition-4 gjs-2)
     ((p:- c:+) (c 25) (two-seventy-three -273.15)))
|#
#|
(out -248.14999999999998
     (definition-3 gjs-2)
     ((p:- c:+) (c 25) (two-seventy-three 273.15)))
|#
;; strongest value:
#| (in 300 (gjs-3) i-told-you-so) |#
;Unspecified return value


(what-is-in thirty-two)
;; cell: (thirty-two)
#| (in 32 (definition-2) i-told-you-so) |#
;Unspecified return value

(what-is-in nine-fifths)
;; cell: (nine-fifths)
#| (in 9/5 (definition-1) i-told-you-so) |#
;Unspecified return value

(what-is-in two-seventy-three)
;; cell: (two-seventy-three)
#| (in -273.15 (definition-4) i-told-you-so) |#
#| (out 273.15 (definition-3) i-told-you-so) |#
;; strongest value:
#| (in -273.15 (definition-4) i-told-you-so) |#
;Unspecified return value

|#