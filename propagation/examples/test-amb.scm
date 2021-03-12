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

;;; Elementary test of amb search.

(install-arithmetic! layered-arith)     ;debugging
(install-core-propagators! merge-value-sets
                           layered-arith
                           layered-propagator-projector)

(initialize-scheduler)

(define-cell a)
(define amb1 (binary-amb a))

(what-is-in a)
#| (the-nothing) |#
#| Strongest value: |#
#| (the-nothing) |#

(run)

(what-is-in a)
#| (out #f (#[hypothetical 287 false])) |#
#| (in #t (#[hypothetical 288 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 288 true])) |#

(define-cell b)
(define amb2 (binary-amb b))

(what-is-in b)
#| (the-nothing) |#
#| Strongest value: |#
#| (the-nothing) |#

(run)

(what-is-in b)
#| (out #f (#[hypothetical 289 false])) |#
#| (in #t (#[hypothetical 290 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 290 true])) |#

;;; So at this point a and b are both #t
;;; We make this impossible:

(define not1 (p:not a b))
;;; The contradiction will appear in b.

(run)

;;; a should be unchanged.
(what-is-in a)
#| (out #f (#[hypothetical 287 false])) |#
#| (in #t (#[hypothetical 288 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 288 true])) |#

;;; but b should be changed:

(what-is-in b)
#| (out (the-contradiction) (#[hypothetical 290 true] #[hypothetical 288 true])) |#
#| (in #f (#[hypothetical 288 true])) |#
#| (in #f (#[hypothetical 289 false])) |#
#| (out #t (#[hypothetical 290 true])) |#
#| Strongest value: |#
#| (in #f (#[hypothetical 288 true])) |#

;;; This looks exactly right.


;;; Now let's force b to have #t with external support!

(tell! b #t 'gjs)
;Warning: Contradiction: #[value-set 24] #[cell 20 b]
;Value: contradiction

(what-is-in a)
#| (in #f (#[hypothetical 287 false])) |#
#| (out #t (#[hypothetical 288 true])) |#
#| Strongest value: |#
#| (in #f (#[hypothetical 287 false])) |#

(what-is-in b)
#|
(out (the-contradiction)
     (#[hypothetical 289 false] #[hypothetical 287 false]))
|#
#| (in #t (#[hypothetical 287 false])) |#
#| (out (the-contradiction) (#[hypothetical 288 true] gjs)) |#
#| (in #t (gjs)) |#
#| (out (the-contradiction) (#[hypothetical 290 true] #[hypothetical 288 true])) |#
#| (out #f (#[hypothetical 288 true])) |#
#| (out #f (#[hypothetical 289 false])) |#
#| (in #t (#[hypothetical 290 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 287 false])) |#
;Unspecified return value

;;; This is sort of right.  The cells a and b both contain good
;;; stuff, but the strongest value of b should be supported by
;;; gjs and the strongest value of a should have gjs in its
;;; support.
