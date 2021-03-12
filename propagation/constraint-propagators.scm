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

;;; A constraint propagator is a compound propagator with
;;; inputs=outputs.

(define (constraint-propagator cells to-build name)
  (compound-propagator cells cells to-build name))

(define-c:prop (c:+ x y sum)
  (p:+ x y sum)
  (p:- sum x y)
  (p:- sum y x))

(define-c:prop (c:* x y product)
  (p:* x y product)
  (p:/ product x y)
  (p:/ product y x))

(define-c:prop (c:negate x y)
  (p:negate x y)
  (p:negate y x))

(define-c:prop (c:invert x y)
  (p:invert x y)
  (p:invert y x))

(define-c:prop (c:exp x y)
  (p:exp x y)
  (p:log y x))

(define-c:prop (c:sin x y)
  (p:sin x y)
  (p:asin y x))

(define-c:prop (c:cos x y)
  (p:cos x y)
  (p:acos y x))

(define-c:prop (c:tan x y)
  (p:tan x y)
  (p:atan y x))

(define-c:prop (c:same x y)
  (p:-> x y)
  (p:-> y x))

(define-c:prop (c:controlled-same a b p)
  (p:spst-switch p a b)
  (p:spst-switch p b a))

(define-c:prop (c:full-same a b p)
  (p:= a b p)
  (p:spst-switch p a b)
  (p:spst-switch p b a))

;;; Logical constraints

(define-c:prop (c:and a b c)
  (p:and a b c)
  (p:dna c a b)
  (p:dna c b a)
  (p:imp c a)
  (p:imp c b))

(define-c:prop (c:or a b c)
  (p:or a b c)
  (p:ro c a b)
  (p:ro c b a)
  (p:pmi c a)
  (p:pmi c b))

(define-c:prop (c:implies a b)
  (p:imp a b)
  (p:pmi b a))

(define-c:prop (c:not a b)
  (p:not a b)
  (p:not b a))

;;; p:sqrt delivers the positive square root.

(define-c:prop (c:quadratic x x^2)
  (p:square x x^2)
  (p:sqrt x^2 x))

;;; These use AMB because square root and
;;;  reverse-abs are ambiguous as to sign.

(define-p:prop (p:honest-sqrt (x^2) (x))
  (let-cells (mul +x)
    (p:amb mul '(-1 +1))
    (p:sqrt x^2 +x)
    (p:* mul +x x)))

(define-c:prop (c:square x x^2)
  (p:square x x^2)
  (p:honest-sqrt x^2 x))

(define-c:prop (c:abs x ax)
  (p:abs x ax)
  (let-cells (mul)
    (p:amb mul '(-1 +1))
    (p:* mul ax x)))

;;; Three boolean values, only one may be true

(define-c:prop (c:choose-exactly-one u v w)
  (let-cells (-u -v -w u+v u+v+w)
    (p:not -u u)
    (p:not -v v)
    (p:not -w w)

    (p:imp u -v)
    (p:imp u -w)

    (p:imp v -u)
    (p:imp v -w)

    (p:imp w -u)
    (p:imp w -v)

    (c:or u v u+v)
    (c:or u+v w u+v+w)
    (add-cell-content! u+v+w #t)

    (binary-amb u)
    (binary-amb v)
    (binary-amb w)))

#|
(install-arithmetic! layered-arith)     ;debugging
(install-core-propagators!
 merge-value-sets
 layered-arith
 layered-propagator-projector)

(define show
  (lambda (name content increment merged)
    (fresh-line)
    (display "----------------\n")
    (display "name: ")
    (write name)
    (newline)
    (display "content:\n")
    (what-is-this content)
    (display "increment:\n")
    (what-is-this increment)
    (display "merged:\n")
    (what-is-this merged)))
(begin
  (initialize-scheduler)
  (define-cell x)
  (define-cell y)
  (define-cell z)
  (set-cell-probe! x show)
  (set-cell-probe! y show)
  (set-cell-probe! z show)
  (c:choose-exactly-one x y z))

(tell! y #f 'gjsy)

(what-is-in x)
#| (in (the-nothing) (#[hypothetical 21 true])) |#
#| (out #f (#[hypothetical 22 false])) |#
#| (in #t (#[hypothetical 21 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 21 true])) |#

(what-is-in y)
#| (in #f (#[hypothetical 21 true])) |#
#| (in (the-nothing) (#[hypothetical 21 true])) |#
#| (out (the-contradiction) (gjsy #[hypothetical 23 true])) |#
#| (in #f (#[hypothetical 24 false])) |#
#| (out #t (#[hypothetical 23 true])) |#
#| (in #f (gjsy)) |#
#| Strongest value: |#
#| (in #f (#[hypothetical 21 true])) |#

(what-is-in z)
#| (out (the-contradiction) (#[hypothetical 25 true] #[hypothetical 21 true])) |#
#| (in #f (#[hypothetical 21 true])) |#
#| (the-nothing) |#
#| (in #f (#[hypothetical 26 false])) |#
#| (out #t (#[hypothetical 25 true])) |#
#| Strongest value: |#
#| (in #f (#[hypothetical 21 true])) |#

(tell! x #f 'gjsx)

(what-is-in x)
#| (in (the-nothing) (#[hypothetical 24 false] gjsx)) |#
#| (out (the-contradiction) (#[hypothetical 21 true] gjsx)) |#
#| (in #f (gjsx)) |#
#| (out (the-nothing) (#[hypothetical 21 true])) |#
#| (in #f (#[hypothetical 22 false])) |#
#| (out #t (#[hypothetical 21 true])) |#
#| Strongest value: |#
#| (in #f (gjsx)) |#

(what-is-in y)
#| (in (the-nothing) (#[hypothetical 24 false] gjsx)) |#
#| (out #f (#[hypothetical 21 true])) |#
#| (out (the-nothing) (#[hypothetical 21 true])) |#
#| (out (the-contradiction) (gjsy #[hypothetical 23 true])) |#
#| (in #f (#[hypothetical 24 false])) |#
#| (out #t (#[hypothetical 23 true])) |#
#| (in #f (gjsy)) |#
#| Strongest value: |#
#| (in #f (#[hypothetical 24 false])) |#

(what-is-in z)
#| (in #t (#[hypothetical 24 false] gjsx)) |#
#| (out (the-contradiction) (#[hypothetical 25 true] #[hypothetical 21 true])) |#
#| (out #f (#[hypothetical 21 true])) |#
#| (the-nothing) |#
#| (out #f (#[hypothetical 26 false])) |#
#| (in #t (#[hypothetical 25 true])) |#
#| Strongest value: |#
#| (in #t (#[hypothetical 24 false] gjsx)) |#


|#
