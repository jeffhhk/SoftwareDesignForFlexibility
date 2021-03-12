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

(setup-propagator-system numeric-arithmetic)

(define (pythagorean integers)
  (let-cells (x y z x2 y2 z2)
    (p:amb x integers)
    (p:amb y integers)
    (p:amb z integers)
    (p:* x x x2)
    (p:* y y y2)
    (p:* z z z2)
    (p:+ x2 y2 z2)
    (list x y z)))
;Value: pythagorean

(initialize-scheduler)
;Value: ok

(all-results (pythagorean (iota 20 1))
             (lambda (strongest-contents)
               (write *number-of-calls-to-fail*)
               (display "  ")
               (pp (map get-base-value strongest-contents))))
1400  (3 4 5)
1779  (4 3 5)
2376  (5 12 13)
2690  (6 8 10)
3449  (8 6 10)
3646  (8 15 17)
3982  (9 12 15)
5034  (12 5 13)
5121  (12 9 15)
5274  (12 16 20)
6304  (15 8 17)
6792  (16 12 20)
;Value: no-more

*number-of-calls-to-fail*
;Value: 8565

