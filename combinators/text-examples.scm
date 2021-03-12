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


;; coderef: first-compose-value
((compose (lambda (x)
            (list 'foo x))
          (lambda (x)
            (list 'bar x)))
 'z)
'expect-value: '(foo (bar z))

;; coderef: full-compose-value
((compose (lambda (a b)
            (list 'foo a b))
          (lambda (x)
            (values (list 'bar x)
                    (list 'baz x))))
 'z)
'expect-value: '(foo (bar z) (baz z))

;; coderef: first-parallel-combine-value
((parallel-combine list
                   (lambda (x y z)
                     (list 'foo x y z))
                   (lambda (u v w)
                     (list 'bar u v w)))
 'a 'b 'c)
'expect-value: '((foo a b c) (bar a b c))

;; coderef: second-parallel-combine-value
((parallel-combine list
                   (lambda (x y z)
                     (values x y z))
                   (lambda (u v w)
                     (values w v u)))
 'a 'b 'c)
'expect-value: '(a b c c b a)

;; coderef: first-spread-combine-value
((spread-combine list
                 (lambda (x y)
                   (list 'foo x y))
                 (lambda (u v w)
                   (list 'bar u v w)))
 'a 'b 'c 'd 'e)
'expect-value: '((foo a b) (bar c d e))

;; coderef: second-spread-combine-value
((spread-combine list
                 (lambda (x y)
                   (values x y))
                 (lambda (u v w)
                   (values w v u)))
 'a 'b 'c 'd 'e)
'expect-value: '(a b e d c)

;; coderef: discard-argument-value
(((discard-argument 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)
'expect-value: '(foo a b d)

;; coderef: curry-argument-value
((((curry-argument 2)
   'a 'b 'c)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'd)
'expect-value: '(foo a b d c)

;; coderef: permute-arguments-value
(((permute-arguments 1 2 0 3)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'a 'b 'c 'd)
'expect-value: '(foo b c a d)
