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

;;; Backtracking is missing some assignments!

(unify-test '((?? x))
            '()
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) 1)
            '(1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1)
            '((?? x) 1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1 (?? x))
            '(1)
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(1)
            '(1 (?? x))
            '(dict (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x))
            '(3 (?? y))
            '(dict (x (3 (?? y)) ??)))
'expect-value: 'matches-including-expected

(unify-test '(3 (?? y))
            '((?? x))
            '(dict (x (3 (?? y)) ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? y) (?? x))
            '(a b b a))
;; verbose output:
;; (((dict (y (a b b a) ??) (x () ??)) (a b b a) (a b b a) #t #f)
;;  ((dict (y (b b) ??) (x (a) ??)) (a b b a) (a b b a) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x))
            '(a b a b))
;; verbose output:
;; (((dict (y (a b a b) ??) (x () ??)) (a b a b) (a b a b) #t #f)
;;  ((dict (y () ??) (x (a b) ??)) (a b a b) (a b a b) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x))
            '(a b c a b))
;; verbose output:
;; (((dict (y (a b c a b) ??) (x () ??)) (a b c a b) (a b c a b) #t #f)
;;  ((dict (y (c) ??) (x (a b) ??)) (a b c a b) (a b c a b) #t #f))
'expect-value: 'matches-and-expected-some

(unify-test '((?? x) (?? y) (?? x) (?? y))
            '(a b c a b)
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((?? x) (?? y) (?? x) (?? x))
            '(a b c a b)
            '(dict (y (a b c a b) ??) (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? x) (?? y) (?? x))
            '(a b c a b)
            '(dict (y (a b c a b) ??) (x () ??)))
'expect-value: 'matches-including-expected
