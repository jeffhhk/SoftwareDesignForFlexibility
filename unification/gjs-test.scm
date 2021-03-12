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

;;; Book examples:
(unify-test '(* (?? a) (+ (?? b)) (?? c))
            '(* x y (+ z w) m (+ n o) p)
            '(dict (c (m (+ n o) p) ??) (b (z w) ??) (a (x y) ??))
            #t)
'expect-value: 'matches-including-expected
#|
as-expected
(succeed (* x y (+ z w) m (+ n o) p)
         (dict (c (m (+ n o) p) ??) (b (z w) ??) (a (x y) ??)))
as-expected
(succeed (* x y (+ z w) m (+ n o) p)
         (dict (c (p) ??) (b (n o) ??) (a (x y (+ z w) m) ??)))
;Value: consistent
|#

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b b (?? w) b b b c)
            '(dict (y (b b b (?? w) b b b) ??) (x () ??))
            #t)
'expect-value: 'matches-including-expected
#|
as-expected
(succeed (a b b b (?? w) b b b c)
         (dict (y (b b b (?? w) b b b) ??) (x () ??)))
as-expected
(succeed (a b b b (?? w) b b b c)
         (dict (y (b b (?? w) b b) ??) (x (b) ??)))
as-expected
(succeed (a b b b (?? w) b b b c) (dict (y (b (?? w) b) ??) (x (b b) ??)))
(succeed (a b b b b b b c) (dict (y () ??) (w () ??) (x (b b b) ??)))
as-expected
(succeed (a b b b (?? y) b b b c) (dict (w ((?? y)) ??) (x (b b b) ??)))
(succeed (a b b b b b b c) (dict (w () ??) (y () ??) (x (b b b) ??)))
;Value: consistent!
|#

;;; A bunch of tests taken from Oleg@pobox.com, whoever that is.

(unify-test '(M abc) '(M abc) '(dict))
'expect-value: 'matches-including-expected

(unify-test '(M abc) '(M abcd) #f)
'expect-value: 'no-matches-as-expected

(unify-test '(M abc) '(M (? x) (? y)) #f)
'expect-value: 'no-matches-as-expected

(unify-test '(M abc)
            '(M (? x))
            '(dict (x abc ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (? x) a (? y))
            '(M a (? z) b)
            '(dict (y b ?) (z a ?) (x a ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (? a) (? b))
            '(M (? b) (? a))
            '(dict (a (? b) ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (? a) abc)
            '(M (? b) (? a))
            '(dict (b abc ?) (a abc ?)))
'expect-value: 'matches-including-expected

(unify-test '(? b)
            '(f (? a))
            '(dict (b (f (? a)) ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (f (? a)))
            '(M (? a))
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '(M (f (? a)))
            '(M (? b))
            '(dict (b (f (? a)) ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (f (? a)) (? a))
            '(M (? b) (? c))
            '(dict (a (? c) ?) (b (f (? c)) ?)))
'expect-value: 'matches-including-expected

(unify-test '(M (f (? a)) (? a))
            '(M (? b) (f (? b)))
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '(M (f (? a)) (? a))
            '(M (f (? b)) (? b))
            '(dict (a (? b) ?)))
'expect-value: 'matches-including-expected

;;; A few tests from Dan Friedman's "Poor-man's Logic system
;;; tutorial"

(unify-test '((? x) (? x))
            '(3 4)
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((? x) 4)
            '(3 (? x))
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((? x) (? y))
            '(3 4)
            '(dict (y 4 ?) (x 3 ?)))
'expect-value: 'matches-including-expected

(unify-test '((? x) 4)
            '(3 (? y))
            '(dict (y 4 ?) (x 3 ?)))
'expect-value: 'matches-including-expected

(unify-test '((? x) 4)
            '((? y) (? y))
            '(dict (y 4 ?) (x 4 ?)))
'expect-value: 'matches-including-expected

(unify-test '((? x) 4 3)
            '((? y) (? y) (? x))
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((? x) 4 3 (? w))
            '(3 (? y) (? x) (? z))
            '(dict (w (? z) ?) (y 4 ?) (x 3 ?)))
'expect-value: 'matches-including-expected

(unify-test '(p (? x) (? x))
            '(p (? y) (f (? y)))
            #f)
'expect-value: 'no-matches-as-expected

;;; The next two illustrate that users of the dictionary must
;;; substitute all the way through.

(unify-test '(p (f a) (g (? x)))
            '(p (? x) (? y))
            '(dict (x (f a) ?) (y (g (f a)) ?)))
'expect-value: 'matches-including-expected

(unify-test '(p (g (? x)) (f a))
            '(p (? y) (? x))
            '(dict (x (f a) ?) (y (g (f a)) ?)))
'expect-value: 'matches-including-expected

;;; Tests for a bad unification from Franz Baader, Wayne Snyder,
;;; "Unification theory" (1999).  The naive algorithm
;;; exponentially explodes on these:

(unify-test
 '(h (? x1) (f (? y0) (? y0)) (? y1))
 '(h (f (? x0) (? x0)) (? y1) (? x1))
 '(dict (x0 (? y0) ?)
        (y1 (f (? y0) (? y0)) ?)
        (x1 (f (? y0) (? y0)) ?)))
'expect-value: 'matches-including-expected

(unify-test
 '(h (? x1)
     (? x2)
     (f (? y0) (? y0))
     (f (? y1) (? y1)) (? y2))
 '(h (f (? x0) (? x0))
     (f (? x1) (? x1))
     (? y1)
     (? y2)
     (? x2))
 '(dict (y0 (? x0) ?)
        (y2 (f (f (? x0) (? x0)) (f (? x0) (? x0))) ?)
        (y1 (f (? x0) (? x0)) ?)
        (x2 (f (f (? x0) (? x0)) (f (? x0) (? x0))) ?)
        (x1 (f (? x0) (? x0)) ?)))
'expect-value: 'matches-including-expected

;;; More, some restrictions.
(unify-test '(a ((? b) 2 3) 1 c)
            '(a (1 2 3) 1 c)
            '(dict (b 1 ?)))
'expect-value: 'matches-including-expected

(unify-test `(a ((? b ,number?) 2 3) 1 c)
            '(a (1 2 3) 1 c)
            '(dict (b 1 ?)))
'expect-value: 'matches-including-expected

(unify-test `(a ((? b ,symbol?) 2 3) 1 c)
                '(a (1 2 3) 1 c)
                #f)
'expect-value: 'no-matches-as-expected

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 2 c)
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 1 c)
            '(dict (b 1 ?)))
'expect-value: 'matches-including-expected

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) 1 (? x))
            '(dict (x c ?) (b 1 ?)))
'expect-value: 'matches-including-expected

(unify-test '(a ((? b) 2 3) (? b) c)
            '(a (1 2 3) (? x) c)
            '(dict (x 1 ?) (b 1 ?)))
'expect-value: 'matches-including-expected

;;; Segments below.
(unify-test '(a (?? x) c)
            '(a b b b b b b c)
            '(dict (x (b b b b b b) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '(dict (x (1 2 3) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '(dict (y () ??) (x (1 2 3) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? y) (?? x) (?? x) c)
            '(a 1 2 3 1 2 3 c)
            '(dict (x () ??) (y (1 2 3 1 2 3) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? x) c)
            '(a b b b b b b c)
            '(dict (x (b b b) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b b b b b c)
            '(dict (y () ??) (x (b b b) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b (?? z) b b b b c)
            '(dict (y (b b (?? z) b b b b) ??) (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b (?? z) b b (?? w) b b (?? z) c)
            '(dict (z (b) ??) (y (b b b b (?? w) b b) ??) (x (b) ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c (?? u))
            '(a b b (?? z) b b (?? w) b b (?? z) c (?? v))
            '(dict (u ((?? v)) ??)
                   (z () ??)
                   (y (b b b b (?? w) b b) ??)
                   (x () ??)))
'expect-value: 'matches-including-expected

(unify-test '(a (?? x) (?? y) (?? x) c (?? u))
            '((? m) b b (?? z) b b (?? w) b b (?? z) c (?? v))
            '(dict (v ((?? y) b b (?? z) b b (?? w) b b (?? z) c c (?? u))
                      ??)
                   (x (b b (?? z) b b (?? w) b b (?? z) c) ??)
                   (m a ?)))
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER gets same missed match as CPH

    GJS
as-expected
;Value: consistent

    CPH
(missed-match!
 (a (?? x) (?? y) (?? x) c (?? u))
 ((? m) b b (?? z) b b (?? w) b b (?? z) c (?? v))
 (dict (v ((?? y) b b (?? z) b b (?? w) b b (?? z) c c (?? u)) ??)
       (x (b b (?? z) b b (?? w) b b (?? z) c) ??)
       (m a ?)))
;Value: see-bad-results-log
|#

(unify-test '((? m) b b (?? z) b b (?? w) b b (?? z) c (?? v))
            '(a (?? x) (?? y) (?? x) c (?? u))
            '(dict (v ((?? y) b b (?? z) b b (?? w) b b (?? z) c c (?? u))
                      ??)
                   (x (b b (?? z) b b (?? w) b b (?? z) c) ??)
                   (m a ?)))
'expect-value: 'matches-including-expected

(unify-test '(((?? x) 4) ((?? x)) (3 4))
            '(((?? y) (?? w)) (3) ((?? y)))
            '(dict (x (3) ??) (y (3 4) ??) (w () ??))
            #t)
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER gets correct answer, as GJS

    GJS
as-expected
(succeed ((3 4) (3) (3 4)) (dict (x (3) ??) (w () ??) (y (3 4) ??)))
;Value: consistent!

    CPH
(missed-essential-match! (((?? x) 4) ((?? x)) (3 4))
                         (((?? y) (?? w)) (3) ((?? y)))
                         (dict (x (3) ??) (y (3 4) ??) (w () ??)))
;Value: see-bad-results-log
|#
(unify-test '(((?? y) (?? w)) (3) ((?? y)))
            '(((?? x) 4) ((?? x)) (3 4))
            '(dict (x (3) ??) (y (3 4) ??) (w () ??))
            #t)
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER gets correct anwer:
as-expected
(succeed
 ((3 4) (3) (3 4))
 (dict (o:1522 (3) ??) (w () ??) (e:1521 (4) ??) (y (3 4) ??) (x (3) ??)))
;Value: consistent

    GJS
(missed-essential-match! (((?? y) (?? w)) (3) ((?? y)))
                         (((?? x) 4) ((?? x)) (3 4))
                         (dict (x (3) ??) (y (3 4) ??) (w () ??)))
;Value: see-bad-results-log

    CPH
(missed-essential-match! (((?? y) (?? w)) (3) ((?? y)))
                         (((?? x) 4) ((?? x)) (3 4))
                         (dict (x (3) ??) (y (3 4) ??) (w () ??)))
;Value: see-bad-results-log
|#


;;; Kenny Chen

(unify-test '((?? y) 3)
            '((?? x))
            '(dict (x ((?? y) 3) ??))
            #t)
'expect-value: 'matches-including-expected
#|  GJS
as-expected
(succeed ((?? y) 3) (dict (x ((?? y) 3) ??)))
;Value: consistent!

    CPH
(succeed (3) (dict (x (3) ??) (y () ??)))
(missed-match! ((?? y) 3) ((?? x)) (dict (x ((?? y) 3) ??)))
;Value: see-bad-results-log
|#

(unify-test '((?? x))
            '((?? y) 3)
            '(dict (x ((?? y) 3) ??))
            #t)
;; Actual: 'matches-excluding-expected
'expect-value: 'matches-including-expected
#|
   SEGMENT SPLITTER gets better answer, as CPH

   GJS
(succeed (3) (dict (x (3) ??) (y () ??)))
(missed-match! ((?? x)) ((?? y) 3) (dict (x ((?? y) 3) ??)))
;Value: see-bad-results-log

   CPH
as-expected
(succeed ((?? y) 3) (dict (x ((?? y) 3) ??)))
;Value: consistent!
|#

;;; Another Kenny Chen bug!

(unify-test '((?? z) ((?? z)))
            '(n n n ((? x) (?? y)))
            '(dict (y (n n) ??) (x n ?) (z (n n n) ??))
            #t)
'expect-value: 'matches-including-expected

(unify-test '((?? z) ((?? z)))
            '(n n ((? x) (?? y)))
            '(dict (y (n) ??) (x n ?) (z (n n) ??))
            #t)
'expect-value: 'matches-including-expected

(unify-test '((?? x) 3)
            '(4 (?? y))
            '(dict (e:1525 (3) ??) (y ((?? o:1526) 3) ??) (x (4 (?? o:1526)) ??)))
;; with segment splitting:
;; 'expect-value: 'matches-including-expected
'expect-value: 'matches-excluding-expected
#|
    SEGMENT SPLITTER gets this one.

|#

(unify-test '((4 (?? y)) (4 5))
            '(((?? x) 3) ((?? x)))
            '(dict (x (4 5) ??) (y (5 3) ??))
            #t)
;; Actual: 'no-matches-but-expected-some
'expect-value: 'matches-including-expected

(unify-test '(((?? x) 3) ((?? x)))
            '((4 (?? y)) (4 5))
            '(dict (x (4 5) ??) (y (5 3) ??))
            #t)
;; Actual: 'no-matches-but-expected-some
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER gets this one.
as-expected
(succeed ((4 5 3) (4 5))
         (dict (o:1530 (5) ??) (e:1529 (3) ??) (y (5 3) ??) (x (4 5) ??)))

    GJS
(missed-essential-match! (((?? x) 3) ((?? x)))
                         ((4 (?? y)) (4 5))
                         (dict (x (4 5) ??) (y (5 3) ??)))
;Value: see-bad-results-log

    CPH
(missed-essential-match! (((?? x) 3) ((?? x)))
                         ((4 (?? y)) (4 5))
                         (dict (x (4 5) ??) (y (5 3) ??)))
;Value: see-bad-results-log
|#

(unify-test '((?? x) (?? y))
            '((?? z) (?? w))
            '(dict (w ((?? y)) ??) (x () ??) (z () ??))
            #t)
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER gets this one

    GJS
(succeed () (dict (y () ??) (w () ??) (x () ??) (z () ??)))
as-expected
(succeed ((?? y)) (dict (w ((?? y)) ??) (x () ??) (z () ??)))
as-expected
(succeed ((?? w)) (dict (y () ??) (x ((?? w)) ??) (z () ??)))
as-expected
(succeed ((?? x)) (dict (w () ??) (y () ??) (z ((?? x)) ??)))
(succeed ((?? x) (?? w)) (dict (y ((?? w)) ??) (z ((?? x)) ??)))
(succeed ((?? x) (?? y)) (dict (w () ??) (z ((?? x) (?? y)) ??)))
;Value: consistent

    CPH
(succeed ((?? z) (?? w)) (dict (y ((?? z) (?? w)) ??) (x () ??)))
(succeed ((?? z) (?? w)) (dict (y ((?? w)) ??) (x ((?? z)) ??)))
(missed-match! ((?? x) (?? y))
               ((?? z) (?? w))
               (dict (w ((?? y)) ??) (x () ??) (z () ??)))
;Value: see-bad-results-log


|#

(unify-test '((?? x) (?? y))
            '((?? x) (?? x))
            '(dict (y ((?? x)) ??))
            #t)
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? y))
            '((?? x) (?? y))
            '(dict)
            #t)
'expect-value: 'matches-including-expected

(unify-test '((?? x) (?? y))
            '((?? y) (?? x))
            '(dict (y () ??))
            #t)
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER loops infinitely, producing new variables.

Both GJS and CPH get this one!
|#

(unify-test '((?? x) 3)
            '((?? y) (?? x) (?? y))
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((?? y) (?? x) (?? y))
            '((?? x) 3)
            #f)
'expect-value: 'no-matches-as-expected

(unify-test '((?? x))
            '((?? y) (?? x))
            '(dict (y () ??))
            #t)
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER wins here.

    GJS
as-expected
(succeed ((?? x)) (dict (y () ??)))
(succeed () (dict (x () ??) (y () ??)))
;Value: consistent

    CPH
(missed-essential-match! ((?? x)) ((?? y) (?? x)) (dict (y () ??)))
;Value: see-bad-results-log
|#

(unify-test '((?? y) (?? x))
            '((?? x))
            '(dict (y () ??))
            #t)
;; Actual: 'matches-excluding-expected
'expect-value: 'matches-including-expected
#|
    SEGMENT SPLITTER misses this one.

(succeed () (dict (y () ??) (x () ??)))
(succeed () (dict (o:58 () ??) (e:57 () ??) (y () ??) (x () ??)))
(missed-match! ((?? y) (?? x)) ((?? x)) (dict (y () ??)))
;Value: see-bad-results-log


   GJS
(succeed () (dict (y () ??) (x () ??)))
(succeed () (dict (y () ??) (x () ??)))
(missed-match! ((?? y) (?? x)) ((?? x)) (dict (y () ??)))
;Value: see-bad-results-log

   CPH
as-expected
(succeed ((?? x)) (dict (y () ??)))
;Value: consistent
|#
