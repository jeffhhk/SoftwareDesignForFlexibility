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

(define-test 'match:pattern-names
  (lambda ()
    (assert-equal '(a b)
                  (match:pattern-names '((? a) (?? b))))))

(define-test 'matcher
  (lambda ()
    (assert-equal '((b 1 ?))
                  ((matcher '(a ((? b) 2 3) 1 c))
                   '(a (1 2 3) 1 c)))
    (let ((m (matcher '(a ((? b) 2 3) (? b) c))))
      (assert-false (m '(a (1 2 3) 2 c)))
      (assert-equal '((b 1 ?))
                    (m '(a (1 2 3) 1 c))))
    (assert-equal
     '(((y (b b b b b b) ??) (x () ??))
       ((y (b b b b) ??) (x (b) ??))
       ((y (b b) ??) (x (b b) ??))
       ((y () ??) (x (b b b) ??)))
     (let ((m (match:compile-pattern '(a (?? x) (?? y) (?? x) c)))
           (results '()))
       (m '((a b b b b b b c))
          (match:new-dict)
          (lambda (dictionary n)
            (set! results
                  (cons (match:bindings dictionary) results))
            #f))
       (reverse results)))
    (assert-equal '((b 1 ?))
                  ((matcher '(a ((? b) 2 3) (? b) c))
                   '(a (1 2 3) 1 c)))))