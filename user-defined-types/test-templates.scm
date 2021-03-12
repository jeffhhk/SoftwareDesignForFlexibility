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

(define-test 'template-patterns
  (lambda ()
    (let ((operators '(? ?* ?+))
          (names '(a b c))
          (polarities '(+ = -)))

      (assert-false (template-pattern? '()))
      (assert-false (template-pattern-element? '()))
      (assert-false (template-pattern? '(())))
      (for-each (lambda (symbol)
                  (assert-false (template-pattern? symbol))
                  (assert-false (template-pattern-element? symbol))
                  (assert-false (template-pattern? (list symbol)))
                  (assert-false (template-pattern-element? (list symbol)))
                  (assert-false (template-pattern? (list (list symbol)))))
                (append operators names polarities))

      (let ((elements (elementwise-lists-of (list operators names polarities))))
        (for-each
         (lambda (element)
           (assert-true (template-pattern? (list element)))
           (assert-false (template-pattern? element))
           (for-each
            (lambda (permutation)
              (let ((assertion
                     (if (equal? permutation element)
                         assert-true
                         assert-false)))
                (assertion (template-pattern-element? permutation))
                (assertion (template-pattern? (list permutation)))
                (assertion (template-pattern-element? (take permutation 2)))
                (assertion (template-pattern? (list (take permutation 2))))))
            (all-permutations-of element)))
         elements)

        (for-each
         (lambda (elements)
           ((if (n:= (length elements)
                     (length (delete-duplicates (map cadr elements) eqv?)))
                assert-true
                assert-false)
            (template-pattern? elements)))
         (append
          (elementwise-lists-of (list elements elements))
          (elementwise-lists-of (list elements elements elements))))))))

(define-test 'match-template-pattern
  (lambda ()
    (assert-error (lambda () (match-numbers '((? a)) 1)))
    (assert-equal '((a + 1))
                  (match-numbers '((? a)) '(1)))
    (assert-equal '((a - 1)
                    (b + 2))
                  (match-numbers '((? a -) (? b)) '(1 2)))
    (assert-equal '((a + (1 2 3))
                    (b - 2))
                  (match-numbers '((?* a) (? b -)) '((1 2 3) 2)))
    (assert-equal '((a - (1 2 3))
                    (b + 2))
                  (match-numbers '((?+ a -) (? b)) '((1 2 3) 2)))
    (assert-equal '((a + ())
                    (b - 2))
                  (match-numbers '((?* a) (? b -)) '(() 2)))
    (assert-error (lambda () (match-numbers '((?+ a -) (? b)) '(() 2))))
    (assert-error (lambda () (match-numbers '((?* a) (? b -)) '(1 2))))
    (assert-error (lambda () (match-numbers '((?+ a -) (? b)) '(1 2))))))

(define (match-numbers pattern values)
  (parameter-bindings->alist (match-template-pattern pattern values number?)))

(define (parameter-bindings->alist bindings)
  (map (lambda (binding)
         (list (parameter-binding-name binding)
               (parameter-binding-polarity binding)
               (parameter-binding-value binding)))
       bindings))