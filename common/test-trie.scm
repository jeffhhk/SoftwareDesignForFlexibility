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

(define features0 '())
(define features1 '(1 2))
(define features2 '(-3 -1))
(define features3 '(3 1))
(define features4 '(-1 -1))
(define features5 '(-3 2))

(define (over-2? x)
  (n:> (n:abs x) 2))

(define (negative-number? x)
  (and (n:number? x) (n:negative? x)))

(define (even-number? x)
  (and (n:number? x) (even? x)))

(define msg:node-has-no-value "Trie node has no value")
(define msg:unable-to-match-features "Unable to match features")

(define-test 'trie-empty
  (lambda ()
    (let ((trie (make-trie)))
      (assert-equal trie (intern-path-trie trie '()))
      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value trie features0)))
      (assert-error-message msg:node-has-no-value (lambda () (get-all-values trie features0)))

      (set-trie-value! trie "win!")
      (assert-equal "win!" (get-a-value trie features0))
      (assert-equal '("win!") (get-all-values trie features0))

      (assert-predicate-satisfied
       (lambda (object)
         (and (trie? object)
              (not (eqv? trie object))))
       (intern-path-trie trie (list negative?)))
      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value trie features1)))
      (assert-equal '() (get-all-values trie features1)))))

(define-test 'trie-non-empty
  (lambda ()
    (let* ((trie (make-trie))
           (neg1 (add-edge-to-trie trie negative?))
           (even1 (add-edge-to-trie trie even?))
           (over-2-1 (add-edge-to-trie trie over-2?))
           (nn (add-edge-to-trie neg1 negative?))
           (oo (add-edge-to-trie over-2-1 odd?))
           (ne (add-edge-to-trie neg1 even?)))

      (assert-equal trie (intern-path-trie trie '()))

      (assert-equal neg1 (intern-path-trie trie (list negative?)))
      (assert-equal even1 (intern-path-trie trie (list even?)))
      (assert-equal over-2-1 (intern-path-trie trie (list over-2?)))

      (assert-equal nn (intern-path-trie trie (list negative? negative?)))
      (assert-equal oo (intern-path-trie trie (list over-2? odd?)))
      (assert-equal ne (intern-path-trie trie (list negative? even?)))

      (set-trie-value! nn "negative;negative")
      (set-trie-value! oo "over-2;odd")
      (add-edge-to-trie neg1 even?)     ;missing value

      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value trie features0)))
      (assert-error-message msg:node-has-no-value (lambda () (get-all-values trie features0)))

      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value trie features1)))
      (assert-equal '() (get-all-values trie features1))

      (assert-one-of '("negative;negative" "over-2;odd") (get-a-value trie features2))
      (assert-contains-exactly '("negative;negative" "over-2;odd") (get-all-values trie features2))

      (assert-equal "over-2;odd" (get-a-value trie features3))
      (assert-equal '("over-2;odd") (get-all-values trie features3))

      (assert-equal "negative;negative" (get-a-value trie features4))
      (assert-equal '("negative;negative") (get-all-values trie features4))
        
      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value trie features5)))
      (assert-error-message msg:node-has-no-value (lambda () (get-all-values trie features5))))))

(define-test 'gjs-example-1
  (lambda ()
    (let ((a-trie (make-trie)))
      (define nn (add-edge-to-trie a-trie negative-number?))
      (define s (add-edge-to-trie a-trie symbol?))
      (define e (add-edge-to-trie a-trie even-number?))
      (define sn (add-edge-to-trie s number?))
      (define ss (add-edge-to-trie s symbol?))
      (set-trie-value! s '(symbol))
      (set-trie-value! ss '(symbol symbol))

      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value a-trie '(a 1))))
      (assert-equal '(symbol symbol) (get-a-value a-trie '(a b)))
      (assert-equal '(symbol) (get-a-value a-trie '(c)))
      (assert-equal '() (get-all-values a-trie '(3)))
      (assert-equal '((symbol symbol)) (get-all-values a-trie '(a b)))

      (set-trie-value! sn '(symbol number))
      (set-trie-value! nn '(negative-number))
      (set-trie-value! e '(even-number))

      (assert-equal '((even-number) (negative-number)) (get-all-values a-trie '(-4))))))

(define-test 'gjs-example-2
  (lambda ()
    (let ((a-trie (make-trie)))
      (set-path-value! a-trie (list symbol?) '(symbol))
      (set-path-value! a-trie (list symbol? symbol?) '(symbol symbol))

      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value a-trie '(a 1))))
      (assert-equal '(symbol symbol) (get-a-value a-trie '(a b)))
      (assert-equal '(symbol) (get-a-value a-trie '(c)))
      (assert-equal '() (get-all-values a-trie '(3)))
      (assert-equal '((symbol symbol)) (get-all-values a-trie '(a b)))

      (set-path-value! a-trie (list negative-number?) '(negative-number))
      (set-path-value! a-trie (list even-number?) '(even-number))
      (set-path-value! a-trie (list symbol? number?) '(symbol number))

      (assert-equal '((even-number) (negative-number)) (get-all-values a-trie '(-4))))))

(define-test 'book-example
  (lambda ()
    (let* ((a-trie (make-trie))
           (s (add-edge-to-trie a-trie symbol?))
           (sn (add-edge-to-trie s number?))
           (ss (intern-path-trie a-trie (list symbol? symbol?))))
      (assert-false (trie-has-value? sn))
      (set-trie-value! sn '(symbol number))
      (assert-true (trie-has-value? sn))
      (assert-equal '(symbol number) (trie-value sn))

      (set-path-value! a-trie (list symbol? symbol?) '(symbol symbol))
      (assert-equal '(symbol symbol) (trie-value ss))

      (assert-equal (list ss) (get-matching-tries a-trie '(a b)))
      (assert-equal (list s) (get-matching-tries a-trie '(c)))

      (assert-equal '(symbol symbol) (get-a-value a-trie '(a b)))
      (assert-error-message msg:unable-to-match-features (lambda () (get-a-value a-trie '(-4))))

      (set-path-value! a-trie (list negative-number?) '(negative-number))
      (set-path-value! a-trie (list even-number?) '(even-number))

      (assert-contains-exactly '((even-number) (negative-number)) (get-all-values a-trie '(-4))))))
