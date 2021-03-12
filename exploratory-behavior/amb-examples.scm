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

(begin
  (newline)
  (write-line (list (amb 1 2 3) (amb 'a 'b)))
  (amb))
'expect-write: '(1 a)
'expect-write: '(1 b)
'expect-write: '(2 a)
'expect-write: '(2 b)
'expect-write: '(3 a)
'expect-write: '(3 b)
'expect-value: 'no-more-alternatives

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (require p)
  (if (not p) (amb) 'ok))

(define (an-element-of list)
  (require (not (null? list)))
  (amb (car list) (an-element-of (cdr list))))

(define (sums-to nums total)
  (require (>= total 0))
  (if (= total 0)
      '()
      (let ((n (an-element-of nums)))
        (cons n (sums-to (delete n nums) (- total n))))))

(begin
  (write-line (sums-to '(1 2 3 5 7 11) 20))
  (amb))
'expect-write: '(1 3 5 11)
'expect-write: '(1 3 11 5)
'expect-write: '(1 5 3 11)
'expect-write: '(1 5 11 3)
'expect-write: '(1 11 3 5)
'expect-write: '(1 11 5 3)
'expect-write: '(2 7 11)
'expect-write: '(2 11 7)
'expect-write: '(3 1 5 11)
'expect-write: '(3 1 11 5)
'expect-write: '(3 5 1 11)
'expect-write: '(3 5 11 1)
'expect-write: '(3 11 1 5)
'expect-write: '(3 11 5 1)
'expect-write: '(5 1 3 11)
'expect-write: '(5 1 11 3)
'expect-write: '(5 3 1 11)
'expect-write: '(5 3 11 1)
'expect-write: '(5 11 1 3)
'expect-write: '(5 11 3 1)
'expect-write: '(7 2 11)
'expect-write: '(7 11 2)
'expect-write: '(11 1 3 5)
'expect-write: '(11 1 5 3)
'expect-write: '(11 2 7)
'expect-write: '(11 3 1 5)
'expect-write: '(11 3 5 1)
'expect-write: '(11 5 1 3)
'expect-write: '(11 5 3 1)
'expect-write: '(11 7 2)
'expect-value: 'no-more-alternatives

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j))
                    (* k k)))
        (list i j k)))))

(define (distinct l)
  (cond ((null? l) true)
        ((null? (cdr l)) true)
        ((member (car l) (cdr l)) false)
        (else (distinct (cdr l)))))

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require
     (distinct (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(begin
  (write-line (multiple-dwelling))
  (amb))
'expect-write: '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
'expect-value: 'no-more-alternatives
