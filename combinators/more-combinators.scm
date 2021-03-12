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

;;; An unoptimal answer to an exercise

(define (curry-arguments . target-indices)
  (lambda (f)
    (let ((n (get-arity f))
          (m (length target-indices)))
      (define (the-combination . oargs)
        (assert (= (length oargs) (- n m)))
        (lambda nargs
          (assert (= (length nargs) m))
          (let lp ((is target-indices)
                   (args oargs)
                   (nargs nargs))
            (if (null? is)
                (apply f args)
                (lp (cdr is)
                    (list-insert args
                                 (car is)
                                 (car nargs))
                    (cdr nargs))))))
      (restrict-arity the-combination (- n m)))))

#|
((((curry-arguments 0 2)
   (lambda (x y z w)
     (list 'foo x y z w)))
  'a 'b)
 'c 'd)
;Value: (foo c a d b)
|#

(define (fan-out-argument . target-indices)
  (lambda (f)
    (let ((n (get-arity f))
          (m (length target-indices)))
      (define (the-combination . oargs)
        (assert (= (length oargs) (- n m)))
        (lambda (x)
          (let lp ((is target-indices)
                   (args oargs))
            (if (null? is)
                (apply f args)
                (lp (cdr is)
                    (list-insert args
                                 (car is)
                                 x))))))
      (restrict-arity the-combination (- n m)))))

#|
((((fan-out-argument 0 2)
   (lambda (x y z w)
     (list 'foo x y z w)))
  'a 'b)
 'c)
;Value: (foo c a c b)
|#
