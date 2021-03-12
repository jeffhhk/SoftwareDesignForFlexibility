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

;;; Elementary Newton root finder

(define (root-newton f initial-guess tolerance)
  (let ((Df (derivative f)))
    (define (improve-guess xn)
      (- xn (/ (f xn) (Df xn))))
    (let loop ((xn initial-guess))
      (let ((xn+1 (improve-guess xn)))
        (if (close-enuf? xn xn+1 tolerance)
            xn+1
            (loop xn+1))))))


#|
;;; Limiting iterations if not converging
(define (root-newton f initial-guess tolerance #!optional maxiter)
  (let ((Df (derivative f)) 
        (maxiter (if (default-object? maxiter) 10 maxiter)))
    (define (improve-guess xn)
      (- xn (/ (f xn) (Df xn))))
    (let loop ((xn initial-guess) (n 0))
      (let ((xn+1 (improve-guess xn)))
        (if (close-enuf? xn xn+1 tolerance)
            xn+1
            (if (> n maxiter)
                #f
                (loop xn+1 (+ n 1))))))))
|#

;;; Example

(define (cs theta)
  (- (cos theta) (sin theta)))

(root-newton cs 0.5 1e-8)
'expect .7853981633974484
