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

;;;; Coordinates

(define (make-coords column row)
  (cons column row))

(define (get-column coords)
  (car coords))

(define (get-row coords)
  (cdr coords))

(define (coords=? c1 c2)
  (and (= (get-column c1) (get-column c2))
       (= (get-row c1) (get-row c2))))

#|;;; Used only to define piece<? in piece.scm
(define (coords<? c1 c2)
  (or (< (get-column c1) (get-column c2))
      (and (= (get-column c1) (get-column c2))
           (< (get-row c1) (get-row c2)))))
|#

(define (coords+ c1 c2)
  (make-coords (+ (get-column c1) (get-column c2))
               (+ (get-row c1) (get-row c2))))

(define (coords- c1 c2)
  (make-coords (- (get-column c1) (get-column c2))
               (- (get-row c1) (get-row c2))))

(define (offset* offset scale)
  (make-coords (* (get-column offset) scale)
               (* (get-row offset) scale)))

(define (offset/ offset scale)
  (make-coords (/ (get-column offset) scale)
               (/ (get-row offset) scale)))

(define (offset->direction offset)
  (make-coords (sign (get-column offset))
               (sign (get-row offset))))

(define forward-direction
  (make-coords 0 1))

(define backward-direction
  (make-coords 0 -1))

(define left-direction
  (make-coords -1 0))

(define right-direction
  (make-coords 1 0))

(define forward-diagonal-directions
  (list (make-coords 1 1)
        (make-coords -1 1)))

(define backward-diagonal-directions
  (list (make-coords 1 -1)
        (make-coords -1 -1)))

(define diagonal-directions
  (append forward-diagonal-directions
          backward-diagonal-directions))