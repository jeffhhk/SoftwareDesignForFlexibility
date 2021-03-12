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

;;; Some elementary unit conversions

(define (degrees->radians degrees)
  (* (/ :2pi 360) degrees))

(define (radians->degrees radians)
  (* (/ 360 :2pi) radians))


(define (xms->x xms)
  (+ (car xms)
     (/ (cadr xms) 60)
     (/ (caddr xms) 3600)))

(define (x->xms x)
  (let* ((d (truncate x))
         (dd (- x d))
         (m (truncate (* 60 dd)))
         (ddm (- dd (/ m 60)))
         (s (* 3600 ddm)))
    (list d m s)))

(define dms->d xms->x)
(define d->dms x->xms)

(define (dms->radians dms)
  (degrees->radians (dms->d dms)))

(define (radians->dms radians)
  (d->dms (radians->degrees radians)))


(define (hours->radians hours)
  (* (/ :2pi 24) hours))

(define (radians->hours radians)
  (* (/ 24 :2pi) radians))


(define hms->h xms->x)
(define h->hms x->xms)

(define (hms->radians hms)
  (* 15 (dms->radians hms)))

(define (radians->hms radians)
  (radians->dms (/ radians 15)))