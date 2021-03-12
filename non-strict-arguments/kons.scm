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

;;; kons -- the nonstrict version of CONS:

(define the-empty-stream '())
(define (empty-stream? thing) 
  (null? thing))

(define kons-registrations
  (make-key-weak-eqv-hash-table))

(define (kons (x lazy memo) (y lazy memo))
  (define (the-pair m)
    (cond ((eq? m 'kar) x)
          ((eq? m 'kdr) y)
          (else (error "Unknown message -- kons" m x y))))
  (hash-table-set! kons-registrations the-pair #t)
  the-pair)

(define (kar x)
  (x 'kar))

(define (kdr x)
  (x 'kdr))

(define (kons? object)
  (hash-table-exists? kons-registrations object))

;;; Streams

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (kons (+ (kar s1) (kar s2))
                (add-streams (kdr s1) (kdr s2))))))

(define (ref-stream stream n)
  (if (= n 0)
      (kar stream)
      (ref-stream (kdr stream) (- n 1))))

(define fibs
  (kons 0
        (kons 1
              (add-streams (kdr fibs)
                           fibs))))

(ref-stream fibs 10)
'expect-value: 55

(ref-stream fibs 100)
'expect-value: 354224848179261915075

(inexact
 (/ (ref-stream fibs 100)
    (ref-stream fibs 99)))
'expect-value: 1.618033988749895

(/ (+ 1 (sqrt 5)) 2)
'expect-value: 1.618033988749895

#|
;;; This fails, but

;; coderef: map-stream
(define (map-stream proc (items lazy memo))
  (if (empty-stream? items)
      items
      (kons (proc (kar items))
            (map-stream proc (kdr items)))))

;; coderef: scale-stream
(define (scale-stream items factor)
  (map-stream (lambda (x) (* x factor))
              items))

;; coderef: integral
(define (integral integrand initial-value dt)
  (define int
    (kons initial-value
          (add-streams (scale-stream integrand dt)
                       int)))
  int)

;; coderef: solve
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map-stream f y))
  y)

(ref-stream (solve (lambda (x) x) 1 0.001)
            1000)
|#
;Unbound variable: dy

;;; This works

(define (map-stream proc (items lazy memo))
  (cond ((empty-stream? items) items)
        ((kons? items)
         (kons (proc (kar items))
            (map-stream proc (kdr items))))
        (else 
         (error "Not a stream -- map" items))))


(define (scale-stream (items lazy memo) factor)
  (map-stream (lambda (x) (* x factor))
              items))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        ((kons? s1)
         (if (kons? s2)
             (kons (+ (kar s1) (kar s2))
                   (add-streams (kdr s1) (kdr s2)))
             (error "Not a stream -- add" s1 s2)))
        (else
         (error "Not a stream -- add" s1 s2))))

(define (ref-stream (stream lazy memo) n)
  (if (kons? stream)
      (if (= n 0)
          (kar stream)
          (ref-stream (kdr stream) (- n 1)))
      (error "Not a stream -- ref" stream)))

(define (integral (integrand lazy memo) initial-value dt)
  (define int
    (kons initial-value
          (add-streams (scale-stream integrand dt)
                       int)))
  int)

(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (map-stream f y))
  y)

(ref-stream (solve (lambda (x) x) 1 0.001)
            1000)
'expect-value: 2.716923932235896