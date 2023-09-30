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

;;;; Data modelling

(define get-base-value
  (simple-generic-procedure 'get-base-value 1 base-layer-value))

(define (equivalent? object1 object2)
  (or (eqv? object1 object2)
      (g:equivalent? object1 object2)))

(define g:equivalent?
  (simple-generic-procedure 'equivalent? 2
    (lambda (x y)
      (declare (ignore x y))
      #f)))

(define-generic-procedure-handler g:equivalent?
  (match-args n:number? n:number?)
  ~=?)

(define value-implies?
  (simple-generic-procedure 'value-implies? 2 eqv?))

#|
;;; Redundant code that causes trouble...  
;;; See common/utils.scm

(define (close-enuf? h1 h2 #!optional ulps)
  (let ((ulps (if (default-object? ulps) 100 ulps)))
    (<= (magnitude (- h1 h2))
        (* .5 (* ulps microcode-id/floating-epsilon)
           (+ (magnitude h1) (magnitude h2) 2.0)))))

(define (n:equivalent? x y)
  (or (eqv? x y)
      (close-enuf? x y)))

(define-generic-procedure-handler value-implies?
  (match-args n:number? n:number?)
  n:equivalent?)
|#

(define-generic-procedure-handler value-implies?
  (match-args n:number? n:number?)
  ~=?)

(define unusable-value?
  (simple-generic-procedure 'unusable-value? 1
    (constant-generic-procedure-handler #f)))

(define strongest-value
  (simple-generic-procedure 'strongest-value 1
    (lambda (object) object)))

;; Arguments:
;; * CONTENT is the current content of the cell
;; * INCREMENT is an incremental update to the cell
;;
;; MERGE returns either a contradiction or a merged value.

(define merge
  (simple-generic-procedure 'merge 2
    (lambda (content increment)
      (cond ((nothing? content) increment)
            ((nothing? increment) content)
            ((contradiction? content) content)
            ((contradiction? increment) increment)
            ((equivalent? content increment) content)
            (else the-contradiction)))))

(define merge-layered
  (make-layered-procedure 'merge 2 merge))

;; Like merge but only on added layers.
(define (merge-metadata content increment)
  (declare (ignore increment))
  content)

;; Like merge-layered but only on added layers.
(define merge-metadata-layered
  (make-layered-procedure 'merge-metadata 2 merge-metadata))