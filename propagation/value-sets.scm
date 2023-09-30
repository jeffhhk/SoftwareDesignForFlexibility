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

;;;; Sets of values

;;; The set elements must be distinct according to equivalent?
(define-record-type <value-set>
    (%make-value-set elements)
    value-set?
  (elements value-set-elements))
(register-predicate! value-set? 'value-set)

(define (make-value-set elements)
  (guarantee list? elements 'make-value-set)
  (%make-value-set (remove nothing? elements)))

(define (value-set . elements)
  (%make-value-set (remove nothing? elements)))

(define (->value-set value)
  (if (value-set? value)
      value
      (value-set value)))

(define-generic-procedure-handler get-base-value
  (match-args value-set?)
  (lambda (set) (get-base-value (strongest-consequence set))))

(define-generic-procedure-handler unusable-value?
  (match-args value-set?)
  (lambda (set) (unusable-value? (strongest-consequence set))))

;; coderef: strongest-value-values
(define-generic-procedure-handler strongest-value
  (match-args value-set?)
  (lambda (set) (strongest-consequence set)))

(define (map-value-set procedure . sets)
  (make-value-set
   (apply map
          procedure
          (map value-set-elements sets))))

;;;; Merge

(define (merge-value-sets content increment)
  (if (nothing? increment)
      (->value-set content)
      (value-set-adjoin (->value-set content) increment)))

(define (value-set-adjoin set elt)
  (if (any (lambda (old-elt)
             (element-subsumes? old-elt elt))
           (value-set-elements set))
      set
      (make-value-set
       (lset-adjoin equivalent?
                    (remove (lambda (old-elt)
                              (element-subsumes? elt old-elt))
                            (value-set-elements set))
                    ;; This seems to work as well.  Why?
                    ;; (value-set-elements set)
                    elt))))

(define (element-subsumes? elt1 elt2)
  (and (value-implies? (base-layer-value elt1)
                       (base-layer-value elt2))
       (support-set<= (support-layer-value elt1)
                      (support-layer-value elt2))))

(define (strongest-consequence set)
  (fold (lambda (increment content)
          (merge-layered content increment))
        the-nothing
        (filter (lambda (elt)
                  (all-premises-in? (support-layer-value elt)))
                (value-set-elements set))))