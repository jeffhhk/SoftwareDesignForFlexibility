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

;;;; Propagator-specific support layer

(define (get-support object)
  (support-layer-value (strongest-value object)))

(define-generic-procedure-handler g:equivalent?
  (any-arg 2 layered-datum? any-object?)
  (lambda (n1 n2)
    (and (equivalent? (get-base-value n1)
                      (get-base-value n2))
         (support-set= (get-support n1)
                       (get-support n2)))))

(define-generic-procedure-handler unusable-value?
  (match-args layered-datum?)
  (lambda (elt)
    (or (unusable-value? (get-base-value elt))
        (and (support-set-any premise-out?
                              (support-layer-value elt))
             'unsupported))))

;; coderef: strongest-value-layered
(define-generic-procedure-handler strongest-value
  (match-args layered-datum?)
  (lambda (elt)
    (if (all-premises-in? (support-layer-value elt))
        elt
        the-nothing)))

(define (support:merge merged-value content increment)
  (cond ((equivalent? merged-value (base-layer-value content))
         (support-layer-value content))
        ((equivalent? merged-value
                      (base-layer-value increment))
         (support-layer-value increment))
        (else
         (support-set-union (support-layer-value content)
                            (support-layer-value increment)))))

;; coderef: merge-layered-support
(define-layered-procedure-handler merge-layered support-layer
  support:merge)