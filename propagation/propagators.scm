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

;;; This file is part of the Layered Propagator Prototype.  It is
;;; (by now) loosely derived from the Artistic Propagator
;;; Prototype previously developed by Alexey Radul and Gerald Jay
;;; Sussman.

;;; Construction of propagators.  A propagator is represented by
;;;  a message-acceptor.  A propagator has a parent, of which it
;;;  is a part.  There is a top-level parent of the sytem.  The
;;;  parameter *my-parent* is defined in hierarchy.scm

;;; Here we have removed lots of code that really belongs to the
;;; layered generic arithmetic.  The tracking of the dependencies
;;; for multiplication, division, etc. should not be part of this
;;; propagator-system code.  The deletions have gone to extra.

(define (propagator inputs outputs activate! name)
  (guarantee-list-of cell? inputs 'propagator)
  (guarantee-list-of cell? outputs 'propagator)
  (let ((relations (make-relations name (*my-parent*))))

    (define (get-inputs) inputs)
    (define (get-outputs) outputs)
    (define (get-relations) relations)

    (define (summarize-self)
      (list (get-name me)))

    (define me
      (bundle propagator? activate!
              get-inputs get-outputs get-relations
              summarize-self))

    (add-child! me (*my-parent*))
    (for-each (lambda (cell)
                (add-cell-neighbor! cell me))
              inputs)
    (alert-propagator! me)
    me))

(define propagator?
  (make-bundle-predicate 'propagator))
(set-predicate<=! propagator? relatable?)

(define (propagator-inputs propagator)
  (propagator 'get-inputs))

(define (propagator-outputs propagator)
  (propagator 'get-outputs))

(define (activate-propagator! propagator)
  (parameterize ((current-reason-source propagator))
    (propagator 'activate!)))

;;; A primitive propagator is constructed from a Scheme function.
;;; * f is the function that computes the output value from some
;;;   given input values.
;;; * activation-policy decides when the inputs justify running
;;;   the function.

(define (primitive-propagator f name)
  (lambda cells
    (let ((output (car (last-pair cells)))
          (inputs (except-last-pair cells)))
      (propagator inputs (list output)
        (lambda ()
          (let ((input-values (map cell-strongest inputs)))
            ;; All inputs must have values.
            (if (any unusable-value? input-values)
                'do-nothing
                (add-cell-content! output
                  (apply f input-values)))))
        name))))

;;; Compound propagators must build themselves.
;;;  Policy is build if there is a usable input.

(define (compound-propagator inputs outputs to-build name)
  (let ((built? #f))

    (define (maybe-build)
      (if (or built?
              (and (not (null? inputs))
                   (every unusable-value?
                          (map cell-strongest inputs))))
          'do-nothing
          (begin
            ;; TODO: consider passing the parent into the
            ;; builder.
            (parameterize ((*my-parent* me))
              (to-build))
            (set! built? #t)
            'built)))

    (define me
      (propagator inputs outputs maybe-build name))
    me))
