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

;;;; Cells

(define (make-cell name)
  (let ((relations (make-relations name (*my-parent*)))
        (neighbors '())
        (content the-nothing)
        (strongest the-nothing)
        (probe #f))                     ;for debugging

    (define (get-relations) relations)
    (define (get-neighbors) neighbors)
    (define (get-content) content)
    (define (get-strongest) strongest)

    (define (add-neighbor! neighbor)
      (set! neighbors (lset-adjoin eq? neighbors neighbor)))

    ;; coderef: add-content!
    (define (add-content! increment)
      (let ((content* (cell-merge content increment)))
        (if probe
            (probe (get-name me) content increment content*))
        (set! content content*))
      (test-content!))

    ;; coderef: test-content!
    (define (test-content!)
      (let ((strongest* (strongest-value content)))
        (cond ((equivalent? strongest strongest*)
               (set! strongest strongest*)
               'content-unchanged)
              ((general-contradiction? strongest*)
               (set! strongest strongest*)
               (handle-cell-contradiction me)
               'contradiction)
              (else
               (set! strongest strongest*)
               (alert-propagators! neighbors)
               'content-changed))))

    (define (probe! new-probe)
      (set! probe new-probe))

    (define (summarize-self)
      (list (get-name me)))

    (define me
      (bundle cell?
              get-relations get-neighbors get-content
              get-strongest add-neighbor! add-content!
              test-content! probe! summarize-self))

    (add-child! me (*my-parent*))
    (set! *all-cells* (cons me *all-cells*))
    me))

(define cell? (make-bundle-predicate 'cell))
(set-predicate<=! cell? relatable?)

(define cell-merge)

(define (add-cell-neighbor! cell neighbor)
  (cell 'add-neighbor! neighbor))

(define (cell-content cell)
  (cell 'get-content))

(define (cell-strongest cell)
  (cell 'get-strongest))

(define (add-cell-content! cell increment)
  (parameterize ((current-reason-source cell))
    (cell 'add-content! increment)))

(define (test-cell-content! cell)
  (cell 'test-content!))

(define (set-cell-probe! cell probe)
  (cell 'probe! probe)
  `(probing ,(get-name cell)))

(define (clear-cell-probe! cell)
  (cell 'probe! #f)
  `(clearing probe ,(get-name cell)))

(define simple-probe
  (lambda (name content increment content*)
    (pp (list name content increment content*))))

;;; Special cell values

(define the-nothing
  (list 'the-nothing))

(define (nothing? thing)
  (eq? thing the-nothing))

(define-generic-procedure-handler unusable-value?
  (match-args nothing?)
  (lambda (thing)
    (declare (ignore thing))
    'nothing))

(define the-contradiction
  (list 'the-contradiction))

(define (contradiction? thing)
  (eq? thing the-contradiction))

(define-generic-procedure-handler unusable-value?
  (match-args contradiction?)
  (lambda (thing)
    (declare (ignore thing))
    'contradiction))

(define (general-contradiction? thing)
  (eq? 'contradiction (unusable-value? thing)))