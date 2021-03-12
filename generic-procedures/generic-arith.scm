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

;;;; Generic arithmetic

(define (make-generic-arithmetic dispatch-store-maker)
  (make-arithmetic 'generic any-object? '()
    constant-union
    (let ((make-generic-procedure
           (generic-procedure-constructor dispatch-store-maker)))
      (lambda (operator)
        (simple-operation operator
                          any-object?
                          (make-generic-procedure
                           operator
                           (operator-arity operator)
                           #f))))))

(define (add-to-generic-arithmetic! generic-arithmetic
                                    arithmetic)
  (add-generic-arith-constants! generic-arithmetic arithmetic)
  (add-generic-arith-operations! generic-arithmetic arithmetic))

(define (add-generic-arith-constants! generic-arithmetic
                                      arithmetic)
  ;; TODO: We have a choice here: do we merge constants with
  ;; non-standard names into the generic arithmetic?  For now, we
  ;; will ignore such constants.
  (for-each
   (lambda (name)
     (let ((binding
            (arithmetic-constant-binding name
                                         generic-arithmetic))
           (element (find-arithmetic-constant name arithmetic)))
       (set-cdr! binding
                 (constant-union name
                                 (cdr binding)
                                 element))))
   (arithmetic-constant-names generic-arithmetic)))

(define (add-generic-arith-operations! generic-arithmetic
                                       arithmetic)
  (for-each
   (lambda (operator)
     (let ((generic-procedure
            (simple-operation-procedure
             (arithmetic-operation operator
                                   generic-arithmetic)))
           (operation
            (arithmetic-operation operator arithmetic)))
       (define-generic-procedure-handler
           generic-procedure
           (operation-applicability operation)
           (operation-procedure operation))))
   (arithmetic-operators arithmetic)))

(define (extend-generic-arithmetic! generic-arithmetic extender)
  (add-to-generic-arithmetic! generic-arithmetic
                              (extender generic-arithmetic)))

(define-generic-procedure-extractor 'arithmetic-overrides
  (lambda (object)
    (and (arithmetic-procedure? object)
         (arithmetic-procedure-metadata object))))

(define-generic-procedure-extractor 'arithmetic-by-name
  (lambda (object)
    (and (symbol? object)
         *current-arithmetic*
         (let ((operation
                (find-arithmetic-operation
                 object
                 *current-arithmetic*)))
           (and operation
                (operation-procedure operation))))))