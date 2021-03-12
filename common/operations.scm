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

;;;; Operation abstraction

(define (operation? object)
  (and (n:list? object)
       (n:= 4 (length object))
       (eq? 'operation (car object))
       (operator? (cadr object))
       (applicability? (caddr object))
       (procedure? (cadddr object))))

(define (make-operation operator applicability procedure)
  (list 'operation operator applicability procedure))

;;; API
(define (operation-operator operation)
  (cadr operation))

;;; API
(define (operation-applicability operation)
  (caddr operation))

;;; API
(define (operation-procedure operation)
  (cadddr operation))

;;; API
(define (apply-operation operation args)
  (apply (operation-procedure operation) args))

;;; API
(define (make-installable-operation-procedure procedure
                                              new-procedure)
  (declare (ignore procedure))
  new-procedure)

;;; API
(define (operation-components operation)
  (list operation))

;;; API
(define (constant-union name . constants)
  (declare (ignore name))
  (let ((unique
         (remove default-object?
                 (delete-duplicates constants eqv?))))
    (if (n:pair? unique)
        (car unique)
        (default-object))))

;;; API
(define (operation-union operator . operations)
  (operation-union* operator operations))

;;; API
(define (operation-union* operator operations)
  (make-operation operator
                  (applicability-union*
                   (map operation-applicability operations))
                  (lambda args
                    (operation-union-dispatch operator
                                              operations
                                              args))))

;; helper to make book description clearer
(define (operation-union-dispatch operator operations args)
  (let ((operation
         (find (lambda (operation)
                 (is-operation-applicable? operation args))
               operations)))
    (if (not operation)
        (error "Inapplicable operation:" operator args))
    (apply-operation operation args)))

;; helper to make book description clearer
(define (is-operation-applicable? operation args)
  (is-applicable? (operation-applicability operation) args))

;;; API
(define (simple-operation operator predicate procedure)
  (make-operation operator
                  (all-args (operator-arity operator)
                            predicate)
                  procedure))

;;; API
(define (simple-operation-procedure operation)
  (operation-procedure operation))

;;; API
(define (transform-operation-procedure procedure operation)
  (make-operation (operation-operator operation)
                  (operation-applicability operation)
                  (procedure (operation-procedure operation))))