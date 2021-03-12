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
  (function? object))
(register-predicate! operation? 'operation)

(define (make-operation operator function)
  (guarantee symbol? operator)
  (guarantee function? function)
  function)

;;; API
(define (operation-procedure operation)
  (guarantee operation? operation)
  operation)

;;; API
(define (operation-applicability operation)
  (map simple-function-domains (function-components operation)))

;;; API
(define (apply-operation operation args)
  (apply-function operation args))

;;; API
(define (make-installable-operation-procedure function
                                              applicator)
  (make-object-applicable installable-operation-procedure?
                          function
                          applicator))

(define (installable-operation-procedure? object)
  (and (applicable-object? object)
       (function? (applicable-object->object object))))
(register-predicate! installable-operation-procedure?
                     'installable-operation-procedure)

(define-generic-procedure-handler value-restriction
  (match-args installable-operation-procedure? predicate?)
  (lambda (value predicate)
    (value-fit (applicable-object->object value) predicate)))

;;; API
(define (operation-components operation)
  (function-components operation))

;;; API
(define (constant-union name . constants)
  (object-union* constants))

;;; API
(define (operation-union operator . operations)
  (operation-union* operator operations))

;;; API
(define (operation-union* operator operations)
  (union-function* operations))

;;; API
(define (simple-operation operator predicate procedure)
  (make-simple-function operator
                        (call-with-values
                            (lambda ()
                              (operator-signature operator predicate))
                          make-function-predicate)
                        procedure))

;;; API
(define (simple-operation-procedure operation)
  (simple-function-procedure operation))

;;; API
(define (transform-operation-procedure procedure operation)
  (make-operation (operation-operator operation)
                  (operation-applicability operation)
                  (procedure (operation-procedure operation))))

;;;; Local extensions to API

(define (extend-operation-function operator base-operation extender)
  (union-function base-operation
                  (extender base-operation)))

(define (make-simple-operation operator domains codomain
                               procedure)
  (make-simple-function operator
                        (make-function-predicate domains
                                                 codomain)
                        procedure))

(define (operation-domains operation)
  (function-predicate-domains (function-predicate operation)))

(define (operation-codomain operation)
  (function-predicate-codomain (function-predicate operation)))

(define (extend-operation-domains extender operation)
  (map (lambda (domain)
         (extend-predicate extender domain))
       (operation-domains operation)))

(define (extend-operation-codomain extender operation)
  (extender (operation-codomain operation)))

(define (extend-predicate extender predicate)
  (disjoin predicate
           (extender predicate)))

(define (extend-constant extender constant)
  (object-union constant
                (extender constant)))