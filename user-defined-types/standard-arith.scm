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

;;;; Symbolic arithmetic

(define symbolic-template
  (make-predicate-template 'symbolic '((? base))
                           tagging-strategy:always
                           (lambda (get-tag) any-object?)))

(define make-symbolic-predicate
  (predicate-template-instantiator symbolic-template))

(define symbolic-predicate?
  (predicate-template-predicate symbolic-template))

(define symbolic-predicate-base
  (predicate-template-accessor 'base symbolic-template))

(define (symbolic-constructor base-predicate)
  (predicate-constructor
   (make-symbolic-predicate base-predicate)))

(define (make-symbolic-arithmetic base-arithmetic)
  (let* ((base-domain
          (arithmetic-domain-predicate base-arithmetic))
         (domain
          (extend-predicate make-symbolic-predicate base-domain)))
    (make-arithmetic 'symbolic
                     domain
                     (list base-arithmetic)
      (lambda (name base-constant)
        base-constant)
      (lambda (operator base-operation)
        (extend-operation-function operator base-operation
          (lambda (base-operation)
            (let ((codomain
                   (extend-operation-codomain
                      make-symbolic-predicate
                      base-operation)))
              (make-simple-operation
               operator
               (extend-operation-domains make-symbolic-predicate
                                         base-operation)
               codomain
               (let ((arg-preds
                      (map make-symbolic-predicate
                           (operation-domains base-operation)))
                     (tagger (predicate-constructor codomain)))
                 (lambda args
                   (if (any (lambda (arg-pred arg)
                              (arg-pred arg))
                            arg-preds
                            args)
                       (tagger (cons operator args))
                       (apply base-operation args))))))))))))

;;;; Function arithmetic

(define (make-endo-function-arithmetic base-arithmetic)
  (make-function-arithmetic
   (list (arithmetic-domain-predicate base-arithmetic))
   base-arithmetic))

(define (make-function-arithmetic domains base-arithmetic)
  (let* ((make-function-signature
          (lambda (codomain)
            (make-function-predicate domains codomain)))
         (make-function
          (lambda (name codomain procedure)
            (make-simple-function
               name
               (make-function-signature codomain)
               procedure)))
         (base-predicate
          (arithmetic-domain-predicate base-arithmetic))
         (pure-function-predicate
          (make-function-signature base-predicate)))
    (make-arithmetic 'function
                     (extend-predicate make-function-signature
                                       base-predicate)
                     (list base-arithmetic)
      (lambda (name base-constant)
        base-constant)
      (lambda (operator base-operation)
        (extend-operation-function operator base-operation
          (lambda (base-operation)
            (make-simple-operation
               operator
               (extend-operation-domains make-function-signature
                                         base-operation)
               (extend-operation-codomain make-function-signature
                                          base-operation)
               (lambda operation-args
                 (make-function
                    operator
                    (operation-codomain base-operation)
                    (lambda args
                      (apply base-operation
                             (map (lambda (operation-arg)
                                    (if (pure-function-predicate
                                           operation-arg)
                                        (apply-function
                                           operation-arg
                                           args)
                                        operation-arg))
                                  operation-args))))))))))))

(define (get-object-name object)
  (if (function? object)
      (function-name object)
      (strip-tags object)))

(define (expand-disjunct-predicates predicates)
  (let loop ((predicates predicates))
    (if (pair? predicates)
        (append-map (lambda (tail)
                      (map (lambda (head)
                             (cons head tail))
                           (expand-disjunct-predicate
                            (car predicates))))
                    (loop (cdr predicates)))
        (list '()))))

(define (expand-disjunct-predicate predicate)
  (if (disjunction? predicate)
      (append-map expand-disjunct-predicate
                  (compound-predicate-components predicate))
      (list predicate)))