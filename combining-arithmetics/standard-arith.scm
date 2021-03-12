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

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
(register-predicate! symbolic? 'symbolic)

(define (symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
           (arithmetic-domain-predicate  base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 symbolic?
                                 base-predicate)
                        (lambda args (cons operator args)))))))

;;;; Function arithmetic

(define (function? object)
  (and (procedure? object)
       (not (bundle? object))))
(register-predicate! function? 'function)

(define (function-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
      (lambda (name codomain-constant)
        codomain-constant)
      (lambda (operator codomain-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (lambda args
              (apply-operation codomain-operation
                               (map (lambda (thing)
                                      (if (function? thing)
                                          (apply thing args)
                                          thing))
                                    things)))))))))

;;;; Book examples

(define (make-arithmetic-1 name get-operation)
  (make-arithmetic name any-object? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (simple-operation operator
                        any-object?
                        (get-operation operator)))))

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
    (lambda (operator)
      (lambda args (cons operator args)))))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(define (literal-function name)
  (lambda args
    (cons name args)))
