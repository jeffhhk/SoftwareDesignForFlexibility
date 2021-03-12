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

;;;; Applicability

;;; An applicability attribute is a list of lists, representing
;;; an OR of some per-argument ANDs.

(define (applicability? object)
  (and (n:list? object)
       (every (lambda (pattern)
                (and (n:list? pattern)
                     (every procedure? pattern)))
              object)
       (or (not (n:pair? object))
           (let ((arity (length (car object))))
             (every (lambda (pattern)
                      (n:= arity (length pattern)))
                    (cdr object))))))

(define (applicability-arity applicability)
  (guarantee applicability? applicability)
  (if (n:pair? applicability)
      (length (car applicability))
      0))

(define (is-applicable? applicability args)
  (any (lambda (and-clause)
         (predicates-match? and-clause args))
       applicability))

(define (predicates-match? predicates args)
  (and (n:= (length predicates) (length args))
       (every (lambda (predicate arg)
                (increment-predicate-count! predicate)
                (predicate arg))
              predicates
              args)))

(define (match-args . predicates)
  (list predicates))

(define (all-args arity predicate)
  (list (make-list arity predicate)))

(define (any-arg arity predicate base-predicate)
  (if (n:= 0 arity)
      (list)
      (cdr (all-sequences-of arity base-predicate predicate))))

(define (applicability-union . applicabilities)
  (applicability-union* applicabilities))

(define (applicability-union* applicabilities)
  (apply lset-union equal? applicabilities))
