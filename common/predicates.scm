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

;;;; Predicates

(define predicate?)
(define get-predicate-metadata)
(define set-predicate-metadata!)
(let ((association (make-metadata-association)))
  (set! predicate? (association 'has?))
  (set! get-predicate-metadata (association 'get))
  (set! set-predicate-metadata! (association 'put!)))

(define (guarantee predicate object #!optional caller)
  (if (not (predicate object))
      (error:not-a predicate object caller))
  object)

(define (error:not-a predicate object #!optional caller)
  (error:wrong-type-argument object
                             (predicate-description predicate)
                             caller))

(define (guarantee-list-of predicate object #!optional caller)
  (if (not (list-of-type? object predicate))
      (error:not-a-list-of predicate object caller))
  object)

(define (error:not-a-list-of predicate object #!optional caller)
  (error:wrong-type-argument object
                             (string-append
                              "list of "
                              (predicate-description predicate))
                             caller))

(define (predicate-description predicate)
  (if (predicate? predicate)
      (object->description (predicate-name predicate))
      (string-append "object satisfying "
                     (object->description predicate))))

(define (object->description object)
  (call-with-output-string
    (lambda (port)
      (write object port))))

(define (is-list-of predicate)
  (guarantee predicate? predicate)
  (register-compound-predicate! (lambda (object)
                                  (and (n:list? object)
                                       (every predicate object)))
                                'is-list-of
                                (list predicate)))

(define (is-non-empty-list-of predicate)
  (guarantee predicate? predicate)
  (register-compound-predicate! (lambda (object)
                                  (and (n:pair? object)
                                       (n:list? (cdr object))
                                       (every predicate object)))
                                'is-non-empty-list-of
                                (list predicate)))

(define (is-pair-of car-predicate cdr-predicate)
  (guarantee predicate? car-predicate)
  (guarantee predicate? cdr-predicate)
  (register-compound-predicate!
   (lambda (object)
     (and (n:pair? object)
          (car-predicate (car object))
          (cdr-predicate (cdr object))))
   'is-pair-of
   (list car-predicate cdr-predicate)))

(define (complement predicate)
  (maybe-register-compound-predicate!
   (lambda (object)
     (not (predicate object)))
   'complement
   (list predicate)))

(define (disjoin . predicates)
  (disjoin* predicates))

(define (disjoin* predicates)
  (maybe-register-compound-predicate!
   (lambda (object)
     (any (lambda (predicate)
            (predicate object))
          predicates))
   'disjoin
   predicates))

(define (conjoin . predicates)
  (conjoin* predicates))

(define (conjoin* predicates)
  (maybe-register-compound-predicate!
   (lambda (object)
     (every (lambda (predicate)
              (predicate object))
            predicates))
   'conjoin
   predicates))

(define (maybe-register-compound-predicate! datum-test
                                            operator operands)
  (if (every predicate? operands)
      (register-compound-predicate! datum-test operator operands)
      datum-test))

(define (equality-predicate-maker name =)
  (lambda (object)
    (let ((predicate
           (lambda (object*)
             (= object object*))))
      (register-predicate! predicate (list name =))
      predicate)))

(define eq-predicate
  (equality-predicate-maker 'eq? eq?))
(define eqv-predicate
  (equality-predicate-maker 'eqv? eqv?))
(define equal-predicate
  (equality-predicate-maker 'equal? equal?))