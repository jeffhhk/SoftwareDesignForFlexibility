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

;;;; Standard predicates

;;; This extends base-predicate to work for tagged data.  The
;;; constructor only wraps the data when the implementation tag
;;; is different.

(define (primitive-predicate name data-test)
  (if (not (predicate? data-test))
      (register-predicate! data-test (symbol 'n: name)))
  (let ((predicate
         (make-simple-predicate name data-test
                                tagging-strategy:optional)))
    (set-predicate<=! data-test predicate)
    predicate))

(define boolean?
  (primitive-predicate 'boolean n:boolean?))

(define complex?
  (primitive-predicate 'complex n:complex?))

(define exact-integer?
  (primitive-predicate 'exact-integer n:exact-integer?))

(define exact-rational?
  (primitive-predicate 'exact-rational n:exact-rational?))

(define inexact-real?
  (primitive-predicate 'inexact-real flo:flonum?))

(define integer?
  (primitive-predicate 'integer n:integer?))

(define null?
  (primitive-predicate 'null n:null?))

(define number?
  (primitive-predicate 'number n:number?))

(define pair?
  (primitive-predicate 'pair n:pair?))

(define rational?
  (primitive-predicate 'rational n:rational?))

(define real?
  (primitive-predicate 'real n:real?))

(define string?
  (primitive-predicate 'string n:string?))

(define symbol?
  (primitive-predicate 'symbol n:symbol?))

(define vector?
  (primitive-predicate 'vector n:vector?))

(define exact-nonnegative-integer?
  (primitive-predicate 'exact-nonnegative-integer
                       n:exact-nonnegative-integer?))

(define exact-positive-integer?
  (primitive-predicate 'exact-positive-integer
                       n:exact-positive-integer?))

(define list?
  (primitive-predicate 'list n:list?))

(define non-empty-list?
  (primitive-predicate 'non-empty-list n:non-empty-list?))

(set-predicate<=! complex? number?)
(set-predicate<=! exact-integer? integer?)
(set-predicate<=! exact-nonnegative-integer? exact-integer?)
(set-predicate<=! exact-positive-integer? exact-integer?)
(set-predicate<=! exact-rational? rational?)
(set-predicate<=! inexact-real? real?)
(set-predicate<=! integer? rational?)
(set-predicate<=! non-empty-list? list?)
(set-predicate<=! null? list?)
(set-predicate<=! rational? real?)
(set-predicate<=! real? complex?)

(register-predicate! procedure? 'procedure)

;;;; Implementation tags

(define implementation-tag
  (let ((boolean-tag (predicate->tag boolean?))
        (null-tag (predicate->tag null?)))
    (lambda (object)
      (cond ((eq? object #t) boolean-tag)
            ((eq? object '()) null-tag)
            (else
             (let ((name (implementation-type-name object)))
               (hash-table-intern! %object-tag-map name
                 (lambda ()
                   (predicate->tag
                    (register-predicate!
                     (implementation-type-predicate name)
                     name))))))))))

(define (%predefine-tags predicate name . type-names)
  (declare (ignore name))
  (for-each (lambda (type-name)
              (hash-table-set! %object-tag-map
                               type-name
                               (predicate->tag predicate)))
            type-names))

;;; MIT/GNU Scheme specific:

(define %object-tag-map
  (make-key-weak-eqv-hash-table))

(%predefine-tags boolean? 'boolean 'false)
(%predefine-tags complex? 'complex 'recnum)
(%predefine-tags exact-integer? 'exact-integer 'bignum 'fixnum)
(%predefine-tags exact-rational? 'exact-rational 'ratnum)
(%predefine-tags inexact-real? 'real 'flonum)
(%predefine-tags pair? 'pair 'pair)
(%predefine-tags procedure? 'procedure
                 'extended-procedure 'procedure 'entity
                 'primitive 'compiled-entry)
(%predefine-tags string? 'string 'string)
(%predefine-tags symbol? 'symbol
                 'interned-symbol 'uninterned-symbol)
(%predefine-tags vector? 'vector 'vector)

;;; End MIT/GNU Scheme specific