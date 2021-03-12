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

;;;; Memoizers

(define (make-list-memoizer make-list= dedup?)
  (lambda (elt= get-key get-datum)
    (let ((table (make-memoizer-table make-list= elt=)))
      (lambda (list)
        (let ((list
               (if dedup?
                   (delete-duplicates list elt=)
                   list)))
          (hash-table-intern! table
                              (get-key list)
                              (lambda () (get-datum list))))))))

(define (make-memoizer-table make-list= elt=)
  (cond ((eqv? eq? elt=)
         (make-hash-table (make-list= eq?)
                          (make-list-hash eq-hash)
                          'rehash-after-gc? #t))
        ((eqv? eqv? elt=)
         (make-hash-table (make-list= eqv?)
                          (make-list-hash eqv-hash)
                          'rehash-after-gc? #t))
        ((eqv? equal? elt=)
         (make-hash-table (make-list= eqv?)
                          (make-list-hash equal-hash)
                          'rehash-after-gc? #t))
        (else
         (error "Don't know how to memoize this:" elt=))))

(define (make-list= elt=)
  (define (list= a b)
    (if (n:pair? a)
        (and (n:pair? b)
             (elt= (car a) (car b))
             (list= (cdr a) (cdr b)))
        (not (n:pair? b))))
  list=)

(define (make-lset= elt=)
  (define (list= a b)
    (lset= elt= a b))
  list=)

(define (make-list-hash elt-hash)
  (define (list-hash list #!optional modulus)
    (let ((hash
           (apply n:+
                  (map (lambda (elt)
                         (elt-hash elt))
                       list))))
      (if (default-object? modulus)
          hash
          (modulo hash modulus))))
  list-hash)

(define list-memoizer (make-list-memoizer make-list= #f))
(define lset-memoizer (make-list-memoizer make-lset= #t))

(define (make-simple-list-memoizer list-memoizer)
  (lambda (elt= get-key get-datum)
    (let ((memoizer
           (list-memoizer elt=
                          (lambda (args)
                            (apply get-key args))
                          (lambda (args)
                            (apply get-datum args)))))
      (lambda args
        (memoizer args)))))

(define simple-list-memoizer
  (make-simple-list-memoizer list-memoizer))

(define simple-lset-memoizer
  (make-simple-list-memoizer lset-memoizer))

;;; This is intended to weakly match a list of items, where each
;;; item is distinguished by eqv?, and ideally where the items
;;; themselves are held weakly.  This is kind of difficult to do
;;; without doing a bunch of implementation-specific hacking, so
;;; for now this is implemented as a strong hash.
(define (memoize-multi-arg-eqv procedure)
  (simple-list-memoizer eqv? list procedure))

(define (memoize-multi-arg-equal procedure)
  (simple-list-memoizer equal? list procedure))
