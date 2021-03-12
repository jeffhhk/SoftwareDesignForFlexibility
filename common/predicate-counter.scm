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

;;;; Log number of uses of registered predicates

(define %predicate-counts
  (make-parameter (make-key-weak-eqv-hash-table)))

(define (reset-predicate-counts!)
  (hash-table-clear! (%predicate-counts)))

(reset-predicate-counts!)

(define (increment-predicate-count! predicate)
  (hash-table-update! (%predicate-counts)
                      predicate
                      (lambda (count) (fix:+ count 1))
                      (lambda () 1)))

(define (get-predicate-count predicate)
  (hash-table-ref/default (%predicate-counts) predicate 0))

(define (get-predicate-counts)
  (hash-table->alist (%predicate-counts)))

(define (with-predicate-counts thunk)
  (parameterize ((%predicate-counts (make-key-weak-eqv-hash-table)))
    (let ((value (thunk)))
      (for-each (lambda (p)
                  (write-line (list (cdr p)
                                    (or (predicate-name (car p))
                                        (car p)))
                              (notification-output-port)))
                (get-predicate-counts))
      value)))
