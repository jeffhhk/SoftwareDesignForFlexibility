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

(define all-tests (make-settable-parameter '()))
(define *test-file* (make-unsettable-parameter #f))
(define *test-name* (make-unsettable-parameter #f))
(define *test-index* (make-settable-parameter #f))
(define *test-failed?* (make-settable-parameter #f))

(define (load-test-file filename show-tests? debug-errors? env)
  (run-tests filename
             show-tests?
             debug-errors?
             (parameterize ((all-tests '()))
               (load-quietly filename env)
               (all-tests))))

(define-record-type <test>
    (make-test name run)
    test?
  (name test-name)
  (run test-run))

(define (define-test name run)
  (let ((existing-test
         (find (lambda (test)
                 (eq? name (test-name test)))
               (all-tests))))
    (if existing-test
        (all-tests (delq! existing-test (all-tests)))))
  (all-tests (append! (all-tests) (list (make-test name run)))))

(define (define-arith-test name get-arith body)
  (define-test name
    (lambda ()
      (let ((old (or *current-arithmetic* numeric-arithmetic)))
        (dynamic-wind
            (lambda ()
              (install-arithmetic! (get-arith)))
            body
            (lambda ()
              (install-arithmetic! old)))))))

(define (run-tests filename show-tests? debug-errors? tests)
  (let ((failed 0))
    (for-each (lambda (test)
                (parameterize ((*test-file* filename)
                               (*test-name* (test-name test))
                               (*test-index* 0)
                               (*test-failed?* #f))
                  (if show-tests?
                      (begin
                        (fresh-line)
                        (display "Running test: ")
                        (write (*test-name*))
                        (display " from file ")
                        (write (->namestring filename))
                        (fresh-line)))
                  (let ((result (ignore-errors (test-run test))))
                    (if (condition? result)
                        (begin
                          (*test-failed?* #t)
                          (if debug-errors?
                              (standard-error-handler result)))))
                  (if (*test-failed?*)
                      (set! failed (n:+ failed 1)))))
              tests)
    (values failed (length tests))))

(define (increment-index)
  (*test-index* (n:+ (*test-index*) 1)))

(define (fail)
  (*test-failed?* #t)
  (warn "test failure:"
        (*test-name*)
        (*test-index*)
        (->namestring (*test-file*))))

(define (assert-predicate-satisfied predicate value)
  (increment-index)
  (if (not (predicate value))
      (begin
        (pp (list 'expected-to-satisfy predicate 'actual value))
        (fail))))

(define (assert-false value)
  (assert-eqv #f value))

(define (assert-true value)
  (assert-not-eqv #f value))

(define (make-equality-asserter =)
  (lambda (o1 o2)
    (increment-index)
    (if (not (= o1 o2))
        (begin
          (pp (list 'expected o1 'actual o2 'equality =))
          (fail)))))

(define (make-inequality-asserter =)
  (lambda (o1 o2)
    (increment-index)
    (if (= o1 o2)
        (begin
          (pp (list 'expected-not-equal o1 'actual o2 'equality =))
          (fail)))))

(define assert-eqv (make-equality-asserter eqv?))
(define assert-not-eqv (make-inequality-asserter eqv?))

(define assert-equal (make-equality-asserter equal*?))
(define assert-not-equal (make-inequality-asserter equal*?))

(define (assert-close z1 tolerance z2)
  (increment-index)
  (if (not (close-enuf? z1 z2 tolerance))
      (begin
        (pp (list 'expected z1 'actual z2 'tolerance tolerance))
        (fail))))

(define (assert-lset= = o1 o2)
  (increment-index)
  (if (not (lset= = o1 o2))
      (begin
        (pp (list 'expected o1 'actual o2))
        (fail))))

(define (assert-one-of values actual)
  (increment-index)
  (if (not (member actual values))
      (begin
        (pp (list 'expected-one-of values 'actual actual))
        (fail))))

(define (assert-contains-exactly values actual)
  (assert-lset= equal? values actual))

(define (assert-error thunk)
  (increment-index)
  (let ((result (ignore-errors thunk)))
    (if (not (condition? result))
        (begin
          (pp (list 'expected-error-but-got-value result))
          (fail)))))

(define (assert-error-message message-fragment thunk)
  (increment-index)
  (let ((result (ignore-errors thunk)))
    (cond ((not (condition? result))
           (pp (list 'expected-a-condition 'actual result))
           (fail))
          ((not (substring? message-fragment (condition/report-string result)))
           (pp (list 'expected-message message-fragment
                     'actual-message (condition/report-string result)))
           (fail)))))

(define (assert-lset-satisfies predicates objects)
  (guarantee-list-of procedure? predicates)
  (assert-true (list? objects))
  (assert-eqv (length predicates) (length objects))
  (for-each (lambda (predicate)
              (assert-eqv 1 (count predicate objects)))
            predicates))
