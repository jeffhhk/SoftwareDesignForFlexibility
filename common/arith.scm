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

;;;; Arithmetic abstraction

(define-record-type <arithmetic>
    (%make-arithmetic name bases domain-predicate constant-alist
                      operation-alist)
    arithmetic?
  (name arithmetic-name)
  (bases arithmetic-bases)
  (domain-predicate arithmetic-domain-predicate)
  (constant-alist arithmetic-constant-alist)
  (operation-alist arithmetic-operation-alist))

(define (make-arithmetic name
                         domain-predicate
                         bases
                         get-constant
                         get-operation)
  (guarantee predicate? domain-predicate)
  (guarantee-list-of arithmetic? bases)
  (%make-arithmetic
   (cons name (map arithmetic-name bases))
   bases
   domain-predicate
   ;; TODO(cph): Eliding these calls when the number of results
   ;; doesn't equal the number of bases is arbitrary and should
   ;; be reconsidered.
   (filter-map (lambda (name)
                 (let ((base-constants
                        (arithmetic-constants-for name bases)))
                   (and (n:= (length bases)
                             (length base-constants))
                        (cons name
                              (apply get-constant
                                     name
                                     base-constants)))))
               (arithmetic-constant-names-for bases))
   (filter-map (lambda (operator)
                 (let ((base-operations
                        (arithmetic-operations-for operator
                                                   bases)))
                   (and (n:= (length bases)
                             (length base-operations))
                        (cons operator
                              (apply get-operation
                                     operator
                                     base-operations)))))
               (arithmetic-operators-for bases))))

(define (arithmetic-constant-names-for bases)
  (if (n:pair? bases)
      (apply lset-union eq?
             (map arithmetic-constant-names bases))
      (constant-names)))

(define (arithmetic-constants-for name bases)
  (remove default-object?
          (map (lambda (base)
                 (find-arithmetic-constant name base))
               bases)))

(define (arithmetic-operators-for bases)
  (if (n:pair? bases)
      (apply lset-union eq?
             (map arithmetic-operators bases))
      (operator-names)))

(define (arithmetic-operations-for operator bases)
  (filter-map (lambda (base)
                (find-arithmetic-operation operator base))
              bases))

(define (arithmetic-constant-names arithmetic)
  (map car (arithmetic-constant-alist arithmetic)))

(define (arithmetic-constant name arithmetic)
  (let ((constant (find-arithmetic-constant name arithmetic)))
    (if (default-object? constant)
        (error "Unknown constant name:" name arithmetic))
    constant))

;; For use only by generic arithmetic.
(define (arithmetic-constant-binding name arithmetic)
  (let ((binding
         (assq name (arithmetic-constant-alist arithmetic))))
    (if (not binding)
        (error "Unknown constant name:" name arithmetic))
    binding))

(define (find-arithmetic-constant name arithmetic)
  (let ((p (assq name (arithmetic-constant-alist arithmetic))))
    (if p
        (cdr p)
        (default-object))))

(define (arithmetic-operators arithmetic)
  (map car (arithmetic-operation-alist arithmetic)))

(define (arithmetic-operation operator arithmetic)
  (let ((operation
         (find-arithmetic-operation operator arithmetic)))
    (if (not operation)
        (error "Unknown operator:" operator))
    operation))

(define (arithmetic-procedure operator arithmetic)
  (operation-procedure
   (arithmetic-operation operator arithmetic)))

(define (find-arithmetic-operation operator arithmetic)
  (let ((p (assq operator
                 (arithmetic-operation-alist arithmetic))))
    (and p
         (cdr p))))

(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))

(define (add-arithmetics* arithmetics)
  (if (n:null? (cdr arithmetics))
      (car arithmetics)
      (make-arithmetic 'add
                       (disjoin*
                        (map arithmetic-domain-predicate
                             arithmetics))
                       arithmetics
                       constant-union
                       operation-union)))

(define (extend-arithmetic extender base-arithmetic)
  (add-arithmetics base-arithmetic (extender base-arithmetic)))

;;;; Installation

(define *current-arithmetic* #f)

(define (install-arithmetic! arithmetic)
  (set! *current-arithmetic* arithmetic)
  (install-package! (arithmetic->package arithmetic)))

(define (with-arithmetic arithmetic thunk)
  (with-installed-package! (arithmetic->package arithmetic)
                           thunk))

(define (arithmetic->package arithmetic)
  (make-package (arithmetic-name arithmetic)
    (arithmetic->bindings arithmetic
                          (+-like '+ 'additive-identity)
                          (--like '- 'negate)
                          (+-like '* 'multiplicative-identity)
                          (--like '/ 'invert)
                          (comparator '<)
                          (comparator '=)
                          (comparator '>)
                          (comparator '<=)
                          (comparator '>=)
                          (min-like 'min)
                          (min-like 'max))))

(define (arithmetic->bindings arithmetic . modifications)
  (let ((overrides
         (filter-map (lambda (modification)
                       (and modification
                            (modification arithmetic)))
                     modifications)))
    (map (lambda (operator)
           (cons operator
                 (make-installable-procedure operator
                                             arithmetic
                                             overrides)))
         (filter operator-installable?
                 (arithmetic-operators arithmetic)))))

(define arithmetic-procedure?)
(define arithmetic-procedure-metadata)
(define set-arithmetic-procedure-metadata!)
(let ((association (make-metadata-association)))
  (set! arithmetic-procedure? (association 'has?))
  (set! arithmetic-procedure-metadata (association 'get))
  (set! set-arithmetic-procedure-metadata! (association 'put!)))

(define (make-installable-procedure operator arithmetic
                                    overrides)
  (let* ((operation
          (arithmetic-operation operator arithmetic))
         (procedure (operation-procedure operation)))
    (let ((override
           (and (not (eqv? (get-implementation-value operator)
                           procedure))
                (assq operator overrides))))
      (if override
          (let ((procedure*
                 (make-installable-operation-procedure
                  procedure
                  (cdr override))))
            (set-arithmetic-procedure-metadata! procedure*
                                                procedure)
            procedure*)
          procedure))))

(define (+-like operator identity-name)
  (lambda (arithmetic)
    (let ((binary-operation
           (find-arithmetic-operation operator arithmetic)))
      (and binary-operation
           (let ((binary
                  (operation-procedure binary-operation))
                 (get-identity
                  (identity-name->getter identity-name
                                         arithmetic)))
             (cons operator
                   (lambda args
                     (case (length args)
                       ((0) (get-identity))
                       ((1) (car args))
                       (else (pairwise binary args))))))))))

(define (identity-name->getter identity arithmetic)
  (let ((constant
         (find-arithmetic-constant identity arithmetic)))
    (if (default-object? constant)
        (lambda ()
          (error "No identity for this arithmetic:" identity))
        (lambda ()
          constant))))

(define (--like operator inversion-operator)
  (lambda (arithmetic)
    (let ((binary-operation
           (find-arithmetic-operation operator arithmetic))
          (unary-operation
           (find-arithmetic-operation inversion-operator
                                      arithmetic)))
      (and binary-operation
           unary-operation
           (let ((binary
                  (operation-procedure binary-operation))
                 (unary
                  (operation-procedure unary-operation)))
             (cons operator
                   (lambda (arg . args)
                     (if (n:null? args)
                         (unary arg)
                         (pairwise binary
                                   (cons arg args))))))))))

(define (comparator operator)
  (lambda (arithmetic)
    (let ((operation
           (find-arithmetic-operation operator arithmetic)))
      (and operation
           (let ((binary
                  (operation-procedure operation)))
             (cons operator
                   (lambda args
                     (or (n:< (length args) 2)
                         (let loop ((args args))
                           (and (binary (car args) (cadr args))
                                (or (not (n:pair? (cddr args)))
                                    (loop (cdr args)))))))))))))

(define (min-like operator)
  (lambda (arithmetic)
    (let ((operation
           (find-arithmetic-operation operator arithmetic)))
      (and operation
           (let ((binary (operation-procedure operation)))
             (cons operator
                   (lambda (arg . args)
                     (if (n:null? args)
                         arg
                         (pairwise binary
                                   (cons arg args))))))))))

(define (pairwise binary args)
  (let loop
      ((args (cddr args))
       (result (binary (car args) (cadr args))))
    (if (n:null? args)
        result
        (loop (cdr args)
              (binary result (car args))))))