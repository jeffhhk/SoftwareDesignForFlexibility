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

;;;; Collections

(define (make-weak-eq-set)
  (let ((elements '()))

    (define (get-elements)
      (weak-list->list elements))

    (define (has-element? element)
      (if (weak-memq element elements) #t #f))

    (define (add-element! element)
      (if (not (weak-memq element elements))
          (set! elements (weak-cons element elements))))

    (lambda (operator)
      (case operator
        ((get-elements) get-elements)
        ((has-element?) has-element?)
        ((add-element!) add-element!)
        (else (error "Unknown operator:" operator))))))

(define (make-alist-store key=?)
  (let ((alist '()))

    (define (get-keys)
      (map car alist))

    (define (has? key)
      (any (lambda (p)
             (key=? (car p) key))
           alist))

    (define (get key)
      (let ((p
             (find (lambda (p)
                     (key=? (car p) key))
                   alist)))
        (if (not p)
            (error "Unknown key:" key))
        (cdr p)))

    (define (get-matching predicate)
      (filter-map (lambda (p)
                    (and (predicate (car p))
                         (cdr p)))
                  alist))

    (define (put! key datum)
      (set! alist
            (cons (cons key datum)
                  (remove! (lambda (p)
                             (key=? (car p) key))
                           alist)))
      key)

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((get-matching) get-matching)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-weak-alist-store key=?)
  (let ((alist '()))

    (define (traverse procedure final)
      (let loop ((ps alist) (prev #f))
        (if (pair? ps)
            (let ((key (weak-car (car ps))))
              (if (weak-pair/car? (car ps))
                  (procedure (car ps)
                             (lambda () (loop (cdr ps) ps)))
                  (begin
                    (if prev
                        (set-cdr! prev (cdr ps))
                        (set! alist (cdr ps)))
                    (loop (cdr ps) prev))))
            (final))))

    (define (get-keys)
      (traverse (lambda (p recur)
                  (cons (weak-car p) (recur)))
                (lambda () '())))

    (define (has? key)
      (traverse (lambda (p recur)
                  (or (key=? (weak-car p) key)
                      (recur)))
                (lambda () #f)))

    (define (get key)
      (let ((p
             (traverse (lambda (p recur)
                         (if (key=? (weak-car p) key)
                             p
                             (recur)))
                       (lambda () #f))))
        (if (not p)
            (error "Unknown key:" key))
        (weak-cdr p)))

    (define (get-matching predicate)
      (traverse (lambda (p recur)
                  (if (predicate (weak-car p))
                      (cons (weak-cdr p) (recur))
                      (recur)))
                (lambda () '())))

    (define (put! key datum)
      (traverse (lambda (p recur)
                  (if (key=? (weak-car p) key)
                      (weak-set-cdr! p datum)
                      (recur)))
                (lambda ()
                  (set! alist
                        (cons (weak-cons key datum)
                              alist))))
      key)

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((get-matching) get-matching)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-hash-table-store make-table)
  (let ((table (make-table)))

    (define (get-keys)
      (hash-table-keys table))

    (define (has? key)
      (hash-table-exists? table key))

    (define (get key)
      (hash-table-ref table key))

    (define (put! key metadata)
      (hash-table-set! table key metadata))

    (lambda (operator)
      (case operator
        ((get-keys) get-keys)
        ((has?) has?)
        ((get) get)
        ((put!) put!)
        (else (error "Unknown operator:" operator))))))

(define (make-metadata-association)
  (let* ((store
          (make-hash-table-store make-key-weak-eqv-hash-table))
         (base-has? (store 'has?))
         (base-get (store 'get))
         (base-put! (store 'put!)))

    (define (put! key metadata)
      (if (base-has? key)
          (let ((metadata* (base-get key)))
            (if (not (eqv? metadata* metadata))
                (error "Can't change metadata for:"
                       key metadata metadata*))))
      (base-put! key metadata))

    (lambda (operator)
      (case operator
        ((put!) put!)
        (else (store operator))))))