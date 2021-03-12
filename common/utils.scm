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

(define (sign x)
  (cond ((positive? x) 1)
        ((negative? x) -1)
        (else 0)))

(define (compose . args)
  (compose* args))

(define (compose* args)
  (case (length args)
    ((0) (lambda (x) x))
    ((1) (car args))
    (else (reduce-right (lambda (f g)
                          (lambda (x) (f (g x))))
                        (lambda (x) x)
                        args))))

(define (~<? x y) (and (n:< x y) (not (~=? x y))))
(define (~>? x y) (and (n:> x y) (not (~=? x y))))
(define (~=? x y)
  (if (and (exact? x) (exact? y))
      (n:= x y)
      (close-enuf? x y default-equality-tolerance)))

(define default-equality-tolerance 1e-10)

(define (close-enuf? h1 h2 tolerance)
  (n:<= (n:magnitude (n:- h1 h2))
        (n:* .5
             (n:max tolerance flo:ulp-of-one)
             (n:+ (n:magnitude h1)
                  (n:magnitude h2)
                  2.))))

#|
;;; This is what flo:ulp-of-one means:

(define flo:ulp-of-one
  (let loop ((e 1.0))
     (if (= 1.0 (+ e 1.0))
         (* 2.0 e)
         (loop (/ e 2.0)))))
|#

;;;; List utilities

(define (all-permutations-of items)
  (guarantee n:list? items)
  (let loop ((items items))
    (if (n:pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (elementwise-lists-of lists)
  (guarantee-list-of n:non-empty-list? lists)
  (let loop ((lists lists))
    (if (n:pair? lists)
        (append-map (let ((tails (loop (cdr lists))))
                      (lambda (head)
                        (map (lambda (tail)
                               (cons head tail))
                             tails)))
                    (car lists))
        '(()))))

(define (partition-by-key = get-key objects)
  (let ((partitions '()))
    (for-each (lambda (object)
                (let ((key (get-key object)))
                  (let ((partition
                         (find (lambda (partition)
                                 (= (car partition) key))
                               partitions)))
                    (if partition
                        (set-cdr! partition
                                  (cons object (cdr partition)))
                        (set! partitions
                              (cons (list key object)
                                    partitions))))))
              objects)
    partitions))

(define (recursive-substitute new old thing)
  (let loop ((thing thing))
    (cond ((pair? thing)
           (cons (loop (car thing))
                 (loop (cdr thing))))
          ((eqv? old thing) new)
          (else thing))))

(define (subsuming-adjoiner set= set<=)
  (lambda (new-set current-sets)
    (if (any (lambda (current-set)
               (set<= current-set new-set))
             current-sets)
        current-sets
        (lset-adjoin set=
                     (lset-difference set=
                       current-sets
                       (filter (lambda (current-set)
                                 (set<= new-set current-set))
                               current-sets))
                     new-set))))

(define (sort-by lst compute-key)
  (map cdr
       (sort (map (lambda (thing)
                    (cons (compute-key thing) thing))
                  lst)
             (lambda (pair1 pair2)
               (n:< (car pair1) (car pair2))))))

(define (for-each-distinct-pair proc lst)
  (if (pair? lst)
      (let loop ((first (car lst)) (rest (cdr lst)))
        (for-each (lambda (other-element)
                    (proc first other-element))
                  rest)
        (if (pair? rest)
            (loop (car rest) (cdr rest))))))

(define (all-sequences-of arity zero one)
  (map (lambda (index)
         (index->choices index arity zero one))
       (iota (n:expt 2 arity))))

(define (index->choices index arity zero one)
  (let loop ((i 0) (index index) (choices '()))
    (if (n:< i arity)
        (loop (n:+ i 1)
              (quotient index 2)
              (cons (if (odd? index) one zero)
                    choices))
        choices)))

;;;; Property lists

(define (plist? object)
  (and (list? object)
       (even? (length object))))

(define (plist->alist plist)
  (guarantee plist? plist 'plist->alist)
  (let loop ((plist plist))
    (if (pair? plist)
        (cons (cons (car plist)
                    (cadr plist))
              (loop (cddr plist)))
        '())))

(define (alist->plist alist)
  (guarantee alist? alist 'alist->plist)
  (let loop ((alist alist))
    (if (pair? alist)
        (cons (car (car alist))
              (cons (cdr (car alist))
                    (loop (cdr alist))))
        '())))

(define (plist-value plist key)
  (define (loop plist)
    (if (pair? plist)
        (begin
          (if (not (pair? (cdr plist)))
              (lose))
          (if (eqv? (car plist) key)
              (car (cdr plist))
              (loop (cdr (cdr plist)))))
        (begin
          (if (not (null? plist))
              (lose))
          (default-object))))

  (define (lose)
    (error:not-a plist? plist 'plist-value))

  (loop plist))

;;; MIT/GNU Scheme implementation specific:

(define (make-bundle-predicate name)
  (let ((predicate (n:make-bundle-predicate name)))
    (register-predicate! predicate name)
    predicate))

(define (implementation-type-name object)
  (microcode-type/code->name (object-type object)))

(define microcode-type/code->name
  (access microcode-type/code->name (->environment '(runtime))))

(define (implementation-type-predicate name)
  (hash-table-intern! %implementation-type-predicates name
    (lambda ()
      (let ((code (microcode-type name)))
        (lambda (object)
          (object-type? code object))))))

(define %implementation-type-predicates
  (make-strong-eq-hash-table))

(define (has-implementation-value? name)
  (environment-bound? system-global-environment name))

(define (get-implementation-value name)
  (environment-lookup system-global-environment name))

(define (save-environment! name environment)
  ;; A hook to use if we want to keep track of
  ;; loaded environments.  Called by the loader.
  (declare (ignore environment))
  name)

(define (load-quietly . args)
  (parameterize ((param:suppress-loading-message? #t))
    (apply load args)))

;;;; Printing

(define (define-record-printer record-type get-parts
          #!optional get-name)
  (define-print-method (record-predicate record-type)
    (standard-print-method
        (if (default-object? get-name)
            (lambda (record)
              (record-type-name
               (record-type-descriptor record)))
            get-name)
      get-parts)))

(define (define-entity-printer record-type get-parts
          #!optional get-name)
  (define-print-method
    (let ((predicate (record-predicate record-type)))
      (lambda (object)
        (and (entity? object)
             (predicate (entity-extra object)))))
    (standard-print-method
        (if (default-object? get-name)
            get-name
            (lambda (record)
              (record-type-name
               (record-type-descriptor record))))
      get-parts)))

;;; This removes those annoying hash numbers after ;Value:
(set! repl:write-result-hash-numbers? #f)

;;; And this removes them from #[...] representations.
(param:print-hash-number-in-objects? #f)

(define (cpp x #!optional port)
  (let ((s
         (string-trim
          (call-with-output-string
            (lambda (port*)
              (parameterize ((param:pp-forced-x-size 65))
                (pp x port*))))))
        (port
         (if (default-object? port)
             (current-output-port)
             port)))
    (fresh-line port)
    (if (string-find-next-char s #\newline)
        (begin
          (display "#|\n" port)
          (display s port)
          (display "\n|#\n" port))
        (begin
          (display "#| " port)
          (display s port)
          (display " |#\n" port)))))

(define (pp-to-string object)
  (call-with-output-string
    (lambda (port)
      (pp object port))))
