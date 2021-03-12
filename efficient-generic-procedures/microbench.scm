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

(define (run-test test)
  (test)                                ;warm up
  (let loop ((n 3) (time 0))
    (if (= n 0)
        (/ time 3.)
        (begin
          (gc-flip)
          (let ((increment))
            (with-timings test
              (lambda (scheme-time gc-time real-time)
                (set! increment scheme-time)
                (write-line (list scheme-time gc-time real-time))))
            (loop (- n 1)
                  (+ time increment)))))))

(define (run-arith-test arithmetic test)
  (with-arithmetic arithmetic (lambda () (run-test test))))

(define (microbench-full-generic-arith dispatcher)
  (let ((g (make-generic-arithmetic dispatcher)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
    g))

(define default-microbench-full-generic-arith
  (microbench-full-generic-arith make-default-dispatch-store))

(define (test-one-arg)
  (let ((g ((generic-procedure-constructor make-default-dispatch-store) 'g 1 #f)))
    (define-generic-procedure-handler g (match-args number?)
      (lambda (x)
        x))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 1000000))
      (g 13))))

(define (test-small-+)
  (let ((g ((generic-procedure-constructor make-default-dispatch-store) 'g 2 #f)))
    (define-generic-procedure-handler g (match-args number? number?)
      (lambda (x y)
        (fix:+ x y)))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 1000000))
      (g 13 17))))

(define (test-full-+)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i 1000000))
    (+ 13 17)))

(define (test-stormer-crunch)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (let ((e (evolver F 'h stormer-2)))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 100000))
      (x 0 (e numeric-s0 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (test-fib)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i 100))
    (fib 20)))

(define (test-fib-counts)
  (with-predicate-counts (lambda () (fib 20))))

(define (memoized-register-compound-predicate! predicate type components)
  (let ((key (cons type (map predicate-name components))))
    (hash-table-intern! memoized-compound-predicate-keys
                        key
                        (lambda ()
                          (register-predicate! predicate key)
                          predicate))))

(define memoized-compound-predicate-keys
  (make-equal-hash-table))

;; (set! register-compound-predicate! memoized-register-compound-predicate!)

#|
;; Rule lists
((#[compound-procedure 96] . 54727)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 95 symbolic?] . 109453)
 (#[compound-procedure 94] . 54727))

;; Tries (filtered)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compound-procedure 82] . 54727)
 (#[compound-procedure 81] . 54727)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 80 symbolic?] . 109453))

;; Tries (searched)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compound-procedure 102] . 54727)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 104] . 54727)
 (#[compound-procedure 103 symbolic?] . 109453))

;; Cache
((#[compound-procedure 90] . 4)
 (#[compound-procedure 89 symbolic?] . 7)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 7)
 (#[compound-procedure 88] . 4)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 7))
|#

(define (test-stormer-counts)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
   (lambda ()
     (x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

#|
;; Rule lists
((#[compound-procedure 96] . 8)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 18)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 18)
 (#[compound-procedure 95 symbolic?] . 25)
 (#[compound-procedure 94] . 13))

;; Tries (filtered)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 28)
 (#[compound-procedure 82] . 13)
 (#[compound-procedure 81] . 16)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 25)
 (#[compound-procedure 80 symbolic?] . 28))

;; Tries (searched)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 18)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 18)
 (#[compound-procedure 104] . 16)
 (#[compound-procedure 103 symbolic?] . 26)
 (#[compound-procedure 102] . 8))

;; Cache
((#[compound-procedure 90] . 12)
 (#[compound-procedure 89 symbolic?] . 19)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 19)
 (#[compound-procedure 88] . 9)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 16))
|#