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

(define (tester graph)
  (lambda (pattern)
    (let ((dict (graph-match pattern (match:new-dict) graph)))
      (and dict
           (match:dict->procedure dict)))))

(define-test 'simple-patterns
  (lambda ()
    (let* ((l (list->graph '(a b c)))
           (cdr1 (g:cdr l))
           (cdr2 (g:cdr cdr1))
           (cdr3 (g:cdr cdr2)))
      (let ((t (tester l)))
        (let ((result (t '((?) (* cdr (?))))))
          (assert-eqv '() (result #f)))
        (let ((result (t '((?) (* cdr (?* x))))))
          (assert-equal (list cdr1 cdr2 cdr3)
                        (result 'x)))
        (let ((result (t '((?) (* cdr (?* x)) car (? y)))))
          (assert-equal (list cdr1 cdr2)
                        (result 'x))
          (assert-eqv 'c (result 'y)))
        (let ((result
               (t
                '((?)
                  (and (cdr (? x) car b)
                       (cdr (? y) cdr (? z) car c))))))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv cdr1 (result 'y))
          (assert-eqv cdr2 (result 'z)))
        (let ((result
               (t
                '((?)
                  (and (cdr (? x) car b)
                       (cdr (? x) cdr (? z) car c))))))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv cdr2 (result 'z)))
        (let ((result
               (t
                '((?)
                  (and (cdr (? x) car b)
                       (cdr (? x) cdr (? z)))
                  car (? w)))))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv cdr2 (result 'z))
          (assert-eqv 'c (result 'w)))
        ;; Continued on next page

        ;; Continuation from previous page
        (let ((result
               (t
                '((?)
                  (or (cdr (? x))
                      (cdr (? x) cdr (? z)))
                  car (? w)))))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv #f (result 'z))
          (assert-eqv 'b (result 'w)))
        (let ((result
               (t
                '((?)
                  (or (cdr (? x) cdr (? z))
                      (cdr (? x)))
                  car (? w)))))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv cdr2 (result 'z))
          (assert-eqv 'c (result 'w)))))))

(define-test 'backtracking
  (lambda ()
    (let* ((l (list->graph '(a b c)))
           (cdr1 (g:cdr l))
           (cdr2 (g:cdr cdr1))
           (cdr3 (g:cdr cdr2)))
      (let ((results '()))
        ;; Can backtrack to get all possible answers:
        ((gmatch:compile-path '((?)
                                (or (cdr (? x) cdr (? z))
                                    (cdr (? x)))
                                car (? w)))
         l
         (match:new-dict)
         (lambda (object* dict*)
           (set! results
                 (cons (match:dict->procedure dict*) results))
           #f))
        (set! results (reverse results))
        (assert-eqv 2 (length results))
        (let ((result (car results)))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv cdr2 (result 'z))
          (assert-eqv 'c (result 'w)))
        (let ((result (cadr results)))
          (assert-eqv cdr1 (result 'x))
          (assert-eqv #f (result 'z))
          (assert-eqv 'b (result 'w)))))))
