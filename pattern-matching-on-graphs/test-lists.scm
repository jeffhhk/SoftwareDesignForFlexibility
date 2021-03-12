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

(define-test 'greedy-graph
  (lambda ()
    (let ((graph (list->graph '(a b c))))
      (test-graph graph #f)
      (assert-true (g:null? (g:cdr (g:cdr (g:cdr graph))))))))

(define-test 'lazy-graph
  (lambda ()
    (let ((graph (list->lazy-graph '(a b c))))
      (test-graph graph #t)
      (assert-true (g:null? (g:cdr (g:cdr (g:cdr graph))))))))

(define-test 'extensible-lazy-graph
  (lambda ()
    (let ((graph (list->extensible-lazy-graph '(a b c))))
      (test-graph graph #t)
      (g:append! graph (list->extensible-lazy-graph '(d e)))
      (assert-eqv (g:cdr (g:cdr (g:cdr graph)))
                  (g:cdr (g:cdr (g:cdr graph))))
      (assert-eqv 'd (g:car (g:cdr (g:cdr (g:cdr graph)))))
      (assert-eqv (g:cdr (g:cdr (g:cdr (g:cdr graph))))
                  (g:cdr (g:cdr (g:cdr (g:cdr graph)))))
      (assert-eqv (g:cdr (g:cdr (g:cdr (g:cdr graph))))
                  (g:last-pair graph))
      (assert-eqv 'e (g:car (g:cdr (g:cdr (g:cdr (g:cdr graph))))))
      (assert-eqv 'e (g:last graph)))))

(define (test-graph graph lazy?)
  (assert-eqv 'a (g:car graph))
  (if lazy?
      (assert-false ((graph 'get-edge 'cdr) 'forced?)))
  (assert-eqv (g:cdr graph)
              (g:cdr graph))
  (if lazy?
      (assert-true ((graph 'get-edge 'cdr) 'forced?)))
  (assert-eqv 'b (g:car (g:cdr graph)))
  (if lazy?
      (assert-false (((g:cdr graph) 'get-edge 'cdr) 'forced?)))
  (assert-eqv (g:cdr (g:cdr graph))
              (g:cdr (g:cdr graph)))
  (if lazy?
      (assert-true (((g:cdr graph) 'get-edge 'cdr) 'forced?)))
  (assert-eqv 'c (g:car (g:cdr (g:cdr graph))))
  (assert-eqv 'c (g:last graph)))