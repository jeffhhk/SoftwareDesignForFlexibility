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

;;;;                The Interpreter

(define (generate-moves-using-rule-interpreter board)
  (execute-rules (map (lambda (piece)
                        (initial-pmove board piece))
                      (current-pieces board))
                 (get-evolution-rules (board-game board))
                 (get-aggregate-rules (board-game board))))

(define (execute-rules initial-pmoves evolution-rules
                       aggregate-rules)
  ((reduce compose (lambda (x) x) aggregate-rules)
   (append-map (lambda (pmove)
                 (evolve-pmove pmove evolution-rules))
               initial-pmoves)))

(define (evolve-pmove-from-text pmove evolution-rules)
  (append-map (lambda (new-pmove)
                (if (is-pmove-finished? new-pmove)
                    (list new-pmove)
                    (evolve-pmove new-pmove evolution-rules)))
              (append-map (lambda (evolution-rule)
                            (evolution-rule pmove))
                          evolution-rules)))

(define (evolve-pmove pmove evolution-rules)
  (append-map (lambda (new-pmove)
                (if (is-pmove-finished? new-pmove)
                    (list new-pmove)
                    (evolve-pmove new-pmove evolution-rules)))
              (append-map (lambda (evolution-rule)
                            (apply-evolution-rule evolution-rule
                                                  pmove))
                          evolution-rules)))

(define (apply-evolution-rule evolution-rule pmove)
  (guarantee-list-of (lambda (pmove*)
                       (and (pmove? pmove*)
                            (is-pmove-derived-from? pmove*
                                                    pmove)))
                     (evolution-rule pmove)))