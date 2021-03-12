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

;;;; Game REPL

(define try-history)

(define (new-game initial-board)
  (set! try-history '())
  (new-board initial-board))

(define (new-board board)
  (set! try-history (cons board try-history))
  (restart-board))

(define (undo)
  (if (null? (cdr try-history))
      (error "No further undo history"))
  (set! try-history (cdr try-history))
  (restart-board))

(define (restart-board)
  (let ((board (car try-history)))
    (newline)
    (show-board board)
    (newline)
    (let ((moves (generate-legal-moves board)))
      (for-each (lambda (i move)
                  (write i)
                  (write-char #\space)
                  (write (summarize-move move))
                  (newline))
                (iota (length moves))
                moves)
      (if (= 1 (length moves))
          (begin
            (write-string "auto move 0")
            (newline)
            (move 0))
          (current-color board)))))

(define (move n)
  (let ((board (car try-history)))
    (let ((moves (generate-legal-moves board)))
      (if (not (and (exact-nonnegative-integer? n)
                    (< n (length moves))))
          (error "Bad move:" n))
      (new-board (get-final-board (list-ref moves n))))))

;;; Consider merging this with the one in graph-api/code/chess-board.scm.

(define (show-board board)
  (for-each (lambda (y)
              (write-char #\|)
              (for-each (lambda (x)
                          (write-string
                           (summarize-location board (make-coords x y)))
                          (write-char #\|))
                        (iota (board-width board)))
              (newline))
            (reverse (iota (board-depth board)))))