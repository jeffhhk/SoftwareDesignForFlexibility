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

;;;; Change and changes

(define-record-type <change>
    (%make-change board piece flags)
    change?
  (board get-board)
  (piece get-piece)
  (flags get-flags))

(define (make-change board piece flags)
  (guarantee board? board)
  (guarantee piece? piece)
  (guarantee list-of-unique-symbols? flags)
  (%make-change board piece flags))

(define-record-printer <change>
  (lambda (change)
    (append (list (piece-coords (get-piece change)))
            (list (get-flags change)))))

(define (pmove? object)
  (and (pair? object)
       (list-of-type? object change?)))

(define (initial-pmove board piece)
  (list (make-change board piece '())))

(define (is-pmove-empty? pmove)
  (guarantee pmove? pmove)
  (null? (cdr pmove)))

(define (final-change pmove)
  (guarantee pmove? pmove)
  (car pmove))

(define (current-board pmove)
  (get-board (final-change pmove)))

(define (current-piece pmove)
  (get-piece (final-change pmove)))

(define (previous-pmove pmove)
  (guarantee pmove? pmove)
  (if (null? (cdr pmove))
      (error "No previous pmove:" pmove))
  (cdr pmove))

(define (add-change change pmove)
  (guarantee change? change)
  (guarantee pmove? pmove)
  (cons change pmove))

(define (pmove->list pmove)
  (guarantee pmove? pmove)
  (reverse pmove))

(define (update-piece updater pmove)
  (let* ((change (final-change pmove))
         (piece (get-piece change))
         (piece* (updater piece)))
    (add-change (make-change (board-replace-piece (get-board change)
                                                  piece
                                                  piece*)
                             piece*
                             (get-flags change))
                pmove)))

(define (remove-piece piece pmove)
  (add-change (let ((change (final-change pmove)))
                (make-change (board-remove-piece (get-board change) piece)
                             (get-piece change)
                             (get-flags change)))
              pmove))

(define (add-flag flag pmove)
  (add-change (let ((change (final-change pmove)))
                (make-change (get-board change)
                             (get-piece change)
                             (lset-adjoin eq? (get-flags change) flag)))
              pmove))

(define (is-flag-set? flag pmove)
  (memq flag (get-flags (final-change pmove))))

(define (get-final-board pmove)
  (board-end-turn (current-board pmove)))

(define (new-piece-position coords pmove)
  (update-piece (lambda (piece) (piece-move piece coords))
                pmove))

(define (new-piece-type type pmove)
  (update-piece (lambda (piece) (piece-new-type piece type))
                pmove))

(define (capture-piece-at coords pmove)
  (let ((board (current-board pmove)))
    (if (not (is-position-occupied-by-opponent? coords board))
        (error "No opponent piece at:" coords))
    (add-flag 'captures-pieces
              (remove-piece (board-get coords board)
                            pmove))))

(define (finish-move pmove)
  (add-flag 'move-is-finished pmove))

(define (is-pmove-finished? pmove)
  (is-flag-set? 'move-is-finished pmove))

(define (captures-pieces? pmove)
  (is-flag-set? 'captures-pieces pmove))

(define (get-piece-at coords pmove)
  (board-get coords (current-board pmove)))

(define (piece-has-type? type pmove)
  (eq? type (piece-type (current-piece pmove))))

(define (is-pmove-derived-from? pm1 pm2)
  (let loop ((pm1 pm1))
    (or (eq? pm1 pm2)
        (and (not (is-pmove-empty? pm1))
             (loop (previous-pmove pm1))))))

(define (compute-new-position direction distance pmove)
  (coords+ (piece-coords (current-piece pmove))
           (offset* direction distance)))