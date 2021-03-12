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

;;;; Boards

(define-record-type <board>
    (%make-board game pieces current-color-index)
    board?
  (game board-game)
  (pieces board-pieces)
  (current-color-index current-color-index))

(define (make-board game)
  (%make-board game ((initial-pieces-generator game) game) 0))

(define (update-board board pieces current-color-index)
  (%make-board (board-game board)
               pieces
               current-color-index))

(define (board-width board)
  (game-width (board-game board)))

(define (board-depth board)
  (game-depth (board-game board)))

(define (board-colors board)
  (game-colors (board-game board)))

(define (current-color board)
  (list-ref (board-colors board)
            (current-color-index board)))

(define (summarize-location board coords)
  ((piece-summarizer (board-game board)) (board-get coords board)))

(define (generate-legal-moves board)
  ((moves-generator (board-game board)) board))

(define (board-max-row board)
  (- (board-depth board) 1))

(define (board-max-column board)
  (- (board-width board) 1))

(define (is-position-on-board? coords board)
  (and (<= 0 (get-row coords) (board-max-row board))
       (<= 0 (get-column coords) (board-max-column board))))

(define (current-pieces board)
  (filter (lambda (piece)
            (eq? (piece-color piece) (current-color board)))
          (board-pieces board)))

(define (board-get coords board)
  (find (lambda (piece)
          (coords=? (piece-coords piece)
                    (if (eq? (piece-color piece) (current-color board))
                        coords
                        (flip-coords board coords))))
        (board-pieces board)))

(define (flip-coords board coords)
  (make-coords (- (board-max-column board) (get-column coords))
               (- (board-max-row board) (get-row coords))))

(define (position-info coords board)
  (let ((piece (board-get coords board)))
    (if piece
        (if (eq? (piece-color piece) (current-color board))
            'occupied-by-self
            'occupied-by-opponent)
        'unoccupied)))

(define (is-position-unoccupied? coords board)
  (eq? 'unoccupied (position-info coords board)))

(define (is-position-occupied-by-self? coords board)
  (eq? 'occupied-by-self (position-info coords board)))

(define (is-position-occupied-by-opponent? coords board)
  (eq? 'occupied-by-opponent (position-info coords board)))

(define (board-end-turn board)
  (update-board board
                (board-pieces board)
                (modulo (+ (current-color-index board) 1) 2)))

(define (board-replace-piece board from to)
  (guarantee-piece-on-board from board)
  (if (not (memq (piece-color to) (board-colors board)))
      (error "Piece has unknown color:" to board))
  (if (let ((piece (board-get (piece-coords to) board)))
        (and piece
             (not (piece=? from piece))))
      (error "Piece can't be placed in occupied position:" to board))
  (update-board board
                (map (lambda (piece)
                       (if (piece=? piece from)
                           to
                           piece))
                     (board-pieces board))
                (current-color-index board)))

(define (board-remove-piece board piece)
  (guarantee-piece-on-board piece board)
  (update-board board
                (%delete-piece piece (board-pieces board))
                (current-color-index board)))

(define (%delete-piece piece pieces)
  (let loop ((pieces pieces))
    (if (pair? pieces)
        (if (piece=? piece (car pieces))
            (loop (cdr pieces))
            (cons (car pieces) (loop (cdr pieces))))
        '())))

(define (guarantee-piece-on-board piece board)
  (if (not (any (lambda (piece*)
                  (piece=? piece* piece))
                (board-pieces board)))
      (error "Piece not found on board:" piece board)))