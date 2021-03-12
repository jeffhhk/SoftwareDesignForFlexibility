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

(define chess
  (make-game 8 8 '(white black) '(pawn rook knight bishop queen king)
             chess-initial-pieces
             generate-moves-using-rule-interpreter
             chess-piece-summary))

(define (chess-initial-pieces game)
  (let ((choose-type
         (lambda (column)
           (case column
             ((0 7) 'rook)
             ((1 6) 'knight)
             ((2 5) 'bishop)
             ((3) 'king)
             ((4) 'queen)
             (else (error "Incorrect column:" column))))))
    (append-map (lambda (color)
                  (append-map (lambda (column)
                                (let ((make
                                       (lambda (type column row)
                                         (make-piece color type
                                                     (make-coords column
                                                                  row)))))
                                 (list (make 'pawn column 1)
                                       (make (choose-type column)
                                         column
                                         0))))
                              (iota (game-width game))))
                (game-colors game))))

(define (chess-piece-summary piece)
  (if piece
      (string (case (piece-type piece)
                ((pawn) #\P)
                ((rook) #\R)
                ((knight) #\N)
                ((bishop) #\B)
                ((king) #\K)
                ((queen) #\Q)
                (else (error "Unknown type:" piece)))
              (case (piece-color piece)
                ((black) #\b)
                ((white) #\w)
                (else (error "Unknown color:" piece))))
      "  "))