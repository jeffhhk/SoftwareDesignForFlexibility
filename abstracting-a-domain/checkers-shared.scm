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

(define (new-checkers-game)
  (new-game (make-board checkers)))

(define (make-checkers moves-generator)
  (make-game 8 8 '(black red) '(man king)
             checkers-initial-pieces
             moves-generator
             checkers-piece-summary))

(define (checkers-initial-pieces game)
  (append-map (lambda (color)
                (let ((make
                       (lambda (column row)
                         (make-piece color 'man (make-coords column row)))))
                  (append-map (lambda (n)
                                (list (make (* n 2) 0)
                                      (make (+ (* n 2) 1) 1)
                                      (make (* n 2) 2)))
                              (iota 4))))
              (game-colors game)))

(define (checkers-piece-summary piece)
  (if piece
      (string (case (piece-color piece)
                ((black) #\B)
                ((red) #\R)
                (else (error "Unknown color:" piece)))
              (if (eq? 'king (piece-type piece))
                  #\k
                  #\space))
      "  "))

(define (summarize-move move)
  (let loop
      ((coords-list
        (map (lambda (change)
               (piece-coords (get-piece change)))
             (pmove->list move))))
    (if (pair? coords-list)
        (let ((tail (loop (cdr coords-list))))
          (if (and (pair? (cdr coords-list))
                   (coords=? (car coords-list)
                             (cadr coords-list)))
              tail
              (cons (car coords-list) tail)))
        '())))

(define (piece-is-king? piece)
  (eq? 'king (piece-type piece)))

(define (should-be-crowned? piece)
  (and (= 7 (get-row (piece-coords piece)))
       (not (piece-is-king? piece))))

(define (crown-piece piece)
  (piece-new-type piece 'king))

(define (possible-directions piece)
  (if (piece-is-king? piece)
      diagonal-directions
      forward-diagonal-directions))