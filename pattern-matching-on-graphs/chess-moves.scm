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

;;; A place on the board is represented as
;;; a node.  Directions on the board are
;;; edges.  In chess there are 8 directions:
;;; north, south, east, west,
;;; northeast, northwest, southeast, southwest.

(define (rewrite-path-edges transform-edge-label)
  (define (transformer path)
    (cons (car path)
          (transform-elts (cdr path))))

  (define (transform-elts path-elts)
    (cond ((and (pair? path-elts)
                (symbol? (car path-elts))
                (pair? (cdr path-elts))
                (match:var? (cadr path-elts)))
           (cons* (transform-edge-label (car path-elts))
                  (cadr path-elts)
                  (transform-tail (cddr path-elts))))
          ((and (pair? path-elts)
                (pair? (car path-elts))
                (memq (caar path-elts) '(* + opt)))
           (cons (cons (caar path-elts)
                       (transform-elts (cdar path-elts)))
                 (transform-tail (cdr path-elts))))
          ((and (pair? path-elts)
                (pair? (car path-elts))
                (memq (caar path-elts) '(and or)))
           (cons (cons (caar path-elts)
                       (map transform-elts (cdar path-elts)))
                 (transform-tail (cdr path-elts))))
          (else
           (error "Unknown path elements:" path-elts))))

  (define (transform-tail tail)
    (if (pair? tail)
        (transform-elts tail)
        '()))

  transformer)

;;; To collect all the appropriate transformations
(define (symmetrize-move move . transformations)
  (let loop ((xforms transformations) (moves (list move)))
    (if (null? xforms)
        moves
        (loop (cdr xforms)
              (append moves
                      (map (rewrite-path-edges (car xforms))
                           moves))))))

(define (occupied-by type)
  (lambda (place-node dict)
    (let ((piece (piece-in place-node dict)))
      (and piece
           (eq? type (piece-type piece))))))

(define (maybe-opponent place-node dict)
  (no-piece-or-opponent?
   (piece-in place-node dict)
   (piece-in (match:get-value 'source-node dict) dict)))

(define (unoccupied place-node dict)
  (not (piece-in place-node dict)))

(define (piece-in place-node dict)
  ((chess-dict:board dict) 'piece-in place-node))

;;; Basic knight move is NNE.

(define basic-knight-move
  `((? source-node ,(occupied-by 'knight))
    north (?)
    north (?)
    east (? target-node ,maybe-opponent)))

(define all-knight-moves
  (symmetrize-move basic-knight-move
                   reflect-ew rotate-90 rotate-180))

#|
(pp all-knight-moves)
((<source> north (?) north (?) east <target>)
 (<source> north (?) north (?) west <target>)
 (<source> east (?) east (?) south <target>)
 (<source> east (?) east (?) north <target>)
 (<source> south (?) south (?) west <target>)
 (<source> south (?) south (?) east <target>)
 (<source> west (?) west (?) north <target>)
 (<source> west (?) west (?) south <target>))
|#

;;; A knight fork is a knight in a position
;;; that threatens more than one opponent
;;; piece.

;;; To be specified...

;;; Basic rook move is N*.

(define basic-rook-move
  `((? source-node ,(occupied-by 'rook))
    (* north (?* ,unoccupied))
    north (? target-node ,maybe-opponent)))

(define all-rook-moves
  (symmetrize-move basic-rook-move
                   rotate-90 rotate-180))

#|
(pp all-rook-moves)
((<source> (* north <unoccupied>) north <target>)
 (<source> (* east <unoccupied>) east <target>)
 (<source> (* south <unoccupied>) south <target>)
 (<source> (* west <unoccupied>) west <target>))
|#

(define basic-bishop-move
  `((? source-node ,(occupied-by 'bishop))
    (* northeast (?* ,unoccupied))
    northeast (? target-node ,maybe-opponent)))

(define all-bishop-moves
  (symmetrize-move basic-bishop-move
                   rotate-90 rotate-180))

#|
(pp all-bishop-moves)
((<source> (* northeast <unoccupied>) northeast <target>)
 (<source> (* southeast <unoccupied>) southeast <target>)
 (<source> (* southwest <unoccupied>) southwest <target>)
 (<source> (* northwest <unoccupied>) northwest <target>))
|#

(define basic-king-move
  `((? source-node ,(occupied-by 'king))
    north (? target-node ,maybe-opponent)))

(define all-king-moves
  (symmetrize-move basic-king-move
                   rotate-45 rotate-90 rotate-180))

#|
(pp all-king-moves)
((<source> north <target>)
 (<source> northeast <target>)
 (<source> east <target>)
 (<source> southeast <target>)
 (<source> south <target>)
 (<source> southwest <target>)
 (<source> west <target>)
 (<source> northwest <target>))
|#


(define basic-queen-move
  `((? source-node ,(occupied-by 'queen))
    (* north (?* ,unoccupied))
    north (? target-node ,maybe-opponent)))

(define all-queen-moves
  (symmetrize-move basic-queen-move
                   rotate-45 rotate-90 rotate-180))

#|
(pp all-queen-moves)
((<source> (* north <unoccupied>) north <target>)
 (<source> (* northeast <unoccupied>) northeast <target>)
 (<source> (* east <unoccupied>) east <target>)
 (<source> (* southeast <unoccupied>) southeast <target>)
 (<source> (* south <unoccupied>) south <target>)
 (<source> (* southwest <unoccupied>) southwest <target>)
 (<source> (* west <unoccupied>) west <target>)
 (<source> (* northwest <unoccupied>) northwest <target>))
|#
