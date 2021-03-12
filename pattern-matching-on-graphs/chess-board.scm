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

;;;; Chess board

(define chess-board-size 8)
(define chess-board-indices (iota chess-board-size))
(define chess-board-last-index (last chess-board-indices))

(define (make-chess-board)
  (let ((board (make-chess-board-internal)))
    (for-each (lambda (address)
                (connect-up-square address board))
              board-addresses)
    (populate-sides board)
    board))

(define (connect-up-square address board)
  (let ((node (board 'node-at address)))
    (node 'connect! 'address address)
    (for-each-direction
     (lambda (label x-delta y-delta)
       (let ((x+ (+ (address-x address) x-delta))
             (y+ (+ (address-y address) y-delta)))
         (if (and (<= 0 x+ chess-board-last-index)
                  (<= 0 y+ chess-board-last-index))
             (node 'connect! label
                   (board 'node-at
                          (make-address x+ y+)))))))))

(define (for-each-direction procedure)
  (procedure 'north 0 1)
  (procedure 'northeast 1 1)
  (procedure 'east 1 0)
  (procedure 'southeast 1 -1)
  (procedure 'south 0 -1)
  (procedure 'southwest -1 -1)
  (procedure 'west -1 0)
  (procedure 'northwest -1 1))

(define (populate-sides board)

  (define (populate-side color home-row pawn-row)

    (define (do-column col type)
      (add-piece col home-row type)
      (add-piece col pawn-row 'pawn))

    (define (add-piece col row type)
      ((board 'node-at (make-address col row))
       'connect! 0 (make-piece type color)))

    (do-column 0 'rook)
    (do-column 1 'knight)
    (do-column 2 'bishop)
    (do-column 3 'queen)
    (do-column 4 'king)
    (do-column 5 'bishop)
    (do-column 6 'knight)
    (do-column 7 'rook))

  (populate-side 'white 0 1)
  (populate-side 'black 7 6))

(define (make-chess-board-internal)
  (let ((nodes
         (map (lambda (x)
                (map (lambda (y)
                       (make-graph-node (string x "," y)))
                     chess-board-indices))
              chess-board-indices)))
    (let loop ((turn 0))

      ;; coderef: node-at
      (define (node-at address)

        (define (get-node address)
          (list-ref (list-ref nodes (address-x address))
                    (address-y address)))

        (if (white-move?)
            (get-node address)
            (graph-node-view (get-node (invert-address address))
                             rotate-180-view)))

      ;; coderef: piece-at
      (define (piece-at address)
        (piece-in (node-at address)))

      ;; coderef: piece-in
      (define (piece-in node)
        (and (node 'has-edge? turn)
             (node 'edge-value turn)))

      ;; coderef: address-of
      (define (address-of node)
        (let ((address (node 'edge-value 'address)))
          (if (white-move?)
              address
              (invert-address address))))

      ;; coderef: set-piece-at
      (define (set-piece-at address piece)
        ((node-at address) 'connect! (+ turn 1) piece))

      ;; coderef: white-move?
      (define (white-move?)
        (even? turn))

      ;; coderef: color
      (define (color)
        (if (white-move?) 'white 'black))

      ;; coderef: next-turn
      (define (next-turn)
        (loop (+ turn 1)))

      (bundle #f
              node-at piece-at piece-in address-of
              set-piece-at color next-turn))))

(define (simple-move board from to)
  (let ((my-piece (get-piece-to-move board from)))
    ;; A bunch of checks for validity of move:
    (let ((captured (board 'piece-at to)))
      (if (not (no-piece-or-opponent? captured my-piece))
          (error "Can't capture piece of same color:" captured)))
    ;; The move looks good; make it so:
    (board 'set-piece-at to my-piece)
    ;; Now update all the unaffected pieces to the next state of
    ;; the board:
    (for-each (lambda (address)
                (if (not (or (address= from address)
                             (address= to address)))
                    (let ((p (board 'piece-at address)))
                      (if p
                          (board 'set-piece-at address p)))))
              board-addresses)
    (board 'next-turn)))

(define (capture? board from path)
  (let* ((my-piece (get-piece-to-move board from))
         (dict
          (graph-match path
                       (match:extend-dict chess-board:var
                                          board
                                          (match:new-dict))
                       (board 'node-at from))))
    (and dict
         (let* ((target (match:get-value 'target-node dict))
                (captured (board 'piece-in target)))
           (and captured
                `(capture ,my-piece
                          ,captured
                          ,(board 'address-of target)))))))

(define (chess-dict:board dict)
  (match:binding-value (match:lookup chess-board:var dict)))

(define chess-board:var
  `(? ,(generate-uninterned-symbol)))

(define (get-piece-to-move board from)
  (let ((my-piece (board 'piece-at from)))
    (if (not my-piece)
        (error "No piece in this square:" from))
    (if (not (eq? (board 'color) (piece-color my-piece)))
        (error "Can move only one's own pieces:" my-piece from))
    my-piece))

;;;; Chess REPL

(define the-board)

(define (start-chess-game)
  (set! the-board (make-chess-board))
  (print-chess-board the-board))

(define (chess-move from to)
  (set! the-board (simple-move the-board from to))
  (print-chess-board the-board))

(define (print-chess-board b)
  (let* ((comment (lambda () (display ";;;")))
         (line-start (lambda () (comment) (display " ")))
         (row-line
          (lambda ()
            (line-start)
            (display "  +")
            (for-each (lambda (x) (display "----+"))
                      chess-board-indices)
            (newline))))
    (line-start)
    (for-each (lambda (x)
                (display "    ")
                (write x))
              chess-board-indices)
    (newline)
    (row-line)
    (for-each (lambda (y)
                (line-start)
                (write y)
                (display " |")
                (for-each (lambda (x)
                            (display " ")
                            (let ((piece
                                   (b 'piece-at
                                      (make-address x y))))
                              (if piece
                                  (display (piece->string piece))
                                  (write-string "  ")))
                            (display " |"))
                          chess-board-indices)
                (newline)
                (row-line))
              (reverse chess-board-indices))
    (line-start)
    (write (b 'color))
    (display " to move")
    (newline)))

;;;; Chess pieces

(define (make-piece type color)
  (guarantee piece-type? type 'make-piece)
  (guarantee piece-color? color 'make-piece)
  (list type color))

(define (piece-type piece)
  (car piece))

(define (piece-color piece)
  (cadr piece))

(define (piece-type? type)
  (memq type '(pawn rook knight bishop queen king)))
(register-predicate! piece-type? 'piece-type)

(define (piece-color? color)
  (memq color '(white black)))
(register-predicate! piece-color? 'piece-color)

(define (no-piece-or-opponent? piece my-piece)
  (or (not piece)
      (piece-is-opponent? piece my-piece)))

(define (piece-is-opponent? piece my-piece)
  (not (eq? (piece-color piece)
            (piece-color my-piece))))

(define (change-piece-type piece type)
  (make-piece type (piece-color piece)))

(define (piece->string piece)
  (string (case (piece-type piece)
            ((pawn) #\P)
            ((rook) #\R)
            ((knight) #\N)
            ((bishop) #\B)
            ((queen) #\Q)
            ((king) #\K))
          (case (piece-color piece)
            ((white) #\w)
            ((black) #\b))))

;;;; Addresses of board squares

(define (make-address x y)
  (guarantee address-index? x 'make-address)
  (guarantee address-index? y 'make-address)
  (list x y))

(define (address-x address)
  (car address))

(define (address-y address)
  (cadr address))

(define (address-index? object)
  (and (integer? object)
       (exact? object)
       (>= object 0)
       (< object chess-board-size)))
(register-predicate! address-index? 'address-index)

(define (address= a b)
  (and (= (address-x a) (address-x b))
       (= (address-y a) (address-y b))))

(define (address-x+ address delta)
  (make-address (+ (address-x address) delta)
                (address-y address)))

(define (address-y+ address delta)
  (make-address (address-x address)
                (+ (address-y address) delta)))

(define (invert-address address)
  (make-address (- chess-board-last-index
                   (address-x address))
                (- chess-board-last-index
                   (address-y address))))

(define board-addresses
  (append-map (lambda (y)
                (map (lambda (x)
                       (make-address x y))
                     chess-board-indices))
              chess-board-indices))

;;;; Coordinate transforms

(define (rotate-45 label)
  (case label
    ((east) 'southeast)
    ((north) 'northeast)
    ((northeast) 'east)
    ((northwest) 'north)
    ((south) 'southwest)
    ((southeast) 'south)
    ((southwest) 'west)
    ((west) 'northwest)
    (else label)))

(define (rotate-90 label)
  (rotate-45 (rotate-45 label)))

(define (rotate-180 label)
  (rotate-90 (rotate-90 label)))

(define (reflect-ew label)
  (case label
    ((east) 'west)
    ((northeast) 'northwest)
    ((northwest) 'northeast)
    ((southeast) 'southwest)
    ((southwest) 'southeast)
    ((west) 'east)
    (else label)))

(define (reflect-ns label)
  (case label
    ((north) 'south)
    ((northeast) 'southeast)
    ((northwest) 'southwest)
    ((south) 'north)
    ((southeast) 'northeast)
    ((southwest) 'northwest)
    (else label)))

(define rotate-180-view
  (make-graph-view 'inverse rotate-180 rotate-180))

#|
(define (giuoco-piano-opening)
  (start-chess-game)
  (chess-move '(4 1) '(4 3))            ;W: P-K4
  (chess-move '(3 1) '(3 3))            ;B: P-K4
  (chess-move '(6 0) '(5 2))            ;W: N-KB3
  (chess-move '(6 0) '(5 2))            ;B: N-QB3
  (chess-move '(5 0) '(2 3))            ;W: B-QB4
  (chess-move '(2 0) '(5 3))            ;B: B-QB4
  )

(giuoco-piano-opening)

;;; lots of output...

;;; Now the white knight at KB3 can attack the black pawn at K5.

(capture? the-board
          (make-address 5 2)
          `((? source-node ,(occupied-by 'knight))
            north (?) north (?)
            west (? target-node ,maybe-opponent)))
;Value: (capture (knight white) (pawn black) (4 4))

;;; And it's the only capture possible with this knight:

(filter-map (lambda (path)
              (capture? the-board
                        (make-address 5 2)
                        path))
            all-knight-moves)
;Value 149: ((capture (knight white) (pawn black) (4 4)))

(filter-map (lambda (path)
              (capture? the-board
                        (make-address 2 3)
                        path))
            all-bishop-moves)
;Value 150: ((capture (bishop white) (pawn black) (5 6)))

(chess-move '(5 2) '(4 4))

(capture? the-board
          '(5 3)
          `((? source-node ,(occupied-by 'bishop))
            (* northwest (?* ,unoccupied))
            northwest (? target-node ,maybe-opponent)))
;Value: (capture (bishop black) (pawn white) (2 6))

(chess-move '(5 3) '(2 6))
|#
