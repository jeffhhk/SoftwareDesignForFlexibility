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

(define-test 'giuoco-piano-opening
  (lambda ()
    (let ((b (make-chess-board)) (s initial-board-summary))

      (define (move from to)
        (set! b (simple-move b from to))
        (set! s (flip-board (move-piece s from to)))
        (assert-board-is s b))

      (assert-board-is s b)
      (move '(4 1) '(4 3))  ;W: P-K4
      (move '(3 1) '(3 3))  ;B: P-K4
      (move '(6 0) '(5 2))  ;W: N-KB3
      (move '(6 0) '(5 2))  ;B: N-QB3
      (move '(5 0) '(2 3))  ;W: B-QB4
      (move '(2 0) '(5 3))  ;B: B-QB4

      ;; Now the white knight at KB3 can attack the black pawn at K5.
      (assert-equal
       '(capture (knight white) (pawn black) (4 4))
       (capture? b
                 '(5 2)
                 `((? source-node ,(occupied-by 'knight))
                   north (?) north (?)
                   west (? target-node ,maybe-opponent))))

      ;; And it's the only capture possible with this knight:
      (assert-equal
       '((capture (knight white) (pawn black) (4 4)))
       (filter-map (lambda (path)
                     (capture? b '(5 2) path))
                   all-knight-moves))

      ;; Try a bishop capture to see a different pattern:
      (assert-equal
       '((capture (bishop white) (pawn black) (5 6)))
       (filter-map (lambda (path)
                     (capture? b '(2 3) path))
                   all-bishop-moves))

      (move '(5 2) '(4 4))

      (assert-equal
       '(capture (bishop black) (pawn white) (2 6))
       (capture? b
                 '(5 3)
                 `((? source-node ,(occupied-by 'bishop))
                   (* northwest (?* ,unoccupied))
                   northwest (? target-node ,maybe-opponent))))

      (move '(5 3) '(2 6)))))

(define (summarize-board board)
  (sort-summary
   (filter-map (lambda (address)
                 (let ((piece (board 'piece-at address)))
                   (and piece
                        (list address piece))))
               board-addresses)))

(define (sort-summary summary)
  (sort summary
        (lambda (a b)
          (or (< (address-y (car a))
                 (address-y (car b)))
              (and (= (address-y (car a))
                      (address-y (car b)))
                   (< (address-x (car a))
                      (address-x (car b))))))))

(define (summary= a1 a2)
  (lset= equal? a1 a2))

(define assert-summary=
  (make-equality-asserter summary=))

(define (assert-board-is summary board)
  (assert-summary= summary (summarize-board board)))

(define (move-piece summary from to)
  (add-piece (remove-piece (remove-piece summary from) to)
             to
             (get-piece summary from)))

(define (get-piece summary address)
  (cadr (find (lambda (s) (equal? address (car s)))
              summary)))

(define (remove-piece summary address)
  (remove (lambda (s) (equal? address (car s)))
          summary))

(define (add-piece summary address piece)
  (cons (list address piece)
        summary))

(define (flip-board summary)
  (map (lambda (s)
         (cons (invert-address (car s))
               (cdr s)))
       summary))

(define initial-board-summary
  '(((0 0) (rook white))
    ((1 0) (knight white))
    ((2 0) (bishop white))
    ((3 0) (queen white))
    ((4 0) (king white))
    ((5 0) (bishop white))
    ((6 0) (knight white))
    ((7 0) (rook white))
    ((0 1) (pawn white))
    ((1 1) (pawn white))
    ((2 1) (pawn white))
    ((3 1) (pawn white))
    ((4 1) (pawn white))
    ((5 1) (pawn white))
    ((6 1) (pawn white))
    ((7 1) (pawn white))
    ((0 6) (pawn black))
    ((1 6) (pawn black))
    ((2 6) (pawn black))
    ((3 6) (pawn black))
    ((4 6) (pawn black))
    ((5 6) (pawn black))
    ((6 6) (pawn black))
    ((7 6) (pawn black))
    ((0 7) (rook black))
    ((1 7) (knight black))
    ((2 7) (bishop black))
    ((3 7) (queen black))
    ((4 7) (king black))
    ((5 7) (bishop black))
    ((6 7) (knight black))
    ((7 7) (rook black))))
