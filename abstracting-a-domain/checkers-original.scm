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

(define checkers
  (make-checkers
   (lambda (board)
     (generate-moves board))))

(define-record-type <step>
    (%make-step to from board is-jump?)
    step?
  (to step-to)
  (from step-from)
  (board step-board)
  (is-jump? step-is-jump?))

(define-record-printer <step>
  (lambda (step)
    (list (piece-coords (step-to step))
          (piece-coords (step-from step)))))

(define (make-simple-move new-coords from board)
  (%replace-piece (piece-move from new-coords) from board #f))

(define (make-jump landing-coords jumped-coords from board)
  (%replace-piece (piece-move from landing-coords)
                  from
                  (board-remove-piece board
                                      (board-get jumped-coords board))
                  #t))

(define (replace-piece to from board)
  (%replace-piece to from board #f))

(define (%replace-piece to from board is-jump?)
  (%make-step to from
              (board-replace-piece board from to)
              is-jump?))

(define (path-contains-jumps? path)
  (any step-is-jump? path))

;;;; REPL interface

(define (get-final-board path)
  (board-end-turn (step-board (car path))))

(define (summarize-move path)
  (let loop
      ((summary
        (let ((rpath (reverse path)))
          (cons (piece-coords (step-from (car rpath)))
                (map (lambda (step)
                       (piece-coords (step-to step)))
                     rpath)))))
    (if (pair? summary)
        (if (and (pair? (cdr summary))
                 (coords=? (car summary) (cadr summary)))
            (loop (cdr summary))
            (cons (car summary) (loop (cdr summary))))
        summary)))

(define (generate-moves board)
  (crown-kings
   (mandate-jumps
    (append-map (lambda (piece)
                  (evolve-paths piece board))
                (current-pieces board)))))

(define (crown-kings paths)
  (map (lambda (path)
         (let ((piece (step-to (car path))))
           (if (should-be-crowned? piece)
               (cons (replace-piece (crown-piece piece)
                                    piece
                                    (step-board (car path)))
                     path)
               path)))
       paths))

(define (mandate-jumps paths)
  (let ((jumps (filter path-contains-jumps? paths)))
    (if (null? jumps)
        paths
        jumps)))

(define (evolve-paths piece board)
  (let ((paths (compute-next-steps piece board '())))
    (let ((jumps (filter path-contains-jumps? paths)))
      (if (null? jumps)
          paths
          (evolve-jumps jumps)))))

(define (evolve-jumps paths)
  (append-map (lambda (path)
                (let ((paths
                       (let ((step (car path)))
                         (compute-next-steps (step-to step)
                                             (step-board step)
                                             path))))
                  (if (null? paths)
                      (list path)
                      (evolve-jumps paths))))
              paths))

(define (compute-next-steps piece board path)
  (filter-map (lambda (direction)
                (try-step piece board direction path))
              (possible-directions piece)))

(define (try-step piece board direction path)
  (let ((new-coords
         (coords+ (piece-coords piece) direction)))
    (and (is-position-on-board? new-coords board)
         (case (position-info new-coords board)
           ((unoccupied)
            (and (not (path-contains-jumps? path))
                 (cons (make-simple-move new-coords
                                         piece
                                         board)
                       path)))
           ((occupied-by-opponent)
            (let ((landing (coords+ new-coords direction)))
              (and (is-position-on-board? landing board)
                   (is-position-unoccupied? landing board)
                   (cons (make-jump landing
                                    new-coords
                                    piece
                                    board)
                         path))))
           ((occupied-by-self) #f)
           (else (error "Unknown position info"))))))