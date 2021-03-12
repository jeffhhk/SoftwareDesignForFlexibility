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

#| 

Six people, two women and four men are seated at a round card
table, playing cards.  Each has a hand; no two of the hands are
equally strong.

   Ben is seated opposite Eva.
   The man at Alyssa's right has a better hand than Jake has.
   The man at Eva's right has a better hand than Ben has.
   The man at Ben's right has a better hand than Fred has.
   The man at Ben's right has a better hand than Eva has.
   The woman at Jake's right has a better hand than Fred has.
   The woman at Fred's right has a better hand than Bill has.

  What is the arrangement at the table?
|#

;;; Needs library.scm

;;; This formulation takes a few minutes on my laptop.

(define (card-table)
  (let (;; Seating positions
	(eva 0)			; arbitrary
	(ben 3)			; opposite Eva
	(alyssa (amb 1 2 4 5)))
    (let ((jake (except '(1 2 4 5) (list alyssa))))
      (let ((fred (except '(1 2 4 5) (list alyssa jake))))
	(let ((bill (except '(1 2 4 5) (list alyssa jake fred)))
	      (values (assignments '(0 1 2 3 4 5)
				   '(10 20 30 40 50 60))))
	  (let ((men (list ben jake fred bill))
		(women (list eva alyssa)))
	    (require (distinct (list alyssa jake fred bill)))

	    (require (memv (to-right alyssa) men))
	    (require (not (= (to-right alyssa) jake)))
	    (require (better-hand? (to-right alyssa) jake values))

	    (require (memv (to-right eva) men))
	    (require (better-hand? (to-right eva) ben values))

	    (require (memv (to-right ben) men))
	    (require (not (= (to-right ben) fred)))
	    (require (better-hand? (to-right ben) eva values))
	    ;;(require (better-hand? (to-right ben) fred values))

	    (require (memv (to-right jake) women))
	    (require (better-hand? (to-right jake) fred values))

	    (require (memv (to-right fred) women))
	    (require (better-hand? (to-right fred) bill values))

	    (list eva ben alyssa jake fred bill)))))))

;;; Amb-Eval input:
(card-table)

;;; Starting a new problem 
;;; Amb-Eval value:
(0 3 2 5 1 4)

;;; Amb-Eval input:
try-again
;;; There are no more values of (card-table)

;;; The following takes too long.  Several hours on a laptop.
#|
(define (card-table)
  (let (;; Seating positions
	(eva 0)			; arbitrary
	(ben 3)			; opposite Eva
	(alyssa (amb 1 2 4 5))
	(jake (amb 1 2 4 5))
	(fred (amb 1 2 4 5))
	(bill (amb 1 2 4 5))

	(values
	 (map (lambda (place)
		(cons place (amb 10 20 30 40 50 60)))
	      (iota 6)))
	)

    (let ((men (list ben jake fred bill))
	  (women (list eva alyssa)))

      (require (memv (to-right alyssa) men))
      (require (not (= (to-right alyssa) jake)))
      (require (better-hand? (to-right alyssa) jake values))

      (require (memv (to-right eva) men))
      (require (better-hand? (to-right eva) ben values))

      (require (memv (to-right ben) men))
      (require (not (= (to-right ben) fred)))
      (require (better-hand? (to-right ben) eva values))
      (require (better-hand? (to-right ben) fred values))

      (require (memv (to-right jake) women))
      (require (better-hand? (to-right jake) fred values))

      (require (memv (to-right fred) women))
      (require (better-hand? (to-right fred) bill values))
      
      (list eva ben alyssa jake fred bill))))
|#
