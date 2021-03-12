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

;;;; Fun with Continuations

#| Adapted from MIT/GNU Scheme Reference Manual [Section 12.4]:

(call/cc
 (lambda (exit)
   (for-each (lambda (x)
	       (if (negative? x)
		   (exit x)))
	     '(54 0 37 -3 245 -19))	; **
   (exit #t)))
;Value: -3

|#

;;; Continuations as Non-Local Exits

(define (funco:first-negative list-of-numbers)
  (call/cc
   (lambda (k_exit)
     (or (call/cc
	  (lambda (k_shortcut)
	    (for-each (lambda (n)
			(cond ((not (number? n))
			       (pp `(not-a-number: ,n))
			       (k_exit #f))
			      ((negative? n)
			       (k_shortcut n))
			      (else
			       ':keep-looking)))
		      list-of-numbers)
	    #f ;; Fall-through sentinel:  no negatives found.
	    ))
	 (k_exit ':no-negatives-found)))))

#|
(funco:first-negative '(54 0 37 -3 245 -19))
;Value: -3

(funco:first-negative '(54 0 37  3 245  19))
;Value: :no-negatives-found

(funco:first-negative '(54 0 37 no 245 boo))
(not-a-number: no)
;Value: #f
|#

;;; Continuations for Proceeding (Suspend/Resume Backtracking)

(define (funco:first-negative-n-proceed list-of-numbers) ;;; **
  (call/cc
   (lambda (k_exit)
     (or (call/cc
	  (lambda (k_shortcut)
	    (for-each (lambda (n)
			(pp				 ;;; **
			 (call/cc                        ;;; **
			  (lambda (k_proceed)		 ;;; **
			    (cond ((not (number? n))
				   (pp `(not-a-number: ,n))
				   (k_exit
				    (cons n k_proceed))) ;;; **
				  ((negative? n)
				   (k_shortcut
				    (cons n k_proceed))) ;;; **
				  (else
				   ':keep-looking)))
			  )))				 ;;; **
		      list-of-numbers)
	    #f ;; Fall-through sentinel:  no negatives found.
	    ))
	 ':no-negatives-found))))

(define (funco:first-negative-n-proceed-more?      smore) (pair? smore))
(define (funco:first-negative-n-proceed-more/found smore) (car   smore))
(define (funco:first-negative-n-proceed-more/k     smore) (cdr   smore))
(define (funco:first-negative-n-proceed-more/next  smore)
       ((funco:first-negative-n-proceed-more/k     smore)
	(funco:first-negative-n-proceed-more/found smore)))

#|
;;;           ------------
(define funco:first-of-two
  (funco:first-negative-n-proceed '(54 0 37 -3 245 -19)))
:keep-looking
:keep-looking
:keep-looking
;Value: funco:first-of-two

(funco:first-negative-n-proceed-more?		funco:first-of-two)
;Value: #t

(funco:first-negative-n-proceed-more/found	funco:first-of-two)
;Value: -3

(funco:first-negative-n-proceed-more/next	funco:first-of-two)
-3
:keep-looking
;Value: funco:first-of-two

(funco:first-negative-n-proceed-more?		funco:first-of-two)
;Value: #t

(funco:first-negative-n-proceed-more/found	funco:first-of-two)
;Value: -19

(funco:first-negative-n-proceed-more/next	funco:first-of-two)
-19
;Value: funco:first-of-two

(funco:first-negative-n-proceed-more?		funco:first-of-two)
;Value: #f

funco:first-of-two
;Value: :no-negatives-found
|#

#|
;;;           ----
(define funco:nada
  (funco:first-negative-n-proceed '(54 0 37  3 245  19)))
:keep-looking
:keep-looking
:keep-looking
:keep-looking
:keep-looking
:keep-looking
;Value: funco:nada

(funco:first-negative-n-proceed-more? funco:nada)
;Value: #f

funco:nada
;Value: :no-negatives-found
|#

#|
;;;           ----
(define funco:nans
  (funco:first-negative-n-proceed '(54 0 37 no 245 boo)))
:keep-looking
:keep-looking
:keep-looking
(not-a-number: no)
;Value: funco:nans

(funco:first-negative-n-proceed-more?		funco:nans)
;Value: #t

(funco:first-negative-n-proceed-more/found	funco:nans)
;Value: no

(funco:first-negative-n-proceed-more/next	funco:nans)
no
:keep-looking
(not-a-number: boo)
;Value: funco:nans

(funco:first-negative-n-proceed-more?		funco:nans)
;Value: #t

(funco:first-negative-n-proceed-more/next	funco:nans)
boo
;Value: funco:nans

(funco:first-negative-n-proceed-more?		funco:nans)
;Value: #f

funco:nans
;Value: :no-negatives-found
|#

;;; Continuations for Backtracking (Re-entrant 1st-Class Continuations)

(define *k_re-funco*)
(define       funco)

#|
(begin
  (set! funco
        (+ 2 (call/cc
              (lambda (k_re-funco)
                (set! *k_re-funco* k_re-funco)
                (k_re-funco 3)))))
  ':ok)
;Value: :ok

funco
;Value: 5

(*k_re-funco* 4)
;Value: :ok

funco
;Value: 6

(*k_re-funco* 5)
;Value: :ok

funco
;Value: 7
|#

;;; Dynamic Contexts and Within-Continuation

(define (funco:test-k-thunk k-thunk)
  (let ((*foo* 2))			;----------------------.
    (define (foo-thunk) *foo*)		; *foo* is 2 out here. :
    (call/cc                     	;                      :
     (lambda (k)			;                      :
       (fluid-let ((*foo* 3))	;---------------------.        :
	 (k-thunk k foo-thunk)	; *foo* is 3 in here. :        :
	 )			;---------------------'        :
       ))				; *foo* is 2 out here. :
    ))					;----------------------'
#|
(funco:test-k-thunk (lambda (k thunk)
		      (k (thunk))))
;Value: 3

(funco:test-k-thunk (lambda (k thunk)
		      (within-continuation k thunk)))
;Value: 2
|#
