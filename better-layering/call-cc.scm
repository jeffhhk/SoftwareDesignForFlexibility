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

;;; Implementation of call/cc: predicate in rtdata-extra; dispatch in
;;; l:apply-strict; also modifies primitive-or-simple-procedure?

(define (l:deliver-continuation receiver continue)
  (l:apply-strict receiver
                  (list continue)
                  continue))

#|
;;;From Wikipedia

;;; For compatibility, must define in repl:
(repl)

(define call-with-current-continuation call/cc)

(define (for-each p l)
  (if (not (null? l))
      (begin (p (car l))
             (for-each p (cdr l)))))



;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)
  ;; Both internal functions are closures over lst

  ;; Internal variable/Function which passes the current element in a
  ;; list to its return argument (which is a continuation), or passes
  ;; an end-of-list marker if no more elements are left. On each step
  ;; the function name is rebound to a continuation which points back
  ;; into the function body, while return is rebound to whatever
  ;; continuation the caller specifies.

  (define (control-state return)
    (for-each 
     (lambda (element)
               (set! return
                     (call/cc
                      (lambda (resume-here)
                        ;; Grab the current continuation
                        (set! control-state resume-here)
                        ;; (return element) evaluates to next return
                        (return element))))) 
     lst)
    (return 'you-fell-off-the-end))
  
  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a
  ;; time.
  (define (generator)
    (call/cc control-state))

  ;; Return the generator 
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit) ;; 0
(generate-digit) ;; 1
(generate-digit) ;; 2
(generate-digit) ;; you-fell-off-the-end
|#

#|
;;; In 1999, David Madore (inventor of the Unlambda programming
;; language) accidentally discovered a 12-character Unlambda term, making
;; use of call/cc, that printed all natural numbers sequentially in unary
;; representation: ``r`ci`.*`ci.[1] This program and the apparent mystery
;; surrounding its effect have attracted some attention, and are commonly
;; known as the yin-yang puzzle.[2] A Scheme translation, provided by
;; Madore, is as follows:

(let* ((yin
         ((lambda (cc) (display "@") cc) (call/cc (lambda (c) c))))
       (yang
         ((lambda (cc) (display "*") cc) (call/cc (lambda (c) c)))))
    (yin yang))
#|
@*@**@***@****@*****@******@*******@********@*********@**********@ ...
;Quit!
|#
|#

#|
;; Cooperative multitasking using call-with-current-continuation
;; in 25 lines of scheme

;; The list of threads waiting to run. This is a list of one
;; argument non-returning functions (continuations, mostly)
;; A continuation is a non-returning function, just like (exit),
;; in that it never gives up control to whatever called it.

(define ready-list '())

;; A non-returning function. If there is any other thread
;; waiting to be run, it causes the next thread to run if there
;; is any left to run, otherwise it calls the original exit
;; which exits the whole environment.
(define exit
  ;; The original exit which we override.
  (let ((exit exit))
    ;; The overriding function.
    (lambda ()
      (if (not (null? ready-list))
	  ;; There is another thread waiting to be run.
	  ;; So we run it.
          (let ((cont (car ready-list)))
            (set! ready-list (cdr ready-list))
	    ;; Since the ready-list is only non-returning
	    ;; functions, this will not return.
            (cont #f))
	  ;; Nothing left to run.
	  ;; The original (exit) is a non returning function,
	  ;; so this is a non-returning function.
          (exit)))))

;; Takes a one argument function with a given
;; argument and forks it off. The forked function's new
;; thread will exit if/when the function ever exits.
(define (fork fn arg)
  (set! ready-list
        (append ready-list
		;; This function added to the 
		;; ready-list is non-returning,
		;; since exit is non returning.
		(list
		 (lambda (x)
		   (fn arg)
		   (exit))))))

;; Gives up control for the next thread waiting to be run.
;; Although it will eventually return, it gives up control
;; and will only regain it when the continuation is called.
(define (yield)
  (call/cc
   ;; Capture the continuation representing THIS call to yield
   (lambda (cont)
     ;; Stick it on the ready list
     (set! ready-list
           (append ready-list
                   (list cont)))
     ;; Get the next thread, and start it running.
     (let ((cont (car ready-list)))
       (set! ready-list (cdr ready-list))
       ;; Run it.
       (cont #f)))))
|#
