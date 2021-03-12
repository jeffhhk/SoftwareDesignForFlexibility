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

;;; Tests from Wikipedia, modified for double continuations

(define (f return fail)
  (return 2 fail)
  3)

(f (lambda (x y) x) #f)
'expect-value: 3

(call/ccs f)
'expect-value: 2

(define (show-lossage)
  (write-line
   (let ((x (amb 3 4)))
     (call/ccs
      (lambda (succeed fail)
        (succeed x
                 (lambda ()
                   (write-line (list 'loser x))
                   (fail))))))))

(begin
  (show-lossage)
  (amb))
'expect-write: 3
'expect-write: '(loser 3)
'expect-write: 4
'expect-write: '(loser 4)
'expect-value: 'no-more-alternatives

;;;-------------------------------------------------------
;;;   https://en.wikipedia.org/wiki/Call-with-current-continuation

;; [LISTOF X] -> (-> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)

  (define (for-each proc lst)
    (cond ((null? lst) 'done)
          (else (proc (car lst))
                (for-each proc (cdr lst)))))

  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return fail)
    (for-each
     (lambda (element)
       (set! return
             (call/ccs
              (lambda (resume-here fail)
                ;; Grab the current continuation
                (set! control-state resume-here)
                (return element fail)))))
     lst)
    (return 'you-fell-off-the-end fail))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
  (define (generator)
    (call/ccs control-state))

  ;; Return the generator
  generator)

(define generate-digit
  (generate-one-element-at-a-time '(0 1 2)))

(generate-digit)
'expect-value: 0

(generate-digit)
'expect-value: 1

(generate-digit)
'expect-value: 2

(generate-digit)
'expect-value: 'you-fell-off-the-end

(generate-digit)
'expect-value: 'you-fell-off-the-end

;;;-------------------------------------------------------
;; In 1999, David Madore (the inventor of Unlambda programming
;; language) found a 12 characters term in Unlambda that have the
;; same effect (write all integers in unary form) of an over 600
;; characters term with an operation equivalent to call/cc.[1]
;; When converting this term into Scheme he obtained a scheme
;; program that known as the yin-yang puzzle.  It was then being
;; customary [2] to show while discussing call/cc:

#|
(let* ((yin
        ((lambda (cc) (display "@") cc)
         (call/ccs (lambda (c f) (c (list c f) f)))))
       (yang
        ((lambda (cc) (display "*") cc)
         (call/ccs (lambda (c f) (c (list c f) f))))))
  ((car yin) yang (cadr yang))
  )
|#

;; An illustration of the puzzle: Every statements between
;; brackets are the states of yin and yang immediately before
;; (yin yang).  The number means whether the 1st continuation or
;; the 2nd to jump.  The statement after the number represents
;; the context.

#|
;@*
[(yin 1 ())
(yang 2 (yin 1 ()))]
;@*
[(yin 2 (yin 1 ()))
(yang 2 (yin 2 (yin 1 ())))]
;*
[(yin 1 ())
(yang 2 (yin 2 (yin 1 ())))]
;@*
[(yin 2 (yin 2 (yin 1 ())))
(yang 2 (yin 2 (yin 2 (yin 1 ()))))]
;*
[(yin 2 (yin 1 ()))
(yang 2 (yin 2 (yin 2 (yin 1 ()))))]
;*
[(yin 1 ())
(yang 2 (yin 2 (yin 2 (yin 1 ()))))]
;@*
[(yin 2 (yin 2 (yin 2 (yin 1 ()))))
(yang 2 (yin 2 (yin 2 (yin 2 (yin 1 ())))))]
;...
;...
|#