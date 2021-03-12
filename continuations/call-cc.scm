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

(define (call/cc receiver)
  (call/ccs (lambda (succeed fail)
              (receiver (lambda (value)
                          (succeed value fail))))))

(define call-with-current-continuation call/cc)

(define (for-each proc items)
  (if (pair? items)
      (begin
        (proc (car items))
        (for-each proc (cdr items)))))

(call/cc
 (lambda (exit)
   (for-each (lambda (x)
               (if (negative? x) (exit x)))
             '(54 0 37 -3 245 -19))     ; **
   (exit #t)))
'expect-value: -3

(define (first-negative list-of-numbers)
  (call/cc
    (lambda (k_exit)
      (let ((v
             (call/cc
               (lambda (k_shortcut)
                 (for-each
                  (lambda (n)
                    (cond ((not (number? n))
                           (pp (list 'not-a-number: n))
                           (k_exit #f))
                          ((negative? n)
                           (k_shortcut n))
                          (else
                           'keep-looking)))
                  list-of-numbers)
                 #f))))
        (if v
            v
            (k_exit 'no-negatives-found))))))

(first-negative '(54 0 37 -3 245 -19))
'expect-value: -3

(first-negative '(54 0 37  3 245  19))
'expect-value: 'no-negatives-found

(first-negative '(54 0 37 no 245 boo))
'expect-write: '(not-a-number: no)
'expect-value: #f

(define the-continuation #f)

(define (test)
  (let ((i 0))
    ;; The argument to call/cc assigns the
    ;; continuation produced by call/cc to the
    ;; global variable, the-continuation.
    (call/cc (lambda (k) (set! the-continuation k)))
    ;; When the-continuation is called, execution
    ;; resumes here.
    (set! i (+ i 1))
    i))

(test)
'expect-value: 1

(the-continuation 'ok)
'expect-value: 2

(the-continuation 'ok)
'expect-value: 3

(define another-continuation the-continuation)

(test)
'expect-value: 1

(the-continuation 'ok)
'expect-value: 2

(another-continuation 'ok)
'expect-value: 4

#|
;;; Tests from
;;;   https://en.wikipedia.org/wiki/Call-with-current-continuation

(define (f return)
  (return 2)
  3)

(f (lambda (x) x)) ; displays 3

(call/cc f)        ; displays 2

;;;-------------------------------------------------------

;; [LISTOF X] -> ( -> X u 'you-fell-off-the-end)
(define (generate-one-element-at-a-time lst)

  ;; Added by gjs because don't want to load library for test.
  (define (for-each proc lst)
    (cond ((null? lst) 'done)
          (else (proc (car lst))
                (for-each proc (cdr lst)))))

  ;; Hand the next item from a-list to "return" or an end-of-list marker
  (define (control-state return)
    (for-each
     (lambda (element)
               (set! return (call/cc
                              (lambda (resume-here)
                                ;; Grab the current continuation
                               (set! control-state resume-here)
                               (return element)))))
     lst)
    (return 'you-fell-off-the-end))

  ;; (-> X u 'you-fell-off-the-end)
  ;; This is the actual generator, producing one item from a-list at a time
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

;;;-------------------------------------------------------
(define the-continuation #f)

(define (test)
  (let ((i 0))
    ;; call/cc calls its first function argument, passing
    ;; a continuation variable representing this point in
    ;; the program as the argument to that function.
    ;;
    ;; In this case, the function argument assigns that
    ;; continuation to the variable the-continuation.
    ;;
    (call/cc (lambda (k) (set! the-continuation k)))
    ;;
    ;; The next time the-continuation is called, we start here.
    (set! i (+ i 1))
    i))

(test)
;Value: 1

(the-continuation 'OK)
;Value: 2

(the-continuation 'OK)
;Value: 3

;;; stores the current continuation (which will print 4 next) away
(define another-continuation the-continuation)

(test) ; resets the-continuation
;Value: 1

(the-continuation 'OK)
;Value: 2

(another-continuation 'OK) ; uses the previously stored continuation
;Value: 4
|#
