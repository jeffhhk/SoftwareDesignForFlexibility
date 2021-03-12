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

;;;; Simple deque Abstraction

(define-record-type <deque>
    (%make-deque front back)
    deque?
  (front deque-front set-deque-front!)
  (back deque-back set-deque-back!))

(define (make-deque)
  (%make-deque '() '()))

(define (reset-deque! stq)
  (set-deque-front! stq '())
  (set-deque-back! stq '()))

(define (deque-empty? stq)
  (not (pair? (deque-front stq))))

(define (dequed? stq item)
  (memq item (deque-front stq)))

(define (push! stq object)
  (if (pair? (deque-front stq))
      (set-deque-front! stq (cons object (deque-front stq)))
      (begin
        (set-deque-front! stq (cons object (deque-front stq)))
        (set-deque-back! stq (deque-front stq)))))

(define (add-to-end! stq object)
  (let ((new (cons object '())))
    (if (pair? (deque-back stq))
        (set-cdr! (deque-back stq) new)
        (set-deque-front! stq new))
    (set-deque-back! stq new)))

(define (pop! stq)
  (let ((next (deque-front stq)))
    (if (not (pair? next))
        (error "Empty deque -- POP"))
    (if (pair? (cdr next))
        (set-deque-front! stq (cdr next))
        (begin
          (set-deque-front! stq '())
          (set-deque-back! stq '())))
    (car next)))