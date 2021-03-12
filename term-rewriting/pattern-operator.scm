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

(define (make-pattern-operator . rules)
  (let ((rules
         (cons 'rules
               (if (pair? rules)
                   (except-last-pair rules)
                   '())))
        (default-rule
         (and (pair? rules)
              (last rules))))
    (define (the-operator . data)
      (define (succeed value fail) value)
      (define (fail)
        (error "No applicable operations:" data))
      (try-rules data
                 (cdr rules)
                 succeed
                 (if default-rule
                     (lambda ()
                       (try-rule data
                                 default-rule
                                 succeed
                                 fail))
                     fail)))
    (set-pattern-metadata! the-operator rules)
    the-operator))

(define (attach-rule! operator rule)
  (let ((metadata (pattern-metadata operator)))
    (set-cdr! metadata
              (append (cdr metadata)
                      (list rule)))))

(define (override-rule! operator rule)
  (let ((metadata (pattern-metadata operator)))
    (set-cdr! metadata
              (cons rule (cdr metadata)))))

(define pattern-metadata)
(define set-pattern-metadata!)
(let ((store (make-metadata-association)))
  (set! pattern-metadata (store 'get))
  (set! set-pattern-metadata! (store 'put!)))