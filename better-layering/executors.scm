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

(define executor?)
(define get-executor-metadata)
(define set-executor-metadata!)
(let ((association (make-metadata-association)))
  (set! executor? (association 'has?))
  (set! get-executor-metadata (association 'get))
  (set! set-executor-metadata! (association 'put!)))
(register-predicate! executor? 'executor)

(define (executors? object)
  (and (list? object)
       (every executor? object)))
(register-predicate! executors? 'executors)

(define (make-executor implementation #!optional arg-checker)
  (letrec ((executor
            (lambda args
              (if (not (default-object? arg-checker))
                  (apply arg-checker args))
              (execution-trace 'save! (cons executor args))
              (apply implementation args))))
    (set-executor-metadata! executor implementation)
    executor))

(define (make-circular-buffer size)
  (let ((buffer (make-list size #f)))

    (define (save! item)
      (set-car! buffer item)
      (set! buffer (cdr buffer)))

    (define (get-all)
      (let loop ((b (cdr buffer)) (items (list (car buffer))))
        (if (eq? b buffer)
            items
            (loop (cdr b)
                  (cons (car b) items)))))

    (set-cdr! (last-pair buffer) buffer)
    (bundle circular-buffer? save! get-all)))

(define circular-buffer?
  (make-bundle-predicate 'circular-buffer))

(define (get-execution-trace)
  (execution-trace 'get-all))

(define execution-trace
  (make-circular-buffer 10))