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

;;;; Read-eval-print loop for extended Scheme interpreter

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    (write-line (x:eval input the-global-environment))
    (repl)))

(define (load-library filename)
  (check-repl-initialized)
  (call-with-input-file filename
    (lambda (port)
      (let lp ()
        (let ((input (read port)))
          (if (not (eof-object? input))
              (begin
                (write-line
                 (x:eval input the-global-environment))
                (lp))
              'done))))))

;;; Output handlers for special runtime objects

(define-generic-procedure-handler write
  (match-args deferred?)
  (compose write x:advance))

(define-generic-procedure-handler write-line
  (match-args deferred?)
  (compose write-line x:advance))

(define-generic-procedure-handler pp
  (match-args deferred?)
  (compose pp x:advance))

(define-generic-procedure-handler write
  (match-args advanced-memo?)
  (compose write advanced-value))

(define-generic-procedure-handler write-line
  (match-args advanced-memo?)
  (compose write-line advanced-value))

(define-generic-procedure-handler pp
  (match-args advanced-memo?)
  (compose pp advanced-value))