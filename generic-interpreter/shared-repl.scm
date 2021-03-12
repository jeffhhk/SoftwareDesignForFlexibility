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

;;;; Shared parts of REPL

;;; Global environment for REPL.
(define the-global-environment
  'not-initialized)

(define (initialize-repl!)
  (set! the-global-environment (make-global-environment))
  'done)

(define (check-repl-initialized)
  (if (eq? the-global-environment 'not-initialized)
      (error "Interpreter not initialized. Run (init) first.")))

(define write
  (simple-generic-procedure 'write 1
    (access write user-initial-environment)))


(define write-line
  (simple-generic-procedure 'write-line 1
    (access write-line user-initial-environment)))

(define pp
  (simple-generic-procedure 'pretty-print 1
    (access pp user-initial-environment)))

(define-generic-procedure-handler write
  (match-args compound-procedure?)
  (compose write procedure-printable-representation))

(define-generic-procedure-handler write-line
  (match-args compound-procedure?)
  (compose write-line procedure-printable-representation))

(define-generic-procedure-handler pp
  (match-args compound-procedure?)
  (compose pp procedure-printable-representation))

(define (g:read)
  (prompt-for-command-expression "eval> "))

(define (init)
  (initialize-repl!)
  (repl))

(define (go)
  (repl))