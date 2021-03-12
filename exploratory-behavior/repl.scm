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

(define input-prompt ";;; Amb-Eval input:\n")
(define output-prompt "\n;;; Amb-Eval value:\n")

(define (repl)

  (define (internal-loop succeed fail)
    (let ((input
           (prompt-for-command-expression input-prompt)))

      (define (fail-k)
        (display ";;; There are no more values of ")
        (pp input)
        (internal-loop success-k no-problem))

      (if (eq? input 'try-again)
          (fail)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (a:eval input the-global-environment
                    succeed fail-k)))))

  (define (success-k value fail*)
    (display output-prompt)
    (pp value)
    (internal-loop success-k fail*))

  (define (no-problem)
    (display ";;; There is no current problem")
    (internal-loop success-k no-problem))

  (check-repl-initialized)
  (internal-loop success-k no-problem))

(define (load-library filename)
  (check-repl-initialized)
  (call-with-input-file filename
    (lambda (port)
      (let lp ()
        (let ((input (read port)))
          (if (not (eof-object? input))
              (begin
                (if (not (eq? (car input) 'define))
                    (error "can only load definitions in library"
                           input))
                (a:eval input
                        the-global-environment
                        (lambda (val next-alternative)
                          (display output-prompt)
                          (pp val)
                          (lp))
                        (lambda ()
                          (error "Failure in loading library"
                                 input)))))))
      'done)))

;;; Output handlers for special runtime objects

;;; side effects
(define (doit! effect-procedure)
  (lambda (object)
    (a:advance object
               (lambda (real-object fail)
                 (effect-procedure real-object))
               (lambda ()
                 (effect-procedure 'failed)))))

(define-generic-procedure-handler write
  (match-args deferred?)
  (doit! write))

(define-generic-procedure-handler write-line
  (match-args deferred?)
  (doit! write-line))

(define-generic-procedure-handler pp
  (match-args deferred?)
  (doit! pp))


(define-generic-procedure-handler write
  (match-args advanced-memo?)
  (doit! write))

(define-generic-procedure-handler write-line
  (match-args advanced-memo?)
  (doit! write-line))

(define-generic-procedure-handler pp
  (match-args advanced-memo?)
  (doit! pp))