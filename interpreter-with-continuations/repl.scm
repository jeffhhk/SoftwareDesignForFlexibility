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
;;;; with explicit continuations.

(define *in-repl* #f)

(define (repl)
  (define (do-output val)
    (show-result val)
    (do-it (g:read) do-output))
  (define (do-it input output)
    (let ((val 
           (c:eval input
                   the-global-environment
                   ;; no need to advance, because 
                   ;; show-result handles deferred.
                   output)))
      (error "c:eval returned!" input val)
      'ugh!)
    (repl))
  (check-repl-initialized)
  (if *in-repl*
      'already-in-repl
      (begin
        (if *debugging*
            (fluid-let ((*in-repl* #t))
              (do-output 'REPL))
            (let ((result (ignore-errors
                           (lambda ()
                             (fluid-let ((*in-repl* #t))
                               (do-output 'REPL))))))
              (if (condition? result)
                  (standard-warning-handler result))))
        (repl))))

;;; May be false if not debugging.  Can avoid having
;;; to restart repl if hit an obvious error.
(define *debugging* #t)  

(define (g:read)
  (prompt-for-command-expression ";eval>\n"))

(define (load-library filename)
  (check-repl-initialized)
  (call-with-input-file filename
    (lambda (port)
      (let do-it ()
        (let ((input (read port)))
          (if (not (eof-object? input))
              (begin
                (c:eval input
                        the-global-environment
                        show-result)
                (do-it))
              'done))))))

(define (show-result val)
  (cond ((strict-primitive-procedure? val)
         (display ";Value: ")
         (pp val))
        ((simple-compound-procedure? val)
         (display ";Value: ")
         (pp `(procedure ,(procedure-parameters val))))
        ((complex-compound-procedure? val)
         (display ";Value: ")
         (pp `(procedure ,(procedure-parameters val))))
        ((call/cc? val)
         (display ";Value: ")
         (pp `(continuation)))
        (else (display ";=> ") (pp val))))

;;; For printing output

(define write
  (simple-generic-procedure 'write 1
    (access write user-initial-environment)))


(define write-line
  (simple-generic-procedure 'write-line 1
    (access write-line user-initial-environment)))

(define pp
  (simple-generic-procedure 'pretty-print 1
    (access pp user-initial-environment)))


(define (doit! effect-procedure)
  (lambda (object)
    (c:advance object
               (lambda (real-object)
                  (effect-procedure real-object)))))

(define-generic-procedure-handler write
  (match-args deferred?)
  (doit! write))

(define-generic-procedure-handler write-line
  (match-args deferred?)
  (doit! write-line))

(define-generic-procedure-handler pp
  (match-args deferred?)
  (doit! pp))

(define-generic-procedure-handler pp
  (match-args simple-compound-procedure?)
  (doit!
   (lambda (x)
     (pp `(simple-compound-procedure
           ,(procedure-parameters x))))))

(define-generic-procedure-handler pp
  (match-args complex-compound-procedure?)
  (doit!
   (lambda (x)
     (pp `(complex-compound-procedure
           ,(procedure-parameters x))))))

(define-generic-procedure-handler write
  (match-args advanced-memo?)
  (doit! write))

(define-generic-procedure-handler write-line
  (match-args advanced-memo?)
  (doit! write-line))

(define-generic-procedure-handler pp
  (match-args advanced-memo?)
  (doit! pp))
