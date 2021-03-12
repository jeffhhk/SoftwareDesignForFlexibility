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

;;; Exposing the continuations: call/ccs as a special form

(define (analyze-call/ccs exp)
  (let ((rproc
         (analyze (call/ccs-receiver exp))))
    (lambda (env succeed fail)
      (let ((succeed-proc
             (lambda (env s f) (s succeed f)))
            (fail-proc
             (lambda (env s f) (s fail f))))
        (rproc env
               (lambda (proc fail-1)
                 (a:advance proc
                            (lambda (procedure fail-2)
                              (a:apply procedure
                                       (list succeed-proc fail-proc)
                                       env
                                       succeed
                                       fail-2))
                            fail-1))
               fail)))))

(define-generic-procedure-handler analyze
  (match-args call/ccs?)
  analyze-call/ccs)


(define (call/ccs? exp)
  (and (pair? exp)
       (eq? (car exp) 'call/ccs)))

(register-predicate! call/ccs? 'call/ccs)

(define (call/ccs-receiver exp)
  (cadr exp))
