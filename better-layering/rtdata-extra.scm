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

;;; Compound procedures are split into simple ones
;;; that have no decorated formal parameters and 
;;; complicated ones that have decorations.

(define-record-type <*simple-compound-procedure>
    (make-simple-compound-procedure vars bproc env)
    simple-compound-procedure?
  (vars  simple-procedure-parameters)
  (bproc simple-procedure-body)
  (env   simple-procedure-environment))
(register-predicate! simple-compound-procedure? 
                     'simple-compound-procedure?)

(define-record-type <*complex-compound-procedure>
    (make-complex-compound-procedure vars bproc env)
    complex-compound-procedure?
  (vars  complex-procedure-parameters)
  (bproc complex-procedure-body)
  (env   complex-procedure-environment))
(register-predicate! complex-compound-procedure?
                     'complex-compound-procedure?)

(define (procedure-printable-representation procedure)
  `(compound-procedure
    ,(procedure-parameters procedure)
    ,(procedure-body procedure)
    <procedure-environment>))

(define (compound-procedure? x)
  (or (simple-compound-procedure? x)
      (complex-compound-procedure? x)))

;;; Allow dotted rest variables:

#|
(define (extend-environment variables values base-environment)
  (if (not (fix:= (length variables) (length values)))
      (if (fix:< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values)))
  (vector variables values base-environment))
|#

(define (extend-environment variables values base-environment)
  (let lp ((invars variables) (invals values)
           (outvars '())      (outvals '()))
    (cond ((null? invars)
           (if (null? invals)
               (vector outvars outvals base-environment)
               (error "Too many arguments supplied"
                      variables values)))
          ((symbol? invars)
           (vector (cons invars outvars)
                   (cons invals outvals)
                   base-environment))
          ((pair? invars)
           (if (pair? invals)
               (lp (cdr invars)
                   (cdr invals)
                   (cons (car invars) outvars)
                   (cons (car invals) outvals))
               (error "Too few arguments supplied"
                      variables values)))
          (else
           (error "Bad formal parameter list"
                  variables values)))))

(define call/cc (list 'call/cc-tag))

(define (call/cc? p) (eq? p call/cc))
(register-predicate! call/cc? 'call/cc)