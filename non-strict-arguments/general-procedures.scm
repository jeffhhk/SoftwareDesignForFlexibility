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

;;; Modifications to provide for generalized procedures.

;;; Syntax extension: allow decorated parameter names.

;; coderef: pair-param
(define-generic-procedure-handler procedure-parameter-name
  (match-args pair?)
  car)

(define (general-compound-procedure? object)
  (and (compound-procedure? object)
       (any (lambda (parameter)
              (not (symbol? parameter)))
            (procedure-parameters object))))

;; coderef: general-compound-procedure
(define-generic-procedure-handler g:apply
  (match-args general-compound-procedure?
              operands?
              environment?)
  (lambda (procedure operands calling-environment)
    (if (not (n:= (length (procedure-parameters procedure))
                  (length operands)))
        (error "Wrong number of operands supplied"))
    (let ((params (procedure-parameters procedure))
          (body (procedure-body procedure)))
      (let ((names (map procedure-parameter-name params))
            (arguments
             (map (lambda (param operand)
                    (g:handle-operand param
                                      operand
                                      calling-environment))
                  params
                  operands)))
        (g:eval body
                (extend-environment names arguments
                  (procedure-environment procedure)))))))

;;; This handler was replaced to allow ordinary strict parameters
;;; to work as before.

(define g:handle-operand
  (simple-generic-procedure 'g:handle-operand 3
    (lambda (parameter operand environment)
      (declare (ignore parameter))
      (g:advance (g:eval operand environment)))))

;; coderef: handle-lazy
(define-generic-procedure-handler g:handle-operand
  (match-args lazy? operand? environment?)
  (lambda (parameter operand environment)
    (postpone operand environment)))

;; coderef: handle-lazy-memo
(define-generic-procedure-handler g:handle-operand
   (match-args lazy-memo? operand? environment?)
  (lambda (parameter operand environment)
     (postpone-memo operand environment)))

;; coderef: advance-postponed
(define-generic-procedure-handler g:advance
  (match-args postponed?)
  (lambda (object)
    (g:advance (g:eval (postponed-expression object)
                       (postponed-environment object)))))

;; coderef: advance-postponed-memo
(define-generic-procedure-handler g:advance
  (match-args postponed-memo?)
  (lambda (object)
    (let ((value
           (g:advance (g:eval (postponed-expression object)
                              (postponed-environment object)))))
      (advance-memo! object value)
      value)))

;; coderef: advance-advanced-memo
(define-generic-procedure-handler g:advance
  (match-args advanced-memo?)
  advanced-value)

;;; For printing output

(define-generic-procedure-handler write
  (match-args deferred?)
  (compose write g:advance))

(define-generic-procedure-handler write-line
  (match-args deferred?)
  (compose write-line g:advance))

(define-generic-procedure-handler pp
  (match-args deferred?)
  (compose pp g:advance))


(define-generic-procedure-handler write
  (match-args advanced-memo?)
  (compose write advanced-value))

(define-generic-procedure-handler write-line
  (match-args advanced-memo?)
  (compose write-line advanced-value))

(define-generic-procedure-handler pp
  (match-args advanced-memo?)
  (compose pp advanced-value))