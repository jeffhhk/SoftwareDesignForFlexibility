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

;;;; Separating analysis from execution.
;;;   Generic analysis; handles nonstrict operands.

(define (x:eval expression environment)
  ((analyze expression) environment))

(define (analyze expression)
  (make-executor (x:analyze expression)))

(define (default-analyze expression)
  (cond ((application? expression) (analyze-application expression))
        (else (error "Unknown expression type" expression))))

(define x:analyze
  (simple-generic-procedure 'x:analyze 1 default-analyze))

(define (analyze-application expression)
  (let ((operator-exec (analyze (operator expression)))
        (operand-execs (map analyze (operands expression))))
    (lambda (environment)
      (x:apply (x:advance (operator-exec environment))
               operand-execs
               environment))))

(define (analyze-self-evaluating expression)
  (lambda (environment)
    (declare (ignore environment))
    expression))

;; coderef: self-evaluating-handler
(define-generic-procedure-handler x:analyze
  (match-args self-evaluating?)
  analyze-self-evaluating)

(define (analyze-variable expression)
  (lambda (environment)
    (lookup-variable-value expression environment)))

;; coderef: variable-handler
(define-generic-procedure-handler x:analyze
  (match-args variable?)
  analyze-variable)

(define (analyze-quoted expression)
  (let ((qval (text-of-quotation expression)))
    (lambda (environment)
      (declare (ignore environment))
      qval)))

;; coderef: quoted-handler
(define-generic-procedure-handler x:analyze
  (match-args quoted?)
  analyze-quoted)

(define (analyze-lambda expression)
  (let ((vars (lambda-parameters expression))
        (body-exec (analyze (lambda-body expression))))
    (lambda (environment)
      (make-compound-procedure vars body-exec environment))))

;; coderef: lambda-handler
(define-generic-procedure-handler x:analyze
  (match-args lambda?)
  analyze-lambda)

(define (analyze-if expression)
  (let ((predicate-exec (analyze (if-predicate expression)))
        (consequent-exec (analyze (if-consequent expression)))
        (alternative-exec (analyze (if-alternative expression))))
    (lambda (environment)
      (if (x:advance (predicate-exec environment))
          (consequent-exec environment)
          (alternative-exec environment)))))

;; coderef: if-handler
(define-generic-procedure-handler x:analyze
  (match-args if?)
  analyze-if)

(define (analyze-begin expression)
  (reduce-right (lambda (exec1 exec2)
                  (lambda (environment)
                    (exec1 environment)
                    (exec2 environment)))
                #f
                (map analyze
                     (let ((exps (begin-actions expression)))
                       (if (null? exps)
                           (error "Empty sequence"))
                       exps))))

;; coderef: begin-handler
(define-generic-procedure-handler x:analyze
  (match-args begin?)
  analyze-begin)

(define (analyze-assignment expression)
  (let ((var (assignment-variable expression))
        (value-exec (analyze (assignment-value expression))))
    (lambda (environment)
      (set-variable-value! var (value-exec environment) environment)
      'ok)))

;; coderef: assignment-handler
(define-generic-procedure-handler x:analyze
  (match-args assignment?)
  analyze-assignment)

(define (analyze-definition expression)
  (let ((var (definition-variable expression))
        (value-exec (analyze (definition-value expression))))
    (lambda (environment)
      (define-variable! var (value-exec environment) environment)
      var)))

;; coderef: definition-handler
(define-generic-procedure-handler x:analyze
  (match-args definition?)
  analyze-definition)

;;; Macros (definitions are in syntax.scm)

;; coderef: cond-handler
(define-generic-procedure-handler x:analyze
  (match-args cond?)
  (compose analyze cond->if))

;; coderef: let-handler
(define-generic-procedure-handler x:analyze
  (match-args let?)
  (compose analyze let->combination))

(define-generic-procedure-handler x:analyze
  (match-args let*?)
  (compose analyze let*->let-nest))

(define (default-apply procedure operand-execs environment)
  (declare (ignore operand-execs environment))
  (error "Unknown procedure type" procedure))

(define x:apply
  (simple-generic-procedure 'x:apply 3 default-apply))

;; coderef: apply-strict-primitive
(define-generic-procedure-handler x:apply
  (match-args strict-primitive-procedure? executors?
              environment?)
  (lambda (procedure operand-execs environment)
    (apply-primitive-procedure procedure
     (map (lambda (operand-exec)
            (x:advance (operand-exec environment)))
          operand-execs))))

;; coderef: apply-compound
(define-generic-procedure-handler x:apply
  (match-args compound-procedure? executors? environment?)
  (lambda (procedure operand-execs calling-environment)
    (if (not (n:= (length (procedure-parameters procedure))
                  (length operand-execs)))
        (error "Wrong number of operands supplied"))
    (let ((params (procedure-parameters procedure))
          (body-exec (procedure-body procedure)))
      (let ((names (map procedure-parameter-name params))
            (arguments
             (map (lambda (param operand-exec)
                    (x:handle-operand param
                                      operand-exec
                                      calling-environment))
                  params
                  operand-execs)))
        (body-exec (extend-environment names arguments
                     (procedure-environment procedure)))))))

(define x:handle-operand
  (simple-generic-procedure 'x:handle-operand 3
    (lambda (parameter operand-exec environment)
      (declare (ignore parameter))
      (operand-exec environment))))

;; coderef: handle-lazy
(define-generic-procedure-handler x:handle-operand
  (match-args lazy? executor? environment?)
  (lambda (parameter operand-exec environment)
    (declare (ignore parameter))
    (postpone operand-exec environment)))

;; coderef: handle-lazy-memo
(define-generic-procedure-handler x:handle-operand
  (match-args lazy-memo? executor? environment?)
  (lambda (parameter operand-exec environment)
    (declare (ignore parameter))
    (postpone-memo operand-exec environment)))

(define-generic-procedure-handler procedure-parameter-name
  (match-args pair?)
  car)

(define x:advance
  (simple-generic-procedure 'x:advance 1 (lambda (x) x)))

;; coderef: advance-postponed
(define-generic-procedure-handler x:advance
  (match-args postponed?)
  (lambda (object)
    (x:advance ((postponed-expression object)
                (postponed-environment object)))))

;; coderef: advance-postponed-memo
(define-generic-procedure-handler x:advance
  (match-args postponed-memo?)
  (lambda (object)
    (let ((value
           (x:advance ((postponed-expression object)
                       (postponed-environment object)))))
      (advance-memo! object value)
      value)))

(define-generic-procedure-handler x:advance
  (match-args advanced-memo?)
  advanced-value)