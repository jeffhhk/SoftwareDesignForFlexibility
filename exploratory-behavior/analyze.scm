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

;;;; Analyzing interpreter with AMB.
;;;   Execution procedures take environment
;;;   and two continuations: SUCCEED and FAIL

(define (a:eval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (make-executor (a:analyze exp)
                 check-executor-args))

(define (check-executor-args env succeed fail)
  (if (not (success? succeed))
      (error "Illegal succeed:" succeed))
  (if (not (failure? fail))
      (error "Illegal fail:" fail)))

(define (execute-strict executor env succeed fail)
  (executor env
            (lambda (value fail-1)
              (a:advance value succeed fail-1))
            fail))

(define (default-analyze exp)
  (cond ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type" exp))))

(define a:analyze
  (simple-generic-procedure 'a:analyze 1 default-analyze))

(define (analyze-application exp)
  (let ((operator-exec (analyze (operator exp)))
        (operand-execs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (execute-strict operator-exec
                      env
                      (lambda (procedure fail-2)
                        (a:apply procedure
                                 operand-execs
                                 env
                                 succeed
                                 fail-2))
                      fail))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (declare (ignore env))
    (succeed exp fail)))

(define-generic-procedure-handler a:analyze
  (match-args self-evaluating?)
  analyze-self-evaluating)

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define-generic-procedure-handler a:analyze
  (match-args variable?)
  analyze-variable)

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (declare (ignore env))
      (succeed qval fail))))

(define-generic-procedure-handler a:analyze
  (match-args quoted?)
  analyze-quoted)

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (body-exec (analyze (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-compound-procedure vars body-exec env)
               fail))))

(define-generic-procedure-handler a:analyze
  (match-args lambda?)
  analyze-lambda)

(define (analyze-if exp)
  (let ((predicate-exec (analyze (if-predicate exp)))
        (consequent-exec (analyze (if-consequent exp)))
        (alternative-exec (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (execute-strict predicate-exec
                      env
                      (lambda (pred-value pred-fail)
                        ((if pred-value
                             consequent-exec
                             alternative-exec)
                         env succeed pred-fail))
                      fail))))

(define-generic-procedure-handler a:analyze
  (match-args if?)
  analyze-if)

(define (analyze-begin exp)
  (reduce-right (lambda (exec1 exec2)
                  (lambda (env succeed fail)
                    (exec1 env
                           (lambda (exec1-value exec1-fail)
                             (declare (ignore exec1-value))
                             (exec2 env succeed exec1-fail))
                           fail)))
                #f
                (map analyze
                     (let ((exps (begin-actions exp)))
                       (if (null? exps)
                           (error "Empty sequence"))
                       exps))))

(define-generic-procedure-handler a:analyze
  (match-args begin?)
  analyze-begin)

;;; There are two useful kinds of assignments in AMB
;;; interpreters.  Undoable assignments and permanent
;;; assignments.

(define (analyze-undoable-assignment exp)
  (let ((var (assignment-variable exp))
        (value-exec (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (value-exec env
                  (lambda (new-val val-fail)
                    (let ((old-val
                           (lookup-variable-value var env)))
                      (set-variable-value! var new-val env)
                      (succeed 'OK
                               (lambda ()
                                 (set-variable-value! var old-val
                                                      env)
                                 (val-fail)))))
                  fail))))

(define-generic-procedure-handler a:analyze
  (match-args undoable-assignment?)
  analyze-undoable-assignment)

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (value-exec (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (value-exec env
                  (lambda (new-val val-fail)
                    (set-variable-value! var new-val env)
                    (succeed 'ok val-fail))
                  fail))))

(define-generic-procedure-handler a:analyze
  (match-args assignment?)
  analyze-assignment)

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (value-exec (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (value-exec env
                  (lambda (new-val val-fail)
                    (define-variable! var new-val env)
                    (succeed var val-fail))
                  fail))))

(define-generic-procedure-handler a:analyze
  (match-args definition?)
  analyze-definition)

;;; Macros (definitions are in syntax.scm)

(define-generic-procedure-handler a:analyze
  (match-args cond?)
  (compose analyze cond->if))

(define-generic-procedure-handler a:analyze
  (match-args let?)
  (compose analyze let->combination))

(define-generic-procedure-handler a:analyze
  (match-args let*?)
  (compose analyze let*->let-nest))

;;; AMB, itself!

(define (analyze-amb exp)
  (let ((alternative-execs (map analyze (amb-alternatives exp))))
    (lambda (env succeed fail)
      (let loop ((alts alternative-execs))
        (if (pair? alts)
            ((car alts) env succeed
              (lambda ()
                (loop (cdr alts))))
            (fail))))))

(define-generic-procedure-handler a:analyze
  (match-args amb?)
  analyze-amb)

(define (default-apply procedure operand-execs env succeed fail)
  (declare (ignore operand-execs env succeed fail))
  (error "Unknown procedure type" procedure)
  ;; Preserve environment for error.
  #f)

(define a:apply
  (simple-generic-procedure 'a:apply 5 default-apply))

;;; Assumption: strict primitive procedures may error, but do not
;;; fail.
(define-generic-procedure-handler a:apply
  (match-args strict-primitive-procedure? executors? environment?
              success? failure?)
  (lambda (procedure operand-execs env succeed fail)
    (execute-operands execute-strict operand-execs env
      (lambda (args fail)
        (succeed (apply-primitive-procedure procedure
                                            args)
                 fail))
      fail)))

(define-generic-procedure-handler a:apply
  (match-args compound-procedure? executors? environment?
              success? failure?)
  (lambda (procedure operand-execs env succeed fail)
    (if (not (n:= (length (procedure-parameters procedure))
                  (length operand-execs)))
        (error "Wrong number of operands supplied"))
    (let ((params (procedure-parameters procedure))
          (penv (procedure-environment procedure))
          (body-exec (procedure-body procedure)))
      (let ((names (map procedure-parameter-name params)))
        (execute-operands (lambda (operand env succeed fail)
                            (a:handle-operand (car operand)
                                              (cdr operand)
                                              env succeed fail))
                          (map cons params operand-execs)
                          env
                          (lambda (args fail)
                            (body-exec (extend-environment names
                                                           args
                                                           penv)
                                       succeed
                                       fail))
                          fail)))))

(define (execute-operands execute operands env succeed fail)
  (if (pair? operands)
      (let loop
          ((operands operands)
           (succeed succeed)
           (fail fail))
        (if (pair? (cdr operands))
            (execute (car operands) env
              (lambda (arg fail)
                (loop (cdr operands)
                      (lambda (args fail)
                        (succeed (cons arg args) fail))
                      fail))
              fail)
            (execute (car operands) env
              (lambda (arg fail)
                (succeed (list arg) fail))
              fail)))
      (succeed '() fail)))

(define a:handle-operand
  (simple-generic-procedure 'a:handle-operand 5
    (lambda (parameter operand-exec env succeed fail)
      (declare (ignore parameter))
      (operand-exec env succeed fail))))

(define-generic-procedure-handler a:handle-operand
  (match-args lazy? executor? environment? success? failure?)
  (lambda (parameter operand-exec environment succeed fail)
    (succeed (postpone operand-exec environment) fail)))

(define-generic-procedure-handler a:handle-operand
  (match-args lazy-memo? executor? environment?
              success? failure?)
  (lambda (parameter operand-exec environment succeed fail)
     (succeed (postpone-memo operand-exec environment) fail)))

(define-generic-procedure-handler procedure-parameter-name
  (match-args pair?)
  car)

(define a:advance
  (simple-generic-procedure 'a:advance 3
    (lambda (x succeed fail)
      (succeed x fail))))

(define-generic-procedure-handler a:advance
  (match-args postponed? success? failure?)
  (lambda (object succeed fail)
    ((postponed-expression object)
     (postponed-environment object)
     (lambda (val fail)
       (a:advance val succeed fail))
     fail)))

(define-generic-procedure-handler a:advance
  (match-args postponed-memo? success? failure?)
  (lambda (object succeed fail)
    (let ((pexpr (postponed-expression object))
          (penv (postponed-environment object)))
      (execute-strict pexpr penv
                      (lambda (value fail)
                        (advance-memo! object value)
                        (succeed value
                                 (lambda ()
                                   (continue-revert! object
                                                     pexpr
                                                     penv)
                                   (fail))))
                      fail))))

(define-generic-procedure-handler a:advance
  (match-args advanced-memo? success? failure?)
  (lambda (object succeed fail)
    (succeed (advanced-value object) fail)))