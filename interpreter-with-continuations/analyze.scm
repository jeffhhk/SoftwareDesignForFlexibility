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

;;;; Interpreter that provides call/cc

;;;  Separating analysis from execution.
;;;    Continuation-passing style execution.
;;;  Generic analysis, with lazy and lazy-memo parameters.
;;;    Simple compound procedures (Scheme) separated.
;;;    Complex compound procedure parameters 
;;;      are parsed at runtime, ugh.
;;;      no rest parameters.

(define (c:eval expression environment continue)
  ((analyze expression) environment continue))

(define (analyze expression)
  (make-executor (c:analyze expression)))

(define (default-analyze expression)
  (cond ((application? expression)
         (analyze-application expression))
        (else (error "Unknown expression type" expression)
              barf)))

(define c:analyze
  (simple-generic-procedure 'c:analyze 1 default-analyze))

(define (analyze-application expression)
  (let ((operator-exec (analyze (operator expression)))
        (operand-execs (map analyze (operands expression))))
    (lambda (environment continue)
      (c:execute-strict operator-exec environment
         (lambda (proc)
           (c:apply proc
                    operand-execs
                    environment
                    continue))))))

(define (c:execute-strict executor env continue)
  (executor env
            (lambda (value)
              (c:advance value continue))))

(define (analyze-self-evaluating expression)
  (lambda (environment continue)
    (declare (ignore environment))
    (continue expression)))

(define-generic-procedure-handler c:analyze
  (match-args self-evaluating?)
  analyze-self-evaluating)


(define (analyze-variable expression)
  (lambda (environment continue)
    (continue
     (lookup-variable-value expression environment))))

(define-generic-procedure-handler c:analyze
  (match-args variable?)
  analyze-variable)


(define (analyze-quoted expression)
  (let ((qval (text-of-quotation expression)))
    (lambda (environment continue)
      (declare (ignore environment))
      (continue qval))))

(define-generic-procedure-handler c:analyze
  (match-args quoted?)
  analyze-quoted)

;;; This is a bit more sophisticated... I separated procedures
;;; with simple Scheme-like parameter lists from more general
;;; procedures with declarations like lazy on the parameters.

(define (simple-parameter-list? vars)
  (or (null? vars)
      (symbol? vars)
      (and (pair? vars)
           (symbol? (car vars))
           (simple-parameter-list? (cdr vars)))))

(define (analyze-lambda expression)
  (let ((vars (lambda-parameters expression))
        (body-exec (analyze (lambda-body expression))))
    (if (simple-parameter-list? vars)
        (lambda (environment continue)
          (continue
           (make-simple-compound-procedure vars
                                           body-exec
                                           environment)))
        (lambda (environment continue)
          (continue
           (make-complex-compound-procedure vars
                                            body-exec
                                            environment))))))

(define-generic-procedure-handler c:analyze
  (match-args lambda?)
  analyze-lambda)

(define (analyze-if expression)
  (let ((predicate-exec (analyze (if-predicate expression)))
        (consequent-exec (analyze (if-consequent expression)))
        (alternative-exec (analyze (if-alternative expression))))
    (lambda (environment continue)
      (define (decide predicate-value continue)
        (if predicate-value
            (consequent-exec environment continue)
            (alternative-exec environment continue)))
      (c:execute-strict predicate-exec environment
        (lambda (pval)
          (decide pval continue))))))

(define-generic-procedure-handler c:analyze
  (match-args if?)
  analyze-if)

(define (analyze-begin expression)
  (reduce-right
   (lambda (exec1 exec2)
     (lambda (environment continue)
       (exec1 environment
              (lambda args           ;ignore exec1 value(s)
                (exec2 environment continue)))))
   #f
   (map analyze
        (let ((exps (begin-actions expression)))
          (if (null? exps)
              (error "Empty sequence"))
          exps))))

(define-generic-procedure-handler c:analyze
  (match-args begin?)
  analyze-begin)

(define (analyze-assignment expression)
  (let ((var
         (assignment-variable expression))
        (value-exec
         (analyze (assignment-value expression))))
    (lambda (environment continue)
      (value-exec environment
        (lambda (val)
          (set-variable-value! var val environment)
          (continue `(,var assigned)))))))

(define-generic-procedure-handler c:analyze
  (match-args assignment?)
  analyze-assignment)


(define (analyze-definition expression)
  (let ((var (definition-variable expression))
        (value-exec (analyze (definition-value expression))))
    (lambda (environment continue)
      (value-exec environment
        (lambda (val)
          (define-variable! var val environment)
          (continue `(,var defined)))))))

(define-generic-procedure-handler c:analyze
  (match-args definition?)
  analyze-definition)

;;; Macros (definitions are in syntax.scm)

(define-generic-procedure-handler c:analyze
  (match-args cond?)
  (compose analyze cond->if))

(define-generic-procedure-handler c:analyze
  (match-args let?)
  (compose analyze let->combination))

(define-generic-procedure-handler c:analyze
  (match-args let*?)
  (compose analyze let*->let-nest))

;;; Operand evaluation

(define (c:execute-operands execute operands env continue)
  (map-continuation (lambda (operand continue)
                      (execute operand env continue))
                    operands
                    continue))

(define (map-continuation f things continue)
  (if (null? things)
      (continue '())
      (f (car things)
         (lambda (val)
           (map-continuation f
                             (cdr things)
                             (lambda (vals)
                               (continue (cons val vals))))))))

;;; Application

(define (continuation? x) (procedure? x))
(register-predicate! continuation? 'continuation)

(define (default-apply procedure operand-execs
                       environment continue)
  (declare (ignore operand-execs environment))
  (error "Unknown procedure type" procedure)
  barf)

(define c:apply
  (simple-generic-procedure 'c:apply 4 default-apply))

(define (primitive-or-simple-procedure? p)
  (or (strict-primitive-procedure? p)
      (simple-compound-procedure? p)
      (call/cc? p)))
(register-predicate! primitive-or-simple-procedure?
                     'primitive-or-simple-procedure)

(define (general-procedure? p)
  (or (strict-primitive-procedure? p)
      (simple-compound-procedure? p)
      (complex-compound-procedure? p)
      (call/cc? p)))

(register-predicate! general-procedure? 'general-procedure)

(define-generic-procedure-handler c:apply
  (match-args primitive-or-simple-procedure?
              executors?
              environment?
              continuation?)
  (lambda (procedure operand-execs env continue)
    (c:execute-operands c:execute-strict operand-execs env
      (lambda (args)
        (c:apply-strict procedure
                        args
                        continue)))))

;;; Traditional APPLY, as in Scheme
(define (c:apply-strict procedure args continue)
  (cond ((strict-primitive-procedure? procedure)
         (continue (apply-primitive-procedure procedure args)))
        ((simple-compound-procedure? procedure)
         (c:compound-apply procedure args continue))
        ((call/cc? procedure)
         (c:deliver-continuation (car args) continue))
        (else (error "Bad strict procedure" procedure args)
              'to-retain-stack)))

(define (c:compound-apply procedure args continue)
  (let ((params (simple-procedure-parameters procedure))
        (penv (simple-procedure-environment procedure))
        (body-exec (simple-procedure-body procedure)))
    (body-exec (extend-environment params args penv)
               continue)))

(define-generic-procedure-handler c:apply
  (match-args complex-compound-procedure?
              executors?
              environment?
              continuation?)
  (lambda (procedure operand-execs calling-environment continue)
    (if (not (n:= (length
                   (complex-procedure-parameters procedure))
                  (length operand-execs)))
        (error "Wrong number of operands supplied"))
    
    (let ((params (complex-procedure-parameters procedure))
          (penv (complex-procedure-environment procedure))
          (body-exec (complex-procedure-body procedure)))
      (let ((names (map procedure-parameter-name params)))
        (c:execute-operands
         (lambda (operand env continue)
           (c:handle-operand (car operand)
                             (cdr operand)
                             env
                             continue))
         (map cons params operand-execs)
         calling-environment
         (lambda (args)
             (body-exec (extend-environment names args penv)
                        continue)))))))

(define c:handle-operand
  (simple-generic-procedure 'c:handle-operand 4
    (lambda (parameter operand-exec environment continue)
      (declare (ignore parameter))
      (operand-exec environment continue))))

(define-generic-procedure-handler c:handle-operand
  (match-args lazy? executor? environment? continuation?)
  (lambda (parameter operand-exec environment continue)
    (declare (ignore parameter))
    (continue (postpone operand-exec environment))))

(define-generic-procedure-handler c:handle-operand
  (match-args lazy-memo? executor? environment? continuation?)
  (lambda (parameter operand-exec environment continue)
    (declare (ignore parameter))
    (continue (postpone-memo operand-exec environment))))

(define-generic-procedure-handler procedure-parameter-name
  (match-args pair?)
  car)

(define c:advance
  (simple-generic-procedure 'c:advance 2
    (lambda (x continue) (continue x))))

(define-generic-procedure-handler c:advance
  (match-args postponed? continuation?)
  (lambda (object continue)
    (let ((pexpr (postponed-expression object))
          (penv (postponed-environment object)))
      (c:execute-strict pexpr penv continue))))

(define-generic-procedure-handler c:advance
  (match-args postponed-memo? continuation?)
  (lambda (object continue)
    (let ((pexpr (postponed-expression object))
          (penv (postponed-environment object)))
      (c:execute-strict pexpr penv
        (lambda (val)
          (advance-memo! object val)
          (continue val))))))

(define-generic-procedure-handler c:advance
  (match-args advanced-memo? continuation?)
  (lambda (object continue)
    (continue (advanced-value object))))

;;; Implementation of call/cc: predicate in rtdata-extra; dispatch in
;;; c:apply-strict; also modifies primitive-or-simple-procedure?

;;; This definition does not fully abandon continue.  Thus there
;;; are strange reentry properties of the code, but it works with
;;; a repl that ignores extra returns by always running a
;;; continuation.

(define (c:deliver-continuation receiver continue)
  (c:apply-strict receiver
                  (list continue)
                  continue))
