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

;;; -*- Mode:Scheme -*-

;;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (boolean? exp)
      (string? exp)))   ; Our prompt (viz., "EVAL==> ") is a string.
(register-predicate! self-evaluating? 'self-evaluating)


;;; Variables

(define (variable? exp) (symbol? exp))
(register-predicate! variable? 'variable)

;;; Nice abstraction
(define (same-variable? var1 var2) (eq? var1 var2))

;;; Special forms (in general)

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))
(register-predicate! tagged-list? 'tagged-list)

;;; Quotations

(define (quoted? exp) (tagged-list? exp 'quote))
(register-predicate! quoted? 'quoted)

(define (text-of-quotation quot) (cadr quot))


;;; Assignment--- SET!

(define (assignment? exp) (tagged-list? exp 'set!))
(register-predicate! assignment? 'assignment)

;;; Undoable assignment
(define (undoable-assignment? exp)
  (tagged-list? exp 'maybe-set!))
(register-predicate! undoable-assignment? 'undoable-assignment)

(define (assignment-variable assn) (cadr  assn))
(define (assignment-value    assn) (caddr assn))

;;; Definitions

(define (definition? exp) (tagged-list? exp 'define))
(register-predicate! definition? 'definition)

(define (definition-variable defn)
  (if (variable? (cadr defn))           ;;   (DEFINE  foo      ...)
      (cadr  defn)
      (caadr defn)))                    ;;   (DEFINE (foo ...) ...)

(define (definition-value defn)
  (if (variable? (cadr defn))           ;;   (DEFINE  foo        ...)
      (caddr defn)
      (cons 'lambda                     ;;   (DEFINE (foo p...) b...)
            (cons (cdadr defn)          ;; = (DEFINE  foo
                  (cddr  defn)))))      ;;     (LAMBDA (p...) b...))


;;; LAMBDA expressions

(define (lambda? exp) (tagged-list? exp 'lambda))
(register-predicate! lambda? 'lambda)

(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp)
  (let ((full-body (cddr lambda-exp)))
    (sequence->begin full-body)))

(define (make-lambda parameters body)
  (cons 'lambda
        (cons parameters
              (if (begin? body)
                  (begin-actions body)
                  (list body)))))

(define procedure-parameter-name
  (simple-generic-procedure 'parameter-name 1 (lambda (x) x)))

(define (parameter-name var-decl)
  (if (pair? var-decl)
      (car var-decl)
      var-decl))

(define (lazy? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (not (memq 'memo (cdr var-decl)))))
(register-predicate! lazy? 'lazy)

(define (lazy-memo? var-decl)
  (and (pair? var-decl)
       (memq 'lazy (cdr var-decl))
       (memq 'memo (cdr var-decl))))
(register-predicate! lazy-memo? 'lazy-memo)

;;; If conditionals

(define (if? exp) (tagged-list? exp 'if))
(register-predicate! if? 'if)

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'the-unspecified-value))

(define (make-if pred conseq alternative)
  (list 'if pred conseq alternative))


;;; COND Conditionals

(define (cond? exp) (tagged-list? exp 'cond))
(register-predicate! cond? 'cond)

(define (cond-clauses exp) (cdr exp))

(define (cond-clause-predicate clause)
  (car clause))

(define (cond-clause-consequent clause)
  (sequence->begin (cdr clause)))

(define (else-clause? clause)
  (eq? (cond-clause-predicate clause) 'else))

(define (cond->if cond-exp)
  (define (expand clauses)
    (cond ((null? clauses)
           (error "COND: no values matched"))
          ((else-clause? (car clauses))
           (if (null? (cdr clauses))
               (cond-clause-consequent (car clauses))
               (error "COND: ELSE not last"
                      cond-exp)))
          (else
           (make-if (cond-clause-predicate (car clauses))
                    (cond-clause-consequent (car clauses))
                    (expand (cdr clauses))))))
  (expand (cond-clauses cond-exp)))

(define (sequence->begin seq)
  (cond ((null? seq) seq)
        ((null? (cdr seq)) (car seq))
        (else
         (make-begin
          (append-map (lambda (exp)
                        (if (begin? exp)
                            (begin-actions exp)
                            (list exp)))
                      seq)))))

;;; BEGIN expressions (a.k.a. sequences)

(define (begin? exp) (tagged-list? exp 'begin))
(register-predicate! begin? 'begin)

(define (begin-actions begin-exp) (cdr begin-exp))

(define (make-begin actions) (cons 'begin actions))

;;; LET expressions

(define (let? exp) (tagged-list? exp 'let))
(register-predicate! let? 'let)

(define (let-varspecs let-exp)
  (cadr let-exp))

(define (let-bound-variables let-exp)
  (map car (cadr let-exp)))
(define (let-bound-values let-exp) (map cadr (cadr let-exp)))
(define (let-body let-exp) (sequence->begin (cddr let-exp)))
(define (let->combination let-exp)
  (let ((names (let-bound-variables let-exp))
        (values (let-bound-values let-exp))
        (body (let-body let-exp)))
    (cons (make-lambda names body)
          values)))

(define (let*? exp) (tagged-list? exp 'let*))
(register-predicate! let*? 'let*)

(define (let*->let-nest let-exp)
  (let lp ((specs (let-varspecs let-exp)))
    (cond ((null? specs) (let-body let-exp))
          (else
           `(let (,(car specs))
              ,(lp (cdr specs)))))))

;;; Procedure applications -- NO-ARGS? and LAST-OPERAND? added

(define (application? exp) (pair? exp))
(register-predicate! application? 'application)

(define (operands? exps) (list? exps))
(register-predicate! operands? 'operands)


;;; perhaps do better?
(define (operand? exp) #t)
(register-predicate! operand? 'operand)


(define (no-args? exp)          ;; Added for tail recursion
  (and (pair? exp)
       (null? (cdr exp))))
(register-predicate! no-args? 'no-args)

(define (args-application? exp)         ;; Changed from 5.2.1
  (and (pair? exp)
       (not (null? (cdr exp)))))
(register-predicate! args-application? 'args-application)

(define (operator app) (car app))
(define (operands app) (cdr app))

(define (last-operand? args)      ;; Added for tail recursion
  (null? (cdr args)))
(register-predicate! last-operand? 'last-operand)

(define (no-operands? args) (null? args))
(register-predicate! no-operands? 'no-operands)

(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))


;;; Another special form that will be needed later.

(define (amb? exp)
  (and (pair? exp) (eq? (car exp) 'amb)))
(register-predicate! amb? 'amb)

(define (amb-alternatives exp) (cdr exp))