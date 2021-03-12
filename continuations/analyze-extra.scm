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

;;; call/ccs is a special named procedure

(define call/ccs (list 'call/ccs))
(define (call/ccs? x) (eq? x call/ccs))
(register-predicate! call/ccs? 'call/ccs)
(define-initial-env-binding 'call/ccs call/ccs)

;; coderef: apply-call/ccs
(define-generic-procedure-handler a:apply
  (match-args call/ccs? executors? environment?
              success? failure?)
  (lambda (ignore operand-execs env succeed fail)
    (declare (ignore ignore))
    (if (not (n:= 1 (length operand-execs)))
        (error "Wrong number of operands supplied"))
    (let ((receiver-exec (car operand-execs))
          (reified-succeed (make-reified-succeed succeed))
          (reified-fail (make-reified-fail fail)))
      (execute-strict receiver-exec env
        (lambda (receiver fail)
          (a:apply receiver
                   (list (constant-executor reified-succeed)
                         (constant-executor reified-fail))
                   env
                   succeed
                   fail))
        fail))))

(define-record-type <reified-succeed>
    (make-reified-succeed procedure)
    reified-succeed?
  (procedure reified-succeed-procedure))

(define-record-type <reified-fail>
    (make-reified-fail procedure)
    reified-fail?
  (procedure reified-fail-procedure))

(define (constant-executor constant)
  (make-executor
   (lambda (env succeed fail)
     (declare (ignore env))
     (succeed constant fail))))

;; coderef: apply-reified-succeed
(define-generic-procedure-handler a:apply
  (match-args reified-succeed? executors? environment?
              success? failure?)
  (lambda (reified-succeed operand-execs env succeed fail)
    (declare (ignore succeed))
    (if (not (n:= 2 (length operand-execs)))
        (error "Wrong number of arguments supplied"))
    (execute-operands execute-strict operand-execs env
      (lambda (args fail)
        (declare (ignore fail))
        ((reified-succeed-procedure reified-succeed)
         (car args)                     ;maybe not strict?
         (let ((fail-arg (cadr args)))
           (if (reified-fail? fail-arg)
               (reified-fail-procedure fail-arg)
               ;;(error "WTF?")
               (lambda ()
                 (a:apply fail-arg '() the-empty-environment
                          (lambda (val fail*)
                            (error "Should not be called"))
                          (lambda ()
                            (error "Should not be called"))))
               ))))
      fail)))

;; coderef: apply-reified-fail
(define-generic-procedure-handler a:apply
  (match-args reified-fail? executors? environment?
              success? failure?)
  (lambda (reified-fail operand-execs env succeed fail)
    (declare (ignore env succeed fail))
    (if (not (null? operand-execs))
        (error "Wrong number of arguments supplied"))
    ((reified-fail-procedure reified-fail))))