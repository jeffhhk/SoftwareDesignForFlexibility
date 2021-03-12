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

;;; A rule is a procedure constructed from a pattern and a
;;; consequent procedure (the handler).  A rule takes data that
;;; the rule may apply to and a success and failure continuation.
;;; The consequent procedure binds the variables named in the
;;; pattern.  It produces the result of the rule as a function of
;;; the values of those variables that matched the data
;;; presented.

(define (make-rule pattern handler)
  (let ((match-procedure (match:compile-pattern pattern)))
    (define (the-rule data succeed fail)
      (or (run-matcher match-procedure data
            (lambda (dict)
              (let ((result
                     (apply handler
                            (match:all-values dict))))
                (and result
                     (succeed result
                              (lambda () #f))))))
          (fail)))
    the-rule))

(define-syntax rule
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((pattern (cadr form))
           (handler-body (caddr form))
           (r-make-rule (rename 'make-rule))
           (r-lambda (rename 'lambda)))
       `(,r-make-rule ,pattern
                      (,r-lambda ,(match:pattern-names pattern)
                        ,handler-body))))))

#|
;;; Alternate implementation:

(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((pattern (cadr form))
           (handler-body (caddr form)))
       `(make-rule
         ,(close-syntax pattern use-env)
         ,(compile-handler handler-body use-env
                           (match:pattern-names pattern)))))))

(define (compile-handler form env names)
  ;; See magic in utils.scm
  (make-lambda names env
    (lambda (env*) (close-syntax form env*))))
|#
