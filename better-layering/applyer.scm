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

;;;; This is the applicator for layered procedures.

(define (l:apply-layered layered-procedure operand-execs
                         env continue)
  (l:execute-operands l:execute-strict operand-execs env
    (extend-continuation
     (lambda (args)
       (l:dispatch layered-procedure args continue)))))

(define-generic-procedure-handler l:apply
  (match-args layered-thing? executors?
              environment? continuation?)
  l:apply-layered)

(define (l:dispatch layered-procedure args continue)
  (define process-a-layer         
    (continue *layered-procedure-values*))
  (layers-processor layered-procedure
                    args
                    (apply lset-union eq?
                           (available-layers layered-procedure)
                           (map available-layers args))
                    process-a-layer)
  (continue *end-of-layers*))

(define (layers-processor layered-procedure args
                          relevant-layers process-a-layer)
  (for-each (lambda (layer-name)
              (process-a-layer layer-name
                (layer-applicator layered-procedure
                                  args
                                  layer-name)))
            relevant-layers))

(define (layer-applicator layered-procedure args layer-name)
  (lambda (current-layer-value layer-value-setter)
    (cond ((eq? layer-name 'base)
           (base-layer-applicator layered-procedure
                                  args
                                  layer-value-setter))
          ((any (lambda (arg)      ; A changeable policy.
                  (memq layer-name
                        (available-layers arg)))
                args)
           (l:apply-strict (procedure-layer-or-default
                            layer-name layered-procedure)
                           (cons current-layer-value args)
                           layer-value-setter))
          (else 'OK))))

(define (base-layer-applicator layered-procedure args setter)
  (let ((base-procedure (base-value layered-procedure)))
    (cond ((strict-primitive-procedure? base-procedure)
           (l:apply-strict base-procedure
                           (map base-value args)
                           setter))
          ((simple-compound-procedure? base-procedure)
           (l:apply-strict base-procedure args setter))
          (else
           (error "Bad layered procedure"
                  base-procedure args)))))

(define (layer-args procedure layer)
  (lambda (current . args)
    (apply procedure
           current
           (map (lambda (arg)
                  (get-layer-value-or-default layer arg))
                args))))

(define (base-and-layer-args procedure layer)
  (lambda (current . args)
    (apply procedure
           current
           (map (lambda (arg)
                  (cons (get-layer-value 'base arg)
                        (get-layer-value-or-default layer arg)))
                args))))

#|
;;; For example

(define (only-provenance-layer-args proc)
  (with-new-layer! proc
                   'provenance
                   (layer-args
                     (lambda args       ;including current
                       (apply lset-union 'equal? args))
                     'provenance)))

;;; + does not need to look at base args

(global-environment-define '+ (only-provenance-layer-args +))


;;; but if an arg to * is zero, that is all that matters!

(global-environment-define '*
  (with-new-layer! * 'provenance
                   (base-and-layer-args 
                    (lambda (current . blargs)
                      (let ((a (assv 0 blargs)))
                        (if a
                            (cdr a)
                            (apply union
                                   (cons current
                                         (map cdr blargs))))))
                    'provenance)))
|#