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

;;;;  Trivial support set layer test.

(check-repl-initialized)

;;; The provenance layer of a layered thing is a
;;; provenance entry in the layer map.  The
;;; provenance layer value is a support set,
;;; represented as a list.

;;; To make things easier to type:

(define (signed x signature)
  (cond ((layered-thing? x)
         ;;(pp `(signing a layered thing ,x))
         (let* ((svalue (base-value x))
                (thing (layered-thing x))
                (pvalue (thing 'provenance)))
           (if (general-procedure? svalue)
               ((thing 'set-layer-value!)
                'provenance
                (add-signature-to-procedure signature))
               (if (has-layer-value? pvalue)
                   ((thing 'set-layer-value!)
                    'provenance
                    (lset-adjoin equal? pvalue signature))
                   ((thing 'set-layer-value!)
                    'provenance
                    (list signature))))
           x))
        ((general-procedure? x)
         ;;(pp `(signing a procedure ,x))
         (with-new-layer! (make-base-value x) 'provenance
               (add-signature-to-procedure signature)))
        (else
         ;;(pp `(signing a base object ,x))
         (with-new-layer! (make-base-value x)
                          'provenance (list signature)))))

(define (add-signature-to-procedure signer)
  (lambda (value . args)
    (lset-adjoin equal?
                 (apply lset-union
                        equal?
                        (unset->empty value)
                        (map (lambda (arg)
                               (get-layer-value-or-default
                                'provenance arg))
                             args))
                 signer)))


;;; Setting up a layer for tracking provenance of data:

(set-default-layer-value! 'provenance '())

(define (unset->empty value)
  (if (unset? value) '() value))

(define (union-contributions value . args)
  (union-contributions* value args))

(define (union-contributions* value args)
  (apply lset-union equal? (unset->empty value) args))

;;; Merger for provenance in IF conditionals

(define (provenance-merge value new-value)
  (union-contributions value
    (get-layer-value-or-default 'provenance new-value)))

(declare-merge-handler 'provenance provenance-merge)

(declare-if-predicate-handler 'provenance provenance-merge)
(declare-if-c/a-handler 'provenance provenance-merge)

;;; Now for primitive procedures

(define (only-provenance-layer-args operator)
  (layer-args union-contributions 'provenance))

;;; **code smell** here
(declare-layered-procedure-default 'provenance 
  (only-provenance-layer-args 'unknown-procedure)) 

(install-layer-handler! '+ 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '- 'provenance
                        only-provenance-layer-args)

#|
(define (provenance:* operator)
  ;; This needs to be more sophisticated, see below
  (base-and-layer-args 
   (lambda (value . blargs)
     (let ((a (assv 0 blargs)))
       (if a
           (cdr a)
           (union-contributions* value
                                 (map cdr blargs)))))
   'provenance))
|#

(define (provenance:* operator)
  (base-and-layer-args 
   ;; blarg = (base-arg . support-set-arg)
   (lambda (value . blargs)
     (let ((a ; zero arg with smallest support set
            (sort (filter (lambda (blarg)
                            (eqv? (car blarg) 0))
                          blargs)
                  (lambda (bl1 bl2)
                    (< (length (cdr bl1))
                       (length (cdr bl2)))))))
                                           
       (if (not (null? a))
           (cdar a)
           (union-contributions* value
                                 (map cdr blargs)))))
   'provenance))

(install-layer-handler! '* 'provenance provenance:*)

(define (provenance:/ operator)
  (base-and-layer-args 
   (lambda (value . blargs)
     (if (zero? (caar blargs))
         (cdar blargs)
         (union-contributions* value (map cdr blargs))))
   'provenance))

(install-layer-handler! '/ 'provenance provenance:/)


(install-layer-handler! '= 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '< 'provenance
                        only-provenance-layer-args)
(install-layer-handler! '> 'provenance
                        only-provenance-layer-args)

(install-layer-handler! 'exp 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'sin 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cos 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'sqrt 'provenance
                        only-provenance-layer-args)

(define (provenance:atan operator)
  (base-and-layer-args
   (lambda (value . blargs)
     (let ((a (assv 0 blargs)))
       (if a
           (cdr a)
           (union-contributions* value (map cdr blargs)))))
   'provenance))

(install-layer-handler! 'atan 'provenance provenance:atan)

(install-layer-handler! 'car 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cdr 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'cons 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'pair? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'eq? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'null? 'provenance
                        only-provenance-layer-args)
(install-layer-handler! 'list 'provenance
                        only-provenance-layer-args)
