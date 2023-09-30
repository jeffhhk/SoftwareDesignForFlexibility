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

;;; This takes an ordinary continuation and extends it
;;; to allow the provider to pass in values for layers 
;;; separately. 

(define (extend-continuation continuation)
  (let ((dict (make-dictionary)) (nested-applications 0))
      
    (define (return-if-complete)
      (if (= nested-applications 0)
          (continuation
           (make-layered-thing ((dict 'filled-entries))))
          'multiple-nested-applications))

    (define (process-a-layer layer-name updater)
      (updater ((dict 'get-value) layer-name)
               (lambda (new-value)
                 (value-dispatch new-value
                   (lambda (new-value)
                     (if (and (eq? layer-name 'base)
                              (layered-thing? new-value))
                         (accept-value
                          (available-layers new-value)
                          merge-handler 
                          new-value)
                         (value-receiver layer-name
                                         new-value)))))))

    (define (value-receiver layer-name new-value)
      (cond ((ignore-value? new-value)
             'nothing-to-do)
            (else
             ((dict 'update!) layer-name new-value)
             'updated-layer-with-new-value)))

    (define (process-predicate-layers predicate-value)
      (lambda (continue-to-c/a)
        (accept-value (available-layers predicate-value)
                      if-predicate-handler
                      predicate-value)
        (continue-to-c/a)))

    (define (process-c/a-layers c/a-value)
      (value-dispatch c/a-value
        (lambda (c/a-value)
          (accept-value (available-layers c/a-value)
                        if-c/a-handler
                        c/a-value)
          (return-if-complete))))

    (define (accept-value layer-names handler value)
      (for-each
       (lambda (layer-name)
         (value-receiver layer-name
                         ((handler layer-name)
                          ((dict 'get-value) layer-name)
                          value)))
       layer-names))

    (define (value-dispatch arg else-arg)
      (cond ((if-predicate-value? arg)
             process-predicate-layers)
            ((if-c/a-value? arg)
             process-c/a-layers)
            ((layered-procedure-values? arg)
             (set! nested-applications
                   (+ nested-applications 1))
             process-a-layer)
            ((end-of-layers? arg)
             (set! nested-applications
                   (- nested-applications 1))
             (return-if-complete))
            (else (else-arg arg))))
    (define (the-extended-continuation arg)
      (value-dispatch arg continuation))
    the-extended-continuation))

(define *layered-procedure-values*
  (list 'layered-procedure-values))
(define (layered-procedure-values? x)
  (eq? x *layered-procedure-values*))

(define *if-predicate-value*
  (list 'if-predicate-value))
(define (if-predicate-value? x)
  (eq? x *if-predicate-value*))

(define *if-c/a-value*
  (list 'if-c/a-value))
(define (if-c/a-value? x)
  (eq? x *if-c/a-value*))

(define *end-of-layers*
  (list 'end-of-layers))
(define (end-of-layers? x)
  (eq? x *end-of-layers*))

(define *unset* (list '*unset*))
(define (unset? x) (eq? x *unset*))

(define *ignore-value*
  (list 'ignore-value))
(define (ignore-value? x)
  (eq? x *ignore-value*))

(define (merge-handler layer-name)
  (cond ((assq layer-name *merge-handlers*)
         => cadr)
        (else
         (lambda (p-value-in-dict p-value)
           *ignore-value*))))

(define (if-predicate-handler layer-name)
  (cond ((assq layer-name *if-predicate-handlers*)
         => cadr)
        (else
         (lambda (p-value-in-dict p-value)
           *ignore-value*))))

(define (if-c/a-handler layer-name)
  (cond ((assq layer-name *if-c/a-handlers*)
         => cadr)
        (else
         (lambda (c/a-value-in-dict c/a-value)
           *ignore-value*))))

(define (declare-merge-handler layer-name handler)
  (cond ((assq layer-name *merge-handlers*)
         => (lambda (entry) (set-car! (cdr entry) handler)))
        (else
         (set! *merge-handlers*
               (cons (list layer-name handler)
                     *merge-handlers*)))))

(define (declare-if-predicate-handler layer-name handler)
  (cond ((assq layer-name *if-predicate-handlers*)
         => (lambda (entry) (set-car! (cdr entry) handler)))
        (else
         (set! *if-predicate-handlers*
               (cons (list layer-name handler)
                     *if-predicate-handlers*)))))

(define (declare-if-c/a-handler layer-name handler)
  (cond ((assq layer-name *if-c/a-handlers*)
         => (lambda (entry) (set-car! (cdr entry) handler)))
        (else
         (set! *if-c/a-handlers*
               (cons (list layer-name handler)
                     *if-c/a-handlers*)))))

;;; Some handlers.  Probably do not need separate predicate and
;;; c/a handlers.

(define (base-merge-handler base-value-in-dict new-base-value)
  (if (or (unset? base-value-in-dict)
          (equal? base-value-in-dict
                  (base-value new-base-value)))
      (base-value new-base-value)
      (begin (bkpt "Multiple Base values?")
             'error-not-in-tail-position-marker)))

(define *merge-handlers*
  (list (list 'base base-merge-handler)))


(define *if-predicate-handlers* '())

(define (if-c/a-base-handler base-value-in-dict c/a-value)
  (if (or (unset? base-value-in-dict)
          (equal? base-value-in-dict (base-value c/a-value)))
      (base-value c/a-value)
      (begin (bkpt "Multiple Base values?")
             'error-not-in-tail-position-marker)))

(define *if-c/a-handlers*
  (list (list 'base if-c/a-base-handler)))

(define (make-dictionary)
  (let ((dict '()))
    (define (get-entry layer-name)
      (let ((entry (assq layer-name dict)))
        (if (not entry)
            (let ((entry (list layer-name *unset*)))
              (set! dict (cons entry dict))
              entry)
            entry)))
    (define (update! layer-name new-value)
      (let ((entry (get-entry layer-name)))
        (set-car! (cdr entry) new-value)))
    (define (get-value layer-name)
      (cadr (get-entry layer-name)))
    (define (filled-entries)
      (filter (lambda (entry)
                  (not (unset? (cadr entry))))
                dict))
    (define (the-dict m)
      (case m
        ((get-value) get-value)
        ((update!) update!)
        ((filled-entries) filled-entries)
        ((get-entry) get-entry)
        ((all-entries) dict)
        (else (error "unknown message -- dictionary" m))))
    the-dict))