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

;;;; This is the setup for a units layer.

(check-repl-initialized)

;;; A layered thing with units has a units entry in the
;;; layer map.  The layer value of the units entry is
;;; an alist of entries pairing each unit-name with an
;;; exponent.  For convenience we keep the units entry
;;; sorted by the unit names.
  
(define (with-units x units-alist)
  (let ((ua (sort units-alist
                  (lambda (u1 u2)
                    (symbol<? (car u1) (car u2))))))
    (if (layered-thing? x)
        (let ((thing (layered-thing x)))
          ((thing 'set-layer-value!) 'units ua)
          x)
        (with-new-layer! (make-base-value x) 'units ua))))

(define *unitless-alist* '())
(define (unitless-alist? x) (eq? x *unitless-alist*))

(set-default-layer-value! 'units *unitless-alist*)

;;; This suppresses the unit-layer output of a layered
;;; procedure without an explicit units layer.
(declare-layered-procedure-default 'units
 (lambda (current . args) *ignore-value*))

(declare-merge-handler 'units
  (lambda (current new)
    (let ((new-units (layer-value 'units new)))
      (if (not (unset? current))
          (assert (equal? current new-units)))
      new-units)))

(define (unitless? x)
  (or (not (layered-thing? x))
      (let ((v (layer-value 'units (layered-thing x))))
        (or (eq? v *novalue*)
            (eq? v *unitless-alist*)))))

(define (all-inputs-and-output-same-units operator)
  (layer-args
   (lambda (current . args)
     (if (unset? current)
         (if (not (null? args))
             (begin
               (assert (apply lset= equal? args)
                       "not all args have same units"
                       operator args)
               (car args))
             *unitless-alist*)
         (begin (assert (apply lset= equal? (cons current args))
                        "not all args have same units"
                        operator current args)
                current)))
   'units))

(install-layer-handler! '+ 'units
                        all-inputs-and-output-same-units)

(install-layer-handler! '- 'units
                        all-inputs-and-output-same-units)

(define (multiply-input-units operator)
  (layer-args
   (lambda (current . args)
     (let ((output
            (reduce units:*
                    *unitless-alist*
                    args)))
       (if (not (unset? current))
           (lset= equal? current output))
       output))
   'units))

(define (units:* unit-alist1 unit-alist2)
  (cond ((null? unit-alist1) unit-alist2)
        ((null? unit-alist2) unit-alist1)
        ((symbol<? (caar unit-alist1)
                   (caar unit-alist2))
         (cons (car unit-alist1)
               (units:* (cdr unit-alist1)
                        unit-alist2)))
        ((symbol<? (caar unit-alist2)
                   (caar unit-alist1))
         (cons (car unit-alist2)
               (units:* unit-alist1
                        (cdr unit-alist2))))
        (else                           ;equal
         (let ((s
                (+ (cadar unit-alist1)
                   (cadar unit-alist2))))
           (if (zero? s)
               (units:* (cdr unit-alist1)
                        (cdr unit-alist2))
               (cons (list (caar unit-alist1) s)
                     (units:* (cdr unit-alist1)
                              (cdr unit-alist2))))))))

(install-layer-handler! '* 'units multiply-input-units)

(define (negate-exponents unit-alist)
  (map (lambda (entry)
         (list (car entry)
               (- (cadr entry))))
       unit-alist))

(define (divide-input-units operator)
  (layer-args
   (lambda (current . args)
     (let ((output-units 
            (case (length args)
              ((0)
               (error "No args to division"
                      operator))
              ((1)
               (negate-exponents (car args)))
              (else
               (units:* (car args)
                        (negate-exponents
                         (reduce units:*
                                 (cadr args)
                                 (cddr args))))))))
       (if (not (unset? current))
           (assert (lset= equal? current output-units)))
       output-units))
   'units))
                                   

(install-layer-handler! '/ 'units divide-input-units)

(define (all-inputs-have-same-units-output-unitless operator)
  (layer-args
   (lambda (current . args)
     (assert
      (apply lset= equal? args)
      "not all args have same units"
      operator args)
     (if (not (unset? current))
         (lset= equal? current *unitless-alist*))
     *unitless-alist*)
   'units))

(install-layer-handler! '= 'units
        all-inputs-have-same-units-output-unitless)

(install-layer-handler! '< 'units
        all-inputs-have-same-units-output-unitless)

(install-layer-handler! '> 'units
        all-inputs-have-same-units-output-unitless)


(define (all-inputs-and-output-are-unitless operator)
  (layer-args
   (lambda (current . args)
     (assert (every (lambda (arg)
                      (eq? arg *unitless-alist*))
                    (if (unset? current)
                        args
                        (cons current args)))
             "not all args are unitless"
             operator args)
     *unitless-alist*)
   'units))

(install-layer-handler! 'exp 'units
        all-inputs-and-output-are-unitless)

(install-layer-handler! 'sin 'units
        all-inputs-and-output-are-unitless)

(install-layer-handler! 'cos 'units
        all-inputs-and-output-are-unitless)

(define (units:sqrt operator)
  (layer-args
   (lambda (current . args)
     (let ((output
            (map (lambda (entry)
                   (list (car entry)
                         (/ (cadr entry) 2)))
                 (car args))))
       (if (not (unset? current))
           (assert (lset= equal? current output)))
       output))
   'units))

(install-layer-handler! 'sqrt 'units units:sqrt)

;;; Need to do atan and list stuff later
