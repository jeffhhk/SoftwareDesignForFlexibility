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

(define (unit? object)
  (and (pair? object)
       (eq? (car object) %unit-tag)
       (list? (cdr object))
       (every (lambda (elt)
                (and (pair? elt)
                     (symbol? (car elt))
                     (exact-rational? (cdr elt))))
              (cdr object))))

(define %unit-tag
  '|#[unit]|)

(register-predicate! unit? 'unit)

(define (alist->unit alist)
  (cons %unit-tag alist))

(define (unit->alist unit)
  (guarantee unit? unit 'unit->alist)
  (cdr unit))

(define (unit=? u1 u2)
  (guarantee unit? u1 'unit=?)
  (guarantee unit? u2 'unit=?)
  (equal? u1 u2))

;;; This printing support is magic, just assume it works.
((access register-predicate! system-global-environment)
 unit? 'unit)
(define-print-method unit?
  (lambda (unit port)
    (write (cons 'unit (alist->plist (unit->alist unit)))
           port)))

(define unit:none
  (alist->unit '()))

(define (unitless? unit)
  (guarantee unit? unit 'unitless?)
  (null? (cdr unit)))

(define (unit . plist)
  (guarantee plist? plist 'unit)
  (let ((alist
         (sort (plist->alist plist)
               (lambda (p1 p2)
                 (symbol<? (car p1) (car p2))))))
    (if (sorted-alist-repeated-key? alist)
        (error "Base unit repeated" plist))
    (for-each (lambda (p)
                (guarantee exact-rational? (cdr p)))
              alist)
    (alist->unit alist)))

(define (sorted-alist-repeated-key? alist)
  (and (pair? alist)
       (pair? (cdr alist))
       (or (eq? (caar alist) (caadr alist))
           (sorted-alist-repeated-key? (cdr alist)))))

(define unit-layer
  (make-annotation-layer 'unit
    (lambda (get-name has-value? get-value)

      (define (get-default-value)
        unit:none)

      ;; coderef: get-procedure
      (define (get-procedure name arity)
        (if (operator? name)
            (let ((procedure (unit-procedure name)))
              (case name
                ((expt)
                 (lambda (base-value base power)
                   (declare (ignore base-value))
                   (procedure (get-value base)
                              (base-layer-value power))))
                (else
                 (lambda (base-value . args)
                   (declare (ignore base-value))
                   (apply procedure (map get-value args))))))
            #f))

      (define (summarize-self)
        (list (get-name)))

      (bundle layer?
              get-name has-value? get-value
              get-default-value get-procedure
              summarize-self))))

(define (unit-procedure operator)
  (case operator
    ((*) unit:*)
    ((/) unit:/)
    ((remainder) unit:remainder)
    ((expt) unit:expt)
    ((invert) unit:invert)
    ((square) unit:square)
    ((sqrt) unit:sqrt)
    ((atan) unit:atan)
    ((abs ceiling floor negate round truncate)
     unit:simple-unary-operation)
    ((+ - max min)
     unit:simple-binary-operation)
    ((acos asin cos exp log sin tan)
     unit:unitless-operation)
    ((angle imag-part magnitude make-polar make-rectangular
            real-part)
     ;; first approximation:
     unit:unitless-operation)
    (else
     (if (eq? 'boolean (operator-codomain operator))
         (if (n:= 1 (operator-arity operator))
             unit:unary-comparison
             unit:binary-comparison)
         unit:unitless-operation))))


(define (unit-arithmetic)
  (make-arithmetic 'unit unit? '()
    (lambda (name)
      (if (eq? name 'multiplicative-identity)
          unit:none
          (default-object)))
    (lambda (operator)
      (case operator
        ((expt)                         ;GJS:21Apr2021
         (make-operation operator
                         (match-args unit? number?)
                         unit:expt))
        (else
         (simple-operation operator
                           unit?
                           (unit-procedure operator)))))))

(define (unit:* u1 u2)
  (alist->unit
   (let loop ((u1 (unit->alist u1)) (u2 (unit->alist u2)))
     (if (and (pair? u1) (pair? u2))
         (let ((factor1 (car u1)) (factor2 (car u2)))
           (if (eq? (car factor1) (car factor2))
               (let ((n (n:+ (cdr factor1) (cdr factor2))))
                 (if (n:= 0 n)
                     (loop (cdr u1) (cdr u2))
                     (cons (cons (car factor1) n)
                           (loop (cdr u1) (cdr u2)))))
               (if (symbol<? (car factor1) (car factor2))
                   (cons factor1 (loop (cdr u1) u2))
                   (cons factor2 (loop u1 (cdr u2))))))
         (if (pair? u1) u1 u2)))))

(define (unit:invert u)
  (alist->unit
   (map (lambda (f)
          (cons (car f) (n:- (cdr f))))
        (unit->alist u))))

(define (unit:expt u1 r)
  (guarantee exact-rational? r 'unit:expt)
  (if (n:= r 0)
      (unit)
      (alist->unit
       (map (lambda (f)
              (cons (car f) (n:* r (cdr f))))
            (unit->alist u1)))))

(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))

(define (unit:remainder u1 u2)
  u1)

(define (unit:square u)
  (unit:expt u 2))

(define (unit:sqrt u)
  (unit:expt u 1/2))

(define (unit:atan u)
  unit:none)

(define (unit:unary-comparison u)
  unit:none)

(define (unit:binary-comparison u1 u2)
  (if (not (unit=? u1 u2))
      (error "incompatible units:" u1 u2))
  unit:none)

(define (unit:simple-unary-operation u)
  u)

(define (unit:simple-binary-operation u1 u2)
  (if (not (unit=? u1 u2))
      (error "incompatible units:" u1 u2))
  u1)

(define (unit:unitless-operation . us)
  (if (not (every unitless? us))
      (error "Units not allowed in this operation:" us))
  unit:none)

#|
(define test-arith
  (extend-arithmetic layered-extender
                     numeric-arithmetic))

(install-arithmetic! test-arith)

(pp (layered-datum 3 unit-layer unit:none))
#[layered-datum 3]
(base 3)
(unit ())

(pp (layered-datum 3 unit-layer (unit 'meter 4)))
#[layered-datum 3]
(base 3)
(unit ((meter 4)))

(define mass
  (layered-datum 101
                 unit-layer (unit 'kilogram 1)))

(define velocity
  (layered-datum 37
                 unit-layer (unit 'meter 1 'second -1)))

(define acceleration
  (layered-datum 41
                 unit-layer (unit 'meter 1 'second -2)))

(define force
  (* mass acceleration))

(pp force)
#[layered-datum 4141]
(base 4141)
(unit ((kilogram 1) (meter 1) (second -2)))

;; coderef: c-with-units
(define c
  (layered-datum 299792458
                 unit-layer (unit 'meter 1 'second -1)))

(pp c)
#[layered-datum 299792458]
(base 299792458)
(unit ((meter 1) (second -1)))

(define e (* mass (expt c 2)))

(pp e)
#[layered-datum 9077427305241858164]
(base 9077427305241858164)
(unit ((kilogram 1) (meter 2) (second -2)))

(define unitless-2
  (layered-datum 2 unit-layer (unit)))

(define e (* mass (expt c unitless-2)))

(pp e)
#[layered-datum 9077427305241858164]
(base 9077427305241858164)
(unit ((kilogram 1) (meter 2) (second -2)))
|#
