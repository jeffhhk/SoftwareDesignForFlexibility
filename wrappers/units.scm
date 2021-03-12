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

;;; Unit conversions

(define (make-unit-conversion to from)
  (let ((to-op (make-apply-hook to #f))
        (from-op (make-apply-hook from #f)))
    (set-apply-hook-extra! to-op
                           (make-unit-conversion-record from-op))
    (set-apply-hook-extra! from-op
                           (make-unit-conversion-record to-op))
    to-op))

(define-record-type <unit-conversion-record>
    (make-unit-conversion-record inverse)
    unit-conversion-record?
  (inverse unit-conversion-record-inverse))

(define (unit-conversion? object)
  (and (apply-hook? object)
       (unit-conversion-record? (apply-hook-extra object))))
(register-predicate! unit-conversion? 'unit-conversion)

(define (unit:invert unit-conversion)
  (guarantee unit-conversion? unit-conversion)
  (unit-conversion-record-inverse (apply-hook-extra unit-conversion)))

(define (unit:* u1 u2)
  (make-unit-conversion (compose u2 u1)
                        (compose (unit:invert u1)
                                 (unit:invert u2))))

(define (unit:/ u1 u2)
  (unit:* u1 (unit:invert u2)))

(define (unit:expt conversion n)
  (let ((positive-case
         (lambda (conversion n)
           (let loop ((count n))
             (cond ((n:= count 1)
                    conversion)
                   ((even? count)
                    (let ((a (loop (n:/ count 2))))
                      (unit:* a a)))
                   (else
                    (unit:* conversion
                            (loop (n:- count 1)))))))))
    (cond ((n:= n 0)
           identity-unit-conversion)
          ((n:< n 0)
           (positive-case (unit:invert conversion)
                          (n:negate n)))
          (else
           (positive-case conversion n)))))

;;;; Unit-conversion registry

(define (register-unit-conversion input-unit output-unit unit-conversion)
  (hash-table-set! unit-conversion-table
                   (cons input-unit output-unit)
                   unit-conversion)
  (hash-table-set! unit-conversion-table
                   (cons output-unit input-unit)
                   (unit:invert unit-conversion)))

(define (register-expt-conversion input-type output-type exponent converter)
  (register-unit-conversion `(expt ,input-type ,exponent)
                            `(expt ,output-type ,exponent)
                            (unit:expt converter exponent)))

(define (make-converter input-unit output-unit)
  (or (and (equal? input-unit output-unit)
           identity-unit-conversion)
      (hash-table-ref/default unit-conversion-table
                              (cons input-unit output-unit)
                              #f)
      (error "Unable to find unit converter:" input-unit output-unit)))

(define unit-conversion-table
  (make-equal-hash-table))

;;; Creates a factory that produces a procedure specialized for given units.
(define (unit-specializer procedure implicit-output-unit
                          . implicit-input-units)

  (define (specializer specific-output-unit
                       . specific-input-units)
    (let ((output-converter
           (make-converter implicit-output-unit
                           specific-output-unit))
          (input-converters
           (map make-converter
                specific-input-units
                implicit-input-units)))

      (define (specialized-procedure . arguments)
        (output-converter
         (apply procedure
                (map (lambda (converter argument)
                       (converter argument))
                     input-converters
                     arguments))))

      specialized-procedure))

  specializer)

;;;; Unit converters

(define identity-unit-conversion
  (make-unit-conversion (lambda (x) x)
                        (lambda (x) x)))

(define inch-to-meter
  (let ((meters-per-inch .0254))
    (make-unit-conversion (lambda (inches)
                            (* inches meters-per-inch))
                          (lambda (meters)
                            (/ meters meters-per-inch)))))
(register-unit-conversion 'inch 'meter inch-to-meter)

(define fahrenheit-to-celsius
  (make-unit-conversion (lambda (f) (* 5/9 (- f 32)))
                        (lambda (c) (+ (* c 9/5) 32))))
(register-unit-conversion 'fahrenheit 'celsius fahrenheit-to-celsius)

(define celsius-to-kelvin
  (let ((zero-celsius 273.15))
    (make-unit-conversion (lambda (c) (+ c zero-celsius))
                          (lambda (k) (- k zero-celsius)))))
(register-unit-conversion 'celsius 'kelvin celsius-to-kelvin)

(define pound-to-newton
  (let ((pounds-per-newton 0.224808943))
    (make-unit-conversion (lambda (pounds)
                            (/ pounds pounds-per-newton))
                          (lambda (newtons)
                            (* newtons pounds-per-newton)))))
(register-unit-conversion 'pound 'newton pound-to-newton)

(register-expt-conversion 'inch 'meter 3 inch-to-meter)

;; coderef: fahrenheit-to-kelvin
(register-unit-conversion 'fahrenheit 'kelvin
                          (unit:* fahrenheit-to-celsius celsius-to-kelvin))

;; coderef: psi-to-nm2
(register-unit-conversion '(/ pound (expt inch 2))
                          '(/ newton (expt meter 2))
                          (unit:/ pound-to-newton
                                  (unit:expt inch-to-meter 2)))
