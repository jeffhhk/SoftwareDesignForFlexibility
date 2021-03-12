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

;;;; Arithmetic operators

(define (operator? object)
  (assq object %arithmetic-operator-alist))

(define (operator-arity operator)
  (length (operator-domains operator)))

(define (operator-domains operator)
  (cadr (%operator-entry operator)))

(define (operator-codomain operator)
  (caddr (%operator-entry operator)))


#|
(define (operator-installable? operator)
  (not (assq operator internal-operators)))
|#

;;; GJS: simplifies explanation in text: no need to prevent
;;; negate and invert from being installed.

(environment-define system-global-environment
                    'negate 
                    (environment-lookup system-global-environment
                                        '-))

(environment-define system-global-environment
                    'invert
                    (environment-lookup system-global-environment
                                        '/))

(define (operator-installable? operator)
  #t)


(define (operator->procedure-name operator)
  (let ((p (assq operator internal-operators)))
    (if p
        (cdr p)
        operator)))

(define internal-operators
  '((negate . -)
    (invert . /)))

(define (constant-names)
  '(additive-identity multiplicative-identity))

(define (operator-names)
  (map car %arithmetic-operator-alist))

(define (%operator-entry operator)
  (or (assq operator %arithmetic-operator-alist)
      (error "Unknown operator:" operator)))

(define (operator-signature operator domain)
  (let ((mapper
         (lambda (indicator)
           (case indicator
             ((domain) domain)
             ((boolean) boolean?)
             ((number) number?)
             (else (error "Unknown domain:" indicator))))))
    (values (map mapper (operator-domains operator))
            (mapper (operator-codomain operator)))))

(define %arithmetic-operator-alist
  '((* (domain domain) domain)
    (+ (domain domain) domain)
    (- (domain domain) domain)
    (/ (domain domain) domain)
    (< (domain domain) boolean)
    (<= (domain domain) boolean)
    (= (domain domain) boolean)
    (> (domain domain) boolean)
    (>= (domain domain) boolean)
    (abs (domain) domain)
    (acos (domain) domain)
    (angle (domain) domain)
    (asin (domain) domain)
    (atan (domain) domain)
    (ceiling (domain) domain)
    (cos (domain) domain)
    (exp (domain) domain)
    (expt (domain number) domain)
    (floor (domain) domain)
    (imag-part (domain) domain)
    (invert (domain) domain)
    (log (domain) domain)
    (magnitude (domain) domain)
    (make-polar (domain domain) domain)
    (make-rectangular (domain domain) domain)
    (max (domain domain) domain)
    (min (domain domain) domain)
    (negate (domain) domain)
    (negative? (domain) boolean)
    (positive? (domain) boolean)
    (real-part (domain) domain)
    (remainder (domain domain) domain)
    (round (domain) domain)
    (sin (domain) domain)
    (square (domain) domain)
    (sqrt (domain) domain)
    (tan (domain) domain)
    (truncate (domain) domain)
    (zero? (domain) boolean)))