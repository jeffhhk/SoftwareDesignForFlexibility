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

;;; Combining provenance and units

(repl)

;eval>
(set! Is (with-units Is '((ampere 1))))
;=> ok

;;; coulomb = ampere*second
;eval>
(set! :q 
      (with-units :q '((ampere 1) (second 1))))
;=> ok

;;; joule = kilogram*meter^2*second^-2
;eval>
(set! :k
      (with-units :k 
        '((kilogram 1) (meter 2) (second -2) (kelvin -1))))
;=> ok

;eval>
(set! T (with-units T '((kelvin 1))))
;=> OK

;;; volt = kilogram^2*meter^2*second^-3*ampere^-1

;eval>
(signed (with-units 0.6 
          '((kilogram 1) (meter 2) (second -3) (ampere -1))) 
        "GJS-V")
;provenance;=> ("GJS-V")
;units;=> ((ampere -1) (kilogram 1) (meter 2) (second -3))
;base;=> .6

(define (Id V)
        (* Is (- (exp (/ (* :q V) (* :k T))) 1)))
;=> (id defined)

;eval> 
(Id (signed (with-units 0.6 
              '((kilogram 1) (meter 2) (second -3) (ampere -1))) 
            "GJS-V"))
;provenance;=> ("NIST:CODATA-2006" "GJS-V" "GJS-T" "IRC")
;units;=> ((ampere 1))
;base;=> 1.2010041136964896e-3

;eval>
(define (Id V)
        (signed (* Is (- (exp (/ (* :q V) (* :k T))) 1))
                "Searle&Gray"))
;=> (id defined)

(Id (signed (with-units 0.6 
              '((kilogram 1) (meter 2) (second -3) (ampere -1))) 
            "GJS-V"))
;provenance;=> ("Searle&Gray" "NIST:CODATA-2006"
;               "GJS-V" "GJS-T" "IRC")
;units;=> ((ampere 1))
;base;=> 1.2010041136964896e-3

;eval>
(set! Id (signed Id  "GJS-code"))
;=> (id assigned)

(Id (signed (with-units 0.6 
              '((kilogram 1) (meter 2) (second -3) (ampere -1))) 
            "GJS-V"))
;provenance;=> ("IRC" "GJS-T" "NIST:CODATA-2006" 
;               "Searle&Gray" "GJS-code" "GJS-V")
;units;=> ((ampere 1))
;base;=> 1.2010041136964896e-3

#|
;;; But we still have problems because we cannot pass in a
;;; bare number, hoping that it is not interpreted as
;;; unitless.  Bare values should engage only the base layer
;;; of any procedure!

;eval> (Id 0.6)
;Assertion failed: (every (lambda ... ...) (if ... args ...)) 
;  "not all args are unitless" exp

;;; It works to have only bare arguments
;eval> 
(* 0.6 4)
;base;=> 2.4

;;; But if any argument has units the bare argument is seen
;;; as unitless.  Of course, this policy is what is wanted
;;; in many cases.  Not clear what to do!

;eval>
(* 0.6 (signed (with-units 4 '((kilogram 1))) "GJS"))
;provenance;=> ("GJS")
;base;=> 2.4
;units;=> ((kilogram 1))

;;; We need a way to distinguish bare values from unitless
;;; values.  We should not always promote a bare value to
;;; unitlesss.

;;; Perhaps the policy of calling the layered-procedure
;;; layer component if any arg has that layer is wrong.
;;; Perhaps only if all args have that layer?  But then what
;;; about (expt x n)?  Certainly if x has units, and n is
;;; bare we want the units layer of expt, but perhaps this
;;; is a special case?  But if we choose that option (all
;;; args) then we have to explicitly make constants like the
;;; 1 in Id unitless.  UGH, a dilemma.
|#