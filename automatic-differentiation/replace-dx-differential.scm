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

;;; The code defining replace-dx-differential on page 122 is not
;;; quite right.  The bug was reported by Sam Ritchie.  My bug is
;;; that I forgot to collect like terms.  Here is a patch.
;;; Unfortunately, combine-like-terms is a bit of code.  UGH.

(define (replace-dx-differential new-dx old-dx object)
  (make-differential
   (combine-like-terms
    (sort (map (lambda (term)
                 (make-diff-term
                  (diff-coefficient term)
                  (sort (substitute new-dx old-dx
                                    (diff-factors term))
                        diff-factor>?)))
               (diff-terms object))
          diff-term>?))))


;;; Combine-like-terms takes advantage of the fact that the
;;; termlist is constructed in sorted order, so like terms are
;;; adjacent.

;;; combine-like-terms duplicates some of the work done by
;;; +diff-termlists, which suggests that there is an opportunity
;;; for abstraction here.

;;; Also, make-differential redundantly removes zero terms, but
;;; does not remove terms with duplicate factors.  That is also
;;; done in *diff-termlists.  Perhaps make-differential should
;;; handle all these cases and then they can be removed from here
;;; and *diff-termlists.

(define (combine-like-terms termlist)
  (cond ((null? termlist) '())            ;empty
        ((or (duplicate-factors? (diff-factors (car termlist)))
             (and (number? (diff-coefficient (car termlist)))
                  (= 0 (diff-coefficient (car termlist)))))
         (combine-like-terms (cdr termlist)))
        ((null? (cdr termlist)) termlist) ; one-term
        ((equal? (diff-factors (car termlist))
                 (diff-factors (cadr termlist)))
         (let ((newcoeff
                (+ (diff-coefficient (car termlist))
                   (diff-coefficient (cadr termlist)))))
           (combine-like-terms
            (cons (make-diff-term newcoeff
                                  (diff-factors (car termlist)))
                  (cddr termlist)))))
        (else
         (cons (car termlist)
               (combine-like-terms (cdr termlist))))))

;;; Terms with duplicate factors must be eliminated because the
;;; square of the infinitesimal part of a dual number is zero.
;;; Since the factors are kept sorted this test is easy:

(define (duplicate-factors? factors)
  (and (not (null? factors))
       (let lp ((lst factors))
         (and (not (null? (cdr lst)))
              (or (eqv? (car lst) (cadr lst))
                  (lp (cdr lst)))))))

;; coderef: replace-dx:differential
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? differential?)
  replace-dx-differential)

