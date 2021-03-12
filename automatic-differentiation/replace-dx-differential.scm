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

;;; object is a differential object...

(define (replace-dx-differential new-dx old-dx object)
  (make-differential
   (combine-like-terms
    (sort (append-map
           (lambda (term)
             (let ((factors
                    (sort (substitute new-dx old-dx
                                      (diff-factors term))
                          diff-factor>?))
                   (c (diff-coefficient term)))
               (if (or (and (number? c) (= c 0))
                       (duplicate-factors? (diff-factors term)))
                   '()
                   (list (make-diff-term c factors)))))
           (diff-terms object))
          diff-term>?))))

(define (duplicate-factors? lst)
  (and (not (null? lst))
       (let lp ((lst lst))
         (and (not (null? (cdr lst)))
              (or (eqv? (car lst) (cadr lst))
                  (lp (cdr lst)))))))

(define (combine-like-terms terms)
  (cond ((null? terms) '())
        ((null? (cdr terms)) terms)
        (else
         (let ((current (car terms)) (next (cadr terms)))
           (cond ((equal? (diff-factors current)
                          (diff-factors next))
                  (let ((newcoeff
                         (+ (diff-coefficient current)
                            (diff-coefficient next))))
                    (if (and (number? newcoeff) (= newcoeff 0))
                        (combine-like-terms (cddr terms))
                        (combine-like-terms
                         (cons (make-diff-term newcoeff
                                               (diff-factors current))
                               (cddr terms))))))
                 (else 
                  (cons current
                        (combine-like-terms (cdr terms)))))))))
               
;; coderef: replace-dx:differential
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? differential?)
  replace-dx-differential)
