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

(define (unify-test p1 p2 expected)
  (let ((dict (unify p1 p2)))
    (if expected
	;; should succeed
	(if (and dict
                 (match:equivalent-patterns? p1 p2 dict))
            (if (equal? (expression-value p1 expected)
                        (expression-value p2 expected))
                #t
                'maybe)
            (warn "failure:" p1 p2))
	;; should fail
        (if (not dict)
            #t
            (warn "failure:" p1 p2)))))

;;; A substitution algorithm with back-substitution.
(define (expression-value expression dict)
  (let loop ((expression expression))
    (cond ((match:var? expression)
           (let ((binding
                  (assq (match:var-name expression) dict)))
             (if binding
                 (loop (cadr binding))
                 expression)))
          ((pair? expression)
           (cons (loop (car expression))
                 (loop (cdr expression))))
          (else expression))))

;;; A bunch of tests taken from Oleg@pobox.com, whoever that is.

(unify-test '(M abc) '(M abc) '())
(unify-test '(M abc) '(M abcd) #f)
(unify-test '(M abc) '(M (? x) (? y)) #f)
(unify-test '(M abc) '(M (? x)) '((x abc)))
(unify-test '(M (? x) a (? y)) '(M a (? z) b) '((y b) (z a) (x a)))
(unify-test '(M (? a) (? b)) '(M (? b) (? a)) '((a (? b))))
(unify-test '(M (? a) abc) '(M (? b) (? a)) '((b abc) (a (? b))))
(unify-test '(? b) '(f (? a)) '((b (f (? a)))))

(unify-test '(M (f (? a))) '(M (? a)) #f)
(unify-test '(M (f (? a))) '(M (? b)) '((b (f (? a)))))
(unify-test '(M (f (? a)) (? a)) '(M (? b) (? c)) '((a (? c)) (b (f (? a)))))
(unify-test '(M (f (? a)) (? a)) '(M (? b) (f (? b))) #f)
(unify-test '(M (f (? a)) (? a)) '(M (f (? b)) (? b)) '((a  (? b))))


; A few tests from Dan Friedman's "Poor-man's Logic system tutorial"

(unify-test '((? x) (? x)) '(3 4) #f)
(unify-test '((? x) 4) '(3 (? x)) #f)
(unify-test '((? x) (? y)) '(3 4) '((y 4) (x 3)))
(unify-test '((? x) 4) '(3 (? y)) '((y 4) (x 3)))
(unify-test '((? x) 4) '((? y) (? y)) '((y 4) (x (? y))))
(unify-test '((? x) 4 3) '((? y) (? y) (? x)) #f)
(unify-test '((? x) 4 3 (? w)) '(3 (? y) (? x) (? z)) '((w (? z)) (y 4) (x 3)))
(unify-test '(p (? x) (? x)) '(p (? y) (f (? y))) #f)

;;; The next two illustrate that users of the dictionary must
;;; substitute all the way through.

(unify-test '(p (f a) (g (? x))) '(p (? x) (? y)) '((y (g (? x))) (x (f a))))
(unify-test '(p (g (? x)) (f a)) '(p (? y) (? x)) '((x (f a)) (y (g (? x)))))



; Tests for a bad unification from Franz Baader, Wayne Snyder,
; "Unification theory" (1999).  The naive algorithm exponentially
; explodes on these:

(unify-test
 '(h (? x1) (f (? y0) (? y0)) (? y1))
 '(h (f (? x0) (? x0)) (? y1) (? x1))
 '((x0 (? y0)) (y1 (f (? y0) (? y0))) (x1 (f (? y0) (? y0)))))

(unify-test
 '(h (? x1) (? x2) (f (? y0) (? y0)) (f (? y1) (? y1)) (? y2))
 '(h (f (? x0) (? x0))(f (? x1) (? x1)) (? y1) (? y2)  (? x2))
 '((y0 (? x0))
   (y2 (f (f (? x0) (? x0)) (f (? x0) (? x0))))
   (y1 (f (? x0) (? x0)))
   (x2 (f (f (? x0) (? x0)) (f (? x0) (? x0))))
   (x1 (f (? x0) (? x0)))))

(let ((dict
       (unify
        '(h (? x1) (f (? y0) (? y0)) (? y1))
        '(h (f (? x0) (? x0)) (? y1) (? x1)))))
  ;; (pp dict)
  ;; ((x0 (? y0)) (y1 (f (? y0) (? y0))) (x1 (f (? x0) (? x0))))
  #t)

(let ((dict
       (unify
        '(h (? x1) (? x2) (f (? y0) (? y0)) (f (? y1) (? y1)) (? y2))
        '(h (f (? x0) (? x0))(f (? x1) (? x1)) (? y1) (? y2)  (? x2)))))
  ;; (pp dict)
  ;; ((y0 (? x0))
  ;;  (y2 (f (f (? x0) (? x0)) (f (? x0) (? x0))))
  ;;  (y1 (f (? x0) (? x0)))
  ;;  (x2 (f (f (? x0) (? x0)) (f (? x0) (? x0))))
  ;;  (x1 (f (? x0) (? x0))))
  #t)

(define expr1
  '(h (? x1)
      (? x2)
      (? x3)
      (? x4)
      (f (? y0) (? y0))
      (f (? y1) (? y1))
      (f (? y2) (? y2))
      (f (? y3) (? y3))
      (? y4)))

(define expr2
  '(h (f (? x0) (? x0))
      (f (? x1) (? x1))
      (f (? x2) (? x2))
      (f (? x3) (? x3))
      (? y1)
      (? y2)
      (? y3)
      (? y4)
      (? x4)))

(define substitution (unify expr1 expr2))

;; (pp substitution)
;;; ((y0 (? x0))
;;;  (y4 (f (? y3) (? y3)))
;;;  (y3 (f (? y2) (? y2)))
;;;  (y2 (f (? y1) (? y1)))
;;;  (y1 (f (? y0) (? y0)))
;;;  (x4 (f (? x3) (? x3)))
;;;  (x3 (f (? x2) (? x2)))
;;;  (x2 (f (? x1) (? x1)))
;;;  (x1 (f (? x0) (? x0))))

(if (and substitution
         (match:equivalent-patterns? expr1 expr2 substitution))
    #t
    (warn "failure:" expr1 expr2))
;Value: #t

;;; So it is a correct unifier.
