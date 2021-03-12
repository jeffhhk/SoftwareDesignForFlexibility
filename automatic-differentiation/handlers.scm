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

;;;            Calculus of Infinitesimals

;;; The finite-part is all terms except for terms containing the
;;; maximal factor in a term of highest order, and
;;; infinitesimal-part is the remaining terms, all of which
;;; contain that factor.  So:

;;;                           f
;;;    x + dx + dy + dx*dy |----> f(x+dx) + Df(x+dx)*(dy+dx*dy)

;;; Alternatively, we might have computed the following, but we
;;; think that the ultimate values of derivatives don't care,
;;; because mixed partials of R^2 --> R commute.

;;;                           f
;;;    x + dx + dy + dx*dy |----> f(x+dy) + Df(x+dy)*(dx+dx*dy)

(define (finite-part x #!optional factor)
  (if (differential? x)
      (let ((factor (default-maximal-factor x factor)))
        (make-differential
         (remove (lambda (term)
                   (memv factor (diff-factors term)))
                 (diff-terms x))))
      x))

(define (infinitesimal-part x #!optional factor)
  (if (differential? x)
      (let ((factor (default-maximal-factor x factor)))
        (make-differential
         (filter (lambda (term)
                   (memv factor (diff-factors term)))
                 (diff-terms x))))
      0))

(define (default-maximal-factor x factor)
  (if (default-object? factor)
      (maximal-factor x)
      (begin
        (assert (diff-factor? factor))
        factor)))

(define (maximal-factor . args)
  (let ((candidates
         (filter-map (lambda (arg)
                       (let ((terms (diff-terms arg)))
                         (and (pair? terms)
                              (let ((factors
                                     (diff-factors (car terms))))
                                (and (pair? factors)
                                     (car factors))))))
                     args)))
    (if (null? candidates)
        (error "No differentials in args:" args))
    (choose-maximal-factor candidates)))

(define (choose-maximal-factor factors)
  (let loop ((factors (cdr factors)) (maximal (car factors)))
    (if (pair? factors)
        (loop (cdr factors)
              (if (diff-factor>? (car factors) maximal)
                  (car factors)
                  maximal))
        maximal)))

;;; To turn a unary function into one that operates on
;;; differentials we must supply the derivative.  This is the
;;; essential chain rule.

(define (diff:unary-proc f df)
  (define (uop x)
      (let ((xf (finite-part x))
            (dx (infinitesimal-part x)))
        (d:+ (f xf) (d:* (df xf) dx))))
  uop)

;;; To turn a binary function into one that operates on
;;; differentials we must supply the partial derivatives with
;;; respect to each argument.

(define (diff:binary-proc-wrong f d0f d1f)
  (define (bop x y)
    (let ((dx (infinitesimal-part x))
          (dy (infinitesimal-part y))
          (xf (finite-part x))
          (yf (finite-part y)))
      (d:+ (f xf yf)
           (d:+ (d:* dx (d0f xf yf))
                (d:* (d1f xf yf) dy)))))
  bop)

;;; For multivariate functions we must choose the finite-part and
;;; the infinitesimal-part of each input to be consistent with
;;; respect to the factor, we do this as follows:

(define (diff:binary-proc f d0f d1f)
  (define (bop x y)
    (let ((factor (maximal-factor x y)))
      (let ((dx (infinitesimal-part x factor))
            (dy (infinitesimal-part y factor))
            (xf (finite-part x factor))
            (yf (finite-part y factor)))
        (d:+ (f xf yf)
             (d:+ (d:* dx (d0f xf yf))
                  (d:* (d1f xf yf) dy))))))
  bop)

;;; Here are the handlers

(define diff:+
  (diff:binary-proc +
                    (lambda (x y) 1)
                    (lambda (x y) 1)))

(define diff:-
  (diff:binary-proc -
                    (lambda (x y) 1)
                    (lambda (x y) -1)))

(define diff:*
  (diff:binary-proc *
                    (lambda (x y) y)
                    (lambda (x y) x)))

(define diff:/
  (diff:binary-proc /
                    (lambda (x y)
                      (/ 1 y))
                    (lambda (x y)
                      (* -1 (/ x (square y))))))

(define diff:expt
  (diff:binary-proc expt
                    (lambda (x y)
                      (* y (expt x (- y 1))))
                    (lambda (x y)
                      (if (and (number? x) (zero? x))
                          (if (number? y)
                              (if (positive? y)
                                  0
                                  (error "Derivative undefined: EXPT"
                                         x y))
                              0)        ;assuming not y<0
                          (* (log x) (expt x y))))))

(define diff:sqrt
  (diff:unary-proc sqrt
                   (lambda (x) (/ 1 (* 2 (sqrt x))))))

(define diff:square
  (diff:unary-proc square
                   (lambda (x) (* 2 x))))

(define diff:exp
  (diff:unary-proc exp exp))

(define diff:log
  (diff:unary-proc log (lambda (x) (/ 1 x))))

(define diff:sin
  (diff:unary-proc sin cos))

(define diff:cos
  (diff:unary-proc cos (lambda (x) (* -1 (sin x)))))

(define diff:asin
  (diff:unary-proc asin
                   (lambda (x)
                     (/ 1 (sqrt (- 1 (square x)))))))

(define diff:acos
  (diff:unary-proc acos
                   (lambda (x)
                     (/ -1 (sqrt (- 1 (square x)))))))

(define diff:atan1
  (diff:unary-proc atan
                   (lambda (x)
                     (/ 1 (+ 1 (square x))))))

(define diff:atan2
  (diff:binary-proc atan
                    (lambda (y x)
                      (/ x (+ (square x) (square y))))
                    (lambda (y x)
                      (/ (* -1 y)
                         (+ (square x) (square y))))))


;;; This is redundantly defined in several places.

;; coderef: sqrt-assignment
(assign-handler!  sqrt     diff:sqrt    differential?)
(assign-handler!  square   diff:square  differential?)
(assign-handler!  exp      diff:exp     differential?)
(assign-handler!  log      diff:log     differential?)
(assign-handler!  sin      diff:sin     differential?)
(assign-handler!  cos      diff:cos     differential?)
(assign-handler!  asin     diff:asin    differential?)
(assign-handler!  acos     diff:acos    differential?)
(assign-handler!  atan     diff:atan1   differential?)

;; coderef: +-assignment-1
(assign-handler!  +        diff:+       differential? any-object?)
;; coderef: +-assignment-2
(assign-handler!  +        diff:+       any-object? differential?)
(assign-handler!  -        diff:-       differential? any-object?)
(assign-handler!  -        diff:-       any-object? differential?)
(assign-handler!  *        diff:*       differential? any-object?)
(assign-handler!  *        diff:*       any-object? differential?)
(assign-handler!  /        diff:/       differential? any-object?)
(assign-handler!  /        diff:/       any-object? differential?)
(assign-handler!  expt     diff:expt    differential? any-object?)
(assign-handler!  expt     diff:expt    any-object? differential?)
