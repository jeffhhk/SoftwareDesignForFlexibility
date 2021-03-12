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

;;; The idea is that we compute derivatives by passing special
;;; "differential objects" [x,dx] through functions.  A first
;;; approximation to the idea is as follows:

;;;               f
;;;      [x,dx] |---> [f(x), Df(x)*dx]

;;; Note that the derivative of f at the point x, DF(x), is the
;;; coefficient of dx in the result.  If we then pass this result
;;; through another function, we obtain the chain-rule answer we
;;; would hope for.

;;;                         g
;;;      [f(x), Df(x)*dx] |---> [g(f(x)), DG(f(x))*DF(x)*dx]

;;; Thus, we can find the derivative of a composition by this
;;; process.  We need only define how each of the primitives act
;;; on these "differentials" and then we can use ordinary Scheme
;;; compositions of these to do the job.  See the procedure
;;; diff:derivative near the bottom to understand how derivatives
;;; are computed given this differential algebra.  This idea was
;;; "discovered" by Dan Zuras and Gerald Jay Sussman in 1992.  DZ
;;; and GJS made the first version of this code during an all
;;; nighter in 1992.

;;; To expand this idea to work for multiple derivatives of
;;; functions of several variables we define an algebra in
;;; "infinitesimal space".  The objects are multivariate power
;;; series in which no incremental has exponent greater than 1.
;;; This was worked out in detail by Hal Abelson around 1994, and
;;; painfully redone in 1997 by Sussman with the help of Hardy
;;; Mayer and Jack Wisdom.

;;; A rare and surprising bug was discovered by Alexey Radul in
;;; 2011.  This was fixed by remapping the infinitesimals for
;;; derivatives of functions that returned functions.  This was
;;; done kludgerously, but it works.

(declare (usual-integrations))

(define differential-tag 'differential)

(define (make-differential terms) ;coerces to real, if possible.
  (let ((terms
         (filter
          (lambda (term)
            (let ((coeff (diff-coefficient term)))
              (not (and (number? coeff) (= coeff 0)))))
          terms)))
    (cond ((null? terms) 0)
          ((and (null? (cdr terms))
                (null? (diff-factors (car terms))))
           (diff-coefficient (car terms)))
          ((every diff-term? terms)
           (cons differential-tag terms))
          (else (error "Bad terms")))))

(define (differential? x)
  (and (pair? x) (eq? (car x) differential-tag)))

(define (diff-terms h)
  (if (differential? h)
      (cdr h)
      (list (make-diff-term h '()))))


;;; A diff-term has a coefficient and diff-factors
;;; The diff-factors (dx1 dx2 ...) are sorted in a diff-term.
;;; The terms are built with the high-order terms in front.

(define diff-term-tag 'diff-term)

(define (make-diff-term coefficient factors)
  (list diff-term-tag coefficient factors))

(define (diff-term? x)
  (and (pair? x) (eq? (car x) diff-term-tag)))

(define (diff-coefficient x)
  (if (not (diff-term? x)) (error "diff-coefficient" x))
  (cadr x))

(define (diff-factors x)
  (if (not (diff-term? x)) (error "diff-factors" x))
  (caddr x))

;;; Differential term lists represent a kind of power series, so
;;; they can be added and multiplied.  It is important to note
;;; that when terms are multiplied, no contribution is made if
;;; the terms being multiplied have a factor in common.  Thus
;;; dx^2 = zero.

(define (+diff-termlists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else
         (let ((t1 (car l1)) (t2 (car l2)))
           (cond ((equal? (diff-factors t1) (diff-factors t2))
                  (let ((newcoeff (+ (diff-coefficient t1)
                                     (diff-coefficient t2))))
                    (if (and (number? newcoeff) (= newcoeff 0))
                        (+diff-termlists (cdr l1) (cdr l2))
                        (cons
                         (make-diff-term newcoeff
                                         (diff-factors t1))
                         (+diff-termlists (cdr l1) (cdr l2))))))
                 ((diff-term>? t1 t2)
                  (cons t1 (+diff-termlists (cdr l1) l2)))
                 (else
                  (cons t2 (+diff-termlists l1 (cdr l2)))))))))

(define (d:+ x y)
  (make-differential
   (+diff-termlists (diff-terms x)
                    (diff-terms y))))

(define (*diff-termlists l1 l2)
  (reduce (lambda (x y)
            (+diff-termlists y x))
          '()
          (map (lambda (t1)
                 (append-map (lambda (t2)
                               (*diff-terms t1 t2))
                             l2))
               l1)))

(define (d:* x y)
  (make-differential
   (*diff-termlists (diff-terms x)
                    (diff-terms y))))

(define (*diff-terms x y)
  (if (not (and (diff-term? x) (diff-term? y)))
      (error "*diff-terms" x y))
  (let ((fx (diff-factors x)) (fy (diff-factors y)))
    (if (null? (ordered-intersect diff-factor>? fx fy))
        (list (make-diff-term
               (* (diff-coefficient x) (diff-coefficient y))
               (ordered-union diff-factor>? fx fy)))
        '())))

;;; Want high-order stuff first.
(define (diff-term>? x y)
  (let ((fx (diff-factors x)) (fy (diff-factors y)))
    (let ((lx (length fx)) (ly (length fy)))
      (cond ((> lx ly) #t)
            ((= lx ly) (diff-factor>? (car fx) (car fy)))
            (else #f)))))

;;; This will be a unique \delta{x}
(define (make-new-dx)
  (generate-uninterned-symbol "delta-"))

(define (diff-factor? object)
  (uninterned-symbol? object))

(define (diff-factor>? x y)
  (symbol>? y x))

(define (make-infinitesimal dx)
  (make-differential (list (make-diff-term 1 (list dx)))))

;;; Simple derivative of unary function
(define (derivative f)
  (define (the-derivative x)
    (let* ((dx (make-new-dx))
           (value (f (d:+ x (make-infinitesimal dx)))))
      (extract-dx-part value dx)))
  the-derivative)

;;; Partial derivative of n-ary function
(define ((partial i) f)
  (define (the-derivative . args)
    (if (not (< i (length args)))
        (error "Not enough arguments for PARTIAL" i f args))
    (let* ((dx (make-new-dx))
           (value
            (apply f
                   (map (lambda (arg j)
                          (if (= i j)
                              (d:+ arg (make-infinitesimal dx))
                              arg))
                        args
                        (iota (length args))))))
      (extract-dx-part value dx)))
  the-derivative)

;;; General derivative of an n-ary function
(define (general-derivative g)
  (define ((the-derivative . args) . increments)
    (let ((n (length args)))
      (assert (= n (length increments)))
      ;;(assert (= n (arity g)))
      (if (= n 1)
          (* ((derivative g) (car args))
             (car increments))
          (reduce (lambda (x y) (+ y x))
                  0
                  (map (lambda (i inc)
                         (* (apply ((partial i) g) args)
                            inc))
                       (iota n)
                       increments)))))
  the-derivative)

;;; This is almost the right thing, but it has a subtle bug
;;; discovered by Radul and Perlmutter.  The very complex
;;; patch, which requires renaming the infinitesimals, is in
;;; radul-extractor.scm.
#|

(define (default-extractor value dx)
  0)

(define extract-dx-part
  (simple-generic-procedure 'extract-dx-part 2
                            default-extractor))

(define (extract-partial-from value dx)
  (let ((dx-diff-terms
         (append-map
          (lambda (term)
            (let ((factors (diff-factors term)))
              (if (memv dx factors)
                  (list (make-diff-term
                         (diff-coefficient term)
                         (delv dx factors)))
                  '())))
          (diff-terms value))))
    (cond ((null? dx-diff-terms) 0)
          ((and (null? (cdr dx-diff-terms))
                (null? (diff-factors (car dx-diff-terms))))
           (diff-coefficient (car dx-diff-terms)))
          (else
           (make-differential dx-diff-terms)))))

(define-generic-procedure-handler extract-dx-part
  (match-args differential? any-object?)
  extract-partial-from)

(define (defer-extraction value dx)
  (lambda args
    (extract-dx-part (apply value args) dx)))

(define-generic-procedure-handler extract-dx-part
  (match-args function? any-object?)
  defer-extraction)
|#
