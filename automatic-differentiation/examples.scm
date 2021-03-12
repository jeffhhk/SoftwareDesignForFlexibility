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

;;; Shows signs of life!

((literal-function 'f) 'x)
'expect-value:
'(f x)

((derivative (literal-function 'f)) 'x)
'expect-value: '((derivative f) x)

((derivative (derivative (literal-function 'f))) 'x)
'expect-value: '((derivative (derivative f)) x)

((derivative (compose (literal-function 'f)
                      (literal-function 'g)))
 'x)
'expect-value: '(* ((derivative f) (g x)) ((derivative g) x))

(((partial 0) (literal-function 'f)) 'x 'y)
'expect-value: '(((partial 0) f) x y)

(((partial 1) (literal-function 'f)) 'x 'y)
'expect-value: '(((partial 1) f) x y)

((derivative
  (lambda (x)
    ((literal-function 'h)
     ((literal-function 'f) x)
     ((literal-function 'g) x))))
 'x)
'expect-value: '
(+ (* (((partial 1) h) (f x) (g x)) ((derivative g) x))
   (* (((partial 0) h) (f x) (g x)) ((derivative f) x)))

(define (foo x y)
  (square (+ (square x) y)))

(((partial 0) foo) 'x 'y)
'expect-value: '(* (* 2 (+ (square x) y)) (* 2 x))

(((partial 1) foo) 'x 'y)
'expect-value: '(* 2 (+ (square x) y))

(((partial 1) ((partial 0) foo)) 'x 'y)
'expect-value: '(* 2 (* 2 x))

((derivative
  (lambda (x) (* x x x)))
 'a)
'expect-value: '(+ (* (+ a a) a) (* a a)) ;=3a^2

(((derivative
   (lambda (x)
     (lambda (y z)
       (* x y z))))
  2)
 3
 4)
'expect-value: '12

(((derivative
   (lambda (x)
     (lambda (y z)
       ((literal-function 'f) x y z))))
  2)
 3
 4)
'expect-value: '
(((partial 0) f) 2 3 4)

((derivative
  (lambda (x)
    (((partial 1) (literal-function 'f))
     x 'v)))
 'u)
'expect-value: '(((partial 1) ((partial 0) f)) u v)

;;;            Here are some hard problems.

(((derivative
   (lambda (x)
     (derivative
      (lambda (y)
        ((literal-function 'f)
         x y)))))
  'u)
 'v)
'expect-value: '(((partial 1) ((partial 0) f)) u v)

;;; Eliminating complexities of literal functions.

;; coderef: deferred-extract
(((derivative
   (lambda (x)
     (derivative
      (lambda (y)
        (* x y)))))
  'u)
 'v)
'expect-value: '1

(define (foo u v) (+ (* u v v) u))
'expect-value: 'foo

(((derivative
   (lambda (x)
     (derivative (lambda (y) (foo x y)))))
  'u)
 'v)
'expect-value: '(+ v v)

(((derivative
   (lambda (x)
     (lambda (y) ((literal-function 'f) x y))))
  'u)
 'v)
'expect-value: '(((partial 0) f) u v)

(((lambda (x)
    (derivative
     (lambda (y) ((literal-function 'f) x y))))
  'u)
 'v)
'expect-value: '(((partial 1) f) u v)

;;; Alexey Radul problem!  With Scmutils version of extract.

(define (((p x) g) y)
  (g (+ x y)))

(define f-hat ((derivative p) 3))

(define (cube x) (* x x x))

((f-hat cube) 5)
'expect-value: '192

((f-hat (f-hat cube)) 5)
'expect-value: '66

((f-hat exp) 5)
'expect-value: '2980.9579870417283

((f-hat (f-hat exp)) 5)
'expect-value: '59874.14171519782

((f-hat cube) 'a)
'expect-value: '
(+ (* (+ (+ 3 a) (+ 3 a)) (+ 3 a))
   (* (+ 3 a) (+ 3 a)))
;;; = (+ 27 (* 3 (expt a 2)) (* 18 a)) as in Scmutils

((f-hat (f-hat cube)) 'a)
'expect-value: '
(+ (+ (* 2 (+ 3 (+ 3 a))) (+ (+ 3 (+ 3 a)) (+ 3 (+ 3 a))))
   (+ (+ 3 (+ 3 a)) (+ 3 (+ 3 a))))
;;; = (+ 36 (* 6 a)) as in Scmutils (with Scmutils extract)

((f-hat (literal-function 'g)) 5)
'expect-value: '((derivative g) 8)

((f-hat (f-hat (literal-function 'g))) 'a)
'expect-value: '((derivative (derivative g)) (+ 3 (+ 3 a)))

(define (legendre-transform-procedure F)
  (let ((w-of-v (derivative F)))
    (define (G w)
      (let ((z 0))
        (let ((M ((derivative w-of-v) z))
              (b (w-of-v z)))
          (let ((v (/ (- w b) M)))
            (- (* w v) (F v))))))
    G))

((legendre-transform-procedure
  (lambda (x)
    (* 'c (square x))))
 'y)
'expect-value: '
(- (* y (/ (- y 0) (* c 2)))
   (* c (square (/ (- y 0) (* c 2)))))
;;  = (/ (* 1/4 (expt y 2)) c)

((legendre-transform-procedure
  (lambda (x)
    (+ (* 'a x) (* 'b (square x)))))
 'y)
'expect-value: '
(- (* y (/ (- y a) (* b 2)))
   (+ (* a (/ (- y a) (* b 2)))
      (* b (square (/ (- y a) (* b 2))))))
;;  = (+ (/ (* 1/4 (expt a 2)) b)
;;       (/ (* -1/2 a y) b)
;;       (/ (* 1/4 (expt y 2)) b))

((let ((m 'm) (k 'k) (x 'x))
   (legendre-transform-procedure
    (lambda (v)
      (- (* 1/2 m (square v))
         (* 1/2 k (square x))))))
'p)
'expect-value: '
(- (* p (/ (- p 0) (* (* 1/2 m) 2)))
   (- (* (* 1/2 m)
         (square (/ (- p 0) (* (* 1/2 m) 2))))
      (* (* 1/2 k) (square x))))
;;   = (+ (* 1/2 k (expt x 2)) (/ (* 1/2 (expt p 2)) m))

((let ((m 'm) (k 'k) (x 'x))
   (legendre-transform-procedure
    (lambda (v)
      (- (* 1/2 m (square v))
         ((literal-function 'V) x)))))
'p)
'expect-value: '
(- (* p (/ (- p 0) (* (* 1/2 m) 2)))
   (- (* (* 1/2 m)
         (square (/ (- p 0)
                    (* (* 1/2 m) 2))))
      (v x)))
;;  = (+ (v x) (/ (* 1/2 (expt p 2)) m))

(((general-derivative square) 'x) 'dx)
'expect-value: '(* (* 2 x) dx)

(((general-derivative *) 'u 'v) 'du 'dv)
'expect-value: '(+ (* v du) (* u dv))

(((general-derivative
   (lambda (x y) (* (cos x) (exp y))))
  'u 'v)
 'du 'dv)
'expect-value: '
(+ (* (* (* -1 (sin u)) (exp v)) du)
   (* (* (cos u) (exp v)) dv))
;;   = (+ (* -1 du (sin u) (exp v))
;;        (* dv (exp v) (cos u)))

((((derivative
    (lambda (x)
      (derivative
       (lambda (y)
         (derivative
          (lambda (z)
            (+ (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect-value: '0

((((derivative
    (lambda (x)
      (derivative
       (lambda (y)
         (derivative
          (lambda (z)
            (* (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect-value: '(+ v v)

((((derivative
    (lambda (x)
      (derivative
       (lambda (y)
         (derivative
          (lambda (z)
            ((literal-function 'f) x y z)))))))
   'u)
  'v)
 'w)
'expect-value: '(((partial 2) ((partial 1) ((partial 0) f))) u v w)

((((derivative
    (lambda (x)
      (derivative
       (lambda (y)
         (derivative
          (lambda (z)
            ((literal-function 'f)
             (* x y) (* y z))))))))
   'u)
  'v)
 'w)
'expect-value: '
(+ (* (+ (+ (* (* (((partial 1) ((partial 1) ((partial 0) f)))
                   (* u v)
                   (* v w))
                  v)
               w)
            (((partial 1) ((partial 0) f))
             (* u v)
             (* v w)))
         (* (* (((partial 1) ((partial 0) ((partial 0) f)))
                (* u v)
                (* v w))
               v)
            u))
      v)
   (* (((partial 1) ((partial 0) f))
       (* u v)
       (* v w))
      v))

;;; Using elementary simplifier...
#|
(+ (* 2
      v
      (((* (partial 0) (partial 1)) f) (* u v) (* v w)))
   (* u
      (expt v 2)
      (((* (partial 1) (expt (partial 0) 2)) f) (* u v) (* v w)))
   (* w
      (expt v 2)
      (((* (partial 0) (expt (partial 1) 2)) f) (* u v) (* v w))))
|#

((derivative (lambda (x) (/ (sin x) x))) 'a)
'expect-value: '
(+ (* (cos a) (/ 1 a)) (* -1 (/ (sin a) (square a))))
;;; Using elementary simplifier...
#|
(/ (+ (* (cos a) (expt a 2))
      (* -1 a (sin a)))
   (expt a 3))
|#

((derivative (lambda (x)
               (/ (- 1 (exp (expt x 2))) x)))
 'a)
'expect-value: '
(+ (* (* -1 (* (exp (expt a 2)) (* 2 a))) (/ 1 a))
   (* -1 (/ (- 1 (exp (expt a 2))) (square a))))
;;; Using elementary simplifier...
#|
(/ (+ -1 (exp (expt a 2))
      (* -2
         (exp (expt a 2))
         (expt a 2)))
   (expt a 2))
|#

;;; Version of Alexey's bug from paper by Manzyuk, et.al.

;;; With patch, using with-new-increment

(define s
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))
'expect-value: 's

((((derivative S) 0) (literal-function 'f)) 'x)
'expect-value: '((derivative f) x)

((((derivative s) 0)
  (((derivative s) 0)
   (literal-function 'f))) 'x)
'expect-value: '((derivative (derivative f)) x)

((((derivative s) 3)
  (((derivative s) 5)
   (literal-function 'f))) 'x)
'expect-value: '((derivative (derivative f)) (+ (+ x 3) 5))

(define s
  (lambda (u)
    (lambda (f)
      (lambda (x)
        (f (+ x u))))))
'expect-value: 's

((((derivative S) 0) (literal-function 'f)) 'x)
'expect-value: '((derivative f) x)

((((derivative s) 0) (((derivative s) 0) (literal-function 'f))) 'x)
'expect-value: '((derivative (derivative f)) x)

((((derivative s) 3) (((derivative s) 5) (literal-function 'f))) 'x)
'expect-value: '((derivative (derivative f)) (+ (+ x 3) 5))

;;; But by defining s-hat rather than creating it twice we get
;;; the expected failure.

(define s-hat ((derivative s) 3))

((s-hat (literal-function 'f)) 'x)
'expect-value: '((derivative f) (+ x 3))

((s-hat (s-hat (literal-function 'f))) 'x)
;;; Without patch ;Value: 0 ;;; Wrong!
;;; With patch -- correct:
'expect-value: '((derivative (derivative f)) (+ (+ x 3) 3))

;;; May 2019.  Siskind found a new bug!

(define s
 (lambda (u)
   (lambda (f1)
     (lambda (f2)
       (lambda (x)
         ((f1 f2)
          (+ x u)))))))
#| s |#

(define (identity x) x)
#| identity |#

(define d-hat
  ((derivative s) 0))
#| d-hat |#

(((((derivative s) 0)
   (((derivative s) 0)
    identity))
  exp)
 1)
'expect-value: '2.718281828459045

(((d-hat (d-hat identity))
  exp)
 1)
'expect-value: '2.718281828459045

(((d-hat (d-hat identity))
  (literal-function 'f))
 'a)
'expect '((derivative (derivative f)) a)

#|
;;; Must (load "load-simplifier")

(pp
 (simplify
  (((((derivative s) 0)
     (((derivative s) 0)
      identity))
    (lambda (x) (* x x x x)))
   'a)))
'expect-value: '(* 12 (expt a 2))

(pp
 (simplify
  (((d-hat (d-hat identity))
    (lambda (x) (* x x x x)))
   'a)))
'expect-value: '(* 12 (expt a 2))
|#

;;; Church pairs

(define kons
  (lambda (a d)
    (lambda (m)
      (m a d))))

(define kar
  (lambda (x)
    (x (lambda (a d) a))))

(define kdr
  (lambda (x)
    (x (lambda (a d) d))))

(define p1
  (kons 'x1 'y1))

(define p2
  (kons 'x2 'y2))

(define (-p p q)
  (kons (- (kar p) (kar q))
        (- (kdr p) (kdr q))))

(define (n*p n p)
  (kons (* n (kar p))
        (* n(kdr p))))

(define (len dp)
  (sqrt (+ (square (kar dp))
           (square (kdr dp)))))

(len (-p p2 p1))
'expect-value: '
(sqrt (+ (square (- x2 x1))
         (square (- y2 y1))))

(define (f z)
  (len (- (n*p z p2) p1)))

(f 'a)
'expect-value: '
(sqrt (+ (square (- (* a x2) x1))
         (square (- (* a y2) y1))))

((derivative f) 'a)
'expect-value: '
(* (/ 1
      (* 2
         (sqrt (+ (square #0=(- (* a x2) x1))
                  (square #1=(- (* a y2) y1))))))
   (+ (* (* 2 #0#) x2)
      (* (* 2 #1#) y2)))
#|
(/ (+ (* a (expt x2 2))
      (* a (expt y2 2))
      (* -1 x1 x2)
      (* -1 y1 y2))
   (sqrt
    (+ (* (expt a 2) (expt x2 2))
       (* (expt a 2) (expt y2 2))
       (* -2 a x1 x2)
       (* -2 a y1 y2)
       (expt x1 2)
       (expt y1 2))))

;;; The correct derivative!
|#

(define (g x y)
  (len (kons x y)))

(((partial 0) g) 'a 'b)
'expect-value: '
(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 a))

(((partial 1) g) 'a 'b)
'expect-value: '
(* (/ 1 (* 2 (sqrt (+ (square a) (square b))))) (* 2 b))

(((partial 0) ((partial 1) g)) 'a 'b)
'expect-value: '
(* (* (* -1
         (/ 1
            (square (* 2 (sqrt #0=(+ (square a) (square b)))))))
      (* 2 (* (/ 1 (* 2 (sqrt #0#))) (* 2 b))))
   (* 2 a))
#|
(simplify (((partial 0) ((partial 1) g)) 'a 'b))
(/ (* -4 a b)
   (* (sqrt (+ (expt a 2) (expt b 2)))
      (expt (* 2 (sqrt (+ (expt a 2) (expt b 2))))
            2)))
= (* -1 a b (expt (+ (expt a 2) (expt b 2)) -3/2))
;;; The correct derivative!

(simplify (((partial 1) ((partial 0) ((partial 1) g))) 'a 'b))
(/ (+ (* -4 a)
      (* 48
         a
         (expt b 2)
         (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) -2)))
   (* (sqrt (+ (expt a 2) (expt b 2)))
      (expt (* 2 (sqrt (+ (expt a 2) (expt b 2)))) 2)))
;;; The correct derivative!
|#

;;; New test problem from Sam Ritchie <sritchie09@gmail.com>
(define (f x)
  (lambda (cont)
    (cont (lambda (y) (* x y))
          (lambda (g) (g x)))))

(((derivative f) 5) (lambda (f1 f2) (f2 f1)))
'expect-value: '
10


