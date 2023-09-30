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

(define (simplify expression)
  (parameterize ((*assumptions* (list 'assumptions)))
    (let ((result
           ((compose algebra-3
                     composed-operators-multiply
                     combine-derivatives
                     algebra-3)
            expression)))
      (if (null? (cdr (*assumptions*)))
          result
          `(assuming ,@(cdr (*assumptions*)) ,result)))))

(define *assumptions* (make-parameter (list 'assumptions)))

(define (assume predicate . args)
  (set-cdr! (*assumptions*)
            (cons (cons predicate args)
                  (cdr (*assumptions*)))))

(define (positive-number? x)
  (and (number? x) (> x 0)))


(define algebra-3
  (rule-simplifier
   (list

    ;; Sums

    (rule `(+) 0)

    (rule `(+ (? a)) a)

    (rule `(+ (?? a) (+ (?? b)) (?? c))
          `(+ ,@a ,@b ,@c))

    (rule `(+ (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(+ ,@a ,x ,y ,@b)))


    ;; Products

    (rule `(*) 1)

    (rule `(* (? a)) a)

    (rule `(* (?? a) (* (?? b)) (?? c))
          `(* ,@a ,@b ,@c))

    (rule `(* (?? a) (? y) (? x) (?? b))
          (and (expr<? x y)
               `(* ,@a ,x ,y ,@b)))


    ;; Distributive law

    (rule `(* (?? a) (+ (?? b)) (?? c))
          `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


    ;; Combining like terms

    (rule `(+ (?? a) (? b) (? b) (?? c))
          `(+ ,@a (* 2 ,b) ,@c))

    (rule `(+ (?? a) (? b) (?? c) (* (? n ,number?) (? b)) (?? d))
          `(+ ,@a ,@c (* ,(+ n 1) ,b) ,@d))

    ;; Possibly unnecessary because terms are sorted and "b" < "3*b"?
    (rule `(+ (?? a) (* (? n ,number?) (? b)) (?? c) (? b) (?? d))
          `(+ ,@a ,@c (* ,(+ n 1) ,b) ,@d))

    (rule `(+ (?? a)
              (* (? m ,number?) (?? b))
              (?? c)
              (* (? n ,number?) (?? b))
              (?? d))
          `(+ ,@a (* ,(+ m n) ,@b) ,@c ,@d))

    (rule `(+ (?? a)
              (* (?? b))
              (* (? n ,number?) (?? b))
              (?? d))
          `(+ ,@a (* ,(+ n 1) ,@b) ,@d))


    ;; Combining like factors

    (rule `(* (?? a) (? b) (? b) (?? c))
          `(* ,@a (expt ,b 2) ,@c))

    (rule `(expt (+ (?? x)) 2)
          `(+ ,@(append-map (lambda (a1)
                              (map (lambda (a2)
                                     `(* ,a1 ,a2))
                                   x))
                            x)))

    (rule `(* (?? a) (? b) (?? c) (expt (? b) (? n)) (?? d))
          `(* ,@a ,@c (expt ,b ,(+ n 1)) ,@d))

    ;; Possibly unnecessary because terms are sorted and "b" < "b^3"?
    (rule `(* (?? a) (expt (? b) (? n)) (?? c) (? b) (?? d))
          `(* ,@a ,@c (expt ,b ,(+ n 1)) ,@d))

    (rule `(* (?? a)
              (expt (? b) (? m ,number?))
              (expt (? b) (? n ,number?))
              (?? d))
          `(* ,@a (expt ,b ,(+ m n)) ,@d))

    (rule `(expt (expt (? x) (? n)) (? m))
          `(expt ,x ,(* n m)))

    (rule `(/ (expt (? x) (? n)) (expt (? x) (? m)))
          `(expt ,x ,(- n m)))

    (rule `(/ (* (?? a) (expt (? x) (? n)) (?? b))
              (expt (? x) (? m)))
          `(* ,@a (expt ,x ,(- n m)) ,@b))

    (rule `(/ (expt (? x) (? n))
              (* (?? a) (expt (? x) (? m)) (?? b)))
          `(/ (expt ,x ,(- n m)) (* ,@a ,@b)))

    (rule `(/ (* (?? a) (expt (? x) (? n)) (?? b))
              (* (?? c) (expt (? x) (? m)) (?? d)))
          `(/ (* ,@a (expt ,x ,(- n m)) ,@b)
              (* ,@c ,@d)))


    ;; Simple common factors

    (rule `(/ (? a) (? a)) 1)

    (rule `(/ (? a)
              (* (?? b) (? a) (?? c)))
          `(/ 1 (* ,@b ,@c)))

    (rule `(/ (* (?? b) (? a) (?? c))
              (? a))
          `(* ,@b ,@c))

    (rule `(/ (* (?? b) (? a) (?? c))
              (* (?? d) (? a) (?? e)))
          `(/ (* ,@b ,@c)
              (* ,@d ,@e)))
    
    (rule `(* (?? x)
              (? a)
              (?? y)
              (/ (? b) (? a))
              (?? z))
          `(* ,@x ,@y ,b ,@z))

    (rule `(* (?? x)
              (? a)
              (?? y)
              (/ (? b) (* (?? u) (? a) (?? v)))
              (?? z))
          `(/ (* ,@x ,@y ,b ,@z)
              (* ,@u ,@v)))


    ;; Numerical simplifications below

    (rule `(+ 0 (?? x)) `(+ ,@x))

    (rule `(+ (? x ,number?) (? y ,number?) (?? z))
          `(+ ,(+ x y) ,@z))


    (rule `(- (? x ,number?)) (- x))

    (rule `(- (? x ,number?) (? y ,number?))
          (- x y))


    (rule `(* 0 (?? x)) 0)

    (rule `(* 1 (?? x)) `(* ,@x))

    (rule `(* (? x ,number?) (? y ,number?) (?? z))
          `(* ,(* x y) ,@z))


    (rule `(/ (? x ,number?)) (/ x))

    (rule `(/ (? x ,number?) (? y ,number?))
          (/ x y))

    (rule `(square (? x)) `(expt ,x 2))

    (rule `(expt (? x) 0) 1)

    (rule `(expt (? x) 1) x)

    (rule `(expt 0 (? x))
          (begin
            (assume 'positive-number? x)
            0))

    (rule `(expt (? m ,number?) (? n ,number?))
          (expt m n))

    (rule `(expt (? x) -1) `(/ 1 ,x))

    (rule `(* (?? a) (? b) (?? c) (expt (? b) (? n)) (?? d))
          `(* ,@a ,@c (expt ,b ,(+ n 1))))

    (rule `(sqrt (? x ,number?)) (sqrt x))


    ;; Negation

    (rule `(- (* (? x ,number?) (?? y)))
          `(* ,(- x) ,@y))

    (rule `(- (? x)) `(* -1 ,x))

    (rule `(- (* (?? y))) `(* -1 ,@y))

    (rule `(- (? a) (?? b))
          `(+ ,a (* -1 (+ ,@b))))


    ;; Inversion

    (rule `(/ (* (? x ,number?) (?? y)))
          `(* ,(/ 1 x) ,@y))

    (rule `(/ (? x) (? y ,number?))
          `(* ,(/ 1 y) ,x))

    (rule `(/ (* (?? y))) `(/ 1 (* ,@y)))

    (rule `(* (?? a) (/ (? b) (?? c)) (?? d))
          `(/ (* ,@a ,b ,@d) (* ,@c)))

    (rule `(/ (? a) (* (?? d) (/ (? b) (?? c)) (?? e)))
          `(/ (* ,a ,@c) (* ,b ,@d ,@e)))

    (rule `(/ (/ (? a) (?? b)) (?? c))
          `(/ ,a (* ,@b ,@c)))

    (rule `(+ (?? a) (/ (? b) (?? c)) (?? d))
          `(/ (+ (* ,@c (+ ,@a ,@d)) ,b) (* ,@c)))


    ;; Derivatives: partials of R^n->R^m commute,
    ;;  so standardize their order.

    (rule `((partial (? i)) ((partial (? j)) (? f)))
          (and (expr<? j i)
               `((partial ,j) ((partial ,i) ,f))))


    )))

(define combine-derivatives
  (rule-simplifier
   (list
    (rule `((partial (? i)) ((partial (? j)) (? f)))
          `((compose (partial ,i) (partial ,j)) ,f))

    (rule `(((compose (partial (? i))
                      (?? ps))
             ((partial (? k))
              (? f))))
          `((compose (partial ,i)
                     ,@ps
                     (partial ,k))
            ,f))

    (rule `((partial (? k))
             ((compose (partial (? i))
                       (?? ps))
              (? f)))
          `((compose (partial ,k)
                     (partial ,i)
                     ,@ps) ,f))

    (rule `(derivative (derivative (? f)))
          `(compose derivative derivative ,f))

    (rule `(derivative ((compose derivative (?? ds)) (? f)))
          `(compose derivative derivative ,@ds ,f))

    (rule `((compose derivative (?? ds)) (derivative (? f)))
          `(compose derivative ,@ds derivative ,f))

    )))


;;; The following is to produce scmutils style.  The derivative
;;; program does not separately define operators, for which
;;; multiplication is defined as composition.

(define composed-operators-multiply
  (rule-simplifier
   (list

    (rule `(compose (partial (? i)) (?? ps))
          `(* (partial ,i) ,@ps))

    (rule `(compose derivative (?? ds))
          `(* derivative ,@ds))

    )))
