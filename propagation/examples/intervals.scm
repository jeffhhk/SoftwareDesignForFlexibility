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

(define (make-interval low high)
  (list 'interval low high))

(define (interval? object)
  (and (list? object)
       (n:= 3 (length object))
       (eq? 'interval (car object))))
(register-predicate! interval? 'interval)

(define (interval-low interval)
  (cadr interval))

(define (interval-high interval)
  (caddr interval))

(define interval make-interval)         ;useful alias

(define (->interval x)
  (if (interval? x) x (make-interval x x)))

(define (empty-interval? x)
  (n:> (interval-low x) (interval-high x)))

(define (intersect-intervals x y)
  (make-interval
   (n:max (interval-low x) (interval-low y))
   (n:min (interval-high x) (interval-high y))))

(define (subinterval? interval-1 interval-2)
  (and (n:>= (interval-low interval-1)
             (interval-low interval-2))
       (n:<= (interval-high interval-1)
             (interval-high interval-2))))

(define (within-interval? number interval)
  (n:<= (interval-low interval) number (interval-high interval)))

(define-generic-procedure-handler value-implies?
  (any-arg 2 interval? real?)
  (lambda (x y)
    (cond ((not (interval? x)) (within-interval? x y))
          ((not (interval? y)) #f)
          (else (subinterval? x y)))))

(define (add-interval x y)
  (make-interval (n:+ (interval-low x) (interval-low y))
                 (n:+ (interval-high x) (interval-high y))))

(define (sub-interval x y)
  (make-interval (n:- (interval-low x) (interval-high y))
                 (n:- (interval-high x) (interval-low y))))

(define (mul-interval x y)
  (let ((ll (n:* (interval-low x) (interval-low y)))
        (lh (n:* (interval-low x) (interval-high y)))
        (hl (n:* (interval-high x) (interval-low y)))
        (hh (n:* (interval-high x) (interval-high y))))
    (make-interval (n:min ll lh hl hh)
                   (n:max ll lh hl hh))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (n:/ 1.0 (interval-high y))
                               (n:/ 1.0 (interval-low y)))))

(define (square-interval x)
  (make-interval (n:square (interval-low x))
                 (n:square (interval-high x))))

(define (sqrt-interval x)
  (make-interval (n:sqrt (interval-low x))
                 (n:sqrt (interval-high x))))

;;; exp log sin cos tan
(define (exp-interval x)
  (make-interval (n:exp (interval-low x))
                 (n:exp (interval-high x))))

(define (log-interval x)
  (if (n:<= (interval-low x) 0)
      (error "Bad range log interval" x))
  (make-interval (n:log (interval-low x))
                 (n:log (interval-high x))))

;; Fix this!  These ranges are safe for examples, but really
;; should be worked out carefully.

(define (sin-interval x)
  (if (not (and (n:<= :-pi/2 (interval-low x) :pi/2)
                (n:<= :-pi/2 (interval-high x) :pi/2)))
      (error "Bad range sin interval" x))
  (make-interval (n:sin (interval-low x))
                 (n:sin (interval-high x))))

(define (asin-interval x)
  (if (not (and (n:<= -1 (interval-low x) +1)
                (n:<= -1 (interval-high x) +1)))
      (error "Bad range asin interval" x))
  (make-interval (n:asin (interval-low x))
                 (n:asin (interval-high x))))

(define (cos-interval x)
  ;; Fix this!
  (if (not (and (n:<= 0 (interval-low x) :pi)
                (n:<= 0 (interval-high x) :pi)))
      (error "Bad range cos interval" x))
  (make-interval (n:cos (interval-high x))
                 (n:cos (interval-low x))))

(define (acos-interval x)
  (if (not (and (n:<= -1 (interval-low x) +1)
                (n:<= -1 (interval-high x) +1)))
      (error "Bad range acos interval" x))
  (make-interval (n:acos (interval-high x))
                 (n:acos (interval-low x))))

(define (tan-interval x)
  (if (not (and (n:< :-pi/2 (interval-low x) :pi/2)
                (n:< :-pi/2 (interval-high x) :pi/2)))
      (error "Bad range tan interval" x))
  (make-interval (n:tan (interval-low x))
                 (n:tan (interval-high x))))

(define (atan-interval x)
  (make-interval (n:atan (interval-low x))
                 (n:atan (interval-high x))))

(define (sign-interval x)
  (let ((sl (n:sign (interval-low x)))
        (sh (n:sign (interval-high x))))
    (cond ((and (n:= sl 1) (n:= sh 1)) 1)
          ((and (n:= sl -1) (n:= sh -1)) -1)
          (else 0))))

(define (negate-interval y)
  (make-interval (n:- 0.0 (interval-high y))
                 (n:- 0.0 (interval-low y))))

(define (invert-interval y)
  (make-interval (n:/ 1.0 (interval-high y))
                 (n:/ 1.0 (interval-low y))))

(define (abs-interval x)
  (let ((al (n:abs (interval-low x)))
        (ah (n:abs (interval-high x))))
    (make-interval (n:min al ah) (n:max al ah))))

(define (interval=? x y)
  (and (~=? (interval-high x) (interval-high y))
       (~=? (interval-low x) (interval-low y))))

(define-generic-procedure-handler g:equivalent?
  (match-args interval? interval?)
  interval=?)

(define (interval<? x y)
  (~<? (interval-high x) (interval-low y)))

(define (interval>? x y)
  (~>? (interval-low x) (interval-high y)))

(define (interval<=? x y)
  (n:<= (interval-high x) (interval-low y)))

(define (interval>=? x y)
  (n:>= (interval-low x) (interval-high y)))

(define (interval-extender base-arithmetic)
  (declare (ignore base-arithmetic))
  (make-arithmetic 'interval interval? (list numeric-arithmetic)
    (lambda (name base-constant)
      (declare (ignore name base-constant))
      (default-object))
    (lambda (operator base-operation)
      (declare (ignore base-operation))
      (let ((operation
             (case operator
               ((+) add-interval)
               ((-) sub-interval)
               ((*) mul-interval)
               ((/) div-interval)
               ((=) interval=?)
               ((<) interval<?)
               ((>) interval>?)
               ((<=) interval<=?)
               ((>=) interval>=?)
               ((square) square-interval)
               ((sqrt) sqrt-interval)
               ((sign) sign-interval)
               ((negate) negate-interval)
               ((invert) invert-interval)
               ((abs) abs-interval)
               ((exp) exp-interval)
               ((log) log-interval)
               ((sin) sin-interval)
               ((asin) asin-interval)
               ((cos) cos-interval)
               ((acos) acos-interval)
               ((tan) tan-interval)
               ((atan) atan-interval)
               (else #f))))
        (and operation
             (make-operation operator
                             (any-arg (operator-arity operator)
                                      interval?
                                      real?)
                             (lambda args
                               (apply operation
                                      (map ->interval
                                           args)))))))))

(define-generic-procedure-handler merge
  (any-arg 2 interval? real?)
  (lambda (x y)
    (cond ((not (interval? x)) (merge-interval-real y x))
          ((not (interval? y)) (merge-interval-real x y))
          (else (merge-intervals x y)))))

(define (merge-interval-real int x)
  (if (within-interval? x int)
      x
      the-contradiction))

(define (merge-intervals content increment)
  (let ((new-range (intersect-intervals content increment)))
    (cond ((interval=? new-range content) content)
          ((interval=? new-range increment) increment)
          ((empty-interval? new-range) the-contradiction)
          (else new-range))))

;;; For using center-spread

(define (+->interval value delta)
  (make-interval (n:- value delta) (n:+ value delta)))

(define (interval>+- interval)
  (let ((center
         (n:/ (n:+ (interval-high interval)
                   (interval-low interval))
              2)))
    `(+- ,center
         ,(n:- (interval-high interval) center))))
