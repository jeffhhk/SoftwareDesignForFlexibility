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

;;;; Generic unify, generalized to segments.
;;;    Summer 2019.  First draft by GJS, improved with help
;;;    of Will Byrd and Michael Ballantyne.  Further
;;;    debugged with Kenny Chen, and later with Chris Hanson.

(define (unifier pattern1 pattern2)
  (let ((dict (unify pattern1 pattern2)))
    (and dict
         ((match:dict-substitution dict) pattern1))))

(define (unify pattern1 pattern2)
  (unify:internal pattern1 pattern2
                  (match:new-dict)
                  (lambda (dict) dict)))

;;; This is the programmer's interface to the unifier.

(define (unify:internal pattern1 pattern2 dict succeed)
  ((unify:dispatch (list pattern1) (list pattern2))
   dict
   (lambda (dict fail rest1 rest2)
     (assert (list? rest1))
     (assert (list? rest2))
     (or (and (null? rest1) (null? rest2)
              (succeed dict))
         (fail)))
   (lambda () #f)))

;;; terms1 and terms2 are lists of terms to be equated.

(define (unify:dispatch terms1 terms2)
  (assert (list? terms1))
  (assert (list? terms2))
  (define (unify-dispatcher dict succeed fail)
    (if (and (null? terms1) (null? terms2))
        (succeed dict fail terms1 terms2)
        ((unify:gdispatch terms1 terms2)
         dict
         (lambda (dict* fail* rest1 rest2)
           ((unify:dispatch rest1 rest2)
            dict* succeed fail*))
         fail)))
  unify-dispatcher)

(define (unify:fail terms1 terms2)
  (define (unify-fail dict succeed fail)
    (fail))
  unify-fail)

(define unify:gdispatch
  (simple-generic-procedure 'unify 2 unify:fail))

(define (car-satisfies pred)
  (lambda (terms)
    (and (pair? terms)
         (pred (car terms)))))

(define (unify:constant-terms terms1 terms2)
  (let ((first1 (car terms1)) (rest1 (cdr terms1))
        (first2 (car terms2)) (rest2 (cdr terms2)))
    (define (unify-constants dict succeed fail)
      (if (eqv? first1 first2)
          (succeed dict fail rest1 rest2)
          (fail)))
    unify-constants))

(define (constant-term? term)
  (and (not (match:var? term))
       (not (list? term))))

;; coderef: unify-constant-terms
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies constant-term?)
              (car-satisfies constant-term?))
  unify:constant-terms)

(define (unify:list-terms terms1 terms2)
  (let ((first1 (car terms1)) (rest1 (cdr terms1))
        (first2 (car terms2)) (rest2 (cdr terms2)))
    (define (unify-lists dict succeed fail)
      ((unify:dispatch first1 first2)
       dict
       (lambda (dict* fail* null1 null2)
         (assert (null? null1))
         (assert (null? null2))
         (succeed dict* fail* rest1 rest2))
       fail))
    unify-lists))

(define (list-term? term)
  (and (not (match:var? term))
       (list? term)))

;; coderef: unify-list-terms
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies list-term?)
              (car-satisfies list-term?))
  unify:list-terms)

;;; This is the syntactic equation solver for element vars.

(define (maybe-substitute var-first terms)
  (define (unify-substitute dict succeed fail)
    (let ((var (car var-first)) (rest1 (cdr var-first))
          (term (car terms)) (rest2 (cdr terms)))
      (cond ((and (match:element-var? term)
                  (match:vars-equal? var term))
             (succeed dict fail rest1 rest2))
            ((match:has-binding? var dict)
             ((unify:dispatch (cons (match:get-value var dict) rest1)
                              terms)
              dict succeed fail))
            (else
             (let ((dict* (do-substitute var term dict)))
               (if dict*
                   (succeed dict* fail rest1 rest2)
                   (fail)))))))
  unify-substitute)

(define (do-substitute var term dict)
  (let ((term* ((match:dict-substitution dict) term)))
    (and (match:satisfies-restriction? var term*)
         (or (and (match:var? term*)
                  (match:vars-equal? var term*))
             (not (match:occurs-in? var term*)))
         (match:extend-dict var term*
           (match:map-dict-values
            (match:single-substitution var term*)
            dict)))))

(define (element-1? term)
  (any-object? term))

(define (element? term)
  (not (match:segment-var? term)))

;; coderef: element-var-thing
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies match:element-var?)
              (car-satisfies element?))
  (lambda (var-first terms)
    (maybe-substitute var-first terms)))

;; coderef: thing-element-var
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies element?)
              (car-satisfies match:element-var?))
  (lambda (terms var-first)
    (maybe-substitute var-first terms)))

;;; Segment variable extensions

(define (unify:segment-var-var var-first1 var-first2)
  (define (unify-seg-var-var dict succeed fail)
    (if (match:vars-equal? (car var-first1) (car var-first2))
        (succeed dict fail (cdr var-first1) (cdr var-first2))
        ((maybe-grab-segment var-first1 var-first2)
         dict
         succeed
         (lambda ()
           ((maybe-grab-segment var-first2 var-first1)
            dict
            succeed
            fail)))))
  unify-seg-var-var)

;; coderef: segment-var-var
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies match:segment-var?)
              (car-satisfies match:segment-var?))
  unify:segment-var-var)

;; coderef: segment-var-thing
(define-generic-procedure-handler unify:gdispatch
  (match-args (car-satisfies match:segment-var?)
              (complement (car-satisfies match:segment-var?)))
  (lambda (var-first terms)
    (maybe-grab-segment var-first terms)))

;; coderef: thing-segment-var
(define-generic-procedure-handler unify:gdispatch
  (match-args (complement (car-satisfies match:segment-var?))
              (car-satisfies match:segment-var?))
  (lambda (terms var-first)
    (maybe-grab-segment var-first terms)))

(define (maybe-grab-segment var-first terms)
  (define (maybe-grab dict succeed fail)
    (let ((var (car var-first)))
      (if (match:has-binding? var dict)
          ((unify:dispatch
            (append (match:get-value var dict)
                    (cdr var-first))
            terms)
           dict succeed fail)
          ((grab-segment var-first terms)
           dict succeed fail))))
  maybe-grab)

;; Try to match against each possible list.  If the last element
;; in the match is a segvar and bound, then expand it otherwise,
;; you have to worry about partial containment.

(define (grab-segment var-first terms)
  (define (grab dict succeed fail)
    (let ((var (car var-first)))
      (let slp ((initial '()) (terms* terms))
        (define (continue)
          (if (null? terms*)
              (fail)
              (slp (append initial (list (car terms*)))
                   (cdr terms*))))
        (let ((dict* (do-substitute var initial dict)))
          (if dict*
              (succeed dict* continue (cdr var-first) terms*)
              (continue))))))
  grab)