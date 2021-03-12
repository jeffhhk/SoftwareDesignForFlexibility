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

;;; Complicated extractor, from scmutils.  Fixes Radul bug.

;;; extract-dx-part must be generic to handle compound values.

(define (extract-dx-default value dx)
  0)

(define extract-dx-part
  (simple-generic-procedure 'extract-dx-part 2
                            extract-dx-default))

(define (extract-dx-differential-1 value dx)
  (extract-dx-coefficient-from (infinitesimal-part value) dx))

(define (extract-dx-differential value dx)
  (let ((dx-diff-terms
         (filter-map
          (lambda (term)
            (let ((factors (diff-factors term)))
              (and (memv dx factors)
                   (make-diff-term
                    (diff-coefficient term)
                    (delv dx factors)))))
          (diff-terms value))))
    (cond ((null? dx-diff-terms) 0)
          ((and (null? (cdr dx-diff-terms))
                (null? (diff-factors (car dx-diff-terms))))
           (diff-coefficient (car dx-diff-terms)))
          (else
           (make-differential dx-diff-terms)))))

;; coderef: extract-dx-part:differential
(define-generic-procedure-handler extract-dx-part
  (match-args differential? diff-factor?)
  extract-dx-differential)

#|
;; coderef: extract-dx-function-wrong
(define (extract-dx-function fn dx)
  (lambda args
    (extract-dx-part (apply fn args) dx)))
|#

(define (extract-dx-function fn dx)
  (lambda args
    (let ((eps (make-new-dx)))
       (replace-dx dx eps
        (extract-dx-part
         (apply fn
           (map (lambda (arg)
                  (replace-dx eps dx arg))
                args))
         dx)))))

;; coderef: extract-dx-part:function
(define-generic-procedure-handler extract-dx-part
  (match-args function? diff-factor?)
  extract-dx-function)

(define (replace-dx-default new-dx old-dx object)
  (if (not (and (diff-factor? new-dx)
                (diff-factor? old-dx)))
      (error "Bad args to REPLACE-DX"
             new-dx old-dx object))
  object)

(define replace-dx
  (simple-generic-procedure 'replace-dx 3
                            replace-dx-default))


;;; This has a bug, reported by Sam Richie <sritchie09@gmail.com>
;;;  see replace-dx-differential.scm for a better version.
(define (replace-dx-differential new-dx old-dx object)
  (make-differential
   (sort (map (lambda (term)
                (make-diff-term
                 (diff-coefficient term)
                 (sort (substitute new-dx old-dx
                         (diff-factors term))
                       diff-factor>?)))
              (diff-terms object))
         diff-term>?)))

;; coderef: replace-dx:differential
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? differential?)
  replace-dx-differential)


(define (replace-dx-function new-dx old-dx fn)
  (lambda args
    (let ((eps (make-new-dx)))
      (replace-dx old-dx eps
        (replace-dx new-dx old-dx
          (apply fn
            (map (lambda (arg)
                   (replace-dx eps old-dx arg))
                 args)))))))

;; coderef: replace-dx:function
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? function?)
  replace-dx-function)

;;; Simple substitution
(define (substitute new old x)
  (cond ((pair? x)
         (cons (substitute new old (car x))
               (substitute new old (cdr x))))
        ((vector? x)
         (make-initialized-vector (vector-length x)
           (lambda (i)
             (substitute new old
                         (vector-ref x i)))))
        ((eqv? old x) new)
        (else x)))
