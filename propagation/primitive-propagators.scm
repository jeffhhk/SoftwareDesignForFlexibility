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

;;;; Core unidirectional propagators

(define (install-core-propagators! cell-merge-procedure
                                   arithmetic projector)
  (set! cell-merge cell-merge-procedure)
  (install-package! (arithmetic-propagators arithmetic))
  (install-package! (boolean-propagators projector))
  (install-package! (shortcut-boolean-propagators))
  (install-package! (control-propagators projector)))

(define (simple-propagator-projector name arity base-procedure)
  (declare (ignore name arity))
  base-procedure)

(define (layered-propagator-projector name arity base-procedure)
  (if (and (eq? name 'merge-metadata)
           (= arity 2)
           (eqv? merge-metadata base-procedure))
      merge-metadata-layered
      (make-layered-procedure name arity base-procedure)))

(define (arithmetic-propagators arithmetic)
  (let ((arithmetic (avoid-0/0-wrapper arithmetic)))
    (primitive-propagators-package
     (arithmetic-name arithmetic)
     (map (lambda (operator)
            (list (symbol 'p: operator)
                  (operation-procedure
                   (arithmetic-operation operator arithmetic))))
          (arithmetic-operators arithmetic)))))

(define (avoid-0/0-wrapper base-arithmetic)
  (make-arithmetic 'avoid-0/0-wrapper
                   (arithmetic-domain-predicate base-arithmetic)
                   (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (lambda (operator base-operation)
      (case operator
        ((/)
         (transform-operation-procedure
          (lambda (procedure)
            ;; Our examples benefit from handling 0/0 this way.
            (lambda (dividend divisor)
              (if (and (~zero? dividend)
                       (~zero? divisor))
                  the-nothing
                  (procedure dividend divisor))))
          base-operation))
        (else base-operation)))))

(define (~zero? value)
  (let ((x (get-base-value value)))
    (and (n:number? x)
         (if (exact? x)
             (n:= x 0)
             (let ((m (n:magnitude x)))
               (n:<= m
                     (n:* flo:ulp-of-one m)))))))

;;; This makes the file analyzer happy, because otherwise it
;;; can't see these definitions.
(define p:*)
(define p:+)
(define p:-)
(define p:->)
(define p:/)
(define p:<)
(define p:<=)
(define p:=)
(define p:>)
(define p:>=)
(define p:abs)
(define p:acos)
(define p:and)
(define p:angle)
(define p:asin)
(define p:atan)
(define p:ceiling)
(define p:conditional)
(define p:conjoiner-dumb)
(define p:cos)
(define p:disjoiner-dumb)
(define p:dna)
(define p:exp)
(define p:expt)
(define p:floor)
(define p:imag-part)
(define p:imp)
(define p:invert)
(define p:log)
(define p:magnitude)
(define p:make-polar)
(define p:make-rectangular)
(define p:max)
(define p:min)
(define p:negate)
(define p:negative?)
(define p:not)
(define p:or)
(define p:pmi)
(define p:positive?)
(define p:real-part)
(define p:remainder)
(define p:ro)
(define p:round)
(define p:sin)
(define p:spdt-switch)
(define p:spst-switch)
(define p:sqrt)
(define p:square)
(define p:tan)
(define p:truncate)
(define p:zero?)

(define (boolean-propagators projector)
  (primitive-propagators-package
   'boolean
   (map (lambda (entry)
          (list (symbol 'p: (car entry))
                (projector (car entry)
                           (cadr entry)
                           (caddr entry))))
        `((-> 1 ,(lambda (x) x))
          (not 1 ,not)
          (conjoiner-dumb 2 ,boolean/and)
          (disjoiner-dumb 2 ,boolean/or)
          ;;(eqv 2 ,eqv?)

          ;; Used for logical short cuts
          (imp 1 ,boolean/imp)
          (pmi 1 ,boolean/pmi)

          ;; DNA is to AND as division is to multiplication
          (dna 2 ,boolean/dna)

          ;; RO is to OR as division is to multiplication
          (ro 2 ,boolean/ro)))))

;;; implication
(define (boolean/imp a) (if a #t the-nothing))
;;; converse implication
(define (boolean/pmi a) (if (not a) #f the-nothing))
;;; dna is to and as division is to multiplication
(define (boolean/dna c x) (if (and (not c) x) #f the-nothing))
;;; ro is to or as division is to multiplication
(define (boolean/ro c x) (if (and c (not x)) #t the-nothing))

(define (primitive-propagators-package name alist)
  (make-package
   `(primitive-propagators ,name)
   (map (lambda (p)
          (cons (car p)
                (primitive-propagator (cadr p) (car p))))
        alist)))

(define (shortcut-boolean-propagators)
  ;; Short cut if any input is false
  (define (p:and a b c)
    (define me
      (compound-propagator (list a b) (list c)
        (lambda ()
          (p:conjoiner-dumb a b c)
          (p:pmi a c)
          (p:pmi b c))
        `(p:and (,(get-name a) ,(get-name b))
                ,(get-name c))))
    me)

  ;; Short cut if any input is true
  (define (p:or a b c)
    (define me
      (compound-propagator (list a b) (list c)
        (lambda ()
          (p:disjoiner-dumb a b c)
          (p:imp a c)
          (p:imp b c))
        `(p:or (,(get-name a) ,(get-name b))
               ,(get-name c))))
    me)

  (make-package
   '(shortcut-boolean-propagators)
   `((p:and . ,p:and)
     (p:or . ,p:or))))

(define (control-propagators projector)

  (define (conditional p if-true if-false output)
    (propagator (list p if-true if-false) (list output)
      (lambda ()
        (let ((pval (cell-strongest p)))
          (if (unusable-value? pval)
              'done
              (select pval
                      if-true
                      if-false
                      output))))
      'conditional))

  (define (spst-switch p if-true output)
    (propagator (list p if-true) (list output)
      (lambda ()
        (let ((pval (cell-strongest p)))
          (if (unusable-value? pval)
              'done
              (select pval
                      if-true
                      #f
                      output))))
      'spst-switch))

  (define (select pval if-true if-false output)
    (let ((selected-cell
           (if (get-base-value pval)
               if-true
               if-false)))
      (if selected-cell
          (add-cell-content! output
            (merge-metadata* (cell-strongest selected-cell)
                             pval)))))

  (define merge-metadata*
    (projector 'merge-metadata
               2
               merge-metadata))

  (make-package
   `(control-propagators)
   `((p:conditional . ,conditional)
     (p:spdt-switch . ,conditional)
     (p:spst-switch . ,spst-switch))))