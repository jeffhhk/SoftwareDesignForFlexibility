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

;;;; Function predicates

(define function-template
  (make-predicate-template 'function
                           '((?* domains -) (? codomain))
                           tagging-strategy:never
    (lambda (get-tag)
      (lambda (object)
        (and (simple-function? object)
             (tag<= (applicable-object-tag object) (get-tag)))))))

;;; The resulting predicate represents a "function space" for the
;;; given sets.
(define make-function-predicate
  (predicate-template-instantiator function-template))

(define function-predicate?
  (predicate-template-predicate function-template))

(define function-predicate-domains
  (predicate-template-accessor 'domains function-template))

(define function-predicate-codomain
  (predicate-template-accessor 'codomain function-template))

(define (function-predicate-arity predicate)
  (length (function-predicate-domains predicate)))

(define (extend-function-predicate domain-extender
                                   codomain-extender
                                   function-predicate)
  (make-function-predicate
   (map domain-extender
        (function-predicate-domains function-predicate))
   (codomain-extender
    (function-predicate-codomain function-predicate))))

(define (selectively-extend-function-predicate domain-extender
                                               codomain-extender
                                               function-predicate
                                               selector)
  (let ((maybe-map
         (lambda (extender)
           (lambda (select? predicate)
             (if select?
                 (extender predicate)
                 predicate)))))
    (make-function-predicate
     (map (maybe-map domain-extender)
          (selector 'domains)
          (function-predicate-domains function-predicate))
     ((maybe-map codomain-extender)
      (selector 'codomain)
      (function-predicate-codomain function-predicate)))))

(define (make-signature-selector arity domain-index
                                 codomain-index)
  (guarantee n:exact-nonnegative-integer? arity)
  (guarantee (index-predicate arity) domain-index)
  (guarantee (index-predicate 1) codomain-index)
  (lambda (operator)
    (case operator
      ((domains) (index->booleans domain-index arity))
      ((codomain) (car (index->booleans codomain-index 1)))
      (else (error "Unknown operator:" operator)))))

;;;; Functions

(define (function? object)
  (or (simple-function? object)
      (union-function? object)))
(register-predicate! function? 'function)

(define (apply-function function args)
  (apply function args))

(define (function-name function)
  (cond ((simple-function? function)
         (simple-function-name function))
        ((union-function? function)
         (union-function-name function))
        (else
         (error:not-a function? function))))

(define (function-tag function)
  (cond ((simple-function? function)
         (simple-function-tag function))
        ((union-function? function)
         (predicate->tag (union-function-predicate function)))
        (else
         (error:not-a function? function))))

(define (function-predicate function)
  (tag->predicate (function-tag function)))

(define (function-components function)
  (cond ((simple-function? function)
         (list function))
        ((union-function? function)
         (union-function-components function))
        (else
         (error:not-a function? function))))

(define (map-function procedure function)
  (union-function*
   (map procedure (function-components function))))

(define (append-map-function procedure function)
  (union-function*
   (append-map procedure (function-components function))))

;;;; Simple functions

(define (simple-function? object)
  (and (applicable-object? object)
       (simple-function-metadata?
        (applicable-object->object object))))
(register-predicate! simple-function? 'simple-function)
(set-predicate<=! simple-function? function?)

(define (make-simple-function name predicate procedure)
  (letrec
      ((function
        (make-object-applicable
         predicate
         (make-simple-function-metadata name procedure)
         (lambda args (apply-simple-function function args)))))
    function))

(define-record-type <simple-function-metadata>
    (make-simple-function-metadata name procedure)
    simple-function-metadata?
  (name simple-function-metadata-name)
  (procedure simple-function-metadata-procedure))

(define (simple-function-name function)
  (simple-function-metadata-name
   (applicable-object->object function)))

(define (simple-function-tag function)
  (applicable-object-tag function))

(define (simple-function-predicate function)
  (tag->predicate (simple-function-tag function)))

(define (simple-function-procedure function)
  (simple-function-metadata-procedure
   (applicable-object->object function)))

(define (simple-function-domains function)
  (function-predicate-domains
   (simple-function-predicate function)))

(define (simple-function-codomain function)
  (function-predicate-codomain
   (simple-function-predicate function)))

(define (simple-function-arity function)
  (length (simple-function-domains function)))

(define (simple-function-apply-fit function args)
  (let ((domains (simple-function-domains function)))
    (and (n:= (length domains) (length args))
         (let ((fits (map value-fit args domains)))
           (and (not (memq #f fits))
                (combine-fits
                 (lambda (args)
                   (apply (simple-function-procedure function)
                          args))
                 fits))))))

(define (apply-simple-function function args)
  (let ((fit (simple-function-apply-fit function args)))
    (if (not fit)
        (error "Inapplicable function:" function args))
    (fit)))

(define (simple-generic-function? object)
  (and (simple-function? object)
       (generic-procedure? (simple-function-procedure object))))
(register-predicate! simple-generic-function?
                     'simple-generic-function)
(set-predicate<=! simple-generic-function? simple-function?)

(define-generic-procedure-handler value-restriction
  (match-args simple-generic-function? predicate?)
  (lambda (value predicate)
    (let ((handlers
           (filter predicate
                   (generic-procedure-handlers
                    (simple-function-procedure value)))))
      (and (n:pair? handlers)
           (lambda ()
             (union-function* handlers))))))

;;;; Endo-functions

(define (endo-function-predicate? object)
  (and (function-predicate? object)
       (let ((domains (function-predicate-domains object)))
         (and (n:pair? domains)
              (eqv? (car domains)
                    (function-predicate-codomain object))
              (n:null? (cdr domains))))))
(register-predicate! endo-function-predicate?
                     'endo-function-predicate)
(set-predicate<=! endo-function-predicate? function-predicate?)

(define (make-endo-function-predicate domain)
  (make-function-predicate (list domain) domain))

(define (endo-function-predicate-domain predicate)
  (guarantee endo-function-predicate? predicate)
  (car (function-predicate-domains predicate)))

(define (simple-endo-function? object)
  (and (simple-function? object)
       (endo-function-predicate?
        (simple-function-predicate object))))
(register-predicate! simple-endo-function? 'simple-endo-function)
(set-predicate<=! simple-endo-function? simple-function?)

(define (simple-endo-function-domain function)
  (endo-function-predicate-domain
   (simple-function-predicate function)))

;;;; Union functions

(define (union-function function . functions)
  (union-function* (cons function functions)))

(define (union-function* functions)
  (guarantee-list-of function? functions)
  (if (n:null? functions)
      (error "Can't make an empty union function."))
  (let ((simple-functions
         (append-map function-components functions)))
    (let ((arity (simple-function-arity (car simple-functions))))
      (for-each
       (lambda (simple-function)
         (if (not (n:= arity
                       (simple-function-arity simple-function)))
             (error "Inconsistent arity in union:"
                    arity
                    (simple-function-arity simple-function))))
       (cdr simple-functions)))
    (if (and (n:pair? simple-functions)
             (n:null? (cdr simple-functions)))
        (car simple-functions)
        (letrec
            ((union-function
              (let ((union (make-object-union simple-functions)))
                (make-object-applicable
                 (get-predicate union)
                 union
                 (lambda args
                   (apply-union-function union-function
                                         args))))))
          union-function))))

(define (apply-union-function function args)
  (let ((fit (union-function-apply-fit function args)))
    (if (not fit)
        (error "Inapplicable function:" function args))
    (fit)))

(define (union-function-apply-fit function args)
  (let ((fits (union-function-component-fits function args)))
    (and (n:pair? fits)
         (combine-fits object-union* fits))))

(define (union-function-component-fits function args)
  (let ((fits
         (map (lambda (function)
                (simple-function-apply-fit
                   function args))
              (union-function-components function))))
    (let ((functions
           (filter-map (lambda (function fit)
                         (and fit function))
                       (union-function-components function)
                       fits)))
      (filter-map (lambda (function fit)
                    (and fit
                         (not
                          (is-function-subsumed? function
                                                 functions))
                         fit))
                  (union-function-components function)
                  fits))))

(define (is-function-subsumed? function functions)
  (let ((predicate (simple-function-predicate function)))
    (any (lambda (function*)
           (let ((predicate*
                  (simple-function-predicate function*)))
             (and (not (eqv? predicate* predicate))
                  (every
                   (lambda (domain* domain)
                     (predicate<= domain* domain))
                   (function-predicate-domains predicate*)
                   (function-predicate-domains predicate)))))
         functions)))

(define (union-function? object)
  (and (applicable-object? object)
       (let ((object* (applicable-object->object object)))
         (and (object-union? object*)
              (every simple-function?
                     (object-union-components object*))))))
(register-predicate! union-function? 'union-function)
(set-predicate<=! union-function? function?)

(define (union-function-components union)
  (object-union-components (applicable-object->object union)))

(define (union-function-name union)
  `(union
    ,@(map simple-function-name
           (union-function-components union))))

(define (union-function-predicate union)
  (let ((predicates
         (map simple-function-predicate
              (union-function-components union))))
    (make-function-predicate (apply map
                                    disjoin
                                    (map function-predicate-domains
                                         predicates))
                             (disjoin*
                              (map function-predicate-codomain
                                   predicates)))))

(define-generic-procedure-handler value-restriction
  (match-args union-function? predicate?)
  (lambda (value predicate)
    (let ((components
           (filter predicate
                   (union-function-components value))))
      (and (n:pair? components)
           (lambda () (union-function* components))))))