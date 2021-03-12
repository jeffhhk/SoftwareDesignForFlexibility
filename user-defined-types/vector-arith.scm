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

;;;; Vector arithmetic

(define vector-template
  (make-predicate-template 'vector '((? element))
                           tagging-strategy:always
    (lambda (get-tag)
      (lambda (elts)
        (and (n:vector? elts)
             (every (vector-predicate-element
                     (tag->predicate (get-tag)))
                    (vector->list elts)))))))

(define make-vector-predicate
  (predicate-template-instantiator vector-template))

(define vector-predicate?
  (predicate-template-predicate vector-template))

(define vector-predicate-element
  (predicate-template-accessor 'element vector-template))

(define (vector-constructor elt-predicate)
  (let ((constructor
         (predicate-constructor
          (make-vector-predicate elt-predicate))))
    (lambda (elt . elts)
      (constructor (list->vector (cons elt elts))))))

(define (vector-dimension v)
  (vector-length (tagged-data-data v)))

(define (vector-elt v i)
  (vector-ref (tagged-data-data v) i))

(define (vector-elts v)
  (vector->list (tagged-data-data v)))

(define (same-dimension? v1 v2)
  (n:= (vector-dimension v1) (vector-dimension v2)))

(define (guarantee-same-dimension v1 v2)
  (if (not (same-dimension? v1 v2))
      (error "Vector dimension mismatch:" v1 v2)))

(define (make-vector-arithmetic elt-arithmetic)
  (make-arithmetic 'vector
                   (make-vector-predicate
                    (arithmetic-domain-predicate elt-arithmetic))
                   (list elt-arithmetic)
    (lambda (name elt-constant)
      elt-constant)
    (lambda (operator elt-operation)
      (let ((handlers (get-vector-handlers operator)))
        (if (n:pair? handlers)
            (extend-operation-function operator elt-operation
              (lambda (elt-function)
                (operation-union*
                 operator
                 (map (lambda (handler)
                        (handler elt-arithmetic elt-function))
                      handlers))))
            elt-operation)))))

(define (get-vector-handlers operator)
  (get-matching-vector-handlers
   (lambda (key)
     (eq? (car key) operator))))

(define get-matching-vector-handlers)
(define set-vector-handler!)
(let ((store (make-alist-store equal?)))
  (set! get-matching-vector-handlers (store 'get-matching))
  (set! set-vector-handler! (store 'put!)))

(define (define-vector-handler operators domain-index
                               codomain-index extra-operators
                               make-handler-procedure)
  (for-each (lambda (operator)
              (set-vector-handler!
               (list operator domain-index codomain-index)
               (make-vector-handler operator
                                    domain-index
                                    codomain-index
                                    extra-operators
                                    make-handler-procedure)))
            operators))

(define (make-vector-handler operator domain-index codomain-index
                             extra-operators
                             make-handler-procedure)
  (let ((selector
         (make-signature-selector (operator-arity operator)
                                  domain-index
                                  codomain-index)))
    (lambda (elt-arithmetic elt-operation)
      ;; TODO(cph): revise this to use operation abstraction.
      (let ((codomain
             (let ((codomain (operation-codomain elt-operation)))
               (if (selector 'codomain)
                   (make-vector-predicate codomain)
                   codomain))))
        (make-simple-operation
         operator
         (map (lambda (select? domain)
                (if select?
                    (make-vector-predicate domain)
                    domain))
              (selector 'domains)
              (operation-domains elt-operation))
         codomain
         (let ((handler-procedure
                (apply make-handler-procedure
                       (map (lambda (operator)
                              (operation-procedure
                               (arithmetic-operation
                                operator elt-arithmetic)))
                            (cons operator extra-operators)))))
           (if (selector 'codomain)
               (let ((tagger (predicate-constructor codomain)))
                 (lambda args
                   (tagger (apply handler-procedure args))))
               handler-procedure)))))))

(define-vector-handler '(+ -) 3 1 '()
  (lambda (elt-function)
    (lambda (v1 v2)
      (v:map2 elt-function v1 v2))))

(define-vector-handler '(negate) 1 1 '()
  (lambda (elt-function)
    (lambda (v)
      (v:map1 elt-function v))))

(define-vector-handler '(*) 1 1 '()
  (lambda (elt-function)
    (lambda (e1 v2)
      (v:map1 (lambda (e2)
                (elt-function e1 e2))
              v2))))

(define-vector-handler '(*) 2 1 '()
  (lambda (elt-function)
    (lambda (v1 e2)
      (v:map1 (lambda (e1)
                (elt-function e1 e2))
              v1))))

(define-vector-handler '(*) 3 0 '(+)
  (lambda (e:* e:+)
    (dot-product-maker e:* e:+)))

(define-vector-handler '(magnitude) 1 0 '(+ * sqrt)
  (lambda (elt-function e:+ e:* e:sqrt)
    (let ((dot-product (dot-product-maker e:* e:+)))
      (lambda (v)
        (e:sqrt (dot-product v v))))))

(define-vector-handler '(=) 3 0 '()
  (lambda (elt-function)
    (lambda (v1 v2)
      (let ((limit (vector-dimension v1)))
        (and (n:= limit (vector-dimension v2))
             (every (lambda (i)
                      (elt-function (vector-elt v1 i)
                                    (vector-elt v2 i)))
                    (iota limit)))))))

(define (v:map1 procedure v)
  (vector-map procedure
              (tagged-data-data v)))

(define (v:map2 procedure v1 v2)
  (guarantee-same-dimension v1 v2)
  (vector-map procedure
              (tagged-data-data v1)
              (tagged-data-data v2)))

(define (dot-product-maker e:* e:+)
  (lambda (v1 v2)
    (let ((v (v:map2 e:* v1 v2)))
      (let ((limit (vector-length v)))
        (do ((i 1 (n:+ i 1))
             (sum (vector-ref v 0)
                  (e:+ sum (vector-ref v i))))
            ((not (n:< i limit)) sum))))))