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

;;;; Layers

(define (make-annotation-layer name constructor)

  (define (get-name)
    name)

  (define (has-value? object)
    (and (layered-datum? object)
         (object 'has-layer? layer)))

  (define (get-value object)
    (if (has-value? object)
        (object 'get-layer-value layer)
        (layer 'get-default-value)))

  (define layer
    (constructor get-name has-value? get-value))

  layer)

(define layer?
  (make-bundle-predicate 'layer))

(define base-layer
  (let ()

    (define (get-name)
      'base)

    (define (has-value? object)
      (declare (ignore object))
      #t)

    (define (get-value object)
      (if (layered-datum? object)
          (object 'get-layer-value base-layer)
          object))

    (define (summarize-self)
      (list (get-name)))

    (bundle layer?
            get-name has-value? get-value summarize-self)))

(define (layer-accessor layer)
  (lambda (object)
    (layer 'get-value object)))

(define base-layer-value
  (layer-accessor base-layer))

;;;; Layered data

(define (layered-datum base-value . plist)
  (make-layered-datum base-value (plist->alist plist)))

(define (make-layered-datum base-value alist)
  (guarantee layer-alist? alist 'make-layered-datum)
  (if (null? alist)
      base-value
      (let ((alist
             (cons (cons base-layer base-value)
                   alist)))

        (define (has-layer? layer)
          (and (assv layer alist) #t))

        (define (get-layer-value layer)
          (cdr (assv layer alist)))

        (define (annotation-layers)
          (map car (cdr alist)))

        (define (summarize-self)
          (list base-value))

        (define (describe-self)
          (map (lambda (p)
                 (list (symbol ((car p) 'get-name) '-layer)
                       (cdr p)))
               alist))

        (bundle layered-datum?
                has-layer? get-layer-value annotation-layers
                summarize-self describe-self))))

(define layered-datum?
  (make-bundle-predicate 'layered-datum))

(define (layer-alist? object)
  (and (n:list? object)
       (every (lambda (p)
                (and (n:pair? p)
                     (layer? (car p))))
              object)))
(register-predicate! layer-alist? 'layer-alist)

;;;; Layered procedures

(define layered-procedure?)
(define layered-procedure-metadata)
(define set-layered-procedure-metadata!)
(let ((association (make-metadata-association)))
  (set! layered-procedure? (association 'has?))
  (set! layered-procedure-metadata (association 'get))
  (set! set-layered-procedure-metadata! (association 'put!)))

(define (make-layered-procedure name arity base-procedure)
  (let* ((metadata
          (make-layered-metadata name arity base-procedure))
         (procedure
          (layered-procedure-dispatcher metadata)))
    (set-layered-procedure-metadata! procedure metadata)
    procedure))

(define (define-layered-procedure-handler layered layer handler)
  (guarantee layer? layer)
  (guarantee procedure? handler)
  ((layered-procedure-metadata layered)
   'set-handler! layer handler))

(define (layered-procedure-dispatcher metadata)
  (let ((base-procedure (metadata 'get-base-procedure)))
    (define (the-layered-procedure . args)
      (let ((base-value
             (apply base-procedure
                    (map base-layer-value args)))
            (annotation-layers
             (apply lset-union eqv?
                    (map (lambda (arg)
                           (if (layered-datum? arg)
                               (arg 'annotation-layers)
                               '()))
                         args))))
        (make-layered-datum base-value
          (filter-map                   ; this drops #f values
           (lambda (layer)
             (let ((handler (metadata 'get-handler layer)))
               (and handler
                    (cons layer
                          (apply handler base-value args)))))
           annotation-layers))))
    the-layered-procedure))

(define (make-layered-metadata name arity base-procedure)
  (let ((handlers (make-weak-alist-store eqv?)))

    (define (get-name) name)
    (define (get-arity) arity)
    (define (get-base-procedure) base-procedure)

    (define has? (handlers 'has?))
    (define get (handlers 'get))
    (define set-handler! (handlers 'put!))

    (define (get-handler layer)
      (if (has? layer)
          (get layer)
          (layer 'get-procedure name arity)))

    (bundle layered-metadata?
            get-name get-arity get-base-procedure
            get-handler set-handler!)))

(define layered-metadata?
  (make-bundle-predicate 'layered-metadata))

(define (layered-extender base-arith)
  (let ((base-pred
         (conjoin (arithmetic-domain-predicate base-arith)
                  (complement layered-datum?))))
    (make-arithmetic (list 'layered
                           (arithmetic-name base-arith))
                     layered-datum?
                     (list base-arith)

      (lambda (name base-value)
        (declare (ignore name))
        base-value)

      (lambda (operator base-operation)
        (make-operation operator
          (any-arg (operator-arity operator)
                   layered-datum?
                   base-pred)
          (make-layered-procedure operator
            (operator-arity operator)
            (operation-procedure base-operation)))))))