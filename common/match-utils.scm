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

;;;; Pattern-matching support

;;;; Syntax of variables

;; (<type>)
;; (<type> <name>)
;; (<type> <name> <restriction>)
;; (<type> <restriction>)

(define match:var?
  (simple-generic-procedure 'match:var? 1
    (constant-generic-procedure-handler #f)))

(define (match:var-type var)
  (guarantee match:var? var)
  (car var))

(define (match:var-type? object)
  (memq object match:var-types))

(define match:var-types '(? ??))

(define match:var-name
  (simple-generic-procedure 'match:var-name 1
    (constant-generic-procedure-handler #f)))

(define (match:named-var? object)
  (and (pair? object)
       (match:var-type? (car object))
       (n:>= (length object) 2)
       (symbol? (cadr object))))

;; coderef: method:named-var?
(define-generic-procedure-handler match:var?
  (match-args match:named-var?)
  (constant-generic-procedure-handler #t))

;; coderef: method:named-var-name
(define-generic-procedure-handler match:var-name
  (match-args match:named-var?)
  cadr)

(define (match:element-var? object)
  (and (match:var? object)
       (eq? '? (match:var-type object))))

(define (match:segment-var? object)
  (and (match:var? object)
       (eq? '?? (match:var-type object))))

(define (match:make-var type name)
  (list type name))

(define (match:satisfies-restriction? var value)
  (or (not (match:var-has-restriction? var))
      ((match:var-restriction var) value)))

(define (match:var-has-restriction? var)
  (pair? (cddr var)))

(define (match:var-restriction var)
  (caddr var))

(define (match:vars-equal? v1 v2)
  (and (eqv? (match:var-name v1) (match:var-name v2))
       (eqv? (match:var-type v1) (match:var-type v2))))

;;;; Dictionaries

(define (match:new-dict)
  (list 'dict))

(define (match:dict? object)
  (and (pair? object)
       (eqv? (car object) 'dict)
       (list? (cdr object))
       (every (lambda (binding)
                (and (pair? binding)
                     (symbol? (car binding))
                     (pair? (cdr binding))
                     (pair? (cddr binding))
                     (match:var-type? (caddr binding))
                     (null? (cdddr binding))))
              (cdr object))))

(define (match:bindings dict)
  (cdr dict))

(define (match:new-bindings dict bindings)
  (cons 'dict bindings))

(define (match:make-binding var value)
  (list (match:var-name var)
        value
        (match:var-type var)))

(define (match:map-binding-value procedure binding)
  (list (car binding)
        (procedure (cadr binding))
        (caddr binding)))

(define match:binding-name car)
(define match:binding-type caddr)

(define match:binding-value
  (simple-generic-procedure 'match:binding-value 1 cadr))

(define (match:extend-dict var value dict)
  (match:new-bindings dict
                      (cons (match:make-binding var value)
                            (match:bindings dict))))

(define (match:map-bindings procedure dict)
  (match:new-bindings dict
                      (map procedure
                           (match:bindings dict))))

(define (match:lookup var dict)
  (let ((name
         (if (symbol? var)
             var
             (match:var-name var))))
    (find (lambda (binding)
            (eq? name (match:binding-name binding)))
          (match:bindings dict))))

(define (match:has-binding? var dict)
  (and (match:lookup var dict)
       #t))

(define (match:get-value var dict)
  (let ((binding (match:lookup var dict)))
    (if (not binding)
        (error "Variable not bound:" var))
    (match:binding-value binding)))

(define (match:map-dict-values procedure dict)
  (match:new-bindings
   dict
   (map (lambda (binding)
          (match:map-binding-value procedure binding))
        (match:bindings dict))))

(define (match:all-values dict)
  (reverse (map match:binding-value (match:bindings dict))))

(define (match:dict->procedure dict)
  (lambda (name)
    (if name
        (let ((binding (match:lookup name dict)))
          (and binding
               (match:binding-value binding)))
        (match:bindings dict))))

;;;; Substitution

(define (match:dict-substitution dict)
  (lambda (pattern)
    (match:map-vars (lambda (var get-default)
                      (let ((binding (match:lookup var dict)))
                        (if binding
                            (match:binding-value binding)
                            (get-default))))
                    pattern)))

(define (match:single-substitution var value)
  (lambda (pattern)
    (match:map-vars (lambda (var* get-default)
                      (if (match:vars-equal? var* var)
                          value
                          (get-default)))
                    pattern)))

(define (match:map-vars get-value pattern)
  (let loop ((pattern pattern))
    (cond ((match:element-var? pattern)
           (get-value pattern (lambda () pattern)))
          ((match:segment-var? pattern)
           (if (get-value pattern (lambda () #f))
               (error "Ill-formed pattern:" pattern))
           pattern)
          ((list? pattern)
           (append-map (lambda (sub)
                         (if (match:segment-var? sub)
                             (get-value sub
                                        (lambda () (list sub)))
                             (list (loop sub))))
                       pattern))
          (else pattern))))

(define (match:occurs-in? var pattern)
  (let lp ((pattern pattern))
    (cond ((match:var? pattern) (match:vars-equal? pattern var))
          ((list? pattern) (any lp pattern))
          (else #f))))

(define (match:equivalent-patterns? p1 p2 dict)
  (let ((subst (match:dict-substitution dict)))
    (equal? (subst p1)
            (subst p2))))
