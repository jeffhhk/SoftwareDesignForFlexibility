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

;;;; Predicate registration

(define predicate->tag get-predicate-metadata)

(define (tag-data predicate data)
  ((predicate-constructor predicate) data))

;; Needed by code in common.
(define (register-predicate! predicate name)
  (guarantee procedure? predicate)
  (make-simple-predicate name predicate tagging-strategy:never))

;; Needed by code in common.
(define (register-compound-predicate! joint-predicate operator
                                      components)
  (guarantee procedure? joint-predicate)
  (guarantee have-compound-operator-registrar? operator)
  (guarantee n:list? components)
  (tag->predicate
   ((get-compound-operator-registrar operator)
    joint-predicate
    operator
    (map predicate->tag components))))

(define (make-simple-predicate name data-test tagging-strategy)
  (tag->predicate
   (make-simple-tag name data-test tagging-strategy)))

(define (simple-abstract-predicate name data-test)
  (make-simple-predicate name data-test tagging-strategy:always))

(define have-compound-operator-registrar?)
(define get-compound-operator-registrar)
(define define-compound-operator-registrar)
(let ((store (make-alist-store eq?)))
  (set! have-compound-operator-registrar? (store 'has?))
  (set! get-compound-operator-registrar (store 'get))
  (set! define-compound-operator-registrar (store 'put!)))

(define (standard-compound-tag data-test operator components)
  (make-compound-tag (cons operator (map tag-name components))
                     data-test
                     tagging-strategy:optional
                     operator
                     components))

(define (make-listish-memoizer)
  (simple-list-memoizer eq?
    (lambda (data-test operator tags)
      (declare (ignore data-test operator))
      tags)
    (lambda (data-test operator tags)
      (standard-compound-tag data-test operator tags))))

(define-compound-operator-registrar 'is-list-of
  (make-listish-memoizer))

(define-compound-operator-registrar 'is-non-empty-list-of
  (make-listish-memoizer))

(define-compound-operator-registrar 'is-pair-of
  (make-listish-memoizer))

(define-compound-operator-registrar 'complement
  (make-listish-memoizer))

(define (joinish wrap-constructor)
  (let ((memoizer
         (simple-lset-memoizer eq?
           (lambda (data-test operator tags post-process)
             (declare (ignore data-test operator post-process))
             tags)
           (lambda (data-test operator tags post-process)
             (let ((joint-tag
                    (standard-compound-tag data-test
                                           operator
                                           tags)))
               (post-process joint-tag tags)
               joint-tag)))))
    (lambda (data-test operator tags)
      (let ((tags
             (delete-duplicates
              (append-map
               (lambda (tag)
                 (if (and (compound-tag? tag)
                          (eq? operator
                               (compound-tag-operator tag)))
                     (compound-tag-components tag)
                     (list tag)))
               tags)
              eq?)))
        (if (and (pair? tags) (null? (cdr tags)))
            (car tags)
            (wrap-constructor
             tags
             (lambda (post-process)
               (memoizer data-test operator tags
                         post-process))))))))

(define-compound-operator-registrar 'disjoin
  (joinish
   (lambda (tags continue)
     (or (find top-tag? tags)
         (continue
          (lambda (joint-tag tags)
            (for-each (lambda (tag)
                        (set-tag<=! tag joint-tag))
                      tags)))))))

(define-compound-operator-registrar 'conjoin
  (joinish
   (lambda (tags continue)
     (or (find bottom-tag? tags)
         (continue
          (lambda (joint-tag tags)
            (for-each (lambda (tag)
                        (set-tag<=! joint-tag tag))
                      tags)))))))

;;;; Generic predicate operations

;; Needed by code in common.
(define (predicate-name predicate)
  (tag-name (predicate->tag predicate)))

(define (predicate-constructor predicate)
  (tag-constructor (predicate->tag predicate)))

(define (predicate-accessor predicate)
  (tag-accessor (predicate->tag predicate)))

(define (predicate-supersets predicate)
  (map tag->predicate
       (get-tag-supersets (predicate->tag predicate))))

(define (all-predicate-supersets predicate)
  (map tag->predicate
       (get-all-tag-supersets (predicate->tag predicate))))

(define (predicate<= predicate1 predicate2)
  (tag<= (predicate->tag predicate1)
         (predicate->tag predicate2)))

(define (predicate>= predicate1 predicate2)
  (predicate<= predicate2 predicate1))

(define (predicate= predicate1 predicate2)
  (tag= (predicate->tag predicate1)
        (predicate->tag predicate2)))

(define (set-predicate<=! predicate superset)
  (set-tag<=! (predicate->tag predicate)
              (predicate->tag superset)))

;;;; Simple predicates

(define (simple-predicate? object)
  (and (predicate? object)
       (simple-tag? (predicate->tag object))))

;;;; Compound predicates

(define (compound-predicate? object)
  (and (predicate? object)
       (compound-tag? (predicate->tag object))))

(define (compound-predicate-components predicate)
  (map tag->predicate
       (compound-tag-components (predicate->tag predicate))))

(define (compound-predicate-predicate operator)
  (lambda (object)
    (and (predicate? object)
         (let ((tag (predicate->tag object)))
           (and (compound-tag? tag)
                (eq? operator (compound-tag-operator tag)))))))

(define disjunction? (compound-predicate-predicate 'disjoin))
(define conjunction? (compound-predicate-predicate 'conjoin))

;;;; Parametric predicates

(define (parametric-predicate? object)
  (and (predicate? object)
       (parametric-tag? (predicate->tag object))))

(define (parametric-predicate-template predicate)
  (parametric-tag-template (predicate->tag predicate)))

;;;; Basic tag structure

(define tag?
  (simple-generic-procedure 'tag? 1
    (constant-generic-procedure-handler #f)))

(define get-tag-shared
  (simple-generic-procedure 'get-tag-shared 1 #f))

(define (define-tag-type predicate get-shared)
  (define-generic-procedure-handler tag? (match-args predicate)
    (lambda (object)
      (declare (ignore object))
      #t))
  (define-generic-procedure-handler get-tag-shared
                                    (match-args predicate)
    get-shared))

(define (define-tag-record-printer record-type)
  (define-record-printer record-type
    (lambda (tag) (list (tag-name tag)))))

(define (%invoke-tagging-strategy tagging-strategy name data-test
                                  maker)
  (tagging-strategy
   name
   data-test
   (lambda (predicate constructor accessor)
     (let ((tag
            (maker
             (make-tag-shared name predicate constructor
                              accessor))))
       (set-predicate-metadata! predicate tag)
       tag))))

(define (make-tag-shared name predicate constructor accessor)
  (guarantee procedure? predicate 'make-tag-shared)
  (guarantee procedure? constructor 'make-tag-shared)
  (guarantee procedure? accessor 'make-tag-shared)
  (%make-tag-shared name predicate constructor accessor
                    (make-weak-eq-set)))

(define-record-type <tag-shared>
    (%make-tag-shared name predicate constructor accessor
                      supersets)
    tag-shared?
  (name tag-shared-name)
  (predicate tag-shared-predicate)
  (constructor tag-shared-constructor)
  (accessor tag-shared-accessor)
  (supersets tag-shared-supersets))

(define (make-simple-tag name data-test tagging-strategy)
  (%invoke-tagging-strategy tagging-strategy name data-test
                            %make-simple-tag))

(define-record-type <simple-tag>
    (%make-simple-tag shared)
    simple-tag?
  (shared simple-tag-shared))

(define-tag-type simple-tag? simple-tag-shared)
(define-tag-record-printer <simple-tag>)

(define (make-compound-tag name data-test tagging-strategy
                           operator components)
  (%invoke-tagging-strategy tagging-strategy name data-test
                            (lambda (shared)
                              (%make-compound-tag shared
                                                  operator
                                                  components))))

(define-record-type <compound-tag>
    (%make-compound-tag shared operator components)
    compound-tag?
  (shared compound-tag-shared)
  (operator compound-tag-operator)
  (components compound-tag-components))

(define-tag-type compound-tag? compound-tag-shared)
(define-tag-record-printer <compound-tag>)

(define (make-parametric-tag name data-test tagging-strategy
                             template bindings)
  (%invoke-tagging-strategy tagging-strategy name data-test
                            (lambda (shared)
                              (%make-parametric-tag shared
                                                    template
                                                    bindings))))

(define-record-type <parametric-tag>
    (%make-parametric-tag shared template bindings)
    parametric-tag?
  (shared parametric-tag-shared)
  (template parametric-tag-template)
  (bindings parametric-tag-bindings))

(define-tag-type parametric-tag? parametric-tag-shared)
(define-tag-record-printer <parametric-tag>)

;;;; Generic tag operations

(define (tag-name tag)
  (tag-shared-name (get-tag-shared tag)))

(define (tag->predicate tag)
  (tag-shared-predicate (get-tag-shared tag)))

(define (tag-constructor tag)
  (tag-shared-constructor (get-tag-shared tag)))

(define (tag-accessor tag)
  (tag-shared-accessor (get-tag-shared tag)))

(define (tag-supersets tag)
  (tag-shared-supersets (get-tag-shared tag)))

(define (tags->predicates tags)
  (map tag->predicate tags))

(define (get-tag-supersets tag)
  (((tag-supersets tag) 'get-elements)))

(define (get-all-tag-supersets tag)
  (let loop ((queue (list tag)) (supersets '()))
    (if (pair? queue)
        (let ((tag (car queue))
              (queue (cdr queue)))
          (let ((new-sets
                 (lset-difference eqv?
                                  (get-tag-supersets tag)
                                  supersets)))
            (if (pair? new-sets)
                (loop (append new-sets queue)
                      (append new-sets supersets))
                (loop queue supersets))))
        supersets)))

(define (set-tag<=! tag superset)
  (if (tag>= tag superset)
      (error "Not allowed to create a superset loop:"
             tag superset))
  (if (not (tag<= tag superset))
      (((tag-supersets tag) 'add-element!) superset))
  (hash-table-clear! tag<=-cache))

(define (tag= tag1 tag2)
  (guarantee tag? tag1)
  (guarantee tag? tag2)
  (eqv? tag1 tag2))

(define (tag<= tag1 tag2)
  (guarantee tag? tag1)
  (guarantee tag? tag2)
  (cached-tag<= tag1 tag2))

(define (tag>= tag1 tag2)
  (tag<= tag2 tag1))

(define tag<=-cache
  (make-equal-hash-table))

(define (cached-tag<= tag1 tag2)
  (hash-table-intern! tag<=-cache
                      (cons tag1 tag2)
                      (lambda () (uncached-tag<= tag1 tag2))))

(define (uncached-tag<= tag1 tag2)
  (or (eqv? tag1 tag2)
      (generic-tag<= tag1 tag2)
      (any (lambda (tag)
             (cached-tag<= tag tag2))
           (get-tag-supersets tag1))))

(define (cached-tag>= tag1 tag2)
  (cached-tag<= tag2 tag1))

(define (false-tag<= tag1 tag2) (declare (ignore tag1 tag2)) #f)
(define (true-tag<= tag1 tag2) (declare (ignore tag1 tag2)) #t)

(define (top-tag? object) (eqv? top-tag object))
(define (non-top-tag? object) (not (top-tag? object)))

(define (bottom-tag? object) (eqv? bottom-tag object))
(define (non-bottom-tag? object) (not (bottom-tag? object)))

;; These will be modified below.
(define top-tag #f)
(define bottom-tag #f)

(define generic-tag<=
  (simple-generic-procedure 'generic-tag<= 2 false-tag<=))

(define (define-tag<= predicate1 predicate2 handler)
  (define-generic-procedure-handler generic-tag<=
    (match-args predicate1 predicate2)
    handler))

(define-tag<= bottom-tag? tag? true-tag<=)
(define-tag<= tag? top-tag? true-tag<=)

(define-tag<= non-bottom-tag? bottom-tag? false-tag<=)
(define-tag<= top-tag? non-top-tag? false-tag<=)

(define-tag<= parametric-tag? parametric-tag?
  (lambda (tag1 tag2)
    (and (eqv? (parametric-tag-template tag1)
               (parametric-tag-template tag2))
         (every (lambda (bind1 bind2)
                  (let ((tags1 (parameter-binding-values bind1))
                        (tags2 (parameter-binding-values bind2)))
                    (and (n:= (length tags1) (length tags2))
                         (every (case (parameter-binding-polarity
                                       bind1)
                                  ((+) cached-tag<=)
                                  ((-) cached-tag>=)
                                  (else tag=))
                                tags1
                                tags2))))
                (parametric-tag-bindings tag1)
                (parametric-tag-bindings tag2)))))

(define-tag<= compound-tag? compound-tag?
  (lambda (tag1 tag2)
    (cond ((and (eq? 'disjoin (compound-tag-operator tag1))
                (eq? 'disjoin (compound-tag-operator tag2)))
           (every (lambda (component1)
                    (any (lambda (component2)
                           (tag<= component1 component2))
                         (compound-tag-components tag2)))
                  (compound-tag-components tag1)))
          ;; TODO(cph): add more rules here.
          (else #f))))

;;;; Registrations for this file

;; These must be the first registrations!
(define any-object? (conjoin))
(define no-object? (disjoin))

;; Now that we've got those objects, we can properly set the top
;; and bottom tags.
(set! top-tag (predicate->tag any-object?))
(set! bottom-tag (predicate->tag no-object?))

(register-predicate! predicate? 'predicate)
(register-predicate! simple-predicate? 'simple-predicate)
(register-predicate! compound-predicate? 'compound-predicate)
(register-predicate! parametric-predicate? 'parametric-predicate)
(register-predicate! disjunction? 'disjunction)
(register-predicate! conjunction? 'conjunction)

(set-predicate<=! simple-predicate? predicate?)
(set-predicate<=! compound-predicate? predicate?)
(set-predicate<=! parametric-predicate? predicate?)
(set-predicate<=! disjunction? compound-predicate?)
(set-predicate<=! conjunction? compound-predicate?)

(register-predicate! tag? 'tag)
(register-predicate! simple-tag? 'simple-tag)
(register-predicate! compound-tag? 'compound-tag)
(register-predicate! parametric-tag? 'parametric-tag)

(set-predicate<=! simple-tag? tag?)
(set-predicate<=! compound-tag? tag?)
(set-predicate<=! parametric-tag? tag?)

(register-predicate! tagged-data? 'tagged-data)