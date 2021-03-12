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

;;; Simple labeled-edge directed graph representation

(define (make-graph-node name)
  (let ((edges '()))

    (define (get-name) name)
    (define (all-edges) (list-copy edges))

    (define (%find-edge label)
      (find (lambda (edge)
              (eqv? label (edge 'get-label)))
            edges))

    (define (has-edge? label)
      (and (%find-edge label) #t))

    (define (get-edge label)
      (let ((edge (%find-edge label)))
        (if (not edge)
            (error "No edge with this label:" label))
        edge))

    (define (edge-value label)
      ((get-edge label) 'get-value))

    (define (connect! label value)
      (if (has-edge? label)
          (error "Two edges with same label:" label))
      (set! edges (cons (make-graph-edge label value) edges)))

    (define (maybe-connect! label value)
      (if (not (default-object? value))
          (connect! label value)))

    (define (summarize-self)
      (if name
          (list name)
          '()))

    (bundle graph-node?
            get-name all-edges has-edge? get-edge edge-value
            connect! maybe-connect! summarize-self)))

(define graph-node?
  (make-bundle-predicate 'graph-node))

(define (make-graph-edge label value)

  (define (get-label) label)

  (define (get-value)
    (if (promise? value)
        (force value)
        value))

  (define (forced?)
    (if (promise? value)
        (promise-forced? value)
        #t))

  (define (summarize-self)
    (cons label
          (if (forced?)
              (list (get-value))
              '())))

  (bundle graph-edge?
          get-label get-value forced? summarize-self))

(define graph-edge?
  (make-bundle-predicate 'graph-edge))

(define (graph-node . plist)
  (let ((node (make-graph-node #f)))
    (for-each (lambda (p)
                (node 'maybe-connect! (car p) (cdr p)))
              (plist->alist plist))
    node))

(define (graph-edge-predicate . edge-labels)
  (define (has-value? arg)
    (and (graph-node? arg)
         (every (lambda (edge-label)
                  (arg 'has-edge? edge-label))
                edge-labels)))
  (register-predicate! has-value?
                       (cons 'edge-predicate edge-labels))
  has-value?)

(define (graph-node-applier node procedure)
  (guarantee graph-node? node 'graph-node-applier)
  (make-entity (lambda (self . args)
                 (declare (ignore self))
                 (apply procedure args))
               node))

(define (graph-node-applier->node procedure)
  (guarantee graph-node-applier? procedure
             'graph-node-applier->node)
  (entity-extra procedure))

(define (graph-node-applier? object)
  (and (entity? object)
       (graph-node? (entity-extra object))))
(register-predicate! graph-node-applier? 'graph-node-applier)

(define (pg object)
  (let ((show-edges
         (lambda (node)
           (for-each (lambda (edge)
                       (n:pretty-print
                        (list (edge 'get-label)
                              (edge 'get-value)))
                       (newline))
                     (node 'all-edges)))))
    (cond ((graph-node-applier? object)
           (fresh-line)
           (n:pretty-print object)
           (newline)
           (show-edges (graph-node-applier->node object)))
          ((graph-node? object)
           (fresh-line)
           (n:pretty-print object)
           (newline)
           (show-edges object))
          ((graph-edge? object)
           (fresh-line)
           (n:pretty-print object)
           (newline)
           (n:pretty-print (list 'label (object 'get-label)))
           (newline)
           (n:pretty-print (list 'value (object 'get-value)))
           (newline))
          (else
           (pp object)))))

;;; Currently not being used.
(define (disjoint-union-of-graph-nodes node-name node1 node2)
  (let ((labels1
         (map (lambda (edge) (edge 'get-label))
              (node1 'all-edges)))
        (labels2
         (map (lambda (edge) (edge 'get-label))
              (node2 'all-edges))))
    (let ((intersection (lset-intersection eq? labels1 labels2)))
      (if (not (null? intersection))
          (error "graph nodes can't be combined:"
                 intersection))))
  (let ((node (make-graph-node node-name)))
    (for-each (lambda (edge)
                (node 'connect!
                      (edge 'get-label)
                      (edge 'get-value)))
              (node1 'all-edges))
    (for-each (lambda (edge)
                (node 'connect!
                      (edge 'get-label)
                      (edge 'get-value)))
              (node2 'all-edges))
    node))

;;;; Graph views

;;; A graph view is a reversible mapping from one edge label to
;;; another.  When a graph is projected into a view, it appears
;;; to have all of its edge labels changed as specified by the
;;; view.

(define-record-type <graph-view>
    (make-graph-view name forward backward)
    graph-view?
  (name graph-view-name)
  (forward graph-view-forward)
  (backward graph-view-backward))

(define (invert-graph-view view)
  (make-graph-view (list 'inverse (graph-view-name view))
                   (graph-view-backward view)
                   (graph-view-forward view)))

(define-record-printer <graph-view>
  (lambda (view)
    (list (graph-view-name view))))

(define (graph-edge-view delegate view)

  (define (get-view) view)

  (define (get-label)
    ((graph-view-forward view) (delegate 'get-label)))

  (define (get-value)
    (let ((value (delegate 'get-value)))
      (if (graph-node? value)
          (graph-node-view value view)
          value)))

  (define (forced?)
    (delegate 'forced?))

  (define (summarize-self)
    (cons (get-label)
          (if (forced?)
              (list (get-value))
              '())))

  (guarantee graph-edge? delegate)
  (bundle graph-edge?
          get-label get-value forced?
          get-view summarize-self))

(define (graph-node-view delegate view)

  (define (get-view) view)
  (define (get-name) (delegate 'get-name))

  (define (all-edges)
    (map (lambda (edge)
           (graph-edge-view edge view))
         (delegate 'all-edges)))

  (define (has-edge? label)
    (delegate 'has-edge? ((graph-view-backward view) label)))

  (define (get-edge label)
    (graph-edge-view
     (delegate 'get-edge ((graph-view-backward view) label))
     view))

  (define (edge-value label)
    ((get-edge label) 'get-value))

  (define (connect! label value)
    (delegate 'connect!
              ((graph-view-backward view) label)
              (if (graph-node? value)
                  (graph-node-view value
                                   (invert-graph-view view))
                  value)))

    (define (maybe-connect! label value)
      (if (not (default-object? value))
          (connect! label value)))

  (define (summarize-self)
    (delegate 'summarize-self))

  (guarantee graph-node? delegate)
  (bundle graph-node?
          get-name all-edges has-edge? get-edge connect!
          maybe-connect! edge-value get-view
          summarize-self))