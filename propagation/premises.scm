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

(define (make-premise-metadata name)
  (let ((belief-state 'believed)
        (nogoods '())
        (roots '()))

    (define (get-name)
      name)

    (define (believed?)
      (eq? belief-state 'believed))

    (define (believe!)
      (if (eq? belief-state 'believed)
          #f
          (begin
            (set! belief-state 'believed)
            (if *trace-premise-transitions?*
                (warn "Premise in:" name))
            (wake-up-roots!)
            #t)))

    (define (disbelieve!)
      (if (eq? belief-state 'believed)
          (begin
            (set! belief-state 'not-believed)
            (if *trace-premise-transitions?*
                (warn "Premise out:" name))
            (wake-up-roots!)
            #t)
          #f))

    (define (wake-up-roots!)
      ;; A root is added by the registration of the premise.
      ;; This is either an externally-supplied premise or a
      ;; hypothetical created by an AMB.  So this does not
      ;; alert enough neighbors, for example, neighbors of
      ;; cells that contain values that may depend on the
      ;; premise changing state.

      ;; (for-each (lambda (root)
      ;;             (for-each alert-propagator!
      ;;                       (root 'get-neighbors)))
      ;;           roots)

      ;; Cells whose strongest value changes must 
      ;; alert their neighbors.
      (for-each test-cell-content! (all-cells))

      ;; Any premise change may require changes to 
      ;; choices made by the AMBs, because different
      ;; premise-nogoods may be fully supported.
      (for-each alert-propagator! all-amb-propagators))

    (define (get-roots)
      roots)

    (define (add-root! root)
      (set! roots (lset-adjoin eqv? roots root)))

    (define (get-nogoods)
      nogoods)

    (define (set-nogoods! *nogoods)
      (set! nogoods *nogoods))

    (define (summarize-self)
      (list belief-state))

    (bundle premise-metadata?
            get-name
            believed?
            believe!
            disbelieve!
            get-roots
            add-root!
            get-nogoods
            set-nogoods!
            summarize-self)))

(define premise-metadata?
  (make-bundle-predicate 'premise-metadata))

(define premise-metadata-table
  (make-key-weak-eqv-hash-table))

(define *trace-premise-transitions?* #f)

(define (clear-premises!)
  (hash-table/clear! premise-metadata-table))

(define (premise? premise)
  (hash-table-exists? premise-metadata-table premise))

(define (register-premise! name root)
  (let ((metadata
         (hash-table/intern! premise-metadata-table name
           (lambda () (make-premise-metadata name)))))
    (metadata 'add-root! root)
    name))

(define (%premise-metadata name)
  (hash-table-ref premise-metadata-table name))

(define (premise-in? name)
  ((%premise-metadata name) 'believed?))

(define (premise-out? name)
  (not (premise-in? name)))

(define (mark-premise-in! name)
  ((%premise-metadata name) 'believe!))

(define (mark-premise-out! name)
  ((%premise-metadata name) 'disbelieve!))

(define (all-premises-in? set)
  (support-set-every premise-in? set))

(define (premise-nogoods name)
  ((%premise-metadata name) 'get-nogoods))

(define (set-premise-nogoods! name nogoods)
  ((%premise-metadata name) 'set-nogoods! nogoods))

(define adjoin-support-with-subsumption
  (subsuming-adjoiner support-set= support-set<=))

(define (make-hypotheticals output values)
  (let ((peers
         (parameterize ((*my-parent* output))
           (map (lambda (value)
                  (%make-hypothetical value output))
                values))))
    (for-each (lambda (peer)
                (peer 'set-peers! peers))
              peers)
    peers))

(define (%make-hypothetical value output)
  (let ((relations
         (make-relations (symbol 'hypothetical:
                                 (object->string value))
                         output))
        (peers))

    (define (get-relations)
      relations)

    (define (get-output)
      output)

    (define (get-peers)
      peers)

    (define (set-peers! new-peers)
      (set! peers new-peers))

    (define (summarize-self)
      (path-of me))

    (define me
      (bundle hypothetical?
              get-relations
              get-output
              get-peers
              set-peers!
              summarize-self))

    (add-child! me output)
    (register-premise! me output)
    (add-cell-content! output
                       (supported value
                                  (support-set me)
                                  'hypothesis))
    me))

(define hypothetical?
  (make-bundle-predicate 'hypothetical))
(set-predicate<=! hypothetical? relatable?)