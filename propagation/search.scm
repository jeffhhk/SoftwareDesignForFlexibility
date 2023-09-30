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

(define (binary-amb cell)
  (let ((premises (make-hypotheticals cell '(#t #f))))
    (let ((true-premise (car premises))
          (false-premise (cadr premises)))

      ;; coderef: amb-choose
      (define (amb-choose)
        (let ((reasons-against-true
               (filter all-premises-in?
                 (premise-nogoods true-premise)))
              (reasons-against-false
               (filter all-premises-in?
                 (premise-nogoods false-premise))))
          (cond ((null? reasons-against-true)
                 (mark-premise-in! true-premise)
                 (mark-premise-out! false-premise))
                ((null? reasons-against-false)
                 (mark-premise-out! true-premise)
                 (mark-premise-in! false-premise))
                (else                  ; this amb must fail.
                 (mark-premise-out! true-premise)
                 (mark-premise-out! false-premise)
                 (process-contradictions
                    (pairwise-union reasons-against-true
                                    reasons-against-false)
                    cell)))))
      ;; The cell is a spiritual neighbor...
      (let ((me
             (propagator (list cell) (list cell)
                         amb-choose
                         'binary-amb)))
        (set! all-amb-propagators
              (cons me all-amb-propagators))
        me))))

#|
(define (binary-amb cell)
  (p:amb cell '(#t #f)))
|#

(define (p:amb cell values)
  (let ((premises (make-hypotheticals cell values)))

    (define (amb-choose)
      (let ((to-choose
             (find (lambda (premise)
                     (not (any all-premises-in?
                               (premise-nogoods premise))))
                   premises)))
        (if to-choose
            (for-each (lambda (premise)
                        (if (eq? premise to-choose)
                            (mark-premise-in! premise)
                            (mark-premise-out! premise)))
                      premises)
            (let ((nogoods
                   (cross-product-union
                    (map (lambda (premise)
                           (filter all-premises-in?
                             (premise-nogoods premise)))
                         premises))))
              (for-each mark-premise-out! premises)
              (process-contradictions nogoods cell)))))
    (let ((me
           (propagator (list cell)
                       (list cell)
                       amb-choose
                       'amb)))
      (set! all-amb-propagators
            (cons me all-amb-propagators))
      me)))

(define (cross-product-union nogoodss)
  (reduce-right pairwise-union
                '()
                nogoodss))

(define (pairwise-union nogoods1 nogoods2)
  (append-map (lambda (nogood1)
                (map (lambda (nogood2)
                       (support-set-union nogood1
                                          nogood2))
                     nogoods2))
              nogoods1))

(define (process-contradictions nogoods complaining-cell)
  (update-failure-count!)
  (for-each save-nogood! nogoods)
  (let-values (((to-disbelieve nogood)
                (choose-premise-to-disbelieve nogoods)))
    (maybe-kick-out to-disbelieve nogood complaining-cell)))

(define (choose-first-premise-in-strong-nogood nogoods)
  (choose-first-hypothetical
   (car (sort-by nogoods
          (lambda (nogood)
            (count hypothetical?
                   (support-set-elements nogood)))))))

(define (choose-last-premise-in-strong-nogood nogoods)
  (choose-first-hypothetical
   (last (sort-by nogoods
          (lambda (nogood)
            (count hypothetical?
                   (support-set-elements nogood)))))))

(define choose-premise-to-disbelieve
  choose-first-premise-in-strong-nogood)

(define (choose-first-hypothetical nogood)
  (let ((hyps (support-set-filter hypothetical? nogood)))
    (values (and (not (support-set-empty? hyps))
                 (car (support-set-elements hyps)))
            nogood)))

;;; Called from cell in cell.scm -- GJS

(define (handle-cell-contradiction cell)
  (if *trace-cell-contradictions*
      (warn "Contradiction:" cell))
  (let ((nogood
         (support-layer-value (cell-strongest cell))))
    (if (support-set-empty? nogood)
        (error "Contradiction in cell must be supported:"
               cell))
    (process-contradictions (list nogood) cell)
    ;; Elide tail recursion for debugging.
    ;;'ok
    ))

(define *trace-cell-contradictions* #f)
(define *number-of-calls-to-fail* 0)
(define *debugging-contradiction* #f)

(define (update-failure-count!)
  (set! *number-of-calls-to-fail*
        (n:+ *number-of-calls-to-fail* 1)))

(define (save-nogood! nogood)
  (for-each (lambda (premise)
              (set-premise-nogoods! premise
                (adjoin-support-with-subsumption
                 (support-set-remove nogood premise)
                 (premise-nogoods premise))))
            (support-set-elements nogood)))

(define (maybe-kick-out to-disbelieve nogood cell)
  (if to-disbelieve
      (mark-premise-out! to-disbelieve)
      (begin
        (if *debugging-contradiction*
            (bkpt "contradiction" cell nogood))
        (abort-process (list 'contradiction cell)))))

(define (require cell)
  (add-cell-content! cell (supported #t (support-set))))

(define (abhor cell)
  (add-cell-content! cell (supported #f (support-set))))

;;; Experiments of different premise-choice strategies.

(define (choose-random-premise-from-strong-nogood nogoods)
  (choose-any-hypothetical
   (car (sort-by nogoods
          (lambda (nogood)
            (count hypothetical?
                   (support-set-elements nogood)))))))

(define (choose-any-hypothetical nogood)
  (let ((hyps (support-set-filter hypothetical? nogood)))
    (values (and (not (support-set-empty? hyps))
                 (choose-random-element
                  (support-set-elements hyps)))
            nogood)))

(define (choose-strongest-and-most-common-premise nogoods)
  (let ((counts
         (map (lambda (nogood)
                (count hypothetical?
                       (support-set-elements nogood)))
              nogoods)))
    (let ((min-count (apply n:min counts)))
      (if (n:= min-count 0)
          (values #f (car nogoods))
          (let ((candidates
                 (filter-map (lambda (count nogood)
                               (and (n:= count min-count)
                                    nogood))
                             counts
                             nogoods)))
            (values (choose-most-common-item
                     (make-histogram
                      (map (lambda (nogood)
                             (filter hypothetical?
                               (support-set-elements nogood)))
                           candidates)))
                    (car candidates)))))))

(define (choose-strongest-and-least-common-premise nogoods)
  (let ((counts
         (map (lambda (nogood)
                (count hypothetical?
                       (support-set-elements nogood)))
              nogoods)))
    (let ((min-count (apply n:min counts)))
      (if (n:= min-count 0)
          (values #f (car nogoods))
          (let ((candidates
                 (filter-map (lambda (count nogood)
                               (and (n:= count min-count)
                                    nogood))
                             counts
                             nogoods)))
            (values (choose-least-common-item
                     (make-histogram
                      (map (lambda (nogood)
                             (filter hypothetical?
                               (support-set-elements nogood)))
                           candidates)))
                    (car candidates)))))))

(define (choose-most-common-premise nogoods)
  (values (choose-most-common-item
           (make-histogram
            (map (lambda (nogood)
                   (filter hypothetical?
                           (support-set-elements nogood)))
                 nogoods)))
          (car nogoods)))

(define (choose-least-common-premise nogoods)
  (values (choose-least-common-item
           (make-histogram
            (map (lambda (nogood)
                   (filter hypothetical?
                           (support-set-elements nogood)))
                 nogoods)))
          (car nogoods)))

(define (choose-random-premise nogoods)
  (let ((candidate
         (choose-random-element
          (filter (lambda (nogood)
                    (any hypothetical?
                         (support-set-elements nogood)))
                  nogoods))))
    (values (choose-random-element
             (filter hypothetical?
                     (support-set-elements candidate)))
            candidate)))

(define (choose-random-element items)
  (list-ref items (random (length items))))

(define (make-histogram lists)
  (let ((counts '()))
    (for-each (lambda (list)
                (for-each (lambda (item)
                            (let ((p (assq item counts)))
                              (if p
                                  (set-cdr! p (+ (cdr p) 1))
                                  (set! counts
                                        (cons (cons item 1)
                                              counts)))))
                          list))
              lists)
    counts))

(define (choose-most-common-item histogram)
  (let ((winner (cons #f 0)))
    (for-each (lambda (p)
                (if (> (cdr p) (cdr winner))
                    (set! winner p)))
              histogram)
    (car winner)))

(define (choose-least-common-item histogram)
  (let ((winner (cons #f 100000)))
    (for-each (lambda (p)
                (if (< (cdr p) (cdr winner))
                    (set! winner p)))
              histogram)
    (car winner)))
