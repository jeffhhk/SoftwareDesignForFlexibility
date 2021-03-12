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

(define (what-is-in cell)
  (write-labeled "cell" (path-of cell))
  (what-is-this (cell-content cell)))

(define (write-labeled label . objects)
  (fresh-line)
  (display ";; ")
  (display label)
  (display ":")
  (for-each (lambda (object)
              (display " ")
              (write object))
            objects)
  (newline))

(define (what-is-this content)
  (let ((elements
         (if (value-set? content)
             (value-set-elements content)
             (list content)))
        (strongest (strongest-value content)))
    (if (or (> (length elements) 1)
            (not (equivalent? (car elements) strongest)))
        (begin
          (for-each print-supported elements)
          (write-labeled "strongest value")
          (print-supported strongest))
        (print-supported (car elements)))))

(define (print-supported supported)
  (cpp (supported-value-description supported)))

(define (supported-value-description supported)
  (if (layered-datum? supported)
      (let ((support (support-layer-value supported))
            (reason (reason-layer-value supported)))
        `(,(if (all-premises-in? support)
               'in
               'out)
          ,(get-base-value supported)
          ,(map path-of (support-set-elements support))
          ,@(decode-reason reason
                           get-base-value
                           supported-value-description)))
      supported))

;;; This is required because (run) returns old value if there is
;;; nothing to do.  This is a problem if a contradiction is
;;; resolved by a kick-out! with no propagation.

(define (tell! cell information . premises)
  (guarantee cell? cell 'tell!)
  (for-each (lambda (premise)
              (register-premise! premise cell))
            premises)
  (set! *last-value-of-run* 'done)
  (add-cell-content! cell
    (if (null? premises)
        information
        (supported information
                   (make-support-set premises)
                   'i-told-you-so)))
  (print-run-result (run)))

(define (retract! premise)
  (guarantee premise? premise 'retract!)
  (set! *last-value-of-run* 'done)
  (kick-out! premise)
  (print-run-result (run)))

(define (kick-out! premise)
  (mark-premise-out! premise))

(define (assert! premise)
  (guarantee premise? premise 'assert!)
  (set! *last-value-of-run* 'done)
  (bring-in! premise)
  (print-run-result (run)))

(define (bring-in! premise)
  (mark-premise-in! premise))

(define (print-run-result result)
  (cond ((eq? 'done result))
        ((and (list? result)
              (n:= 2 (length result))
              (eq? 'contradiction (car result))
              (cell? (cadr result)))
         (write-labeled "contradiction")
         (inquire-internal (cadr result)))
        (else
         (cpp result))))

(define (get-value-in cell)
  (get-base-value (cell-content cell)))

(define (get-premises cell)
  (get-support (cell-content cell)))

(define (inquire cell)
  (guarantee cell? cell 'inquire)
  (let ((v (run)))
    (if (not (eq? v 'done))
        (cpp v)))
  (inquire-internal cell))

(define (inquire-internal cell)
  (let ((value (cell-strongest cell)))
    (parameterize ((param:flonum-printer-cutoff
                    '(relative 5 scientific)))
      (cpp
       `(,(path-of cell)
         ,@(inquire-value-description value))))))

(define (inquire-value-description value)
  (let ((value (strongest-value value)))
    `((has-value ,(presentation-base-value value))
      ,@(let ((support (support-layer-value value)))
          (if (support-set-empty? support)
              '()
              `((depends-on
                 ,@(map path-of
                        (support-set-elements support))))))
      ,@(let ((descriptions
               (decode-reason
                (reason-layer-value value)
                presentation-base-value
                inquire-value-description)))
          (if (null? descriptions)
              '()
              `((because ,@descriptions)))))))

(define presentation-value
  (simple-generic-procedure 'presentation-value 1
    (lambda (x) x)))

(define (presentation-base-value object)
  (presentation-value (get-base-value object)))

(define (supported value support #!optional reason)
  (guarantee support-set? support 'supported)
  (layered-datum value
                 support-layer support
                 reason-layer reason))

(define (force-failure! cells)
  (let ((nogood
         (reduce support-set-union
                 (support-set)
                 (map (lambda (cell)
                        (support-layer-value
                         (cell-strongest cell)))
                      cells))))
    (process-contradictions (list nogood) 'user)
    (run)))


;;; If there is more than one result possible:

(define (all-results cells values-receiver)
  (let lp ((result (run)))
    (if (and (pair? result)
             (eq? 'contradiction (car result)))
        'no-more
        (begin (values-receiver
                (map cell-strongest cells))
               (lp (force-failure! cells))))))

#|
;;; For example

(initialize-scheduler)

(all-results (pythagorean-1)
             (lambda (strongest-contents)
               (write *number-of-calls-to-fail*)
               (display "  ")
               (pp (map get-base-value strongest-contents))))
124  (3 4 5)
143  (4 3 5)
;Value: no-more

*number-of-calls-to-fail*
;Value: 184
|#

;;; A default setup to run the propagator system seems appropriate.

(define (setup-propagator-system arithmetic)
  (define layered-arith
    (extend-arithmetic layered-extender arithmetic))
  (install-arithmetic! layered-arith)
  (install-core-propagators! merge-value-sets
                             layered-arith
                             layered-propagator-projector))

;; coderef: setup-propagators-numeric
(setup-propagator-system numeric-arithmetic)
