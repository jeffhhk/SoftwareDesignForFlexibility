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

;;;; Reasons

(define (absolute-reason name)
  (guarantee symbol? name 'absolute-reason)
  name)

(define (reason-is-absolute? reason)
  (symbol? reason))

(define (propagation-reason args)
  (guarantee list? args 'propagation-reason)
  (list (current-reason-source) args))

(define (reason-is-propagation? reason)
  (and (list? reason)
       (n:= 2 (length reason))
       (propagator? (car reason))
       (list? (cadr reason))))

(define (merge-reason args)
  (guarantee list? args 'merge-reason)
  (list (current-reason-source) args 'merge))

(define (reason-is-merge? reason)
  (and (list? reason)
       (n:= 3 (length reason))
       (or (not (car reason))
           (relatable? (car reason)))
       (list? (cadr reason))
       (eq? 'merge (caddr reason))))

(define (reason-source reason) (car reason))
(define (reason-args reason) (cadr reason))

(define (unknown-reason) (absolute-reason 'unknown))
(define (constant-reason) (absolute-reason 'constant))
(define (external-reason) (absolute-reason 'i-told-you-so))

(define current-reason-source
  (make-parameter #f))

;;;; Reason layer

(define reason-layer
  (make-annotation-layer 'reason
    (lambda (get-name has-value? get-value)

      (define (get-default-value)
        (unknown-reason))

      (define (get-procedure name arity)
        (declare (ignore arity))
        (lambda (base-value . args)
          (declare (ignore base-value))
          (propagation-reason args)))

      (define (summarize-self)
        (list (get-name)))

      (bundle layer?
              get-name has-value? get-value get-default-value
              get-procedure summarize-self))))

(define reason-layer-value
  (layer-accessor reason-layer))

(define (get-reason object)
  (reason-layer-value (strongest-value object)))

(define-layered-procedure-handler merge-layered reason-layer
  (lambda (merged-value content increment)
    (cond ((equivalent? merged-value (base-layer-value content))
           (reason-layer-value content))
          ((equivalent? merged-value
                        (base-layer-value increment))
           (reason-layer-value increment))
          (else
           (merge-reason (list content increment))))))

(define-layered-procedure-handler merge-metadata-layered
  reason-layer
  (lambda (merged-value content increment)
    (declare (ignore merged-value))
    (merge-reason (list content increment))))

;;;; Decoding reasons for UI

(define (decode-reason reason
                       get-base-value
                       get-value-description)
  (cond ((reason-is-absolute? reason)
         (list reason))
        ((reason-is-propagation? reason)
         `((,(path-of (reason-source reason))
            ,@(map (lambda (value cell)
                     (list (get-name cell)
                           (get-base-value value)))
                   (reason-args reason)
                   (propagator-inputs (reason-source reason))))))
        ((reason-is-merge? reason)
         (if show-reason-merges?
             (show-reason-merges reason
                                 get-base-value
                                 get-value-description)
             (map get-value-description
                  (remove nothing?
                          (elide-reason-merges reason)))))
        (else
         (error "Unknown reason:" reason))))

(define show-reason-merges? #f)

(define (show-reason-merges reason
                            get-base-value
                            get-value-description)
  (let ((source (reason-source reason))
        (args (remove nothing? (reason-args reason))))
    (case (length args)
      ((0) 'unknown)
      ((1)
       (decode-reason (get-reason (car args))
                      get-base-value
                      get-value-description))
      (else
       `((merge
          ,(if source
               (path-of source)
               'no-source)
          ,@(if (propagator? source)
                (map (lambda (value cell)
                       (list (get-name cell)
                             (get-value-description value)))
                     args
                     (propagator-inputs source))
                (map get-value-description args))))))))

(define (elide-reason-merges reason)
  (apply lset-union
         equivalent?
         (map (lambda (arg)
                (let ((reason* (get-reason arg)))
                  (if (reason-is-merge? reason*)
                      (elide-reason-merges reason*)
                      (list arg))))
              (reason-args reason))))