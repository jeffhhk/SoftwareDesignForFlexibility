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

;;;; Generic procedures

(define generic-procedure?)
(define %generic-procedure-metadata)
(define set-generic-procedure-metadata!)
(let ((association (make-metadata-association)))
  (set! generic-procedure? (association 'has?))
  (set! %generic-procedure-metadata (association 'get))
  (set! set-generic-procedure-metadata! (association 'put!)))

(define (generic-procedure-constructor dispatch-store-maker)
  (lambda (name arity default-handler)
    (guarantee n:exact-nonnegative-integer? arity)
    (let ((metadata
           (make-generic-metadata name
                                  arity
                                  (dispatch-store-maker)
             (or default-handler
                 (error-generic-procedure-handler name)))))
      (define (the-generic-procedure . args)
        (generic-procedure-dispatch metadata args))
      (set-generic-procedure-metadata! the-generic-procedure
                                       metadata)
      the-generic-procedure)))

(define (generic-procedure-dispatch metadata args)
  (let ((handler (get-generic-procedure-handler metadata args)))
    (if (trace-generic-dispatch?)
        (trace-generic-dispatch metadata args handler))
    (apply handler args)))

(define (constant-generic-procedure-handler constant)
  (hash-table-intern! %constant-generic-procedure-handlers
                      constant
                      (lambda ()
                        (lambda args
                          (declare (ignore args))
                          constant))))

(define %constant-generic-procedure-handlers
  (make-key-weak-eqv-hash-table))

(define (error-generic-procedure-handler name)
  (lambda args
    (error "Inapplicable generic procedure:" name args)))

(define (trace-generic-dispatch metadata args handler)
  (parameterize ((trace-generic-dispatch? #f))
    (let ((port (trace-output-port)))
      (fresh-line port)
      (display ";Calling method of " port)
      (display (generic-metadata-name metadata) port)
      (display ": " port)
      (write handler port)
      (for-each (lambda (arg)
                  (display " " port)
                  (write arg port))
                args)
      (newline port))))

(define trace-generic-dispatch?
  (make-parameter #f))

(define (get-generic-procedure-handler metadata args)
  (or ((generic-metadata-getter metadata) args)
      ((generic-metadata-default-getter metadata))))

(define (define-generic-procedure-handler generic-procedure
                                          applicability
                                          handler)
  (((generic-metadata-dispatch-store
     (generic-procedure-metadata generic-procedure))
    'add-handler!)
   applicability
   handler))

(define (generic-procedure-name proc)
  (generic-metadata-name (generic-procedure-metadata proc)))

(define (generic-procedure-arity proc)
  (generic-metadata-arity (generic-procedure-metadata proc)))

(define (generic-procedure-rules proc)
  (((generic-metadata-dispatch-store
     (generic-procedure-metadata proc))
    'get-rules)))

(define (generic-procedure-handlers proc)
  (map cdr (generic-procedure-rules proc)))

(define (generic-procedure-metadata object)
  (define (try-object candidate)
    (if (generic-procedure? candidate)
        (%generic-procedure-metadata candidate)
        (let loop ((extractors generic-procedure-extractors))
          (if (not (pair? extractors))
              (error:not-a generic-procedure? candidate))
          (let ((val ((cdar extractors) candidate)))
            (if val
                (try-object val)
                (loop (cdr extractors)))))))
  (try-object object))

(define (define-generic-procedure-extractor name extractor)
  (let ((p (assq name generic-procedure-extractors)))
    (if p
        (set-cdr! p extractor)
        (let ((p (cons name extractor)))
          (set! generic-procedure-extractors
                (cons p generic-procedure-extractors))))))

(define generic-procedure-extractors '())

(define (assign-handler! procedure handler . preds)
  (define-generic-procedure-handler procedure
    (apply match-args preds)
    handler))

;;;; Metadata

(define-record-type <generic-metadata>
    (%make-generic-metadata name arity dispatcher getter
                            default-getter)
    generic-metadata?
  (name generic-metadata-name)
  (arity generic-metadata-arity)
  (dispatcher generic-metadata-dispatch-store)
  (getter generic-metadata-getter)
  (default-getter generic-metadata-default-getter))

(define (make-generic-metadata name arity dispatcher
                               default-handler)
  ((dispatcher 'set-default-handler!) default-handler)
  (%make-generic-metadata name
                          arity
                          dispatcher
                          (dispatcher 'get-handler)
                          (dispatcher 'get-default-handler)))

;;;; Dispatcher implementations

(define (make-simple-dispatch-store)
  (let ((rules '())
        (default-handler #f))

    (define (get-handler args)
      (let ((rule
             (find (lambda (rule)
                     (predicates-match? (car rule) args))
                   rules)))
        (and rule
             (cdr rule))))

    (define (add-handler! applicability handler)
      (for-each (lambda (predicates)
                  (let ((p (assoc predicates rules)))
                    (if p
                        (set-cdr! p handler)
                        (set! rules
                              (cons (cons predicates handler)
                                    rules)))))
                applicability))

    (define (get-default-handler)
      default-handler)

    (define (set-default-handler! handler)
      (set! default-handler handler))

    (lambda (message)
      (case message
        ((get-handler) get-handler)
        ((add-handler!) add-handler!)
        ((get-default-handler) get-default-handler)
        ((set-default-handler!) set-default-handler!)
        ((get-rules) (lambda () rules))
        (else (error "Unknown message:" message))))))

(define (make-trie-dispatch-store)
  (let ((delegate (make-simple-dispatch-store))
        (trie (make-trie)))

    (define (get-handler args)
      (get-a-value trie args))

    (define (add-handler! applicability handler)
      ((delegate 'add-handler!) applicability handler)
      (for-each (lambda (path)
                  (set-path-value! trie path handler))
                applicability))

    (lambda (message)
      (case message
        ((get-handler) get-handler)
        ((add-handler!) add-handler!)
        (else (delegate message))))))

(define (cache-wrapped-dispatch-store dispatch-store get-key)
  (let ((get-handler
         (simple-list-memoizer eqv?
                               (lambda (args) (map get-key args))
                               (dispatch-store 'get-handler))))
    (lambda (message)
      (case message
        ((get-handler) get-handler)
        (else (dispatch-store message))))))

(define simple-generic-procedure
  (generic-procedure-constructor make-simple-dispatch-store))

;;; API
(define make-default-dispatch-store
  make-simple-dispatch-store)

;;; Extensible equality predicate.
(define equal*?
  (simple-generic-procedure 'equal*? 2 equal?))

(define equal*-predicate
  (equality-predicate-maker 'equal*? equal*?))