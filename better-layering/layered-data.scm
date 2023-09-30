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

;;;; Layered ojects

(define (layered-thing? thing)
  (and (pair? thing)
       (eq? (car thing) 'layered-thing)))

(register-predicate! layered-thing? 'layered-thing)

(define (make-layered-thing layer-map)
  (define (the-thing m)
    (case m
      ((layers-available)
       (layers-available layer-map))
      ((layer-map) layer-map)
      ((has-layer?)
       (lambda (layer-name)
         (layer-entry layer-name layer-map)))
      ((new-layer set-layer-value!)
       (lambda (layer-name new-value)
	 (set! layer-map
	       (new-layer-value layer-map
                                layer-name
				new-value))
	 'ok))
      (else  ; m is a layer name layer-name.
       (let ((entry (layer-entry m layer-map)))
         (cond ((has-layer-value? entry)
                (layer-entry-value entry))
               (else *novalue*))))))
  (list 'layered-thing the-thing))

(define (layered-thing thing)
  (cadr thing))

(define (available-layers thing)
  (if (layered-thing? thing)
      ((layered-thing thing) 'layers-available)
      '(base)))

(define (with-new-layer! thing layer value)
  (if (layered-thing? thing)
      (begin (((layered-thing thing) 'new-layer) layer value)
	     thing)
      (make-layered-thing
       (list (make-layer-entry 'base thing)
             (make-layer-entry layer value)))))

(define (make-layered-result layers results)
  (if (and (null? (cdr layers))
           (eq? (car layers) 'base))
      (car results)
      (make-layered-thing
       (map make-layer-entry layers results))))

(define (layer-value layer-name thing)
  (cond ((layered-thing? thing)
         ((layered-thing thing) layer-name))
	((eq? layer-name 'base) thing)
	(else
         (error "get-layer-value" layer-name thing))))

(define (get-layer-value layer-name thing)
  (cond ((layered-thing? thing)
	 (let ((value ((layered-thing thing) layer-name)))
	   (if (has-layer-value? value)
	       value
	       (error "No value for layer" layer-name thing))))
	((eq? layer-name 'base) thing)
	(else (error "get-layer-value" layer-name thing))))

(define (get-layer-value-or-default layer-name thing)
  (cond ((layered-thing? thing)
	 (let ((value ((layered-thing thing) layer-name)))
	   (cond ((has-layer-value? value) value)
                 (else (get-layer-value-default layer-name)))))
	((eq? layer-name 'base) thing)
	(else (get-layer-value-default layer-name))))

(define (get-layer-value-default layer-name)
  (let ((v (assq layer-name *layer-value-defaults*)))
    (if v
        (cadr v) 
        (error "No value for layer" layer-name thing))))

(define (procedure-layer-or-default layer-name thing)
  (cond ((layered-thing? thing)
	 (let ((value ((layered-thing thing) layer-name)))
	   (cond ((has-layer-value? value) value)
                 (else
                  (layered-procedure-default layer-name)))))
	((eq? layer-name 'base) thing)
	(else
         (error "Not a layered-procedure"
                layer-name thing))))

(define *layer-value-defaults* '())

(define (set-default-layer-value! layer-name
                                  layer-default-value)
  (set! *layer-value-defaults*
        (cons (list layer-name layer-default-value)
              *layer-value-defaults*)))



(define *layered-procedure-defaults* '())

(define (layered-procedure-default layer-name)
  (cond ((assq layer-name *layered-procedure-defaults*)
         => cadr)
        (else
         (lambda (current . args) *ignore-value*))))

(define (declare-layered-procedure-default layer-name handler)
  (cond ((assq layer-name *layered-procedure-defaults*)
         => (lambda (entry)
              (set-car! (cdr entry) handler)))
        (else
         (set! *layered-procedure-defaults*
               (cons (list layer-name handler)
                     *layered-procedure-defaults*)))))

(define (make-base-value value)
  (make-layered-thing
   (list (make-layer-entry 'base value))))

(define (base-value thing)
  (get-layer-value 'base thing))

(define (with-additional-layers layer-map thing)
  (cond ((layered-thing? thing)
	 (for-each
          (lambda (new-entry)
	    (new-layer thing
		       (layer-entry-layer-name new-entry)
		       (layer-entry-value new-entry)))
		   layer-map))
	(else
	 (make-layered-thing
	  (cons (make-layer-entry 'base thing)
		layer-map)))))

(define *novalue* (list '*novalue*))

(define (has-layer-value? entry)
  (not (eq? entry *novalue*)))

;;; For this elementary implementation we will
;;; implement layer maps as association lists.

(define (layers-available layer-map)
  (map layer-entry-layer-name layer-map))

(define (make-layer-entry layer value)
  (list layer value))

(define (layer-entry-layer-name entry)
  (car entry))

(define (layer-entry-value entry)
  (cadr entry))

(define (layer-entry layer-name layer-map)
  (or (assq layer-name layer-map) *novalue*))

(define (new-layer-value layer-map layer-name new-value)
  (let ((entry (layer-entry layer-name layer-map)))
    (cond ((not (eq? entry *novalue*))
	   (set-car! (cdr entry) new-value)
	   layer-map)
	  (else
	   (cons (list layer-name new-value)
		 layer-map)))))


