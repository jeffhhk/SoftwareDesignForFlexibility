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

;;;; Carcinogens for the semicolon part 1: Defining cells

;;; Here be macros that provide syntactic sugar for playing with
;;; the propagator language as embedded in Scheme.  Syntactic
;;; regularities in patterns of definition of cells are captured.

;; (define-cell foo form)
;; is the same as
;; (define foo (ensure-cell foo form))
;; except it grabs the name foo and associates it with the
;; cell that form constructs.
;;
;; For the frequent case when you want a fresh cell
;; (define-cell foo)
;; expands into
;; (define-cell foo (make-cell 'foo))
;; The metadata is then available two ways.

(define-syntax define-cell
  (syntax-rules ()
    ((define-cell symbol form)
     (define symbol
       (ensure-cell 'symbol form)))
    ((define-cell symbol)
     (define symbol
       (make-cell 'symbol)))))

;; (let-cells ((foo foo-form)
;;             (bar bar-form)
;;             (baz baz-form))
;;   stuff)
;; is the same as
;; (let ((foo (ensure-cell foo foo-form))
;;       (bar (ensure-cell foo bar-form))
;;       (baz (ensure-cell foo baz-form)))
;;   stuff)
;; except that it captures the names foo bar and baz and
;; associates them with the cells that the corresponding forms
;; return.
;;
;; For the frequent case when you want fresh cells
;; (let-cells (foo bar baz)
;;   stuff)
;; expands into
;; (let-cells ((foo (make-cell 'foo))
;;             (bar (make-cell 'bar))
;;             (baz (make-cell 'baz)))
;;   stuff)
;; The metadata is then available two ways.

;; The following would suffice for the above:
#;
 (define-syntax let-cells
   (syntax-rules ()
     ((let-cells ((name expr) ...)
        form ...)
      (let ((name (ensure-cell 'name expr)) ...)
        form ...))
     ((let-cells (name ...)
        form ...)
      (let-cells ((name (make-cell 'name))...)
        form ...))))

;; The much more horrible LET-CELLS macro below allows the two
;; use patterns above to mix, as follows,
;; (let-cells ((foo foo-form)
;;             bar
;;             (baz baz-form))
;;   stuff)
;; and have the right thing happen.  It also interprets the
;; slightly more traditional
;; (let-cells ((foo foo-form)
;;             (bar)
;;             (baz baz-form))
;;   stuff)
;; in agreement with Scheme's let.

(define-syntax let-cells
  (syntax-rules ()
    ((let-cells (cell-binding ...)
       form ...)
     (normalize-let-clauses let-cells
       (cell-binding ...)
       ()
       form ...))
    ((let-cells "done"
       ((cell-name cell-form) ...)
       form ...)
     (let ((cell-name cell-form) ...)
       form ...))))

(define-syntax normalize-let-clauses
  (syntax-rules ()
    ((normalize-let-clauses let-form
       ((cell-name cell-form) clause ...)
       (done-clause ...)
       form ...)
     (normalize-let-clauses let-form
       (clause ...)
       ((cell-name (ensure-cell 'cell-name cell-form))
        done-clause ...)
       form ...))
    ((normalize-let-clauses let-form
       ((cell-name) clause ...)
       (done-clause ...)
       form ...)
     (normalize-let-clauses let-form
       (cell-name clause ...)
       (done-clause ...)
       form ...))
    ((normalize-let-clauses let-form
       (cell-name clause ...)
       (done-clause ...)
       form ...)
     (normalize-let-clauses let-form
       (clause ...)
       ((cell-name (make-cell 'cell-name))
        done-clause ...)
       form ...))
    ((normalize-let-clauses let-form
       ()
       done-clauses
       form ...)
     (let-form "done" done-clauses
       form ...))))

;; let-cell is a grammatical convenience if there is only one
;; cell.
;; (let-cell (foo foo-form) stuff) and (let-cell foo stuff) are
;; both ok and equivalent to (let-cells ((foo foo-form)) stuff)
;; and (let-cells (foo) stuff), respectively, but less awkward to
;; read.
(define-syntax let-cell
  (syntax-rules ()
    ((let-cell cell-binding
       form ...)
     (let-cells (cell-binding)
       form ...))))

;; And here is the moral equivalent of let*
(define-syntax let-cells*
  (syntax-rules ()
    ((let-cells* (binding bindings ...)
       form ...)
     (let-cell binding
       (let-cells* (bindings ...)
         form ...)))
    ((let-cells* ()
       form ...)
     (let-cells ()
       form ...))))



;;; Some support

(define (ensure-cell name value)
  (if (cell? value)
      value
      (let ((new-cell (make-cell name)))
        (add-cell-content! new-cell value)
        new-cell)))

(define-syntax define-c:prop
  (sc-macro-transformer
   (lambda (form unev)
     (declare (ignore unev))
     (let ((my-name (caadr form))
           (io-cells (cdadr form))
           (body (cddr form)))
       `(define (,my-name ,@io-cells)
          (constraint-propagator (list ,@io-cells)
            (lambda () ,@body)
            ',my-name))))))
#|
(pp (syntax
     '(define-c:prop
          (c:parallax<->distance parallax distance)
       (let-cells (t)
        (c:tan parallax t)
        (c:* t distance AU)))
     (the-environment)))
#|
(define (c:parallax<->distance parallax distance)
  (constraint-propagator
   (list parallax distance)
   (lambda ()
     (let ((t (make-cell 't)))
       (c:tan parallax t)
       (c:* t distance AU)))
   'c:parallax<->distance))
|#
|#


(define-syntax define-p:prop
  (sc-macro-transformer
   (lambda (form unev)
     (declare (ignore unev))
     (let ((my-name (caadr form))
           (i-cells (cadadr form))
           (o-cells (caddr (cadr form)))
           (body (cddr form)))
       `(define (,my-name ,@i-cells ,@o-cells)
          (compound-propagator (list ,@i-cells)
                               (list ,@o-cells)
            (lambda () ,@body)
            ',my-name))))))



#|
(pp (syntax
     '(define-p:prop (foo (in1 in2) (out1 out2))
        body)
     (the-environment)))
#|
(define (foo in1 in2 out1 out2)
  (compound-propagator
   (list in1 in2)
   (list out1 out2)
   (lambda ()
     body)
   'foo))
|#
|#
