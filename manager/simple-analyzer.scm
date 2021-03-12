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

;;;; A simple bound/free variable analyzer.

(define (analyze-file filename environment)
  (close-top-level (read-and-analyze-file filename environment)
                   system-global-environment))

(define (analyze-files filenames environment)
  (close-top-level (apply merge-analyses
                          (map (lambda (filename)
                                 (read-and-analyze-file filename
                                                        environment))
                               filenames))
                   system-global-environment))

(define (read-and-analyze-file filename environment)
  (analyze-scode
   (syntax* (read-file (pathname-default-type filename "scm"))
            environment)))

(define (analyze-environment environment)
  (analysis-children
   (fold (lambda (analysis result)
           (merge-analyses result
                           (child-analysis analysis)))
         (null-analysis)
         (map (lambda (p)
                (analyze-procedure (car p) (cdr p)))
              (get-environment-procedures environment)))))

(define (get-environment-procedures environment)
  (filter-map (lambda (name)
                (and (eq? 'normal (environment-reference-type environment name))
                     (let ((value (environment-lookup environment name)))
                       (and (procedure? value)
                            (cons name value)))))
              (environment-bound-names environment)))

(define (analyze-procedure name procedure)
  (let ((lam (procedure-lambda procedure)))
    (if lam
        (apply-runtime-bindings name
                                (unchild-analysis (analyze-scode lam))
                                (procedure-environment procedure))
        (null-analysis))))

(define (close-top-level analysis environment)
  (apply-runtime-bindings 'runtime
                          (name-analysis 'top-level analysis)
                          environment))

(define (apply-runtime-bindings name analysis environment)
  (make-analysis name
                 (filter (lambda (name)
                           (environment-bound? environment name))
                         (analysis-free analysis))
                 (analysis-free analysis)
                 (list analysis)))

;;;; Analysis results

(define (analysis? object)
  (and (list? object)
       (>= (length object) 4)
       (eq? 'analysis (car object))
       (list-of-type? (caddr object) symbol?)
       (list-of-type? (cadddr object) symbol?)
       (null? (lset-intersection eq? (caddr object) (cadddr object)))
       (list? (cddddr object))
       (every analysis? (cddddr object))))

(define (make-analysis name bound free children)
  (guarantee-list-of symbol? bound 'make-analysis)
  (guarantee-list-of symbol? free 'make-analysis)
  (guarantee-list-of analysis? children 'make-analysis)
  `(analysis ,name ,bound ,(lset-difference eq? free bound) ,@children))

(define (analysis-name analysis)
  (guarantee analysis? analysis 'analysis-name)
  (cadr analysis))

(define (analysis-bound analysis)
  (guarantee analysis? analysis 'analysis-bound)
  (caddr analysis))

(define (analysis-free analysis)
  (guarantee analysis? analysis 'analysis-free)
  (cadddr analysis))

(define (analysis-children analysis)
  (guarantee analysis? analysis 'analysis-children)
  (cddddr analysis))

(define (null-analysis)
  (make-analysis #f '() '() '()))

(define (bound-analysis names)
  (make-analysis #f names '() '()))

(define (free-analysis name)
  (make-analysis #f '() (list name) '()))

(define (child-analysis analysis)
  (make-analysis #f
                 '()
                 (analysis-free analysis)
                 (list analysis)))

(define (unchild-analysis analysis)
  (if (and (not (analysis-name analysis))
           (null? (analysis-bound analysis))
           (= 1 (length (analysis-children analysis)))
           (eq? (analysis-free analysis)
                (analysis-free (car (analysis-children analysis)))))
      (car (analysis-children analysis))
      analysis))

(define (name-analysis name analysis)
  (if (analysis-name analysis)
      (error "Can't overwrite name:" (analysis-name analysis) name))
  (make-analysis name
                 (analysis-bound analysis)
                 (analysis-free analysis)
                 (analysis-children analysis)))

(define (merge-analyses . analyses)
  (make-analysis (let ((names (filter-map analysis-name analyses)))
                   (and (pair? names)
                        (begin
                          (if (pair? (cdr names))
                              (warn "Merging multiple names:" names))
                          (car names))))
                 (let ((bound (append-map analysis-bound analyses)))
                   (let loop ((names bound) (seen '()))
                     (if (pair? names)
                         (loop (cdr names)
                               (if (memq (car names) seen)
                                   (begin
                                     (warn "Duplicate binding:" (car names))
                                     seen)
                                   (cons (car names) seen)))))
                   bound)
                 (apply lset-union eq? (map analysis-free analyses))
                 (append-map analysis-children analyses)))

(define (analyze-scode scode)
  (cond ((scode-variable? scode)
         (free-analysis (scode-variable-name scode)))
        ((scode-combination? scode)
         (apply merge-analyses
                (analyze-scode (scode-combination-operator scode))
                (map analyze-scode (scode-combination-operands scode))))
        ((scode-definition? scode)
         (merge-analyses (bound-analysis (list (scode-definition-name scode)))
                         (analyze-scode (scode-definition-value scode))))
        ((scode-lambda? scode)
         (lambda-components* scode
           (lambda (name req opt rest body)
             (let ((body-analysis
                    (let ((analysis (analyze-scode body)))
                      (if (pair? (analysis-bound analysis))
                          (child-analysis (name-analysis 'body analysis))
                          analysis))))
               (child-analysis
                (make-analysis
                 (cond ((eq? scode-lambda-name:fluid-let name) 'fluid-let)
                       ((eq? scode-lambda-name:let name) 'let)
                       ((eq? scode-lambda-name:unnamed name) 'lambda)
                       (else name))
                 (append req opt (if rest (list rest) '()))
                 (analysis-free body-analysis)
                 (analysis-children body-analysis)))))))
        ((scode-assignment? scode)
         (merge-analyses (free-analysis (scode-assignment-name scode))
                         (analyze-scode (scode-assignment-value scode))))
        ((scode-conditional? scode)
         (merge-analyses (analyze-scode (scode-conditional-predicate scode))
                         (analyze-scode (scode-conditional-consequent scode))
                         (analyze-scode (scode-conditional-alternative scode))))
        ((scode-sequence? scode)
         (if (scode-open-block? scode)
             (analyze-scode
              (unscan-defines (scode-open-block-names scode)
                              (scode-open-block-declarations scode)
                              (scode-open-block-actions scode)))
             (apply merge-analyses
                    (map analyze-scode (scode-sequence-actions scode)))))

        ((scode-delay? scode)
         (analyze-scode (scode-delay-expression scode)))
        ((scode-disjunction? scode)
         (merge-analyses (analyze-scode (scode-disjunction-predicate scode))
                         (analyze-scode (scode-disjunction-alternative scode))))
        ((scode-access? scode)
         (analyze-scode (scode-access-environment scode)))
        ((scode-comment? scode)
         (analyze-scode (scode-comment-expression scode)))
        ;; ((scode-the-environment? scode)
        ;;  (null-analysis))
        ;; ((scode-quotation? scode)
        ;;  (null-analysis))
        (else
         (null-analysis))))

(define install-in-global!
  (let ((env (the-environment)))
    (lambda ()
      (for-each (lambda (name)
                  (environment-link-name system-global-environment env name))
                '(
                  analysis-bound
                  analysis-children
                  analysis-free
                  analysis-name
                  analysis?
                  analyze-environment
                  analyze-file
                  analyze-files
                  )))))