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

(define most-recent-total-index #f)
(define saved-total-index-filename
  (merge-pathnames "manager/saved-total-index"
                   root-directory))

(define (get-total-index)
  (or most-recent-total-index
      (let ((index (get-total-index-1)))
        (set! most-recent-total-index index)
        index)))

(define (get-total-index-1)
  (if (file-loadable? saved-total-index-filename)
      (read-file saved-total-index-filename)
      (let ((index (get-total-index-2)))
        (call-with-output-file saved-total-index-filename
          (lambda (port)
            (for-each (lambda (entry)
                        (pp entry port))
                      index)))
        index)))

(define (get-total-index-2)
  (let* ((per-file-index (compute-per-file-index (compute-flavor-load-specs)))
         (per-name-index (compute-per-name-index per-file-index)))
    `((by-file ,per-file-index)
      (by-name ,per-name-index))))

(define (per-file-index total-index)
  (cadr (assq 'by-file total-index)))

(define (per-name-index total-index)
  (cadr (assq 'by-name total-index)))

(define (refresh-total-index!)
  (set! most-recent-total-index #f)
  (if (file-exists? saved-total-index-filename)
      (delete-file saved-total-index-filename))
  (get-total-index))

(define (find-index-entry-by-file filename)
  (find (lambda (entry)
          (string=? filename (file-index-entry-filename entry)))
        (per-file-index (get-total-index))))

(define (find-index-entry-by-name name)
  (find (lambda (entry)
          (eq? name (name-index-entry-name entry)))
        (per-name-index (get-total-index))))

;;;; Per-file index

(define (compute-per-file-index load-specs)
  (sort (reconcile-per-flavor-summaries
         (map summarize-load-spec load-specs))
        (lambda (a b)
          (string<? (file-index-entry-filename a)
                    (file-index-entry-filename b)))))

(define (summarize-load-spec load-spec)
  (let ((model (temporary-working-env-model (list load-spec))))
    (filter-map (lambda (spec)
                  (and (not (filespec-test-only? spec))
                       (summarize-file (filespec-filename spec)
                                       (model 'get-environment))))
                (model 'get-loaded-file-specs))))

(define (summarize-file filename environment)
  (let ((analysis (read-and-analyze-file filename environment)))
    `((filename ,(enough-namestring filename root-directory))
      (free ,(analysis-free analysis))
      (defined ,(analysis-bound analysis)))))

(define (file-index-entry-filename summary)
  (cadr (assq 'filename summary)))

(define (file-index-entry-free summary)
  (cadr (assq 'free summary)))

(define (file-index-entry-defined summary)
  (cadr (assq 'defined summary)))

(define (reconcile-per-flavor-summaries summaries)
  (map (lambda (per-file)
         (let ((first (car per-file)))
           (for-each (lambda (other)
                       (if (not (and (lset= eq?
                                            (file-index-entry-free first)
                                            (file-index-entry-free other))
                                     (lset= eq?
                                            (file-index-entry-defined first)
                                            (file-index-entry-defined other))))
                           (warn "Conflicting file entries")))
                     (cdr per-file))
           first))
       (group-per-flavor-summaries-by-filename summaries)))

(define (group-per-flavor-summaries-by-filename summaries)
  (let ((table (make-string-hash-table)))
    (for-each (lambda (per-flavor)
                (for-each (lambda (summary)
                            (hash-table-update!
                             table
                             (file-index-entry-filename summary)
                             (lambda (summaries)
                               (cons summary summaries))
                             (lambda ()
                               '())))
                          per-flavor))
              summaries)
    (hash-table-values table)))

;;;; Per-name index

(define (compute-per-name-index per-file-index)
  (map (lambda (entry)
         `((name ,(car entry))
           (definers ,(cadr entry))
           (referrers ,(cddr entry))))
       (sort (let ((table (make-strong-eq-hash-table)))
               (record-definitions! per-file-index table)
               (record-references! per-file-index table)
               (hash-table->alist table))
             (lambda (a b)
               (symbol<? (car a) (car b))))))

(define ((per-file-recorder selector new-entry entry-adder)
         per-file-index table)
  (for-each (lambda (entry)
              (for-each (let ((adder
                               (entry-adder (file-index-entry-filename entry))))
                          (lambda (name)
                            (hash-table-update! table name adder new-entry)))
                        (selector entry)))
            per-file-index))

(define record-definitions!
  (per-file-recorder file-index-entry-defined
                     (lambda () (cons '() '()))
                     (lambda (item)
                       (lambda (entry)
                         (set-car! entry (cons item (car entry)))
                         entry))))

(define record-references!
  (per-file-recorder file-index-entry-free
                     (lambda () (cons '() '()))
                     (lambda (item)
                       (lambda (entry)
                         (set-cdr! entry (cons item (cdr entry)))
                         entry))))

(define (name-index-entry-name entry)
  (cadr (assq 'name entry)))

(define (name-index-entry-definers entry)
  (cadr (assq 'definers entry)))

(define (name-index-entry-referrers entry)
  (cadr (assq 'referrers entry)))

(define (check-per-name-index index)
  (for-each (lambda (entry)
              (let ((definers (name-index-entry-definers entry)))
                (if (null? definers)
                    (warn "Reference(s) to undefined name:" entry)
                    (warn "Conflicting definitions for name:"
                          (name-index-entry-name entry)
                          definers))))
            (find-non-singly-defined-names index)))

(define (find-non-singly-defined-names per-name-index)
  (remove (lambda (entry)
            (let ((definers (name-index-entry-definers entry)))
              (if (null? definers)
                  (let ((name (name-index-entry-name entry)))
                    (or (environment-bound? system-global-environment name)
                        (memq name known-invisible-definitions)))
                  (null? (cdr definers)))))
          per-name-index))

(define (find-conflicting-definers per-name-index)
  (filter-map (lambda (entry)
                (let ((definers (name-index-entry-definers entry)))
                  (and (pair? definers)
                       (pair? (cdr definers))
                       definers)))
              per-name-index))

(define known-invisible-definitions
  '(load-inline-test))