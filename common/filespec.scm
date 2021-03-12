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

(define loaded-spec)

(define (make-load-spec env-name environment filespecs root-dir
                        relative-dir)
  `((name ,env-name)
    (environment ,environment)
    (filespecs ,filespecs)
    (root-dir ,root-dir)
    (relative-dir ,relative-dir)))

(define (load-spec-environment load-spec)
  (let ((env (cadr (assq 'environment load-spec))))
    (if (eq? env 'generate)
        (let ((env (make-top-level-environment)))
          (load (load-spec-files-to-load load-spec)
                env)
          env)
        env)))

(define (load-spec-files-to-load load-spec)
  (map (let ((root-dir (load-spec-root-dir load-spec)))
         (lambda (filename)
           (merge-pathnames filename root-dir)))
       (map filespec-filename
            (load-spec-expanded-filespecs load-spec))))

(define (load-spec-expanded-filespecs load-spec)
  (remove filespec-data-only?
          (let ((filespecs (load-spec-filespecs load-spec)))
            (append (default-filespecs-of filespecs)
                    filespecs))))

(define (load-spec-name load-spec)
  (cadr (assq 'name load-spec)))

(define (load-spec-filespecs load-spec)
  (cadr (assq 'filespecs load-spec)))

(define (load-spec-root-dir load-spec)
  (cadr (assq 'root-dir load-spec)))

(define (load-spec-relative-dir load-spec)
  (cadr (assq 'relative-dir load-spec)))

(define (all-default-filespecs)
  '(("overrides" from "common")
    ("utils" from "common")
    ("indexes" from "common")
    ("collections" from "common")
    ("memoizers" from "common")
    ("predicates" from "common")
    ("predicate-metadata" from "common")
    ("applicability" from "common")
    ("generic-procedures" from "common")
    ("pretty-printer" from "common")
    ("operators" from "common")
    ("operations" from "common")
    ("package" from "common")
    ("predicate-counter" from "common")
    ("testing" from "common" test-only? #t)
    ("simple-tests" from "common" test-only? #t)
    ("trie" from "common")))

(define (filespec? spec)
  (and (pair? spec)
       (string? (car spec))
       (plist? (cdr spec))))

(define (normalize-from-spec spec from root-dir)
  (let ((normalize-from
         (lambda (from)
           (if (string=? "common" from)
               from
               (let ((from*
                      (string-append from "/code")))
                 (if (file-directory?
                      (merge-pathnames from* root-dir))
                     from*
                     from))))))
    (cond ((string? spec)
           (list spec 'from from))
          ((filespec? spec)
           (let ((tail (memq 'from (cdr spec))))
             (if tail
                 (begin
                   (set-car! (cdr tail)
                             (normalize-from (cadr tail)))
                   spec)
                 `(,(car spec) from ,from
                   ,@(cdr spec)))))
          (else
           (error:not-a filespec? spec)))))

(define (filespec-filename spec)
  (guarantee filespec? spec 'filespec-filename)
  (merge-pathnames
   (string-append (filespec-property 'from spec)
                  "/"
                  (car spec))
   root-directory))

(define (filespec-test-filename spec #!optional load-spec)
  (guarantee filespec? spec 'filespec-test-filename)
  (let ((load-spec
         (if (default-object? load-spec)
             loaded-spec
             load-spec)))
    (let ((from (filespec-property 'from spec)))
      ;; Policy check: don't run tests from other sections:
      (and (or (string=? (load-spec-relative-dir load-spec) from)
               (string=? "common" from))
           (merge-pathnames
            (string-append from
                           "/"
                           (or (filespec-property 'test spec)
                               (filespec-fn-to-test-fn (car spec))))
            root-directory)))))

(define (filespec-fn-to-test-fn filename)
  (->namestring
   (merge-pathnames (string-append "test-"
                                   (pathname-name filename))
                    (directory-pathname filename))))

(define (filespec-loadable-test-name filespec)
  (let ((test-fn (filespec-test-filename filespec)))
    (and test-fn
         (file-loadable?
          (merge-pathnames test-fn
                           (load-spec-root-dir loaded-spec)))
         test-fn)))

(define (default-filespecs-of explicit-filespecs)
  (remove (filespec-override-predicate explicit-filespecs)
          (all-default-filespecs)))

(define (filespec-override-predicate explicit-filespecs)
  (guarantee-list-of filespec? explicit-filespecs
                     'filespec-override-predicate)
  (let ((overrides
         (filter-map (lambda (filespec)
                       (filespec-property 'override filespec))
                     explicit-filespecs)))
    (lambda (filespec)
      (member (or (filespec-property 'override-name filespec)
                  (filespec-filename filespec))
              overrides))))

(define (filespec-property name spec)
  (let ((value (plist-value (cdr spec) name)))
    (and (not (default-object? value))
         value)))

(define (filespec-data-only? filespec)
  (filespec-property 'data-only? filespec))

(define (filespec-test-only? filespec)
  (filespec-property 'test-only? filespec))