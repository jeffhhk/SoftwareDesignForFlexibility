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

;; <filespec> = ("<filename>" <option>*)
;; <option> = from "<dir>"
;;     | test "<name>"
;;     | test-only? (#f | #t)
;;     | inline-test? (#f | #t)
;;     | inline-test-env-file "<filename>"
;;     | data-only? (#f | #t)
;;     | override ("<name>" | "<dir>/<name>")
;;
;; <name> is a filename without a directory part or suffix.
;;
;; <dir> is a relative filename without a file part or a suffix.
;; It is relative to the root directory, and must not contain the
;; "/code" suffix, which will be inserted as needed.
;;
;; <filename> is a relative filename with an optional directory part
;; and no suffix, interpreted as relative to the directory in which
;; the load-spec appears.
;;
;; The filespec <filename> is the name of the file to be loaded,
;; called "this file" below.
;;
;; FROM <dir>: Where this file is located.
;;
;; TEST <name>: Specifies the name of the test file for this file.  If
;; omitted, the test file is this file's name prefixed with "test-".
;;
;; TEST-ONLY? <boolean>: If true, this file is to be loaded only when
;; running tests.  The default is #f.
;;
;; INLINE-TEST? <boolean>: If true, this file is an inline test which
;; is not loaded but can be executed as a test.
;;
;; INLINE-TEST-ENV-FILE "<filename>": A file containing
;; flavor-specific modifications to the inline test environment.  If
;; not specified, the file "inline-testing" is used.  This option is
;; ignored unless INLINE-TEST? is true.
;;
;; DATA-ONLY? <boolean>: If true, this file is not loaded, but is
;; needed by the software.  The default is #f.
;;
;; OVERRIDE ("<name>" | "<dir>/<name>"): If specified, this file
;; overrides the file specified by the argument, which would otherwise
;; be included.

(define (make-load-spec flavor filespecs relative-dir)
  `((name ,flavor)
    (filespecs ,filespecs)
    (relative-dir ,relative-dir)))

(define (load-spec-expanded-filespecs load-spec)
  (remove filespec-data-only?
          (let ((filespecs (load-spec-filespecs load-spec)))
            (append (default-filespecs-of filespecs)
                    filespecs))))

(define (load-spec-filespecs-to-load load-spec)
  (remove filespec-inline-test?
          (load-spec-expanded-filespecs load-spec)))

(define (load-spec-test-only-filespecs load-spec)
  (filter filespec-test-only?
          (load-spec-expanded-filespecs load-spec)))

(define (load-spec-inline-test-filespecs load-spec)
  (filter filespec-inline-test?
          (load-spec-expanded-filespecs load-spec)))

(define (load-spec-name load-spec)
  (cadr (assq 'name load-spec)))

(define (load-spec-filespecs load-spec)
  (cadr (assq 'filespecs load-spec)))

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
    ("simple-tests" from "common")
    ("trie" from "common")))

(define (filespec? spec)
  (and (pair? spec)
       (string? (car spec))
       (keyword-list? (cdr spec))))

(define (filespec-property name spec)
  (let ((value (get-keyword-value (cdr spec) name)))
    (and (not (default-object? value))
         value)))

(define (filespec-name filespec)
  (car filespec))

(define (filespec-from filespec)
  (filespec-property 'from filespec))

(define (filespec-data-only? filespec)
  (filespec-property 'data-only? filespec))

(define (filespec-test-only? filespec)
  (filespec-property 'test-only? filespec))

(define (filespec-inline-test? filespec)
  (filespec-property 'inline-test? filespec))

(define (filespec-inline-test-env-name filespec)
  (filespec-property 'inline-test-env-file filespec))

(define (filespec-test filespec)
  (filespec-property 'test filespec))

(define (filespec-override filespec)
  (filespec-property 'override filespec))

(define (normalize-filespec filespec load-spec-dir)
  (cond ((string? filespec)
         (list filespec
               'from load-spec-dir))
        ((filespec? filespec)
         (let ((filespec*
                (let ((tail (memq 'from (cdr filespec))))
                  (if tail
                      (begin
                        (set-car! (cdr tail)
                                  (normalize-filespec-dir (cadr tail)))
                        filespec)
                      (cons* (car filespec)
                             'from load-spec-dir
                             (cdr filespec))))))

           (define (maybe-normalize! keyword normalize default)
             (let ((tail (memq keyword (cdr filespec*))))
               (cond (tail
                      (set-car! (cdr tail)
                                (normalize (cadr tail) load-spec-dir)))
                     (default
                      (append! filespec*
                               (list keyword
                                     (normalize default load-spec-dir)))))))

           (maybe-normalize! 'override normalize-filespec-link #f)
           (if (filespec-inline-test? filespec*)
               (maybe-normalize! 'inline-test-env-file
                                 normalize-filespec-link
                                 "inline-testing"))
           filespec*))
        (else
         (error:not-a filespec? filespec))))

(define (normalize-filespec-dir dir)
  (let ((dir* (insert-code-directory dir)))
    (if (directory-exists? dir*)
        dir*
        (begin
          (if (not (directory-exists? dir))
              (warn "Can't find top-level directory:" dir))
          dir))))

(define (insert-code-directory dir)
  (slash-joiner
   (let ((parts (slash-splitter dir)))
     (cons* (car parts)
            "code"
            (cdr parts)))))

(define slash-splitter
  (string-splitter 'delimiter #\/))

(define slash-joiner
  (string-joiner* 'infix "/"))

(define slash-trimmer
  (string-trimmer 'to-trim #\/ 'where 'trailing))

(define (directory-exists? relative-dir)
  (file-directory? (merge-pathnames relative-dir root-directory)))

(define (normalize-filespec-link filename load-spec-dir)
  (string-append (let ((dir-part (directory-namestring filename)))
                   (if (string=? dir-part "")
                       load-spec-dir
                       (normalize-filespec-dir (slash-trimmer dir-part))))
                 "/"
                 (file-namestring filename)))

(define (filespec-filename spec)
  (merge-pathnames (filespec-relative-filename spec)
                   root-directory))

(define (filespec-relative-filename spec)
  (guarantee filespec? spec 'filespec-relative-filename)
  (string-append (filespec-from spec)
                 "/"
                 (car spec)))

(define (filespec-test-filename spec load-spec)
  (guarantee filespec? spec 'filespec-test-filename)
  (let ((from (filespec-from spec)))
    ;; Policy check: don't run tests from other sections:
    (and (or (string=? (load-spec-relative-dir load-spec) from)
             (string=? "common" from))
         (merge-pathnames
          (string-append from
                         "/"
                         (or (filespec-test spec)
                             (filespec-fn-to-test-fn (car spec))))
          root-directory))))

(define (filespec-fn-to-test-fn filename)
  (->namestring
   (merge-pathnames (string-append "test-"
                                   (pathname-name filename))
                    (directory-pathname filename))))

(define (default-filespecs-of explicit-filespecs)
  (remove (default-filespec-predicate explicit-filespecs)
          (all-default-filespecs)))

(define (default-filespec-predicate explicit-filespecs)
  (guarantee-list-of filespec? explicit-filespecs
                     'default-filespec-predicate)
  (let ((overrides
         (filter-map filespec-override
                     explicit-filespecs)))
    (lambda (filespec)
      (or (member (filespec-relative-filename filespec)
                  overrides)
          (filespec-test-only? filespec)))))

(define (filespec-inline-test-env-filename spec)
  (guarantee filespec? spec 'filespec-inline-test-env-filename)
  (merge-pathnames (filespec-inline-test-env-name spec)
                   root-directory))

(define (filespecs-difference* filespecs)
  (reduce (lambda (subtrahend minuend)
            (lset-difference (lambda (fs1 fs2)
                               (pathname=? (filespec-filename fs1)
                                           (filespec-filename fs2)))
                             minuend
                             subtrahend))
          '()
          filespecs))

(define (filespecs-difference . filespecs)
  (filespecs-difference* filespecs))

(define (filespecs-union* filespecs)
  (reduce (lambda (fs acc)
            (lset-union (lambda (fs1 fs2)
                          (pathname=? (filespec-filename fs1)
                                      (filespec-filename fs2)))
                        fs
                        acc))
          '()
          filespecs))

(define (filespecs-union . filespecs)
  (filespecs-union* filespecs))

(define (compute-flavor-load-specs)
  (let ((specs
         (map compute-load-spec (find-flavor-load-specs))))
    (let loop ((specs specs))
      (if (pair? specs)
          (let ((p
                 (find (let ((name (load-spec-name (car specs))))
                         (lambda (spec)
                           (eq? name (load-spec-name spec))))
                       (cdr specs))))
            (if p
                (error "Duplicate spec names:" (car specs) p))
            (loop (cdr specs)))))
    specs))

(define (find-flavor-load-specs)
  (append-map (lambda (dir)
                (filter-map (lambda (pn)
                              (and (eq? 'regular (file-type-direct pn))
                                   (let ((name (file-namestring pn)))
                                     (or (string=? "load-spec" name)
                                         (and (string-prefix? "load-spec-" name)
                                              (not (string-suffix? "~" name)))))
                                   pn))
                            (directory-read dir)))
              (let ((subdirs (top-level-subdirs root-directory)))
                (let ((code-dirs
                       (filter-map (lambda (dir)
                                     (let ((code-dir (merge-pathnames "code" dir)))
                                       (and (file-directory? code-dir)
                                            (pathname-as-directory code-dir))))
                                   subdirs)))
                  (if (null? code-dirs)
                      subdirs
                      code-dirs)))))

(define (compute-load-spec pn)
  (let* ((pn (merge-pathnames pn root-directory))
         (exprs (read-file pn))
         (spec (check-load-spec exprs)))
    (if (not spec)
        (error "Unknown contents of load-spec file:" exprs))
    (let ((relative-directory
           (enough-namestring (directory-pathname-as-file
                               (directory-pathname pn))
                              root-directory)))
      (make-load-spec
       (car spec)
       (map (lambda (filespec)
              (normalize-filespec filespec
                                  relative-directory))
            (cdr spec))
       relative-directory))))

(define (check-load-spec exprs)
  (and (pair? exprs)
       (null? (cdr exprs))
       (let ((expr (car exprs)))
         (and (pair? expr)
              (eq? 'define-load-spec (car expr))
              (pair? (cdr expr))
              (symbol? (cadr expr))
              (list? (cddr expr))
              (cdr expr)))))

(define (flavor-load-spec name #!optional load-specs)
  (let ((load-spec
         (find (lambda (spec)
                 (eq? name (load-spec-name spec)))
               (if (default-object? load-specs)
                   (compute-flavor-load-specs)
                   load-specs))))
    (if (not load-spec)
        (error "Unknown flavor:" name))
    load-spec))

(define (guarantee-flavor-compatibility flavors)
  (let ((incompatibilities (flavor-incompatibilities flavors)))
    (if (pair? incompatibilities)
        (apply failure
               "specified flavors have incompatibilities: "
               incompatibilities))))

(define (flavor-incompatibilities flavors)
  (filter-map (lambda (entry)
                (let ((members
                       (filter (lambda (flavor)
                                 (memq flavor entry))
                               flavors)))
                  (and (> (length members) 1)
                       members)))
              incompatible-flavors))

(define incompatible-flavors
  '((combining-arithmetics user-defined-types)
    (automatic-differentiation combining-arithmetics)
    (generic-interpreter
     non-strict-arguments
     compiling-to-execution-procedures
     exploratory-behavior)
    (propagation continuations-to-amb)
    (pattern-matching-on-graphs abstracting-a-domain:factoring)
    (wrappers layers)))