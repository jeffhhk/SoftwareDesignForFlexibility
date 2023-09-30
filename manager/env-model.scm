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

(define (make-working-env-model)
  (let ((environment (make-top-level-environment))
        (load-specs '())
        (loaded-file-specs '())
        (test-file-names '()))

    (define (get-environment)
      environment)

    (define (get-load-specs)
      load-specs)

    (define (add-load-spec! load-spec)
      (set! load-specs (append! load-specs (list load-spec))))

    (define (get-loaded-flavors)
      (map load-spec-name load-specs))

    (define (get-inline-test-file-specs)
      (append-map load-spec-inline-test-filespecs
                  load-specs))

    (define (get-test-only-file-specs)
      (filespecs-union*
       (map load-spec-test-only-filespecs
            load-specs)))

    (define (get-test-only-file-names)
      (map filespec-filename (get-test-only-file-specs)))

    (define (get-loaded-file-specs)
      loaded-file-specs)

    (define (add-loaded-file-spec! file-spec)
      (set! loaded-file-specs (append! loaded-file-specs (list file-spec))))

    (define (get-loaded-file-names)
      (map filespec-filename loaded-file-specs))

    (define (get-test-file-names)
      test-file-names)

    (define (add-test-file-name! file-name)
      (set! test-file-names (append! test-file-names (list file-name))))

    (bundle #f
            get-environment
            get-load-specs
            add-load-spec!
            get-loaded-flavors
            get-inline-test-file-specs
            get-test-only-file-specs
            get-test-only-file-names
            get-loaded-file-specs
            add-loaded-file-spec!
            get-loaded-file-names
            get-test-file-names
            add-test-file-name!)))

(define (temporary-working-env-model load-specs)
  (let ((model (make-working-env-model)))
    (parameterize ((param:suppress-loading-message? #t))
      (for-each (lambda (load-spec)
                  (load-flavor-from-spec load-spec model))
                load-specs))
    model))

(define (temporary-working-environment load-specs)
  ((temporary-working-env-model load-specs)
   'get-environment))

(define (load-flavor-from-spec load-spec model #!optional file-specs)
  (for-each (lambda (file-spec)
              (let ((pn (filespec-filename file-spec)))
                (if (not (filespec-test-only? file-spec))
                    (begin
                      (load pn (model 'get-environment))
                      (model 'add-loaded-file-spec! file-spec)))
                (let ((test-pn
                       (filespec-test-filename file-spec
                                               load-spec)))
                  (if (and test-pn (file-loadable? test-pn))
                      (model 'add-test-file-name! test-pn)))))
            (if (default-object? file-specs)
                (load-spec-filespecs-to-load load-spec)
                file-specs))
  (model 'add-load-spec! load-spec))

(define (enter-working-environment model)
  (set! the-current-working-env-model model)
  (environment-define system-global-environment
                      'current-working-environment
                      (model 'get-environment)))

(define (current-working-env-model)
  (or the-current-working-env-model
      (message "no working environment, make one first.")))

(define the-current-working-env-model #f)