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

(define (manage-software command-name . args)
  (catch-and-report-errors
    (lambda ()
      (let ((command (find-command-by-apropos command-name)))
        (if (not command)
            (failure "unknown command " command-name))
        (apply (command-handler command) args)))))

(define (catch-and-report-errors thunk)
  (if debug-internal-errors?
      (thunk)
      (call-with-current-continuation
        (lambda (k)
          (parameterize ((manager-continuation k))
            (bind-condition-handler (list condition-type:error)
                (lambda (condition)
                  (with-notification
                      (lambda (port)
                        (write-string "Error: " port)
                        (write-condition-report condition port)))
                  (k #f))
              thunk))))))

(define debug-internal-errors? #f)
(define manager-continuation (make-parameter #f))

(define (failure . objects)
  (apply message objects)
  ((manager-continuation) #f))

(define (message . objects)
  (with-notification
      (lambda (port)
        (write-string "Manager: " port)
        (for-each (lambda (object)
                    (display object port))
                  objects))))

(define (define-command template doc handler)
  (guarantee command-template? template 'define-command)
  (guarantee string? doc 'define-command)
  (let ((name (car template)))
    (let ((command (find-command name)))
      (if command
          (begin
            (set-car! command template)
            (set-car! (cdr command) doc)
            (set-cdr! (cdr command) handler))
          (set! all-commands
                (cons (cons* template doc handler)
                      all-commands))))
    name))

(define (find-command name)
  (find (lambda (command)
          (eq? name (command-name command)))
        all-commands))

(define (find-command-by-apropos name)
  (let ((matches (apropos-matches name (all-command-names))))
    (and (pair? matches)
         (if (null? (cdr matches))
             (find-command (car matches))
             (failure "multiple commands containing "
                      name
                      ": "
                      matches)))))

(define (command-template? object)
  (and (pair? object)
       (symbol? (car object))
       (mit-lambda-list? (cdr object))))

(define (command-name command)
  (car (command-template command)))

(define (command-template command)
  (car command))

(define (command-doc command)
  (cadr command))

(define (command-handler command)
  (cddr command))

(define all-commands '())

(define-command '(help)
  "Prints out all of the commands and their documentation."
  (lambda ()
    (for-each (lambda (command)
                (newline)
                (write-string ";")
                (pp
                 (let ((template (command-template command)))
                   `(manage ',(car template) ,@(cdr template))))
                (for-each (lambda (line)
                            (write-string ";    ")
                            (write-string line)
                            (newline))
                          (line-splitter (command-doc command))))
              (sort all-commands
                    (lambda (a b)
                      (symbol<? (command-name a)
                                (command-name b)))))))

(define line-splitter
  (string-splitter 'delimiter #\newline))

(define-command '(manager-environment)
  "Returns the environment in which the manager is defined."
  (lambda ()
    manager-env))

(define-command '(debug-internal-errors boolean)
  "If BOOLEAN is true, let software manager errors throw an exception.
Otherwise, the errors are caught and printed."
  (lambda (on?)
    (let ((old debug-internal-errors?))
      (set! debug-internal-errors? on?)
      old)))

(define-command '(command-apropos string)
  "Returns a list of the command names containing STRING.
STRING may be a symbol or a string."
  (lambda (string)
    (apropos-matches string (all-command-names))))

(define (all-command-names)
  (sort (map command-name all-commands)
        symbol<?))

(define-command '(list-flavors)
  "Returns a list of the available flavors."
  (lambda ()
    (all-flavor-names)))

(define-command '(flavor-apropos string)
  "Returns a list of the flavor names containing STRING.
STRING may be a symbol or a string."
  (lambda (string)
    (apropos-matches string (all-flavor-names))))

(define (all-flavor-names)
  (sort (map load-spec-name (compute-flavor-load-specs))
        symbol<?))

(define (choose-flavor-name match)
  (let ((names (apropos-matches match (all-flavor-names))))
    (cond ((not (pair? names))
           (failure "no matching flavors for " match))
          ((pair? (cdr names))
           (failure "multiple matching flavors for " match ": " names))
          (else
           (car names)))))

(define-command '(new-environment . flavors)
  "Creates a new working environment and loads FLAVORS into it.
The read-eval-print loop is moved to be in that environment."
  (lambda flavors
    (let ((flavors (map choose-flavor-name flavors))
          (model (make-working-env-model)))
      (guarantee-flavor-compatibility flavors)
      (if (pair? flavors)
          (let ((load-specs (compute-flavor-load-specs)))
            (load-flavor-from-spec (flavor-load-spec (car flavors) load-specs)
                                   model)
            (for-each (lambda (flavor)
                        (add-flavor model flavor load-specs))
                      (cdr flavors))))
      (enter-working-environment model)
      (force-top-level-repl! flavors
                             `(manage 'new-environment
                                      ,@(map (lambda (flavor) `',flavor)
                                             flavors))
                             (model 'get-environment)))))

(define-command '(add-flavor flavor)
  "Adds FLAVOR to an existing working environment."
  (lambda (flavor)
    (let ((flavor (choose-flavor-name flavor)))
      (add-flavor (current-working-env-model) flavor)
      flavor)))

(define (add-flavor model flavor #!optional load-specs)
  (guarantee-flavor-compatibility (cons flavor (model 'get-loaded-flavors)))
  (let ((load-spec (flavor-load-spec flavor load-specs)))
    (load-flavor-from-spec load-spec
                           model
                           (filespecs-difference
                            (load-spec-filespecs-to-load load-spec)
                            (model 'get-loaded-file-specs)))))

(define-command '(load-test-only-files)
  "Loads the test-only files into the current working environment.
Useful for debugging tests."
  (lambda ()
    (let ((model (current-working-env-model)))
      (load (model 'get-test-only-file-names)
            (model 'get-environment)))))

(define-command '(working-environment)
  "Returns the current working environment."
  (lambda ()
    ((current-working-env-model) 'get-environment)))

(define-command '(name-current-environment name)
  "Assigns the name NAME to the current working environment."
  (lambda (name)
    (let ((model (hash-table-ref/default named-environments name #f)))
      (cond ((not model)
             (hash-table-set! named-environments
                              name
                              (current-working-env-model))
             name)
            ((eq? model (current-working-env-model))
             name)
            (else
             (failure "The name "
                      name
                      " is already assigned."
                      "  Please choose a different name."))))))

(define-command '(environment-names)
  "Returns all existing environment names."
  (lambda ()
    (all-environment-names)))

(define-command '(environment-apropos string)
  "Returns a list of the environment names containing STRING."
  (lambda (string)
    (apropos-matches string (all-environment-names))))

(define-command '(remove-environment-name name)
  "Removes NAME from the set of environment names.
This associated environment will then be inaccessible unless it is
the current working environment or has another name."
  (lambda (name)
    (if (not (hash-table-exists? named-environments name))
        (failure name " is not a known environment name."))
    (hash-table-delete! named-environments name)
    name))

(define-command '(use-environment name)
  "Makes the environment with name NAME be the current working environment.
The previous working environment will then be inaccessable unless it has
been given a name."
  (lambda (name)
    (let* ((name (choose-environment-name name))
           (model (hash-table-ref/default named-environments name #f)))
      (if (not model)
          (failure name " is not a known environment name."))
      (enter-working-environment model)
      (force-top-level-repl! name
                             `(manage 'use-environment ',name)
                             (model 'get-environment)))))

(define (all-environment-names)
  (hash-table-keys named-environments))

(define (choose-environment-name match)
  (let ((names (apropos-matches match (all-environment-names))))
    (cond ((not (pair? names))
           (failure "no matching environments for " match))
          ((pair? (cdr names))
           (failure "multiple matching environments for " match ": " names))
          (else
           (car names)))))

(define named-environments (make-strong-eqv-hash-table))

(define-command '(run-tests)
  "Run all tests in the current working environment."
  (lambda ()
    (run-tests (current-working-env-model))))

(define-command '(run-all-tests . flavors)
  "Run all tests for FLAVORS in pristine working environments.
If no FLAVORS are specified, runs tests for all flavors.
Each FLAVOR is tested independently by creating a working environment
containing only that FLAVOR and running its tests."
  (lambda flavors
    (let ((flavors (map choose-flavor-name flavors)))
      (for-each (lambda (load-spec)
                  (with-notification
                      (lambda (port)
                        (write-string "running tests for flavor: " port)
                        (write (load-spec-name load-spec) port))
                    (lambda ()
                      (run-tests
                       (temporary-working-env-model (list load-spec))))))
                (let ((load-specs (compute-flavor-load-specs)))
                  (if (null? flavors)
                      load-specs
                      (map (lambda (flavor)
                             (flavor-load-spec flavor load-specs))
                           flavors)))))))

(define-command '(show-individual-tests boolean)
  "Sets the flag that controls whether the test platform shows each test."
  (lambda (value)
    (set! show-tests? value)))

(define-command '(debug-test-errors boolean)
  "Sets the flag that controls whether errors in tests enter a REPL."
  (lambda (value)
    (set! debug-test-errors? value)))

(define show-tests? #f)
(define debug-test-errors? #t)

(define (run-tests model)
  (let ((failed 0)
        (all 0))

    (define (accumulate-results failed* all*)
      (set! failed (+ failed failed*))
      (set! all (+ all all*)))

    (let ((env (model 'get-environment))
          (test-only-file-names (model 'get-test-only-file-names)))
      (for-each (lambda (filename)
                  (with-notification (test-file-notifier filename)
                    (lambda ()
                      (let ((env (extend-top-level-environment env)))
                        (parameterize ((param:suppress-loading-message? #t))
                          (load (merge-pathnames
                                 "manager/standard-test-framework"
                                 root-directory)
                                env)
                          (load test-only-file-names env))
                        (call-with-values
                            (lambda ()
                              ((access load-test-file env)
                               filename
                               show-tests?
                               debug-test-errors?
                               env))
                          accumulate-results)))))
                (model 'get-test-file-names))
      (for-each (lambda (filespec)
                  (let ((filename (filespec-filename filespec))
                        (testing (filespec-inline-test-env-filename filespec)))
                    (with-notification (test-file-notifier filename)
                      (lambda ()
                        (let ((env (extend-top-level-environment env)))
                          (parameterize ((param:suppress-loading-message? #t))
                            (load test-only-file-names env)
                            (if (file-loadable? testing)
                                (load testing env)))
                          (let ((results
                                 ((access load-inline-test-1 env)
                                  filename
                                  env
                                  (careful-lookup env 'inline-testing:eval))))
                            (call-with-values
                                (lambda ()
                                  ((access skeletal-test-results env)
                                   results))
                              accumulate-results)
                            ((access summarize-failing-results env)
                             results)))))))
                (model 'get-inline-test-file-specs)))
    (write-test-summary failed all)))

(define ((test-file-notifier filename) port)
  (write-string "running tests in file: " port)
  (write-string (enough-namestring filename root-directory)
                port))

(define (write-test-summary failed all)
  (fresh-line)
  (display "Ran ")
  (write all)
  (display " test")
  (if (not (= 1 all))
      (display "s"))
  (display "; ")
  (write failed)
  (display " failure")
  (if (not (= 1 failed))
      (display "s")))

(define (careful-lookup env name)
  (if (and (environment-bound? env name)
           (environment-assigned? env name))
      (environment-lookup env name)
      (default-object)))

(define-command '(defining-files name)
  "Gets a list of filenames that define NAME."
  (lambda (name)
    (let ((entry (find-index-entry-by-name name)))
      (if entry
          (sort (name-index-entry-definers entry)
                string<?)
          (begin
            (warn "Unable to find name:" name)
            '())))))

(define-command '(referring-files name)
  "Gets a list of filenames that refer to NAME."
  (lambda (name)
    (let ((entry (find-index-entry-by-name name)))
      (if entry
          (sort (name-index-entry-referrers entry)
                string<?)
          (begin
            (warn "Unable to find name:" name)
            '())))))

(define-command '(defined-in-file filename)
  "Gets a list of symbols defined in the file FILENAME."
  (lambda (filename)
    (let ((entry (find-index-entry-by-file filename)))
      (if entry
          (sort (file-index-entry-defined entry)
                symbol<?)
          (begin
            (warn "Unable to find file:" filename)
            '())))))

(define-command '(references-in-file filename)
  "Gets a list of symbols referenced in the file FILENAME."
  (lambda (filename)
    (let ((entry (find-index-entry-by-file filename)))
      (if entry
          (sort (file-index-entry-free entry)
                symbol<?)
          (begin
            (warn "Unable to find file:" filename)
            '())))))

(define-command '(refresh-file-analysis)
  "Refreshes analyzed-file index.
This will take a little while.
Should be needed only when files have changed."
  (lambda ()
    (refresh-total-index!)
    'done))

(define-command '(check-file-analysis)
  "Checks the analyzed-file index for potential problems."
  (lambda ()
    (check-per-name-index (per-name-index (get-total-index)))))