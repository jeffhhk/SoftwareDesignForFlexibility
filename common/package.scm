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

;;;; Packages

;;; A package has a name for use in debugging.
;;; It has a set of named bindings.
;;;   It can produce the set of names it knows about.
;;;   Given a name it can produce the value for that name.
;;; It also has a property list.

(define-record-type <package>
    (%make-package name bindings-alist)
    package?
  (name package-debug-name)
  (bindings-alist package-bindings))

(define (make-package name bindings-alist)
  (guarantee-list-of (is-pair-of n:symbol? any-object?)
                     bindings-alist)
  (%make-package name bindings-alist))

(define (package-names package)
  (map car (package-bindings package)))

(define (package-value package name)
  (let ((p (assq name (package-bindings package))))
    (if p
        (cdr p)
        (error "Unknown binding name:" name))))

(define (similar-packages? p1 p2)
  (lset= eq? (package-names p1) (package-names p2)))

;;; This installer is MIT/GNU Scheme specific:
(define (package-installer environment)
  (lambda (package)
    (make-package
     (list 'uninstall (package-debug-name package))
     (map (lambda (binding)
            (let ((name (car binding))
                  (value (cdr binding)))
              (let ((old-value
                     (if (environment-assigned? environment name)
                         (environment-lookup environment name)
                         #f)))
                (environment-define environment name value)
                (cons name old-value))))
          (package-bindings package)))))

;;; **** Implement package-uninstaller and uninstall-package!.

;;; THE-ENVIRONMENT is also MIT/GNU Scheme specific:
(define install-package! (package-installer (the-environment)))

(define (with-installed-package! package thunk)
  (let ((old))
    (shallow-fluid-bind (lambda ()
                          (set! old (install-package! package)))
                        thunk
                        (lambda ()
                          (install-package! (set! old))))))

;;; This is MIT/GNU Scheme specific:
(define-record-printer <package>
  (lambda (package)
    (list (package-debug-name package))))
