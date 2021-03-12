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

(define (find-pathnames predicate directory)
  (let loop ((directory (pathname-as-directory directory)))
    (append-map (lambda (pathname)
                  (cond ((member (file-namestring pathname)
                                 '("." ".."))
                         '())
                        ((file-directory? pathname)
                         (loop (pathname-as-directory pathname)))
                        ((predicate pathname) (list pathname))
                        (else '())))
                (directory-read directory))))

(define (top-level-subdirs directory)
  (let ((directory (pathname-as-directory directory)))
    (filter-map (lambda (pn)
                  (and (not (string-prefix? "." (file-namestring pn)))
                       (eq? 'directory (file-type-direct pn))
                       (pathname-as-directory pn)))
                (directory-read directory))))

(define (apropos-matches match names)

    (define (filter-names frag candidates)
      (filter-map (lambda (candidate)
                    (let loop ((frags (cdr candidate)))
                      (and (pair? frags)
                           (string-prefix? frag (car frags))
                           (cons (car candidate) (cdr frags)))))
                  candidates))

    (map car
         (let loop ((frags (split-name match))
                    (candidates
                     (map (lambda (name)
                            (cons name (split-name name)))
                          names)))
           (if (pair? frags)
               (let ((candidates* (filter-names (car frags) candidates)))
                 (if (and (pair? candidates*)
                          (pair? (cdr candidates*)))
                     (loop (cdr frags) candidates*)
                     candidates*))
               (let ((exact
                      (filter (lambda (candidate)
                                (null? (cdr candidate)))
                              candidates)))
                 (if (and (pair? exact) (null? (cdr exact)))
                     exact
                     candidates))))))

(define (split-name name)
  (name-splitter (string name)))

(define name-splitter
  (string-splitter 'delimiter char-set:punctuation
                   'allow-runs? #f))

(define (force-top-level-repl! value expression environment)
  (abort->top-level
   (lambda (cmdl)
     (set-repl/environment! cmdl environment)
     (repl-write value expression cmdl))))