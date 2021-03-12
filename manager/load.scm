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

(let ((here (directory-pathname (current-load-pathname)))
      (manager-env (make-top-level-environment)))

  (define (load-1 name)
    (load (merge-pathnames name here)
          manager-env))

  (environment-define manager-env
                      'root-directory
                      (directory-pathname
                       (directory-pathname-as-file here)))
  (environment-define manager-env 'manager-env manager-env)

  (load-1 "utils")
  (load-1 "filespec")
  (load-1 "env-model")
  (load-1 "simple-analyzer")
  (load-1 "analyze-sections")
  (load-1 "software-manager")

  (environment-define system-global-environment
                      'manage
                      (access manage-software manager-env)))