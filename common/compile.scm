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

(fluid-let ((sf/default-syntax-table
             (the-environment))
            (sf/default-declarations
             `((usual-integrations ,@overridden-names))))
  (for-each (lambda (path)
              (let ((scm (pathname-new-type path "scm"))
                    (bin (pathname-new-type path "bin"))
                    (com (pathname-new-type path "com")))
                (let ((stime (file-modification-time scm))
                      (btime (file-modification-time bin))
                      (ctime (file-modification-time com)))
                  (cond ((or (not btime) (< btime stime))
                         (cf path))
                        ((or (not ctime) (< ctime btime))
                         (compile-bin-file path))))))
            tests-loaded-pathnames))