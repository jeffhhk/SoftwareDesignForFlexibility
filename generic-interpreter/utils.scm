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

(define (identity x) x)

(define ((compose f g) x) (f (g x)))


;;; This is to keep the Scheme printer from going into an infinite
;;; loop if you try to print a circular data structure, such as an
;;; environment.  This is complicated by the fact that there was
;;; a recent change to MIT/GNU system fluid variables, and this must
;;; work in multiple versions.  Sorry!

(if (or (not *unparser-list-depth-limit*)
        (number? *unparser-list-depth-limit*))
    (begin
      (set! *unparser-list-depth-limit* 10)
      (set! *unparser-list-breadth-limit* 10)
      )
    (begin
      (set-fluid! *unparser-list-depth-limit* 10)
      (set-fluid! *unparser-list-breadth-limit* 10)
      ))