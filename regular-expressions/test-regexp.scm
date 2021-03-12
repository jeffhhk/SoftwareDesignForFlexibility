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

(define tests-file
  (->namestring
   (merge-pathnames "tests.txt"
                    (directory-pathname (current-load-pathname)))))

(define (assert-grep= expected pattern)
  (assert-equal expected
                (r:grep pattern tests-file)))

(define-test 'simple-usage
  (lambda ()
    (assert-grep= '("[00]. abc"
                    "[01]. aac"
                    "[02]. acc"
                    "[03]. zzzaxcqqq"
                    "[10]. catcatdogdog"
                    "[12]. catcatcatdogdogdog")
                  (r:seq (r:quote "a")
                         (r:dot)
                         (r:quote "c")))
    (assert-grep= '("[05]. foo"
                    "[06]. bar"
                    "[07]. foo bar baz quux")
                  (r:alt (r:quote "foo")
                         (r:quote "bar")
                         (r:quote "baz")))
    (assert-grep= '("[09]. catdogcat"
                    "[10]. catcatdogdog"
                    "[11]. dogdogcatdogdog"
                    "[12]. catcatcatdogdogdog"
                    "[13]. acatdogdogcats"
                    "[14]. ifacatdogdogs"
                    "[15]. acatdogdogsme")
                  (r:repeat 3 5
                            (r:alt (r:quote "cat")
                                   (r:quote "dog"))))
    (assert-grep= '("[09]. catdogcat"
                    "[10]. catcatdogdog"
                    "[11]. dogdogcatdogdog")
                  (r:seq " "
                         (r:repeat 3 5
                                   (r:alt (r:quote "cat")
                                          (r:quote "dog")))
                         (r:eol)))
    (assert-grep= '("[13]. acatdogdogcats")
                  (let ((digit (r:char-from "0123456789")))
                    (r:seq (r:bol)
                           (r:quote "[")
                           digit
                           digit
                           (r:quote "]")
                           (r:quote ".")
                           (r:quote " ")
                           (r:char-from "ab")
                           (r:repeat 3 5 (r:alt "cat" "dog"))
                           (r:char-not-from "def")
                           (r:eol))))))

(define-test 'output
  (lambda ()
    (assert-equal "\\(\\(a\\).\\(c\\)\\)"
                  (r:seq (r:quote "a")
                         (r:dot)
                         (r:quote "c")))
    (assert-equal "\\(\\(foo\\)\\|\\(bar\\)\\|\\(baz\\)\\)"
                  (r:alt (r:quote "foo")
                         (r:quote "bar")
                         (r:quote "baz")))

    #|
    ;; With better repeat procedure:
    (assert-equal (string-append "\\("
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\{3,5\\}"
                                 "\\)")
                  (r:repeat 3 5
                            (r:alt (r:quote "cat")
                                   (r:quote "dog"))))
    (assert-equal (string-append "\\( \\("
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\{3,5\\}"
                                 "\\)$\\)")
                  (r:seq " "
                         (r:repeat 3 5
                                   (r:alt (r:quote "cat")
                                          (r:quote "dog")))
                         (r:eol)))
    (assert-equal (string-append "\\(^\\(\\[\\)[0123456789][0123456789]"
                                 "\\(]\\)\\(\\.\\)\\( \\)[ab]"
                                 "\\("
                                 "\\(cat\\|dog\\)"
                                 "\\{3,5\\}"
                                 "\\)"
                                 "[^def]$\\)")
                  (let ((digit (r:char-from "0123456789")))
                    (r:seq (r:bol)
                           (r:quote "[")
                           digit
                           digit
                           (r:quote "]")
                           (r:quote ".")
                           (r:quote " ")
                           (r:char-from "ab")
                           (r:repeat 3 5 (r:alt "cat" "dog"))
                           (r:char-not-from "def")
                           (r:eol))))
    |#
    (assert-equal (string-append "\\("
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(\\(cat\\)\\|\\(dog\\)\\)\\|\\)"
                                 "\\(\\(\\(cat\\)\\|\\(dog\\)\\)\\|\\)"
                                 "\\)")
                  (r:repeat 3 5
                            (r:alt (r:quote "cat")
                                   (r:quote "dog"))))
    (assert-equal (string-append "\\( "
                                 "\\("
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(cat\\)\\|\\(dog\\)\\)"
                                 "\\(\\(\\(cat\\)\\|\\(dog\\)\\)\\|\\)"
                                 "\\(\\(\\(cat\\)\\|\\(dog\\)\\)\\|\\)"
                                 "\\)"
                                 "$\\)")
                  (r:seq " "
                         (r:repeat 3 5
                                   (r:alt (r:quote "cat")
                                          (r:quote "dog")))
                         (r:eol)))
    (assert-equal (string-append "\\(^"
                                 "\\(\\[\\)"
                                 "[0123456789][0123456789]"
                                 "\\(]\\)"
                                 "\\(\\.\\)"
                                 "\\( \\)"
                                 "[ab]"
                                 "\\("
                                 "\\(cat\\|dog\\)"
                                 "\\(cat\\|dog\\)"
                                 "\\(cat\\|dog\\)"
                                 "\\(\\(cat\\|dog\\)\\|\\)"
                                 "\\(\\(cat\\|dog\\)\\|\\)"
                                 "\\)"
                                 "[^def]$\\)")
                  (let ((digit (r:char-from "0123456789")))
                    (r:seq (r:bol)
                           (r:quote "[")
                           digit
                           digit
                           (r:quote "]")
                           (r:quote ".")
                           (r:quote " ")
                           (r:char-from "ab")
                           (r:repeat 3 5 (r:alt "cat" "dog"))
                           (r:char-not-from "def")
                           (r:eol))))))