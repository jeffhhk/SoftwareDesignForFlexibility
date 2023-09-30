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

;;; Here we use the Scheme call-with-current-continuation
;;; to allow us to escape to the Scheme system code that 
;;; is doing the testing.  The embedded interpreter has
;;; its own call/cc.


(define (inline-testing:eval expr env)
  (check-repl-initialized)
  (call-with-current-continuation  ; The underlying one...
   (lambda (return)
     (l:eval expr
             the-global-environment
             (extend-continuation
              (lambda (value)
                ;; show-result does l:advance
                (show-result value)
                (return 'done)))))))

(initialize-repl!)