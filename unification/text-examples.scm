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

;; (load "~/cph/evolve/unification/code/load")
;Value: #[pathname 24 "/home/gjs/cph/evolve/unification/code/"]

(define a
  '(((? gn) franklin) (? bdate) ((? dmo) (? dday) 1790)))

(define b
  '((ben franklin) ((? bmo) 6 1705) (apr 17 (? dyear))))

(define c
  '((ben (? fn)) (jan (? bday) 1705) (apr 17 (? dyear))))

;; coderef: unifier-1
(unifier a b)
'expect-value: '((ben franklin) ((? bmo) 6 1705) (apr 17 1790))

;; coderef: unifier-2
(unifier a c)
'expect-value: '((ben franklin) (jan (? bday) 1705) (apr 17 1790))

;; coderef: unifier-3
(unifier b c)
'expect-value: '((ben franklin) (jan 6 1705) (apr 17 (? dyear)))

;; coderef: unifier-4
(unifier a (unifier b c))
'expect-value: '((ben franklin) (jan 6 1705) (apr 17 1790))

;; coderef: unifier-5
(unifier b (unifier a c))
'expect-value: '((ben franklin) (jan 6 1705) (apr 17 1790))

;; coderef: unifier-6
(unifier c (unifier a b))
'expect-value: '((ben franklin) (jan 6 1705) (apr 17 1790))

(define addition-commutativity
  '(= (+ (? u) (? v)) (+ (? v) (? u))))

;; coderef: unifier-7
(unifier '(= (+ (cos (? a)) (exp (? b))) (? c))
         addition-commutativity)
'expect-value: '(= (+ (cos (? a)) (exp (? b))) (+ (exp (? b)) (cos (? a))))

(let ((pattern '(? a))
      (expression 'a))
  (unify:internal pattern
                  expression
                  (match:extend-dict pattern expression (match:new-dict))
                  match:bindings))
'expect-value: '((a a ?))


;; coderef: unifier-8
(let ((pattern '(* (?? a) (+ (?? b)) (?? c)))
      (expression '(* x y (+ z w) m (+ n o) p)))
  (unify:internal pattern expression (match:new-dict)
    (lambda (dict)
      (pp (match:bindings dict))
      #f)))

;; coderef: unifier-9
(let ((p1 '(a (?? x) (?? y) (?? x) c))
      (p2 '(a b b b (?? w) b b b c)))
  (unify:internal p1 p2 (match:new-dict)
    (lambda (dict)
      (pp (match:bindings dict))
      #f)))

(unify-test '(a (?? x) (?? y) (?? x) c)
            '(a b b b (?? w) b b b c))
'expect-value: 'matches-and-expected-some

;; coderef: unifier-10
(let ((p1 '(a (?? x) (?? y) (?? x) c))
      (p2 '(a b b b (?? w) b b b c)))
  (unify:internal p1 p2 (match:new-dict)
    (lambda (dict)
      (and dict
           (let ((subst (match:dict-substitution dict)))
             (let ((p1* (subst p1)) (p2* (subst p2)))
               (if (not (equal? p1* p2*))
                   (error "Bad dictionary"))
               (pp p1*))))
      #f)))