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

(define even-number?
  (simple-abstract-predicate 'even-number
                             (lambda (object)
                               (and (n:integer? object)
                                    (even? object)))))

(define make-even-number
  (predicate-constructor even-number?))

(define-test 'non-predicate
  (lambda ()
    (assert-false (predicate? odd?))
    (assert-false (predicate-template? odd?))
    (assert-false (simple-predicate? odd?))
    (assert-false (compound-predicate? odd?))
    (assert-false (parametric-predicate? odd?))
    (assert-error (lambda () (predicate-name odd?)))))

(define-test 'simple-predicate
  (lambda ()
    (test-predicate-operations number? 'number)
    (test-predicate-operations boolean? 'boolean)
    (test-predicate-operations even-number? 'even-number)

    (assert-false (predicate-template? even-number?))
    (assert-false (parametric-predicate? even-number?))
    (assert-error (lambda () (parametric-predicate-names even-number?)))

    (for-each (lambda (n)
                (assert-eqv exact-integer? (get-predicate n))
                (assert-eqv n (get-data n))
                (if (even? n)
                    (let ((nt (make-even-number n)))
                      (assert-eqv even-number? (get-predicate nt))
                      (assert-eqv n (get-data nt))
                      (assert-not-eqv n nt))))
              (iota 8 -4))))

(define-test 'parametric-predicate-one-parameter
  (lambda ()
    (let ((pattern '((? base))))
      (let* ((template
              (make-predicate-template 'template pattern
                                       tagging-strategy:always
                                       (lambda (get-tag) any-object?)))
             (instantiator (predicate-template-instantiator template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list number?))
              (params2 (list boolean?)))
          (let ((tn (apply instantiator params1))
                (tb (apply instantiator params2)))
            (test-predicate-operations tn '(template number))
            (test-predicate-operations tb '(template boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)

            (let ((on (tag-data tn 'on))
                  (ob (tag-data tb 'ob)))
              (assert-true (tn on))
              (assert-false (tn ob))
              (assert-true (tb ob))
              (assert-false (tb on))

              (let ((predicate (predicate-template-predicate template)))
                (assert-true (predicate (get-predicate on)))
                (assert-true (predicate (get-predicate ob)))))))))))

(define-test 'parametric-predicate-two-parameters
  (lambda ()
    (let ((pattern '((?* domains -) (? base))))
      (let* ((template
              (make-predicate-template 'template pattern
                                       tagging-strategy:always
                                       (lambda (get-tag) any-object?)))
             (instantiator (predicate-template-instantiator template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list (list number? number?) number?))
              (params2 (list (list boolean? boolean?) boolean?)))
          (let ((tn (apply instantiator params1))
                (tb (apply instantiator params2)))
            (test-predicate-operations tn '(template (number number) number))
            (test-predicate-operations tb '(template (boolean boolean) boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)

            (let ((on (tag-data tn 'on))
                  (ob (tag-data tb 'ob)))
              (assert-true (tn on))
              (assert-false (tn ob))
              (assert-true (tb ob))
              (assert-false (tb on))

              (let ((predicate (predicate-template-predicate template)))
                (assert-true (predicate (get-predicate on)))
                (assert-true (predicate (get-predicate ob)))))))))))

(define-test 'ordering
  (lambda ()
    (let ((one? (lambda (x) (= x 1))))
      (register-predicate! one? 'one)

      (assert-true (predicate<= (disjoin) even-number?))
      (assert-true (predicate<= (disjoin) (disjoin)))
      (assert-false (predicate<= even-number? (disjoin)))

      (assert-false (predicate<= (conjoin) even-number?))
      (assert-true (predicate<= (conjoin) (conjoin)))
      (assert-true (predicate<= even-number? (conjoin)))

      (assert-eqv (disjoin even-number?) even-number?)
      (assert-eqv (conjoin even-number?) even-number?)

      (assert-true (predicate<= even-number? (disjoin even-number? one?)))
      (assert-false (predicate<= (disjoin even-number? one?) even-number?))

      (assert-false (predicate<= even-number? (conjoin even-number? one?)))
      (assert-true (predicate<= (conjoin even-number? one?) even-number?))

      (let* ((template
              (make-predicate-template 'foo '((? a))
                                       tagging-strategy:always
                                       (lambda (get-tag) any-object?)))
             (instantiator (predicate-template-instantiator template)))
        (let ((p1 (instantiator (disjoin even-number? one?)))
              (p2 (instantiator even-number?))
              (p3 (instantiator one?)))

          (assert-true (predicate<= p1 p1))
          (assert-false (predicate<= p1 p2))
          (assert-false (predicate<= p1 p3))

          (assert-true (predicate<= p2 p1))
          (assert-true (predicate<= p2 p2))
          (assert-false (predicate<= p2 p3))

          (assert-true (predicate<= p3 p1))
          (assert-false (predicate<= p3 p2))
          (assert-true (predicate<= p3 p3))

          ))

      (let* ((template
              (make-predicate-template 'foo '((? a -))
                                       tagging-strategy:always
                                       (lambda (get-tag) any-object?)))
             (instantiator (predicate-template-instantiator template)))
        (let ((p1 (instantiator (disjoin even-number? one?)))
              (p2 (instantiator even-number?))
              (p3 (instantiator one?)))

          (assert-true (predicate<= p1 p1))
          (assert-true (predicate<= p1 p2))
          (assert-true (predicate<= p1 p3))

          (assert-false (predicate<= p2 p1))
          (assert-true (predicate<= p2 p2))
          (assert-false (predicate<= p2 p3))

          (assert-false (predicate<= p3 p1))
          (assert-false (predicate<= p3 p2))
          (assert-true (predicate<= p3 p3))


          ))

      (let* ((template
              (make-predicate-template 'foo '((? a -) (? b))
                                       tagging-strategy:always
                                       (lambda (get-tag) any-object?)))
             (instantiator (predicate-template-instantiator template)))
        (let ((p1 (instantiator (disjoin even-number? one?)
                                (disjoin even-number? one?)))
              (p2 (instantiator even-number? even-number?))
              (p3 (instantiator even-number? (disjoin even-number? one?)))
              (p4 (instantiator (disjoin even-number? one?) even-number?)))

          (for-each (lambda (predicate)
                      (assert-true (predicate<= predicate predicate)))
                    (list p1 p2 p3 p4))

          (assert-false (predicate<= p2 p1))
          (assert-false (predicate<= p3 p1))
          (assert-true (predicate<= p4 p1))

          (assert-false (predicate<= p3 p2))
          (assert-true (predicate<= p4 p2))

          (assert-true (predicate<= p2 p3))
          (assert-false (predicate<= p2 p4))

          ))
      )))

(define (test-predicate-operations predicate name)
  (assert-true (predicate? predicate))
  (assert-false (predicate-template? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv predicate (tag->predicate tag))
    (assert-equal name (predicate-name predicate))
    (assert-equal name (tag-name tag))))

(define (test-simple-predicate-operations predicate)
  (assert-true (simple-predicate? predicate))
  (assert-false (compound-predicate? predicate))
  (assert-false (parametric-predicate? predicate)))

(define (test-compound-predicate-operations predicate type components)
  (assert-false (simple-predicate? predicate))
  (assert-true (compound-predicate? predicate))
  (assert-false (parametric-predicate? predicate))
  (assert-eqv type (compound-predicate-type predicate))
  (assert-contains-exactly components
                           (compound-predicate-components predicate)))

(define (test-template-operations template name pattern)
  (assert-true (predicate-template? template))
  (assert-false (predicate? template))
  (assert-eqv name (predicate-template-name template))
  (assert-equal pattern (predicate-template-pattern template))
  (assert-contains-exactly (map template-pattern-element-name pattern)
                           (predicate-template-parameter-names template))
  (let ((predicate (predicate-template-predicate template)))
    (assert-true (predicate? predicate))
    (assert-true (predicate<= predicate parametric-predicate?))
    (assert-false (predicate<= parametric-predicate? predicate))))

(define (test-parametric-predicate-operations predicate template parameters)
  (assert-false (simple-predicate? predicate))
  (assert-false (compound-predicate? predicate))
  (assert-true (parametric-predicate? predicate))
  (assert-eqv template (parametric-predicate-template predicate))
  (assert-contains-exactly (predicate-template-parameter-names template)
                           (parametric-predicate-names predicate))
  (assert-contains-exactly
   parameters
   (map (lambda (name)
          ((predicate-template-accessor name template) predicate))
        (predicate-template-parameter-names template))))

(define (parametric-predicate-names predicate)
  (predicate-template-parameter-names
   (parametric-predicate-template predicate)))