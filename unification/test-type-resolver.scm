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

(define-test 'simple
  (lambda ()
    (assert-equal
     '(t
       (numeric-type)
       ((t
         (type:procedure ((numeric-type) (numeric-type))
                         (numeric-type))
         (lambda (x y)
           (t
            (numeric-type)
            ((t (type:procedure ((numeric-type) (numeric-type))
                                (numeric-type))
                +)
             (t (numeric-type) x)
             (t (numeric-type) y)))))
        (t (numeric-type) 3)
        (t (numeric-type) 4)))
     (infer-program-types '((lambda (x y) (+ x y)) 3 4)))))

(define-test 'fib-recursive
  (lambda ()

    (define fib-program
      '(define fib
         (lambda (n)
           (if (< n 2)
               n
               (+ (fib (- n 1))
                  (fib (- n 2)))))))

    (assert-equal
     '(t
       (type:procedure ((numeric-type)) (numeric-type))
       (define fib
         (t
          (type:procedure ((numeric-type)) (numeric-type))
          (lambda (n)
            (t
             (numeric-type)
             (if (t
                  (boolean-type)
                  ((t (type:procedure ((numeric-type) (numeric-type))
                                      (boolean-type))
                      <)
                   (t (numeric-type) n)
                   (t (numeric-type) 2)))
                 (t (numeric-type) n)
                 (t
                  (numeric-type)
                  ((t (type:procedure ((numeric-type) (numeric-type))
                                      (numeric-type))
                      +)
                   (t
                    (numeric-type)
                    ((t (type:procedure ((numeric-type))
                                        (numeric-type))
                        fib)
                     (t
                      (numeric-type)
                      ((t (type:procedure ((numeric-type)
                                           (numeric-type))
                                          (numeric-type))
                          -)
                       (t (numeric-type) n)
                       (t (numeric-type) 1)))))
                   (t
                    (numeric-type)
                    ((t (type:procedure ((numeric-type))
                                        (numeric-type))
                        fib)
                     (t
                      (numeric-type)
                      ((t (type:procedure ((numeric-type)
                                           (numeric-type))
                                          (numeric-type))
                          -)
                       (t (numeric-type) n)
                       (t (numeric-type) 2)))))))))))))
     (infer-program-types fib-program))))

(define-test 'fib-internal
  (lambda ()

    (define fib-internal
      '(lambda (m)
         (begin
           (define fib
             (lambda (n)
               (if (< n 2)
                   n
                   (+ (fib (- n 1))
                      (fib (- n 2))))))
           (fib m))))

    (assert-equal
     '(t
       (type:procedure ((numeric-type)) (numeric-type))
       (lambda (m)
         (t
          (numeric-type)
          (begin
            (t
             (type:procedure ((numeric-type)) (numeric-type))
             (define fib
               (t
                (type:procedure ((numeric-type)) (numeric-type))
                (lambda (n)
                  (t
                   (numeric-type)
                   (if (t (boolean-type)
                          ((t (type:procedure ((numeric-type) (numeric-type))
                                              (boolean-type))
                              <)
                           (t (numeric-type) n)
                           (t (numeric-type) 2)))
                       (t (numeric-type) n)
                       (t
                        (numeric-type)
                        ((t (type:procedure ((numeric-type) (numeric-type))
                                            (numeric-type))
                            +)
                         (t (numeric-type)
                            ((t (type:procedure ((numeric-type))
                                                (numeric-type))
                                fib)
                             (t (numeric-type)
                                ((t (type:procedure ((numeric-type) (numeric-type))
                                                    (numeric-type))
                                    -)
                                 (t (numeric-type) n)
                                 (t (numeric-type) 1)))))
                         (t (numeric-type)
                            ((t (type:procedure ((numeric-type)) (numeric-type))
                                fib)
                             (t (numeric-type)
                                ((t (type:procedure ((numeric-type) (numeric-type))
                                                    (numeric-type))
                                    -)
                                 (t (numeric-type) n)
                                 (t (numeric-type) 2)))))))))))))
            (t (numeric-type)
               ((t (type:procedure ((numeric-type)) (numeric-type)) fib)
                (t (numeric-type) m)))))))
     (infer-program-types fib-internal))))

(define-test 'fact-iterative
  (lambda ()

    (define fact-iterative
      '(define fact
         (lambda (n)
           (begin
             (define iter
               (lambda (product counter)
                 (if (> counter n)
                     product
                     (iter (* product counter)
                           (+ counter 1)))))
             (iter 1 1)))))

    (let ((annotated (infer-program-types fact-iterative)))
      (assert-equal
       '(t
         (type:procedure ((numeric-type)) (numeric-type))
         (define fact
           (t
            (type:procedure ((numeric-type)) (numeric-type))
            (lambda (n)
              (t
               (numeric-type)
               (begin
                 (t
                  (type:procedure ((numeric-type) (numeric-type)) (numeric-type))
                  (define iter
                    (t
                     (type:procedure ((numeric-type) (numeric-type)) (numeric-type))
                     (lambda (product counter)
                       (t
                        (numeric-type)
                        (if (t
                             (boolean-type)
                             ((t (type:procedure ((numeric-type) (numeric-type))
                                                 (boolean-type))
                                 >)
                              (t (numeric-type) counter)
                              (t (numeric-type) n)))
                            (t (numeric-type) product)
                            (t
                             (numeric-type)
                             ((t (type:procedure ((numeric-type) (numeric-type))
                                                 (numeric-type))
                                 iter)
                              (t
                               (numeric-type)
                               ((t (type:procedure ((numeric-type) (numeric-type))
                                                   (numeric-type))
                                   *)
                                (t (numeric-type) product)
                                (t (numeric-type) counter)))
                              (t
                               (numeric-type)
                               ((t (type:procedure ((numeric-type) (numeric-type))
                                                   (numeric-type))
                                   +)
                                (t (numeric-type) counter)
                                (t (numeric-type) 1)))))))))))
                 (t
                  (numeric-type)
                  ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type))
                      iter)
                   (t (numeric-type) 1)
                   (t (numeric-type) 1)))))))))
       annotated)

      (assert-equal
       '(begin
          (define fact
            (lambda (n)
              (declare-type n (numeric-type))
              (define iter
                (lambda (product counter)
                  (declare-type product (numeric-type))
                  (declare-type counter (numeric-type))
                  (if (> counter n)
                      product
                      (iter (* product counter) (+ counter 1)))))
              (declare-type iter
                            (type:procedure ((numeric-type) (numeric-type))
                                            (numeric-type)))
              (iter 1 1)))
          (declare-type fact (type:procedure ((numeric-type)) (numeric-type))))
       (simplify-annotated-program annotated)))))