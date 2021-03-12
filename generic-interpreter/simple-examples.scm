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

(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (fib-iter n)
  (fib-loop 1 0 n))

(define (fib-loop a b count)
  (if (= count 0)
      b
      (fib-loop (+ a b)
                a
                (- count 1))))


(define (fib-iter1 n)
  (define (loop a b count)
    (if (= count 0)
        b
        (loop (+ a b)
              a
              (- count 1))))
  (loop 1 0 n))

(define (test1)
  (((lambda (x)
      (lambda (y)
        (+ x y)))
    3)
   4))

(fact 5)
'expect-value: 120

(fib 10)
'expect-value: 55

(fib-iter 10)
'expect-value: 55

(fib-iter1 10)
'expect-value: 55

(test1)
'expect-value: 7

#|
(init)

eval> (load-library "elementary-evaluation/code/simple-examples.scm")
fact
fib
fib-iter
fib-loop
fib-iter1
done

eval> (fact 5)
120

eval> (fib 10)
55

eval> (fib-iter 10)
55

eval> (fib-iter1 10)
55

eval> (load-library "elementary-evaluation/code/simple-examples.scm")
fact
fib
fib-iter
fib-loop
fib-iter1
done

eval> (fib-iter1 10)
55

eval> (load-library "elementary-evaluation/code/simple-examples.scm")
fact
fib
fib-iter
fib-loop
fib-iter1
test1
done

eval> (test1)
7

(load "elementary-evaluation/code/load-general")

(init)

eval> (load-library "elementary-evaluation/code/simple-examples.scm")
fact
fib
fib-iter
fib-loop
fib-iter1
test1
done

eval> (fact 5)
120

eval> (fib 10)
55

eval> (fib-iter 10)
55

eval> (fib-iter1 10)
55

eval> (test1)
7

(init)

eval> (load-library "elementary-evaluation/code/kons.scm")
kons-registrations
kons
kar
kdr
kons?
add-streams
ref-stream
fibs
wrong-map-stream
wrong-scale-stream
wrong-integral
wrong-solve
map-stream
scale-stream
add-streams
ref-stream
integral
solve
done

eval> (ref-stream fibs 10)
55

eval> (ref-stream fibs 100)
354224848179261915075

eval> (inexact
       (/ (ref-stream fibs 100)
          (ref-stream fibs 99)))
1.618033988749895

eval> (/ (+ 1 (sqrt 5)) 2)
1.618033988749895

eval> (ref-stream (solve (lambda (x) x) 1 0.001)
            1000)
2.716923932235896

|#
