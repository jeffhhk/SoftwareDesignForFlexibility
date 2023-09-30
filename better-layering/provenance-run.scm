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

;;; Example provenance run

(repl)

;;; A simple function

;eval> 
(define (Id V)
        (* Is (- (exp (/ (* :q V) (* :k T))) 1)))
;=> (id defined)

;eval> 
(define Is 1e-13)
;=> (is defined)

;eval>
(define :q 1.602176487e-19)
;=> (;q defined)

;eval>
(define :k 1.3806505e-23)
;=> (:k defined)

;eval>
(define T 300)
;=> (t defined)

;eval>
(Id 0.6)
;base;=> 1.2010041136964896e-3


;;; Starting to add provenance.

;eval>
(set! Is (signed Is "IRC"))
;=> (is assigned)

;eval>
(set! :q (signed :q "NIST:CODATA-2006"))
;=> (:q assigned)

;eval>
(set! :k (signed :k "NIST:CODATA-2006"))
;=> (:k assigned)

;eval>
(set! T (signed T "GJS-T"))
;=> (t assigned)

;eval>
(Id (signed 0.6 "GJS-V"))
;provenance;=> ("NIST:CODATA-2006" "GJS-V" "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3

;;; Someone needs to sign the formula!

;eval>
(define (Id V)
        (signed (* Is (- (exp (/ (* :q V) (* :k T))) 1))
                "Searle&Gray"))
;=> (id defined)

;eval>
(Id (signed 0.6 "GJS-V"))
;provenance;=> ("Searle&Gray" "NIST:CODATA-2006" 
;               "GJS-V" "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3


;eval>
(set! Id (signed Id  "GJS-code"))
;=> (id assigned)

;eval>
(Id (signed 0.6 "GJS-V"))
;provenance;=> ("IRC" "GJS-T" "NIST:CODATA-2006" 
;               "Searle&Gray" "GJS-code" "GJS-V")
;base;=> 1.2010041136964896e-3

;eval>
(Id 0.6)
;provenance;=> ("Searle&Gray" "NIST:CODATA-2006"
;               "GJS-T" "IRC")
;base;=> 1.2010041136964896e-3

;;; Simple procedures tests

;eval>
((signed (lambda () (+ (signed 2 'gjs))) 'me))
;provenance;=> (gjs)
;base;=> 2

;eval>
((signed (lambda ()
           (+ (signed 2 'gjs) (signed 3 'gjs)))
         'me))
;provenance;=> (gjs)
;base;=> 5

;eval>
((signed (lambda ()
           (signed (+ (signed 2 'gjs) (signed 3 'gjs))
                   'test))
         'me))
;provenance;=> (test gjs)
;base;=> 5

;eval>
((signed (lambda (x)
           (+ (signed x 'jems) (signed 3 'gjs)))
         'me)
 (signed 5 'bilbo))
;provenance;=> (gjs jems me bilbo)
;base;=> 8

;eval>
(if (signed #f 'p)
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct p)
;base;=> a

;eval>
(if (if (signed #f 'p)
        (signed 'c0 'wrong0)
        (signed #f 'correct0))
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct correct0 p)
;base;=> a

;eval>
(if (signed (if (signed #f 'p)
                (signed 'c0 'wrong0)
                (signed #f 'correct0))
            'p1)
    (signed 'c 'wrong)
    (signed 'a 'correct))
;provenance;=> (correct p1 correct0 p)
;base;=> a

;eval>
(if (signed #t 'p)
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct p)
;base;=> c

;eval>
(if (if (signed #t 'p)
        (signed #t 'correct0)
        (signed #f 'wrong0))
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct correct0 p)
;base;=> c

;eval>
(if (signed (if (signed #t 'p)
                (signed #t 'correct0)
                (signed #f 'wrong0))
            'p1)
    (signed 'c 'correct)
    (signed 'a 'wrong))
;provenance;=> (correct p1 correct0 p)
;base;=> c

;eval>
(if (signed (if (signed #t 'p)
                (signed #t 'correct0)
                (signed #f 'wrong0))
            'p1)
    (if (signed #f 'p2)
        (signed 'c 'wrong1)
        (signed 'a 'correct1))
    (if (signed #t 'p2)
        (signed 'c1 'wrong2)
        (signed 'a1 'wrong2)))
;provenance;=> (correct1 p2 p1 correct0 p)
;base;=> a

;;; Conditionals work in procedures

;eval>
(define (count n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count (- n (signed 1 'bilbo)))))
;=> (count defined)

;eval>
(count 0)
;provenance;=> (gjs frodo)
;base;=> done

;eval>
(count 1)
;provenance;=> (gjs bilbo frodo)
;base;=> done

;eval>
(count 2)
;provenance;=> (gjs bilbo frodo)
;base;=> done

;eval>
(count (signed 0 'sam))
;provenance;=> (gjs frodo sam)
;base;=> done

;eval>
(count (signed 1 'sam))
;provenance;=> (gjs bilbo frodo sam)
;base;=> done

;eval>
(count (signed 2 'sam))
;provenance;=> (gjs bilbo frodo sam)
;base;=> done

;eval>
(define (count1 n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count1 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))
;=> (count1 defined)

;eval>
(count1 5)
;provenance;=> (gjs bilbo1 bilbo2 bilbo3 bilbo4 bilbo5 frodo)
;base;=> done

(define (count2 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count2 (- n (signed 1 'bilbo)))))
;=> (count2 defined)

;eval>
(count2 5)
;provenance;=> (gjs frodo0 frodo1 frodo2
;               frodo3 bilbo frodo4 frodo5)
;base;=> done

;eval>
(define (count3 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count3 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))
;=> (count3 defined)

;eval>
(count3 2)
;provenance;=> (gjs bilbo1 frodo0 bilbo2 frodo1 frodo2)
;base;=> done

;eval>
(define (count4 n)
  (if (= n (signed 0 'frodo))
      (signed 'done 'gjs)
      (count4 (- n (signed 1 (layer-value 'base n))))))
;=> (count4 defined)

;eval>
(count4 3)
;provenance;=> (gjs 1 2 3 frodo)
;base;=> done

;eval>
(define (count5 n)
  (if (= n (signed 0 (symbol 'frodo (layer-value 'base n))))
      (signed 'done 'gjs)
      (count5 (- n
                 (signed 1
                         (symbol 'bilbo
                                 (layer-value 'base n)))))))
;=> (count5 defined)

;eval>
(set! count5 (signed count5 'gjs1))
;=> (count5 assigned)

;eval>
(count5 3)
;provenance;=> (gjs frodo0 bilbo1 frodo1 
;               bilbo2 frodo2 gjs1 bilbo3 frodo3)
;base;=> done

;;; complex stuff with conditionals work!

;eval>
(let ((fact:1 (signed 1 'fact1))
            (fact:2 (signed 2 'fact2))
            (fib:1 (signed 1 'fib1))
            (fib:2 (signed 2 'fib2)))
        (let ((f1
               (lambda (f1 f2)
	         (lambda (n)
 
	           (if (< n fact:2)
	               fact:1
	               (* n ((f1 f1 f2) (- n fact:1)))))))
              (f2
               (lambda (f1 f2)
	         (lambda (n)
	           (if (< n fib:2)
	               n
	               (+ ((f2 f1 f2) (- n fib:1))
		          ((f2 f1 f2) (- n fib:2))))))))
          (let ((fact (f1 f1 f2))
	        (fib (f2 f1 f2)))
            (let ((fact5 (fact (signed 5 'gjs5)))
                  (fib10 (fib (signed 10 'gjs10))))
              (show-result fact5)
              (show-result fib10)
              (+ fact5 fib10)))))    
;provenance;=> (fact1 fact2 gjs5)
;base;=> 120
;provenance;=> (fib1 fib2 gjs10)
;base;=> 55
;provenance;=> (gjs10 fib2 fib1 fact1 fact2 gjs5)
;base;=> 175

;;; Layered Procedure with no explicit provenance layer
;;; acts as base.

;eval>
(define a
        (make-base-value (lambda (x)
                             (if (< x (signed 0 'g1))
                                 (- (signed 0 'g2) x)
                                 (signed x 'g3)))))
;=> (a defined)

;eval>
(a 3)
;provenance;=> (g3 g1)
;base;=> 3

;eval>
(a (signed 3 'foo))
;provenance;=> (g3 g1 foo)
;base;=> 3

;eval>
(a (signed -3 'foo))
;provenance;=> (g2 g1 foo)
;=> 3

;;; Since a is a layered procedure with no provenance,
;;; its signed argument loses its provenance as the
;;; value of the formal parameter x, so this does not
;;; propagate internal provenance computations.

;;; Raw scheme just passes through arguments with
;;; provenance Subtlety with **IF** here, see
;;; analyze.scm.

;eval>
(define b 
        (lambda (x) 
          (if (< x (signed 0 'g1))
              (- (signed 0 'g2) x)
              (signed x 'g3))))
;=> (b defined)

;eval>
(b (signed -3 'foo))
;provenance;=> (g2 g1 foo)
;base;=> 3

;eval>
(b (signed 3 'foo))
;provenance;=> (g3 g1 foo)
;base;=> 3

;eval>
(define b (signed b "gjs"))
;=> (b defined)

;eval>
(b (signed -3 'foo))
;provenance;=> (g2 g1 "gjs" foo)
;base;=> 3

;eval>
(b (signed 3 'foo))
;provenance;=> (g3 g1 "gjs" foo)
;base;=> 3

;;; Here is unsigned count.

;eval>
(define count
        (lambda (n)
          (if (= n (signed 0 'g3))
              (signed 'done 'g4)
              (count (- n (signed 1 'g5))))))
;=> (count defined)

;eval>
(count 4)
;provenance;=> (g4 g5 g3)
;base;=> done

;eval>
(count (signed 0 'g6))
;provenance;=> (g4 g3 g6)
;base;=> done

;eval>
(count (signed 4 'g6))
;provenance;=> (g4 g5 g3 g6)
;base;=> done

;;; Now we sign count.

;eval>
(define count (signed count "gjs"))
;=> (count defined)

;eval>
(count (signed 4 'g6))
;provenance;=> (g4 g5 g3 "gjs" g6)
;base;=> done

