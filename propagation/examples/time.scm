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

(define (run-timed-test)
  (show-time
   (lambda ()
     (install-arithmetic! layered-arith) ;debugging
     (install-core-propagators! merge-value-sets
                                layered-arith
                                layered-propagator-projector)

     (initialize-scheduler)

     (define answers (multiple-dwelling))
     (run)
     (let ((values
            (map (lambda (cell)
                   (get-base-value (cell-strongest cell)))
                 answers)))
       (if (not (equal? '(3 2 4 5 1) values))
           (error "Wrong answer!" values))
       (cpp values))

     (cpp *number-of-calls-to-fail*))))

;;; Fully interpreted in new generic system with layering.
;;; On gjs-yoga!
#| (3 2 4 5 1) |#
;process time: 65890 (65880 RUN + 10 GC); real time: 65897
;Value: 90

;;; If I reverse the order of propagator activations in scheduler
;;; I get
#| (3 2 4 5 1) |#
;process time: 46610 (46590 RUN + 20 GC); real time: 46610
;Value: 69

;;; One random order gave:
#| (3 2 4 5 1) |#
;process time: 18070 (18070 RUN + 0 GC); real time: 18077
;Value: 48

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 41150 (41120 RUN + 30 GC); real time: 41157
;Value: 81

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 1730 (1730 RUN + 0 GC); real time: 1725
;Value: 17

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 35340 (35330 RUN + 10 GC); real time: 35343
;Value: 77

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 22610 (22590 RUN + 20 GC); real time: 22601
;Value: 51

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 56540 (56520 RUN + 20 GC); real time: 56543
;Value: 79

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 38020 (38010 RUN + 10 GC); real time: 38030
;Value: 69

;;; Another random order gave:
#| (3 2 4 5 1) |#
;process time: 16370 (16360 RUN + 10 GC); real time: 16381
;Value: 57

;;; So many random orders are better than the obvious orders.
;;; Why?






;;; Running under scmutils

;;; These timings were obtained on 19 May 2018.
;;; On maharal interpreted, running Ubuntu 14.04.5
;;; Intel Xeon CPU w3670 @ 3.20 GHz -- ~6400 bogomips
;;; cache = 12288 KB.
#| (3 2 4 5 1) |#
;process time: 4760 (4760 RUN + 0 GC); real time: 4768
;Value: 47

;;; On binah interpreted, running Ubuntu-mate 18.04
;;; Intel Core 2 Duo T9600 CPU @ 2.80 GHz ~5600 bogomips
;;; cache = 6144 KB.
#| (3 2 4 5 1) |#
;process time: 8920 (8920 RUN + 0 GC); real time: 8939
;Value: 47

;;; On gjs-x1 interpreted, running Ubuntu-mate 18.04
;;;  Intel Core i5-3427U CPU @ 1.80GHz ~4600 bogomips
;;;  cache = 3072 KB.
#| (3 2 4 5 1) |#
;process time: 7320 (7320 RUN + 0 GC); real time: 7311
;Value: 47

;;; On gjs-x2 interpreted, running Ubuntu 14.04.5
;;;  Intel Core i7-5600U CPU @ 2.60GHz ~5200 bogomips
;;; cache = 4096 KB
#| (3 2 4 5 1) |#
;process time: 3210 (3210 RUN + 0 GC); real time: 3207
;Value: 47

;;; On gjs-yoga interpreted, running running Ubuntu 16.04
;;;  Intel Core i7-7600U CPU @ 2.80GHz ~5800 bogomips
;;; cache = 4096 KB.
#| (3 2 4 5 1) |#
;process time: 2800 (2800 RUN + 0 GC); real time: 2798
;Value: 47

;;; On gjs-yoga compiled
#| (3 2 4 5 1) |#
;process time: 40 (40 RUN + 0 GC); real time: 35
;Value: 47
;;; Compiled is about 70x faster.



;;; Extracted from scmutils, 30 May 2018, wow!

;;; On gjs-yoga interpreted, running running Ubuntu 16.04
;;;  Intel Core i7-7600U CPU @ 2.80GHz ~5800 bogomips
;;; cache = 4096 KB.
#| (3 2 4 5 1) |#
;process time: 1030 (1030 RUN + 0 GC); real time: 1024
;Value: 47


;;; On cph virtual machine, interpreted, without
;;; strongest-consequence caching.
#|
(load "load")
*** output flushed ***
;Value: propagators

(gc-flip)
;Value: 47940681

;Value: run-timed-test

(run-timed-test)
#| (3 2 4 5 1) |#
#| 90 |#
;process time: 93770 (93000 RUN + 770 GC); real time: 94141
;Unspecified return value
|#

;;; On cph virtual machine, interpreted, with
;;; strongest-consequence caching.
#|
(load "load")
*** output flushed ***
;Value: propagators

(gc-flip)
;Value: 47600370

;Value: run-timed-test

(run-timed-test)
#| (3 2 4 5 1) |#
#| 90 |#
;process time: 59830 (59350 RUN + 480 GC); real time: 60044
;Unspecified return value
|#
