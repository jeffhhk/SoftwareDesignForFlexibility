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

;;; Make scheduler algorithm be the random order algorithm.
;;; Times are for compiled code.

;;; From scheduler.scm
(define (run-alerted)
  (let lp ((props (alerted-propagators 'get-all-and-clear!)))
    (if (pair? props)
        (let ((propagator
               (list-ref props (random (length props)))))
          (set! *current-propagator* propagator)
          (activate-propagator! propagator)
          (lp (delq propagator props)))
        'end-round))
  (if (alerted-propagators 'is-empty?)
      'done
      (run-alerted)))

(define (get-failure-numbers problem-spec correct-answer)
  (initialize-scheduler)
  (let ((answers (problem-spec)))
    (run)
    (let ((values
           (map (lambda (cell)
                  (get-base-value (cell-strongest cell)))
                answers)))
      (if (not (equal? correct-answer values))
          (error "Wrong answer!" values))))
  *number-of-calls-to-fail*)

(define (get-failure-statistics n-trials name chooser
                                problem-spec correct-answer)
  (set! choose-premise-to-disbelieve chooser)
  (show-time
   (lambda ()
     (let lp ((i n-trials) (results '()))
       (if (n:= i 0)
           (let* ((average
                   (inexact
                    (n:/ (reduce n:+ 0 results) n-trials)))
                  (sd
                   (n:sqrt
                    (n:/ (reduce n:+ 0
                                 (map (lambda (x)
                                        (n:square
                                         (n:- x average)))
                                      results))
                         (n:- n-trials 1)))))
             (newline)
             (write-line name)
             (write-line
              `(average= ,average
                         standard-deviation= ,sd
                         min= ,(apply n:min results)
                         max= ,(apply n:max results))))
           (lp (n:- i 1)
               (cons (get-failure-numbers problem-spec
                                          correct-answer)
                     results)))))))

(define (try-all-choosers n-trials problem-spec correct-answer)
  (for-each (lambda (p)
              (get-failure-statistics n-trials
                                      (car p)
                                      (cdr p)
                                      problem-spec
                                      correct-answer))
            choosers))

(define choosers
  (list (cons 'choose-first-premise-in-strong-nogood
              choose-first-premise-in-strong-nogood)
        (cons 'choose-last-premise-in-strong-nogood
              choose-last-premise-in-strong-nogood)
        (cons 'choose-random-premise-from-strong-nogood
              choose-random-premise-from-strong-nogood)
        (cons 'choose-strongest-and-most-common-premise
              choose-strongest-and-most-common-premise)
        (cons 'choose-most-common-premise
              choose-most-common-premise)
        (cons 'choose-least-common-premise
              choose-least-common-premise)
        (cons 'choose-random-premise
              choose-random-premise)
        (cons 'choose-strongest-and-least-common-premise
              choose-strongest-and-least-common-premise)))
#|
;; cph macbook

(try-all-choosers 200 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 178.03 standard-deviation= 76.91187347482202 min= 57 max= 413)
;process time: 381030 (376930 RUN + 4100 GC); real time: 382351
choose-random-premise-from-strong-nogood
(average= 198.83 standard-deviation= 85.49213781477631 min= 48 max= 485)
;process time: 554560 (548660 RUN + 5900 GC); real time: 556292
choose-strongest-and-most-common-premise
(average= 190.085 standard-deviation= 78.70489232174126 min= 86 max= 526)
;process time: 618330 (612150 RUN + 6180 GC); real time: 620013
choose-most-common-premise
(average= 170.14 standard-deviation= 67.7264702599312 min= 74 max= 384)
;process time: 1293090 (1279870 RUN + 13220 GC); real time: 1296804
choose-least-common-premise
(average= 299.245 standard-deviation= 147.54341827537382 min= 77 max= 799)
;process time: 421820 (417300 RUN + 4520 GC); real time: 423173
choose-random-premise
(average= 212.755 standard-deviation= 90.7211475238282 min= 42 max= 473)
;process time: 468160 (463500 RUN + 4660 GC); real time: 469444
;Unspecified return value

|#
#|
;; GJS Yoga

(try-all-choosers 300 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 174.80666666666667 standard-deviation= 78.92019563978302 min= 56 max= 444)
;process time: 482720 (481780 RUN + 940 GC); real time: 482785
choose-random-premise-from-strong-nogood
(average= 192.35333333333332 standard-deviation= 80.8195592627199 min= 42 max= 450)
;process time: 679710 (678420 RUN + 1290 GC); real time: 679791
choose-strongest-and-most-common-premise
(average= 189.95 standard-deviation= 78.69190270821682 min= 59 max= 453)
;process time: 844780 (843080 RUN + 1700 GC); real time: 844838
choose-most-common-premise
(average= 176.25 standard-deviation= 69.33960462268854 min= 76 max= 352)
;process time: 1510380 (1507430 RUN + 2950 GC); real time: 1510498
choose-least-common-premise
(average= 277.26 standard-deviation= 142.954628534364 min= 68 max= 769)
;process time: 590720 (589600 RUN + 1120 GC); real time: 590784
choose-random-premise
(average= 202.44666666666666 standard-deviation= 91.34033800414393 min= 43 max= 526)
;process time: 647860 (646590 RUN + 1270 GC); real time: 647934
choose-strongest-and-least-common-premise
(average= 205.16666666666666 standard-deviation= 103.75665617793548 min= 56 max= 619)
;process time: 671210 (669890 RUN + 1320 GC); real time: 671278
;Unspecified return value


;;; Using amb, rather than binary amb.
(try-all-choosers 1000 multiple-dwelling '(3 2 4 5 1))
choose-first-premise-in-strong-nogood
(average= 108.328 standard-deviation= 14.224371646886505 min= 61 max= 153)
;process time: 374790 (374120 RUN + 670 GC); real time: 374822
choose-random-premise-from-strong-nogood
(average= 96.154 standard-deviation= 16.80972488920777 min= 52 max= 148)
;process time: 315570 (315040 RUN + 530 GC); real time: 315620
choose-strongest-and-most-common-premise
(average= 84.994 standard-deviation= 16.32582299599309 min= 52 max= 135)
;process time: 263180 (262760 RUN + 420 GC); real time: 263238
choose-most-common-premise
(average= 85.179 standard-deviation= 16.487656043288922 min= 53 max= 134)
;process time: 276010 (275570 RUN + 440 GC); real time: 276074
choose-least-common-premise
(average= 98.996 standard-deviation= 24.284969502631526 min= 53 max= 187)
;process time: 312180 (311650 RUN + 530 GC); real time: 312245
choose-random-premise
(average= 97.127 standard-deviation= 18.30885264250834 min= 51 max= 150)
;process time: 314420 (313880 RUN + 540 GC); real time: 314441
choose-strongest-and-least-common-premise
(average= 87.89 standard-deviation= 17.8024950169584 min= 45 max= 143)
;process time: 261960 (261500 RUN + 460 GC); real time: 262002
;Unspecified return value

;;; Using p:amb and not using require/abhor
(try-all-choosers 3000 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 108.76833333333333 standard-deviation= 13.777467235430565 min= 56 max= 155)
;process time: 1055970 (1053840 RUN + 2130 GC); real time: 1056015
choose-random-premise-from-strong-nogood
(average= 94.67666666666666 standard-deviation= 16.27651981397254 min= 52 max= 151)
;process time: 864820 (863000 RUN + 1820 GC); real time: 864852
choose-strongest-and-most-common-premise
(average= 85.51166666666667 standard-deviation= 16.96939944119085 min= 49 max= 149)
;process time: 738290 (736880 RUN + 1410 GC); real time: 738321
choose-most-common-premise
(average= 85.482 standard-deviation= 16.393207589883957 min= 51 max= 147)
;process time: 751200 (749750 RUN + 1450 GC); real time: 751226
choose-least-common-premise
(average= 98.165 standard-deviation= 24.209966594238946 min= 46 max= 189)
;process time: 860300 (858660 RUN + 1640 GC); real time: 860376
choose-random-premise
(average= 97.59233333333333 standard-deviation= 18.144545996121032 min= 49 max= 158)
;process time: 885600 (883920 RUN + 1680 GC); real time: 885761
choose-strongest-and-least-common-premise
(average= 88.29066666666667 standard-deviation= 18.099890834982993 min= 51 max= 150)
;process time: 757500 (755940 RUN + 1560 GC); real time: 757610
;Unspecified return value
|#

#| 
;;; Interpretive code. 12 Nov 2019, gjs-yoga MIT/GNU Scheme 10.1.10
;;; Using p:amb and not using require/abhor

(try-all-choosers 100 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 117.08 standard-deviation= 13.734627770711516 min= 72 max= 148)
;process time: 289190 (286290 RUN + 2900 GC); real time: 289392
choose-random-premise-from-strong-nogood
(average= 104.86 standard-deviation= 17.899844816211242 min= 63 max= 146)
;process time: 246960 (244630 RUN + 2330 GC); real time: 246975
choose-strongest-and-most-common-premise
(average= 91.84 standard-deviation= 19.18265209542574 min= 61 max= 151)
;process time: 203350 (201390 RUN + 1960 GC); real time: 203854
choose-most-common-premise
(average= 89.8 standard-deviation= 15.21628579960805 min= 59 max= 131)
;process time: 191000 (189310 RUN + 1690 GC); real time: 191807
choose-least-common-premise
(average= 105.29 standard-deviation= 26.58179486381268 min= 61 max= 163)
;process time: 249360 (246800 RUN + 2560 GC); real time: 249397
choose-random-premise
(average= 105.76 standard-deviation= 20.334138086409563 min= 67 max= 162)
;process time: 242840 (240600 RUN + 2240 GC); real time: 243248
choose-strongest-and-least-common-premise
(average= 96.63 standard-deviation= 21.434728264141253 min= 58 max= 147)
;process time: 213880 (211920 RUN + 1960 GC); real time: 214452
;Unspecified return value
|#


#|
(try-all-choosers 300 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 174.80666666666667 standard-deviation= 78.92019563978302 min= 56 max= 444)
;process time: 482720 (481780 RUN + 940 GC); real time: 482785
choose-random-premise-from-strong-nogood
(average= 192.35333333333332 standard-deviation= 80.8195592627199 min= 42 max= 450)
;process time: 679710 (678420 RUN + 1290 GC); real time: 679791
choose-strongest-and-most-common-premise
(average= 189.95 standard-deviation= 78.69190270821682 min= 59 max= 453)
;process time: 844780 (843080 RUN + 1700 GC); real time: 844838
choose-most-common-premise
(average= 176.25 standard-deviation= 69.33960462268854 min= 76 max= 352)
;process time: 1510380 (1507430 RUN + 2950 GC); real time: 1510498
choose-least-common-premise
(average= 277.26 standard-deviation= 142.954628534364 min= 68 max= 769)
;process time: 590720 (589600 RUN + 1120 GC); real time: 590784
choose-random-premise
(average= 202.44666666666666 standard-deviation= 91.34033800414393 min= 43 max= 526)
;process time: 647860 (646590 RUN + 1270 GC); real time: 647934
choose-strongest-and-least-common-premise
(average= 205.16666666666666 standard-deviation= 103.75665617793548 min= 56 max= 619)
;process time: 671210 (669890 RUN + 1320 GC); real time: 671278
;Unspecified return value


;;; Using amb, rather than binary amb.
(try-all-choosers 1000 multiple-dwelling '(3 2 4 5 1))
choose-first-premise-in-strong-nogood
(average= 108.328 standard-deviation= 14.224371646886505 min= 61 max= 153)
;process time: 374790 (374120 RUN + 670 GC); real time: 374822
choose-random-premise-from-strong-nogood
(average= 96.154 standard-deviation= 16.80972488920777 min= 52 max= 148)
;process time: 315570 (315040 RUN + 530 GC); real time: 315620
choose-strongest-and-most-common-premise
(average= 84.994 standard-deviation= 16.32582299599309 min= 52 max= 135)
;process time: 263180 (262760 RUN + 420 GC); real time: 263238
choose-most-common-premise
(average= 85.179 standard-deviation= 16.487656043288922 min= 53 max= 134)
;process time: 276010 (275570 RUN + 440 GC); real time: 276074
choose-least-common-premise
(average= 98.996 standard-deviation= 24.284969502631526 min= 53 max= 187)
;process time: 312180 (311650 RUN + 530 GC); real time: 312245
choose-random-premise
(average= 97.127 standard-deviation= 18.30885264250834 min= 51 max= 150)
;process time: 314420 (313880 RUN + 540 GC); real time: 314441
choose-strongest-and-least-common-premise
(average= 87.89 standard-deviation= 17.8024950169584 min= 45 max= 143)
;process time: 261960 (261500 RUN + 460 GC); real time: 262002
;Unspecified return value

;;; Using p:amb and not using require/abhor
(try-all-choosers 3000 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 108.76833333333333 standard-deviation= 13.777467235430565 min= 56 max= 155)
;process time: 1055970 (1053840 RUN + 2130 GC); real time: 1056015
choose-random-premise-from-strong-nogood
(average= 94.67666666666666 standard-deviation= 16.27651981397254 min= 52 max= 151)
;process time: 864820 (863000 RUN + 1820 GC); real time: 864852
choose-strongest-and-most-common-premise
(average= 85.51166666666667 standard-deviation= 16.96939944119085 min= 49 max= 149)
;process time: 738290 (736880 RUN + 1410 GC); real time: 738321
choose-most-common-premise
(average= 85.482 standard-deviation= 16.393207589883957 min= 51 max= 147)
;process time: 751200 (749750 RUN + 1450 GC); real time: 751226
choose-least-common-premise
(average= 98.165 standard-deviation= 24.209966594238946 min= 46 max= 189)
;process time: 860300 (858660 RUN + 1640 GC); real time: 860376
choose-random-premise
(average= 97.59233333333333 standard-deviation= 18.144545996121032 min= 49 max= 158)
;process time: 885600 (883920 RUN + 1680 GC); real time: 885761
choose-strongest-and-least-common-premise
(average= 88.29066666666667 standard-deviation= 18.099890834982993 min= 51 max= 150)
;process time: 757500 (755940 RUN + 1560 GC); real time: 757610
;Unspecified return value
|#

#|
;;; Interpretive code. 17 April 2020, gjs-yoga MIT/GNU Scheme 10.1.10
;;; Using p:amb and not using require/abhor

;;; Ubuntu 16.04

;;; gjs@gjs-yoga:~/cph/evolve$ inxi

;;; CPU~Dual core Intel Core i7-7600U (-HT-MCP-)
;;; speed/max~2009/3900 MHz Kernel~4.15.0-30-generic x86_64 Up~3
;;; days Mem~6355.0/15919.6MB HDD~NA(-) Procs~216 Client~Shell
;;; inxi~2.2.35

;;; Image saved on Saturday August 10, 2019 at 6:28:48 PM
;;;   Release 10.1.10   || Microcode 15.3 || Runtime 15.7 || SF 4.41
;;;   LIAR/x86-64 4.118
;;;  heap size: 300120
;;;  constant-space size: 1839
;;;  stack size: 1024

(try-all-choosers 100 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 119.7 standard-deviation= 17.17968026889366 min= 63 max= 160)
;process time: 269620 (269540 RUN + 80 GC); real time: 269703
choose-random-premise-from-strong-nogood
(average= 109.73 standard-deviation= 22.51098721634716 min= 52 max= 151)
;process time: 237660 (237590 RUN + 70 GC); real time: 237698
choose-strongest-and-most-common-premise
(average= 115.15 standard-deviation= 12.64222122792342 min= 72 max= 144)
;process time: 241920 (241840 RUN + 80 GC); real time: 241933
choose-most-common-premise
(average= 111.05 standard-deviation= 13.430987087381707 min= 76 max= 135)
;process time: 229940 (229870 RUN + 70 GC); real time: 229947
choose-least-common-premise
(average= 127.56 standard-deviation= 19.063063868130016 min= 91 max= 168)
;process time: 289170 (289070 RUN + 100 GC); real time: 289243
choose-random-premise
(average= 112.69 standard-deviation= 30.920130084533024 min= 46 max= 171)
;process time: 245820 (245770 RUN + 50 GC); real time: 245828
choose-strongest-and-least-common-premise
(average= 115.78 standard-deviation= 15.152810853689052 min= 84 max= 154)
;process time: 244060 (243980 RUN + 80 GC); real time: 244058
;Unspecified return value
|#

#| 
;;; Interpretive code. 17 April 2020, gjs-x3 MIT/GNU Scheme 10.1.10
;;; Using p:amb and not using require/abhor

;;; Ubuntu 18.04.4 Mate

;;; gjs@gjs-x3:~/cph/evolve$ inxi
;;; CPU~Quad core Intel Core i7-8665U (-MT-MCP-)
;;; speed/max~1278/4800 MHz Kernel~5.0.0-1047-oem-osp1 x86_64
;;; Up~3 days Mem~5023.4/15811.3MB HDD~1024.2GB(13.6% used)
;;; Procs~286 Client~Shell inxi~2.3.56

;;; Image saved on Saturday August 10, 2019 at 6:28:48 PM
;;;   Release 10.1.10   || Microcode 15.3 || Runtime 15.7 || SF 4.41
;;;   LIAR/x86-64 4.118
;;;  heap size: 300120
;;;  constant-space size: 1839
;;;  stack size: 1024

;;; Strangely slower than gjs-yoga -- Scheme made with debug flag on!

(try-all-choosers 100 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 119.94 standard-deviation= 15.522430820778636 min= 66 max= 155)
;process time: 658580 (658450 RUN + 130 GC); real time: 658603
choose-random-premise-from-strong-nogood
(average= 103.17 standard-deviation= 25.55131695196547 min= 46 max= 148)
;process time: 531410 (531280 RUN + 130 GC); real time: 531426
choose-strongest-and-most-common-premise
(average= 117.71 standard-deviation= 11.575221707412675 min= 82 max= 140)
;process time: 619910 (619800 RUN + 110 GC); real time: 619930
choose-most-common-premise
(average= 114.1 standard-deviation= 14.220841157692577 min= 82 max= 141)
;process time: 593190 (593060 RUN + 130 GC); real time: 593219
choose-least-common-premise
(average= 127.85 standard-deviation= 21.810652109795928 min= 50 max= 170)
;process time: 703780 (703620 RUN + 160 GC); real time: 703806
choose-random-premise
(average= 113.16 standard-deviation= 28.391818426007053 min= 43 max= 164)
;process time: 607560 (607440 RUN + 120 GC); real time: 607576
choose-strongest-and-least-common-premise
(average= 116.06 standard-deviation= 16.040869519745335 min= 80 max= 149)
;process time: 605040 (604910 RUN + 130 GC); real time: 605065
;Unspecified return value


;;; I was wrong.  Running the same Scheme made without the debug flag 
;;;  yields the same result... Apparently the newer machine is slower!

(try-all-choosers 100 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 118.38666666666667 standard-deviation= 17.092620663883597 min= 59 max= 167)
;process time: 769350 (769140 RUN + 210 GC); real time: 769375
choose-random-premise-from-strong-nogood
(average= 111.52 standard-deviation= 24.84176545732589 min= 39 max= 150)
;process time: 694730 (694560 RUN + 170 GC); real time: 694743
choose-strongest-and-most-common-premise
(average= 115.30666666666667 standard-deviation= 12.308164985349265 min= 75 max= 140)
;process time: 704030 (703820 RUN + 210 GC); real time: 704040
choose-most-common-premise
(average= 110.76 standard-deviation= 14.485458052242512 min= 55 max= 141)
;process time: 672920 (672710 RUN + 210 GC); real time: 672947
choose-least-common-premise
(average= 126.93666666666667 standard-deviation= 21.25807315987169 min= 75 max= 175)
;process time: 774880 (774660 RUN + 220 GC); real time: 774933
choose-random-premise
(average= 110.42 standard-deviation= 29.280849723584147 min= 39 max= 175)
;process time: 637970 (637820 RUN + 150 GC); real time: 638016
choose-strongest-and-least-common-premise
(average= 116.76666666666667 standard-deviation= 14.796689798245659 min= 58 max= 151)
;process time: 666380 (666150 RUN + 230 GC); real time: 666430
;Unspecified return value
|#

#|
;;; On gjs-yoga 16May2020 interpreted

(try-all-choosers 100 multiple-dwelling '(3 2 4 5 1))

choose-first-premise-in-strong-nogood
(average= 115.73 standard-deviation= 18.468020407660855 min= 62 max= 163)
;process time: 260970 (260760 RUN + 210 GC); real time: 260969
choose-last-premise-in-strong-nogood
(average= 121.14 standard-deviation= 20.9453449421936 min= 65 max= 174)
;process time: 268070 (267830 RUN + 240 GC); real time: 268071
choose-random-premise-from-strong-nogood
(average= 110.51 standard-deviation= 25.438575257099423 min= 39 max= 161)
;process time: 236050 (235840 RUN + 210 GC); real time: 236045
choose-strongest-and-most-common-premise
(average= 115.76 standard-deviation= 13.393892773811642 min= 79 max= 141)
;process time: 242110 (241920 RUN + 190 GC); real time: 242112
choose-most-common-premise
(average= 115.32 standard-deviation= 12.96995751649935 min= 63 max= 135)
;process time: 243770 (243520 RUN + 250 GC); real time: 243777
choose-least-common-premise
(average= 126.96 standard-deviation= 20.949154415084898 min= 89 max= 178)
;process time: 282120 (281880 RUN + 240 GC); real time: 282128
choose-random-premise
(average= 110.24 standard-deviation= 29.312361649217987 min= 41 max= 164)
;process time: 237040 (236840 RUN + 200 GC); real time: 237048
choose-strongest-and-least-common-premise
(average= 116.32 standard-deviation= 12.726239496299435 min= 86 max= 146)
;process time: 244600 (244390 RUN + 210 GC); real time: 244606
;Unspecified return value
|#

#|
(try-all-choosers 1000 yacht-problem '(1 3 4 2 5))

choose-first-premise-in-strong-nogood
(average= 18.358 standard-deviation= 1.9798373053753118 min= 16 max= 25)
;process time: 236310 (236090 RUN + 220 GC); real time: 236337
choose-last-premise-in-strong-nogood
(average= 18.234 standard-deviation= 1.853828654617433 min= 16 max= 27)
;process time: 233370 (233160 RUN + 210 GC); real time: 233422
choose-random-premise-from-strong-nogood
(average= 14.143 standard-deviation= 3.474568120446553 min= 8 max= 25)
;process time: 180210 (180030 RUN + 180 GC); real time: 180236
choose-strongest-and-most-common-premise
(average= 14.502 standard-deviation= 1.4745067548743793 min= 11 max= 20)
;process time: 188030 (187850 RUN + 180 GC); real time: 188025
choose-most-common-premise
(average= 14.556 standard-deviation= 1.4748013557896655 min= 11 max= 21)
;process time: 189760 (189590 RUN + 170 GC); real time: 189771
choose-least-common-premise
(average= 15.056 standard-deviation= 1.873599309984492 min= 12 max= 21)
;process time: 197720 (197540 RUN + 180 GC); real time: 197724
choose-random-premise
(average= 14.133 standard-deviation= 3.4442958894633504 min= 8 max= 25)
;process time: 176230 (176040 RUN + 190 GC); real time: 176219
choose-strongest-and-least-common-premise
(average= 14.564 standard-deviation= 1.4394359923164253 min= 12 max= 20)
;process time: 189480 (189320 RUN + 160 GC); real time: 189494
;Unspecified return value
|#
