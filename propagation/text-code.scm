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

;;; We are dealing with milliarcseconds.

(define (mas->radians mas)
  (dms->radians (list 0 0 (/ mas 1000))))

(define (radians->mas rad)
  (let ((dms (radians->dms rad)))
    (assert (and (g:zero? (car dms))
                 (g:zero? (cadr dms))))
    (* 1000 (caddr dms))))

(define AU-in-meters 149597870700)

(define parsec-in-meters
  (/ AU-in-meters (tan (dms->radians (list 0 0 1)))))

(define (meters->parsecs m)
  (/ m parsec-in-meters))

(define (parsecs->meters pc)
  (* pc parsec-in-meters))

(define AU-in-parsecs
  (/ AU-in-meters parsec-in-meters))

(setup-propagator-system
 (extend-arithmetic interval-extender
                    numeric-arithmetic))

(initialize-scheduler)

(define-cell Vega-parallax-distance)
(define-cell Vega-parallax)

(define-c:prop (c:parallax<->distance parallax distance)
  (let-cells (t (AU AU-in-parsecs))
    (c:tan parallax t)
    (c:* t distance AU)))

(define p1
  ;; coderef: c:parallax<->distance
  (c:parallax<->distance Vega-parallax Vega-parallax-distance))

;; coderef: tell:FGWvonStruve1837
(tell! Vega-parallax
       (+->interval (mas->radians 125)
                    (mas->radians 50))
       'FGWvonStruve1837)

;; coderef: Vega-parallax-distance:1
(get-value-in Vega-parallax-distance)
'expect-value: '(interval 5.7142857143291135 13.33333333343721)

;; coderef: Vega-parallax-distance:2
(interval>+- (get-value-in Vega-parallax-distance))
'expect-value: '(+- 9.523809523883163 3.8095238095540473)

;; coderef: Vega-parallax-distance:3
(get-premises Vega-parallax-distance)
'expect-value: '(support-set fgwvonstruve1837)

;; coderef: Vega-parallax-distance:1a
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:1v
((vega-parallax-distance)
 (has-value (interval 5.7143e0 1.3333e1))
 (depends-on fgwvonstruve1837)
 (because
  ((p:/ c:* c:parallax<->distance)
   (au 4.8481e-6)
   (t (interval 3.6361e-7 8.4842e-7)))))

;; coderef: tell:JRussell-etal1982
(tell! Vega-parallax
       (+->interval (mas->radians 124.3) (mas->radians 4.9))
       'JRussell-etal1982)

;; coderef: Vega-parallax-distance:2a
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:2v
((vega-parallax-distance)
 (has-value (interval 7.7399e0 8.3752e0))
 (depends-on jrussell-etal1982)
 (because
  ((p:/ c:* c:parallax<->distance)
   (au 4.8481e-6)
   (t (interval 5.7887e-7 6.2638e-7)))))

;; coderef: tell:Gatewood-deJonge1995
(tell! Vega-parallax
       (+->interval (mas->radians 131)
                    (mas->radians 0.77))
       'Gatewood-deJonge1995)
'expect-write: '
;; contradiction:
;; coderef: tell:Gatewood-deJonge1995:v
((vega-parallax)
 (has-value (the-contradiction))
 (depends-on jrussell-etal1982 gatewood-dejonge1995)
 (because
  ((has-value (interval 5.7887e-7 6.2638e-7))
   (depends-on jrussell-etal1982)
   (because i-told-you-so))
  ((has-value (interval 6.3137e-7 6.3884e-7))
   (depends-on gatewood-dejonge1995)
   (because i-told-you-so))))

;; coderef: retract:Gatewood-deJonge1995
(retract! 'Gatewood-deJonge1995)

;; coderef: Vega-parallax-distance:3
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:3v
((vega-parallax-distance)
 (has-value (interval 7.7399e0 8.3752e0))
 (depends-on jrussell-etal1982)
 (because
  ((p:/ c:* c:parallax<->distance)
   (au 4.8481e-6)
   (t (interval 5.7887e-7 6.2638e-7)))))

;;; Which is what we got from Russell etal.

;; coderef: tell:FvanLeeuwen2007Nov
(tell! Vega-parallax
       (+->interval (mas->radians 130.23) (mas->radians 0.36))
       'FvanLeeuwen2007Nov)
'expect-write: '
;; contradiction:
;; coderef: tell:FvanLeeuwen2007Nov:v
((vega-parallax)
 (has-value (the-contradiction))
 (depends-on jrussell-etal1982 fvanleeuwen2007nov)
 (because
  ((has-value (interval 5.7887e-7 6.2638e-7))
   (depends-on jrussell-etal1982)
   (because i-told-you-so))
  ((has-value (interval 6.2963e-7 6.3312e-7))
   (depends-on fvanleeuwen2007nov)
   (because i-told-you-so))))

;; coderef: retract:JRussell-etal1982
(retract! 'JRussell-etal1982)

;; coderef: Vega-parallax-distance:4
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:4v
((vega-parallax-distance)
 (has-value (interval 7.6576e0 7.7e0))
 (depends-on fvanleeuwen2007nov)
 (because
  ((p:/ c:* c:parallax<->distance)
   (au 4.8481e-6)
   (t (interval 6.2963e-7 6.3312e-7)))))

;; coderef: assert:Gatewood-deJonge1995
(assert! 'Gatewood-deJonge1995)

;; coderef: Vega-parallax-distance:5
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:5v
((vega-parallax-distance)
 (has-value (interval 7.6576e0 7.6787e0))
 (depends-on gatewood-dejonge1995 fvanleeuwen2007nov)
 (because
  ((p:/ c:* c:parallax<->distance)
   (au 4.8481e-6)
   (t (interval 6.3137e-7 6.3312e-7)))))


;;; The following three expressions are not in the text.
;; (retract! 'FvanLeeuwen2007nov)

;; (inquire Vega-parallax-distance)
;; 'expect-write: '
;; ((vega-distance-parsecs)
;;  (has-value (interval 7.589e0 7.6787e0))
;;  (depends-on gatewood-dejonge1995)
;;  (because
;;   ((p:/ c:* c:meters<->parsecs)
;;    (vega-parallax-distance (interval 2.3417e17 2.3694e17))
;;    (meters-per-parsec 3.0857e16))))

;; (assert! 'FvanLeeuwen2007nov)

;;; Relation of distance to magnitudes
;;; Let m be apparent magnitude (as measured)
;;;     M be absolute magnitude (at 10pc)
;;;     d be distance to object in pc

;;; So, taking into account the inverse-square law and
;;; the definition that 5 magnitudes are a factor of 100
;;; in measured brightness, we get
;;;     m - M = 5*((log10 d) - 1)
;;; Note: log10 x = (log x) / (log 10)

;; (define-cell ln10 (log 10))
;; (define-cell one 1)
;; (define-cell five 5)

(define-c:prop
  (c:magnitudes<->distance apparent-magnitude
                           absolute-magnitude
                           magnitude-distance)
  (let-cells (dmod dmod/5 ld10 ld
                   (ln10 (log 10)) (one 1) (five 5))
    (c:+ absolute-magnitude dmod apparent-magnitude)
    (c:* five dmod/5 dmod)
    (c:+ one dmod/5 ld10)
    (c:* ln10 ld10 ld)
    (c:exp ld magnitude-distance)))

(define-cell Vega-apparent-magnitude)
(define-cell Vega-absolute-magnitude)
(define-cell Vega-magnitude-distance)

(define p3
  ;; coderef: call:c:magnitudes<->distance
  (c:magnitudes<->distance Vega-apparent-magnitude
                           Vega-absolute-magnitude
                           Vega-magnitude-distance))

;; coderef: tell:Bohlin-Gilliland2004
(tell! Vega-apparent-magnitude
       (+->interval 0.026 0.008)
       'Bohlin-Gilliland2004)

;; coderef: tell:Gatewood2008
(tell! Vega-absolute-magnitude
       (+->interval 0.582 0.014)
       'Gatewood2008)

;; coderef: Vega-magnitude-distance:1
(inquire Vega-magnitude-distance)
'expect-write: '
;; coderef: Vega-magnitude-distance:1v
((vega-magnitude-distance)
 (has-value (interval 7.663e0 7.8199e0))
 (depends-on gatewood2008 bohlin-gilliland2004)
 (because
  ((p:exp c:exp c:magnitudes<->distance)
   (ld (interval 2.0364e0 2.0567e0)))))

(define p4
  ;; coderef: call:c:same
  (c:same Vega-magnitude-distance Vega-parallax-distance))

;; coderef: Vega-parallax-distance:6
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:6v
((vega-parallax-distance)
 (has-value (interval 7.663e0 7.6787e0))
 (depends-on fvanleeuwen2007nov
             gatewood-dejonge1995
             gatewood2008
             bohlin-gilliland2004)
 (because
  ((p:-> c:same)
   (vega-magnitude-distance (interval 7.663e0 7.6787e0)))))

;; coderef: retract:Gatewood-deJonge1995
(retract! 'Gatewood-deJonge1995)

;; coderef: Vega-parallax-distance:7
(inquire Vega-parallax-distance)
'expect-write: '
;; coderef: Vega-parallax-distance:7v
((vega-parallax-distance)
 (has-value (interval 7.663e0 7.7e0))
 (depends-on fvanleeuwen2007nov
             gatewood2008
             bohlin-gilliland2004)
 (because
  ((p:-> c:same)
   (vega-magnitude-distance (interval 7.663e0 7.7e0)))))

;;; All of the measurements have improved.

;; coderef: improvement1
(+->interval 0.026 0.008)
'expect-value: '(interval .018 .034)

;; coderef: inquire:Vega-apparent-magnitude:1
(inquire Vega-apparent-magnitude)
'expect-write: '
;; coderef: inquire:Vega-apparent-magnitude:1v
((vega-apparent-magnitude)
 (has-value (interval .018 .028456))
 (depends-on gatewood2008 fvanleeuwen2007nov bohlin-gilliland2004)
 (because ((has-value (interval .018 .034))
           (depends-on bohlin-gilliland2004)
           (because i-told-you-so))
          ((has-value (interval .0075442 .028456))
           (depends-on gatewood2008 fvanleeuwen2007nov bohlin-gilliland2004)
           (because ((p:+ c:+ c:magnitudes<->distance)
                     (vega-absolute-magnitude (interval .58554 .596))
                     (dmod (interval -.578 -.56754)))))))

;; coderef: improvement2
(+->interval 0.582 0.014)
'expect-value: '(interval .568 .596)

;; coderef: inquire:Vega-apparent-magnitude:2
(inquire Vega-absolute-magnitude)
'expect-write: '
;; coderef: inquire:Vega-apparent-magnitude:2v
((vega-absolute-magnitude)
 (has-value (interval .58554 .596))
 (depends-on gatewood2008 fvanleeuwen2007nov bohlin-gilliland2004)
 (because
  ((has-value (interval .568 .596)) (depends-on gatewood2008)
                                    (because i-told-you-so))
  ((has-value (interval .58554 .60646))
   (depends-on gatewood2008 fvanleeuwen2007nov bohlin-gilliland2004)
   (because
    ((p:- c:+ c:magnitudes<->distance)
     (vega-apparent-magnitude (interval .018 .028456))
     (dmod (interval -.578 -.56754)))))))

;; coderef: inquire:Vega-parallax:1
(inquire Vega-parallax)
'expect-write: '
;; coderef: inquire:Vega-parallax:1v
((vega-parallax)
 (has-value (interval 6.2963e-7 6.3267e-7))
 (depends-on fvanleeuwen2007nov
             gatewood2008
             bohlin-gilliland2004)
 (because
  ((p:atan c:tan c:parallax<->distance)
   (t (interval 6.2963e-7 6.3267e-7)))))

#|
(explain Vega-parallax-distance)
#|
(((Vega-parallax-distance)
  (has-value (interval 7.663e0 7.7e0))
  (because
   ((identity c:same) ((Vega-magnitude-distance) (interval 7.663e0 7.7e0))))
  (depends-on Gatewood2008 Bohlin-Gilliland2004 FvanLeeuwen2007Nov))
 ((Vega-magnitude-distance)
  (has-value (interval 7.663e0 7.7e0))
  (because
   ((p:exp c:exp c:magnitudes<->distance)
    ((ld c:magnitudes<->distance) (interval 2.0364e0 2.0412e0))))
  (depends-on Gatewood2008 Bohlin-Gilliland2004 FvanLeeuwen2007Nov))
 ((ld c:magnitudes<->distance)
  (has-value (interval 2.0364e0 2.0412e0))
  (because
   ((p:* c:* c:magnitudes<->distance)
    ((ld10 c:magnitudes<->distance) (interval 8.844e-1 8.8649e-1))
    ((ln10) 2.3026e0)))
  (depends-on Gatewood2008 Bohlin-Gilliland2004 FvanLeeuwen2007Nov))
 ((ld10 c:magnitudes<->distance)
  (has-value (interval 8.844e-1 8.8649e-1))
  (because
   ((p:+ c:+ c:magnitudes<->distance)
    ((dmod/5 c:magnitudes<->distance) (interval -1.156e-1 -1.068e-1))
    ((one) 1))
   ((p:/ c:* c:magnitudes<->distance)
    ((ld c:magnitudes<->distance) (interval 2.0357e0 2.0412e0))
    ((ln10) 2.3026e0)))
  (depends-on Gatewood2008 Bohlin-Gilliland2004 FvanLeeuwen2007Nov))
 ((dmod/5 c:magnitudes<->distance)
  (has-value (interval -1.156e-1 -1.068e-1))
  (because
   ((p:/ c:* c:magnitudes<->distance)
    ((dmod c:magnitudes<->distance) (interval -5.78e-1 -5.34e-1))
    ((five) 5)))
  (depends-on Bohlin-Gilliland2004 Gatewood2008))
 ((dmod c:magnitudes<->distance)
  (has-value (interval -5.78e-1 -5.34e-1))
  (because
   ((p:- c:+ c:magnitudes<->distance)
    ((Vega-absolute-magnitude) (interval 5.68e-1 5.96e-1))
    ((Vega-apparent-magnitude) (interval 1.8e-2 3.4e-2))))
  (depends-on Bohlin-Gilliland2004 Gatewood2008))
 ((Vega-absolute-magnitude) (has-value (interval 5.68e-1 5.96e-1))
                            (because (Top-Level))
                            (depends-on Gatewood2008))
 ((Vega-apparent-magnitude) (has-value (interval 1.8e-2 3.4e-2))
                            (because (Top-Level))
                            (depends-on Bohlin-Gilliland2004))
 ((five) (has-value 5) (because (Top-Level)))
 ((one) (has-value 1) (because (Top-Level)))
 ((ld c:magnitudes<->distance)
  (has-value (interval 2.0357e0 2.0412e0))
  (because
   ((p:log c:exp c:magnitudes<->distance)
    ((Vega-magnitude-distance) (interval 7.6576e0 7.7e0))))
  (depends-on FvanLeeuwen2007Nov))
 ((Vega-magnitude-distance)
  (has-value (interval 7.6576e0 7.7e0))
  (because
   ((identity c:same) ((Vega-parallax-distance) (interval 7.6576e0 7.7e0))))
  (depends-on FvanLeeuwen2007Nov))
 ((Vega-parallax-distance)
  (has-value (interval 7.6576e0 7.7e0))
  (because
   ((p:/ c:* c:meters<->parsecs)
    ((Vega-parallax-distance) (interval 2.3629e17 2.376e17))
    ((meters-per-parsec) 3.0857e16)))
  (depends-on FvanLeeuwen2007Nov))
 ((Vega-parallax-distance)
  (has-value (interval 2.3629e17 2.376e17))
  (because
   ((p:/ c:* c:parallax<->distance)
    ((AU) 149597870700)
    ((t c:parallax<->distance) (interval 6.2963e-7 6.3312e-7))))
  (depends-on FvanLeeuwen2007Nov))
 ((AU) (has-value 149597870700) (because (Top-Level)))
 ((t c:parallax<->distance)
  (has-value (interval 6.2963e-7 6.3312e-7))
  (because
   ((p:tan c:tan c:parallax<->distance)
    ((Vega-parallax) (interval 6.2963e-7 6.3312e-7))))
  (depends-on FvanLeeuwen2007Nov))
 ((Vega-parallax) (has-value (interval 6.2963e-7 6.3312e-7))
                  (because (Top-Level))
                  (depends-on FvanLeeuwen2007Nov))
 ((meters-per-parsec) (has-value 3.0857e16) (because (Top-Level)))
 ((ln10) (has-value 2.3026e0) (because (Top-Level))))
;;; This correct, but it is not very good.
;;;  Too much redundant information.
;;;  No way to cut detail.
|#
|#
