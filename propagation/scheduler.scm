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

;;; -------------------------------------------------------------
;;; Copyright 2008 Alexey Radul and Gerald Jay Sussman.
;;; -------------------------------------------------------------
;;; This file is part of Artistic Propagator Prototype.
;;;
;;; Artistic Propagator Prototype is free software; you can
;;; redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software
;;; Foundation, either version 3 of the License, or (at your
;;; option) any later version.
;;;
;;; Artistic Propagator Prototype is distributed in the hope that
;;; it will be useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU General Public License for
;;; more details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with Artistic Propagator Prototype.  If not,
;;; see <http://www.gnu.org/licenses/>.
;;; -------------------------------------------------------------

;;;; Basic scheduling system

;;; This scheduler maintains a list of jobs that need to be run.
;;; Each job is a thunk.  Jobs are run serially and are not
;;; preempted.  When a job exits (normally) it is forgotten and
;;; the next job is run.  The jobs are permitted to schedule
;;; additional jobs, including rescheduling themselves.  Jobs are
;;; presumed idempotent, and specifically it is assumed
;;; acceptable not to count how many times a given job (by
;;; eq?-ness) was scheduled, but merely that it was scheduled.
;;; When the scheduler runs out of jobs, it returns the symbol
;;; 'done to its caller.

;;; The scheduler supplies an escape mechanism: running the
;;; procedure abort-process, with a value, will terminate the
;;; entire job run, and return the supplied value to the
;;; scheduler's caller.  Subsequent calls to the scheduler
;;; without first scheduling more jobs will also return that same
;;; value.  If abort-process is called outside the dynamic extent
;;; of a run, it deschedules any jobs that might be scheduled and
;;; saves the value for future refernce as above.

;;; This scheduler is meant as a low-level support for a
;;; propagator network.  In that use case, the jobs would be
;;; propagators that the network knows need to be run.  Any cells
;;; in the network are invisible to the scheduler, but presumably
;;; help the network schedule more propagators to run (namely
;;; those that may be interested in the cell's goings on).

;;; The public interface is
;;;   (initialize-scheduler)      clears all scheduler state
;;;   (alert-propagator! prop)    schedules a propagator
;;;   (run)                       runs scheduled jobs until done
;;;   (abort-process x)           terminates the run returning x

(define *last-value-of-run*)
(define *current-propagator*)
(define *all-cells*)

(define (initialize-scheduler)
  (alerted-propagators 'clear!)
  (set! propagators-ever-alerted '())
  (set! all-amb-propagators '())
  (clear-relatable-hierarchy!)
  (clear-premises!)
  (set! *last-value-of-run*)
  (set! *current-propagator*)
  (set! *all-cells* '())
  (set! *number-of-calls-to-fail* 0)
  'ok)

;; A propagator set that has a reliable insertion order.
(define (propagator-set)
  (let ((propagators '()))

    (define (get-all)
      (list-copy propagators))

    (define (get-all-and-clear!)
      (let ((result propagators))
        (set! propagators '())
        ;; Return in alerting order:
        (reverse! result)))

    (define (clear!)
      (set! propagators '()))

    (define (add! propagator)
      (set! propagators
            (lset-adjoin eq? propagators propagator)))

    (define (is-empty?)
      (null? propagators))

    (bundle propagator-set?
            get-all
            get-all-and-clear!
            clear!
            add!
            is-empty?)))

(define propagator-set?
  (make-bundle-predicate 'propagator-set))

(define alerted-propagators (propagator-set))
(define propagators-ever-alerted '())
(define all-amb-propagators '())

(define (alert-propagator! propagator)
  (guarantee propagator? propagator 'alert-propagators!)
  (alerted-propagators 'add! propagator)
  (set! propagators-ever-alerted
        (lset-adjoin eq? propagators-ever-alerted propagator)))

(define (alert-propagators! propagators)
  (for-each alert-propagator! propagators))

(define (all-propagators)
  (list-copy propagators-ever-alerted))

(define (all-cells)
  (list-copy *all-cells*))

(define *abort-process*
  (make-parameter
   (lambda (value)
     (set! *last-value-of-run* value)
     value)))

(define (abort-process value)
  ((*abort-process*) value))

(define (run)
  (if (not (alerted-propagators 'is-empty?))
      (set! *last-value-of-run*
            (call/cc
             (lambda (k)
               (parameterize ((*abort-process* k))
                 (run-alerted))))))
  *last-value-of-run*)

(define (run-alerted)
  (for-each (lambda (propagator)
              (set! *current-propagator* propagator)
              ;;(pp (name propagator))
              (activate-propagator! propagator))
            (alerted-propagators 'get-all-and-clear!))
  (if (alerted-propagators 'is-empty?)
      'done
      (run-alerted)))

#|
;;; For testing--reverse the order
(define (run-alerted)
  (for-each (lambda (propagator)
              (set! *current-propagator* propagator)
              ;;(pp (name propagator))
              (activate-propagator! propagator))
            (reverse (alerted-propagators 'get-all-and-clear!)))
  (if (alerted-propagators 'is-empty?)
      'done
      (run-alerted)))

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
|#

#|
(define (run-alerted)
  (let lp ((props (alerted-propagators 'get-all-and-clear!)))
    (if (pair? props)
        (let ((index (random (length props))))
          (let ((propagator (list-ref props index)))
            (set! *current-propagator* propagator)
            (activate-propagator! propagator)
            (lp (let loop ((props props) (i 0))
                  (if (n:= i index)
                      (cdr props)
                      (cons (car props)
                            (loop (cdr props) (+ i 1))))))))
        'end-round))
  (if (alerted-propagators 'is-empty?)
      'done
      (run-alerted)))
|#
