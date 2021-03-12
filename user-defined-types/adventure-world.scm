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

;;;; An adventure game at MIT

(define the-clock)
(define all-places)
(define heaven)
(define all-people)
(define my-avatar)

(define (start-adventure my-name)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name
                       (random-choice all-places)))
  (whats-here))

(define (get-all-places)
  all-places)

(define (get-heaven)
  heaven)

(define (get-clock)
  the-clock)

;;; User interface

(define (go direction)
  (let ((exit
         (find-exit-in-direction direction
                                 (get-location my-avatar))))
    (if exit
        (take-exit! exit my-avatar)
        (narrate! (list "No exit in" direction "direction")
                  my-avatar)))
  'done)

(define (take-thing name)
  (let ((thing (find-thing name (here))))
    (if thing
        (take-thing! thing my-avatar)))
  'done)

(define (drop-thing name)
  (let ((thing (find-thing name my-avatar)))
    (if thing
        (drop-thing! thing my-avatar)))
  'done)

(define (look-in-bag #!optional person-name)
  (let ((person
         (if (default-object? person-name)
             my-avatar
             (find-person person-name))))
    (if person
        (tell! (let ((referent (local-possessive person))
                     (things (get-things person)))
                 (if (n:pair? things)
                     (cons* referent "bag contains" things)
                     (list referent "bag is empty")))
               my-avatar)))
  'done)

(define (whats-here)
  (look-around my-avatar)
  'done)

(define (say . message)
  (say! my-avatar message)
  'done)

(define (tell person-name . message)
  (tell! message (find-person person-name))
  'done)

(define (hang-out ticks)
  (do ((i 0 (n:+ i 1)))
      ((not (n:< i ticks)))
    (tick! (get-clock)))
  'done)

;;; Support for UI

(define (here)
  (get-location my-avatar))

(define (find-person name)
  (let ((person
         (find-object-by-name name (people-here my-avatar))))
    (if (not person)
        (tell! (list "There is no one called" name "here")
               my-avatar))
    person))

(define (find-thing name person-or-place)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell! (cons* "There is nothing called"
                      name
                      (person-or-place-name person-or-place))
               my-avatar))
    thing))

(define (person-or-place-things person-or-place)
  (if (place? person-or-place)
      (all-things-in-place person-or-place)
      (get-things person-or-place)))

(define (person-or-place-name person-or-place)
  (if (place? person-or-place)
      '("here")
      (list "in" (local-possessive person-or-place) "bag")))

(define (local-possessive person)
  (if (eqv? person my-avatar)
      "Your"
      (possessive person)))

(define (create-mit)
  (let ((great-dome (create-place 'great-dome))
        (little-dome (create-place 'little-dome))
        (lobby-10 (create-place 'lobby-10))
        (10-250 (create-place '10-250))
        (barker-library (create-place 'barker-library))
        (lobby-7 (create-place 'lobby-7))
        (infinite (create-place 'infinite-corridor))

        (bldg-26 (create-place 'bldg-26))
        (cp32 (create-place 'bldg-32-cp-hq))
        (tunnel (create-place 'lab-supplies))

        (32-123 (create-place '32-123))
        (32G (create-place 'gates-tower))
        (32D (create-place 'dreyfoos-tower))
        (student-street (create-place 'student-street))
        (great-court (create-place 'great-court))
        (bldg-54 (create-place 'green-building))
        (the-dot (create-place 'the-dot))
        (dorm-row (create-place 'dorm-row)))

    (can-go-both-ways lobby-10 'up 'down 10-250)
    (can-go-both-ways 10-250 'up 'down barker-library)
    (can-go-both-ways barker-library 'up 'down great-dome)
    (can-go-both-ways lobby-10 'west 'east lobby-7)
    (can-go-both-ways lobby-7 'west 'east dorm-row)
    (can-go-both-ways lobby-7 'up 'down little-dome)
    (can-go-both-ways lobby-10 'south 'north great-court)
    (can-go-both-ways lobby-10 'east 'west infinite)
    (can-go-both-ways infinite 'north 'south bldg-26)
    (can-go-both-ways infinite 'east 'west bldg-54)
    (can-go-both-ways bldg-26 'east 'west student-street)
    (can-go-both-ways student-street 'down 'up cp32)
    (can-go-both-ways cp32 'south 'north tunnel)
    (can-go-both-ways tunnel 'up 'down bldg-54)
    (can-go-both-ways bldg-54 'south 'north the-dot)
    (can-go-both-ways the-dot 'west 'east great-court)
    (can-go-both-ways student-street 'in 'out 32-123)
    (can-go-both-ways student-street 'up 'down 32G)
    (can-go-both-ways student-street 'skew 'down 32D)

    ; Add line-of-sight into the mix
    (can-see bldg-54 32G)
    (can-see bldg-54 32D)
    (can-see bldg-54 great-dome)
    (can-see bldg-54 little-dome)
    (can-see bldg-54 great-court)
    (can-see bldg-54 the-dot)
    (can-see lobby-10 great-court)
    (can-see great-dome great-court)
    (can-see-both-ways 32D 32G)
    (can-see-both-ways great-dome little-dome)
    (can-see-both-ways lobby-10 infinite)
    (can-see-both-ways lobby-7 infinite)
    (can-see-both-ways infinite bldg-26)
    (can-see-both-ways lobby-10 lobby-7)

    ; Create some things
    (create-thing 'blackboard 10-250)
    (create-thing 'lovely-trees great-court)
    (create-thing 'flag-pole great-court)
    (create-thing 'calder-sculpture the-dot)
    (create-mobile-thing 'problem-set 32-123)
    (create-mobile-thing 'recitation-problem 32-123)
    (create-mobile-thing 'sicp student-street)
    (create-mobile-thing 'engineering-book barker-library)

    (list great-dome little-dome lobby-10
          10-250 barker-library lobby-7
          infinite bldg-26 cp32
          tunnel 32-123 32D 32G
          student-street bldg-54 the-dot
          dorm-row)))

(define (create-people places)
  (append (create-students places)
          ;;(create-profs places)
          ;;(create-president places)
          (create-house-masters places)
          (create-trolls places)))

(define (create-students places)
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

;; (define (create-profs places)
;;   (map (lambda (name)
;;          (create-professor name
;;                            (random-choice places)
;;                            1/3
;;                            1/3))
;;        '(rob-miller eric-grimson)))

;; (define (create-president places)
;;   (create-president 'rafael-reif
;;                     (random-choice places)
;;                     (random-bias 3)
;;                     (random-bias 3)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-place name)
  (make-place 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name place restlessness hunger)
  (make-troll 'name name
              'location place
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name place)
  (make-avatar 'name name
               'location place
               'screen (make-screen 'name 'the-screen)))

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))