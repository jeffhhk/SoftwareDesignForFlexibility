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

#|
|| SICP Section 4.3.2
|| Logic Puzzles
||
|| Baker, Cooper, Fletcher, Miller, and Smith live on
|| different floors of a building that has only five
|| floors.  Baker does not live on the top floor.
|| Cooper does not live on the bottom floor.  Fletcher
|| does not live on either the top or the bottom
|| floor.  Miller lives on a higher floor than does
|| Cooper.  Smith does not live on a floor adjacent to
|| Fletcher's.  Fletcher does not live on a floor
|| adjacent to Cooper's.  Where does everyone live?
||
||	(From Dinesman, 1968)
|#

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct?
      (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require
     (not (= (abs (- smith fletcher)) 1)))
    (require
     (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(init-amb)

(with-depth-first-schedule multiple-dwelling)
'expect-value: '((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

(amb)
'expect-value: 'no-more-alternatives

;;; From SICP Section 4.3.2
;;; Parsing natural language

(define (parse input)
  (maybe-set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define *unparsed* '())

(define (parse-sentence)
  (let* ((np (parse-noun-phrase))
	 (verb (parse-verb-phrase)))
    (list 'sentence np verb)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
	  (list 'noun-phrase
		noun-phrase
		(parse-prepositional-phrase)))))
  (maybe-extend (parse-s-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
	  (list 'verb-phrase
		verb-phrase
		(parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-s-noun-phrase)
  (let* ((article (parse-word articles))
	 (noun (parse-word nouns)))
    (list 's-noun-phrase article noun)))

(define (parse-prepositional-phrase)
  (let* ((preposition
	  (parse-word prepositions))
	 (np (parse-noun-phrase)))
    (list 'prep-phrase preposition np)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
		 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (maybe-set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define nouns
  '(noun student professor cat class))

(define verbs
  '(verb studies lectures eats sleeps))

(define articles
  '(article the a))

(define prepositions
  '(prep for to in by with))


(init-amb)

(parse '(the student with the cat sleeps in the class))
'expect-value:
'(sentence
  (noun-phrase
   (s-noun-phrase (article the) (noun student))
   (prep-phrase (prep with)
                (s-noun-phrase (article the)
                               (noun cat))))
  (verb-phrase
   (verb sleeps)
   (prep-phrase (prep in)
                (s-noun-phrase (article the)
                               (noun class)))))

(amb)
'expect-value: 'no-more-alternatives

(init-amb)

(parse
  '(the professor lectures
	to the student with the cat))
'expect-value:
'(sentence
  (s-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase (prep to)
                 (s-noun-phrase (article the)
                                (noun student))))
   (prep-phrase (prep with)
                (s-noun-phrase (article the)
                               (noun cat)))))

(amb)
'expect-value:
'(sentence
  (s-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (s-noun-phrase (article the)
                    (noun student))
     (prep-phrase (prep with)
                  (s-noun-phrase (article the)
                                 (noun cat)))))))

(amb)
'expect-value: 'no-more-alternatives
