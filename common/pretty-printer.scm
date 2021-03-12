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

(define (pretty-print object #!optional port)
  (print-token (tokenize object)
               (make-cursor (if (default-object? port)
                                (current-output-port)
                                port))))

;; Data Printer
(define dp pretty-print)

(define space-width 1)

(define-record-type <simple-token>
    (%make-simple-token string)
    simple-token?
  (string simple-token-string))

(define (make-simple-token string)
  (guarantee string? string)
  (%make-simple-token string))

(define (simple-token-width token)
  (string-length (simple-token-string token)))

(define-record-type <compound-token>
    (%make-compound-token open-string close-string
                          separator-string contents)
    compound-token?
  (open-string compound-token-open-string)
  (close-string compound-token-close-string)
  (separator-string compound-token-separator-string)
  (contents compound-token-contents))

(define (make-compound-token open-string close-string
                             separator-string contents)
  (guarantee string? open-string)
  (guarantee string? close-string)
  (guarantee string? separator-string)
  (guarantee-list-of token? contents)
  (%make-compound-token open-string close-string separator-string
                        contents))

(define (compound-token-length token)
  (length (compound-token-contents token)))

(define (compound-token-width token)
  (+ (string-length (compound-token-open-string token))
     (string-length (compound-token-close-string token))
     (apply + (map token-width (compound-token-contents token)))
     ;; Assumes that separator is followed by one space.
     (* (+ (string-length
            (compound-token-separator-string token))
           space-width)
        (- (length (compound-token-contents token)) 1))))

(define (token? object)
  (or (simple-token? object)
      (compound-token? object)))

(define (token-width token)
  (if (simple-token? token)
      (simple-token-width token)
      (compound-token-width token)))

(define (token->tree token)
  (if (simple-token? token)
      (simple-token-string token)
      `(,(compound-token-open-string token)
        ,(compound-token-close-string token)
        ,(compound-token-separator-string token)
        ,@(map token->tree (compound-token-contents token)))))


(define tokenize
  (simple-generic-procedure 'tokenize 1
    (lambda (object)
      (make-simple-token (object->string object)))))

(define-generic-procedure-handler tokenize (match-args pair?)
  (lambda (p)
    (make-compound-token "(" ")" ""
      (let loop ((p p))
        (cons (tokenize (car p))
              (cond ((pair? (cdr p))
                     (loop (cdr p)))
                    ((null? (cdr p))
                     '())
                    (else
                     (list (make-simple-token ".")
                           (tokenize (cdr p))))))))))

(define (object->string object)
  (with-string-output-port
   (lambda (port)
     (write object port))))

(define-generic-procedure-handler tokenize (match-args null?)
  (lambda (n)
    (declare (ignore n))
    (make-compound-token "(" ")" "" '())))

(define-generic-procedure-handler tokenize (match-args vector?)
  (lambda (v)
    (make-compound-token "#(" ")" ""
                         (map tokenize (vector->list v)))))

(define (fits-in-width? token width)
  (<= (token-width token) width))

(define (fits-as-column? token width)
  (<= (if (simple-token? token)
          (simple-token-width token)
          (compound-token-width-as-column token))
      width))

(define (compound-token-width-as-column token)
  (if (= (compound-token-length token) 0)
      (compound-token-width token)
      (+ (string-length (compound-token-open-string token))
         (compute-column-width token
           (compound-token-contents token)))))

(define (fits-as-folded-column? token width)
  (<= (if (simple-token? token)
          (simple-token-width token)
          (compound-token-width-as-folded-column token))
      width))

(define (compound-token-width-as-folded-column token)
  (if (<= (compound-token-length token) 1)
      (compound-token-width token)
      (+ (string-length (compound-token-open-string token))
         (token-width (car (compound-token-contents token)))
         (string-length (compound-token-separator-string token))
         space-width
         (compute-column-width token
           (cdr (compound-token-contents token))))))

(define (compute-column-width token subtokens)
  (apply max
         (+ (token-width (last subtokens))
            (string-length (compound-token-close-string token)))
         (map (lambda (token*)
                (+ (token-width token*)
                   (string-length
                    (compound-token-separator-string token))))
              (except-last-pair subtokens))))

(define (print-token token cursor)
  (if (or (simple-token? token)
          (= (compound-token-length token) 0)
          (and (= (compound-token-length token) 1)
               (simple-token?
                (car (compound-token-contents token))))
          (fits-in-width? token (cursor-remaining-width cursor)))
      (print-token:flat token cursor)
      (print-compound-token:as-column token cursor)))

(define (print-token:flat token cursor)
  (if (simple-token? token)
      (cursor-write-string (simple-token-string token) cursor)
      (print-compound-token:flat token cursor)))

(define (print-open token cursor)
  (cursor-write-string (compound-token-open-string token)
                       cursor))

(define (print-close token cursor)
  (cursor-write-string (compound-token-close-string token)
                       cursor))

(define (print-separator token cursor)
  (cursor-write-string (compound-token-separator-string token)
                       cursor))

(define (print-compound-token:flat token cursor)
  (let ((cursor (print-open token cursor))
        (subtokens (compound-token-contents token)))
    (if (pair? subtokens)
        (let loop
            ((cursor (print-token:flat (car subtokens) cursor))
             (subtokens (cdr subtokens)))
          (if (pair? subtokens)
              (loop (print-token:flat (car subtokens)
                                      (cursor-write-space
                                       (print-separator token
                                                        cursor)))
                    (cdr subtokens))
              (cursor-write-string
               (compound-token-close-string token)
               cursor)))
        (print-close token cursor))))

(define (print-compound-token:as-column token cursor)
  (let* ((cursor (print-open token cursor))
         (target-column (cursor-column cursor)))
    (let loop
        ((cursor
          (print-token (car (compound-token-contents token))
                       cursor))
         (subtokens (cdr (compound-token-contents token))))
      (if (pair? subtokens)
          (loop (print-token (car subtokens)
                             (cursor-newline-indent target-column
                               (print-separator token cursor)))
                (cdr subtokens))
          (print-close token cursor)))))

(define-record-type <cursor>
    (%make-cursor row column port)
    cursor?
  (row cursor-row)
  (column cursor-column)
  (port cursor-port))

(define (make-cursor port)
  (%make-cursor 0 0 port))

(define (cursor-write-string string cursor)
  (write-string string (cursor-port cursor))
  (%make-cursor (cursor-row cursor)
                (+ (cursor-column cursor) (string-length string))
                (cursor-port cursor)))

(define (cursor-write-space cursor)
  (cursor-write-string " " cursor))

(define (cursor-remaining-width cursor)
  (- (output-port/x-size (cursor-port cursor))
     (cursor-column cursor)))

(define (cursor-newline-indent target-column cursor)
  (newline (cursor-port cursor))
  (for-each (lambda (i)
              (declare (ignore i))
              (write-char #\space (cursor-port cursor)))
            (iota target-column))
  (%make-cursor (+ (cursor-row cursor) 1)
                target-column
                (cursor-port cursor)))