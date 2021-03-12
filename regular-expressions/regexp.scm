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

;;;; Scheme Regular Expression Language Implementation -- regexp.scm

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:quote string)
  (r:seq
   (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

(define chars-needing-quoting
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  (append (optional #\])
          (remove (lambda (c)
                    (memv c chars-needing-quoting-in-brackets))
                  members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

;;; Means of combination for patterns

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . exprs)
  (if (pair? exprs)
      (apply r:seq
             (cons (car exprs)
                   (append-map (lambda (expr)
                                 (list "\\|" expr))
                               (cdr exprs))))
      (r:seq)))

(define (r:repeat-from-text min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list expr "*"))
                       ((= max min) '())
                       (else
                        (make-list (- max min)
                                   (r:alt expr "")))))))
(define r:repeat r:repeat-from-text)

;;; Using system's grep.
(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

;;; Works for any string without newlines.
(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))


;;; This is MIT/Scheme specific and compatible with grep for the
;;; purposes of this code.

(load-option 'synchronous-subprocess)

(define (r:grep expr filename)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-grep-command-string expr filename)
             'output port)
            0)
         (r:split-lines (get-output-string port)))))

(define (r:split-lines string)
  (reverse
   (let ((end (string-length string)))
     (let loop ((i 0) (lines '()))
       (if (< i end)
           (let ((j
                  (substring-find-next-char string i end #\newline)))
             (if j
                 (loop (+ j 1)
                       (cons (substring string i j) lines))
                 (cons (substring string i end) lines)))
           lines)))))
