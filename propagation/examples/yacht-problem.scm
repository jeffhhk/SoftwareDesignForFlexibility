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

     Mary Ann Moore's father has a yacht and so has each of his
     four friends:  Colonel Downing, Mr. Hall, Sir Barnacle Hood,
     and Dr. Parker.  Each of the five also has one daughter and
     each has named his yacht after a daughter of one of the
     others.  Sir Barnacle's yacht is the Gabrielle, Mr. Moore
     owns the Lorna; Mr. Hall the Rosalind.  The Melissa, owned
     by Colonel Downing, is named after Sir Barnacle's daughter.
     Gabrielle's father owns the yacht that is named after Dr.
     Parker's daughter.  Who is Lorna's father?

|#
#|
(define (yacht-problem)
  ;; Each protagonist has a daughter; we know two assignments.
  (let-cells ((moore-d 'mary-ann)
              (hood-d 'melissa)
              downing-d
              hall-d
              parker-d)
    (let ((unassigned-daughters '(gabrielle lorna rosalind)))
      (p:amb downing-d unassigned-daughters)
      (p:amb hall-d unassigned-daughters)
      (p:amb parker-d unassigned-daughters)
      (require-distinct (list downing-d hall-d parker-d)))
      ;; Each has a yacht, named after a daughter of another.
      ;; We know the names of the yachts for each protagonist.
    (let-cells ((moore-y 'lorna)
                (hood-y 'gabrielle)
                (downing-y 'melissa)
                (hall-y 'rosalind)
                (parker-y 'mary-ann))
       ;; We know that no yacht is named after its owner's
       ;; daughter.
       (let-cells ((moore-d=y #f)
                   (downing-d=y #f)
                   (hall-d=y #f)
                   (hood-d=y #f)
                   (parker-d=y #f))
         (p:eqv moore-d moore-y moore-d=y)
         (p:eqv downing-d downing-y downing-d=y)
         (p:eqv hall-d hall-y hall-d=y)
         (p:eqv hood-d hood-y hood-d=y)
         (p:eqv parker-d parker-y parker-d=y))
       ;; We know that Gabrielle's father's yacht is named
       ;; after Parker's daughter.  So Gabrielle's father must
       ;; be either Moore or Downing or Hall.  But if
       ;; Gabrielle's father is Moore then Moore's daughter is
       ;; Gabrielle.  But Moore's daughter is Mary Ann.
       (let ((gabrielle-fy parker-d))
         (let-cells ((gfy=my #f) gfy=dy gfy=hy
                     (gabrielle 'gabrielle)
                     dd=g gfy=dy&dd=g
                     hd=g gfy=hy&hd=g
                     (true #t))

           (p:eqv gabrielle-fy moore-y gfy=my)

           (p:eqv gabrielle-fy downing-y gfy=dy)
           (p:eqv downing-d gabrielle dd=g)
           (p:and gfy=dy dd=g gfy=dy&dd=g)

           (p:eqv gabrielle-fy hall-y gfy=hy)
           (p:eqv hall-d gabrielle hd=g)
           (p:and gfy=hy hd=g gfy=hy&hd=g)

           (p:or gfy=dy&dd=g gfy=hy&hd=g true))))

    (list moore-d downing-d hall-d hood-d parker-d)))
|#

;;; Kludge for testing without p:eqv

(define (yacht-problem)
  ;; Each protagonist has a daughter; we know two assignments.
  (let ((mary-ann 1) (melissa 2) (lorna 3) (gabrielle 4) (rosalind 5))
    (let-cells ((moore-d mary-ann)
                (hood-d melissa)
                downing-d
                hall-d
                parker-d)
      (let ((unassigned-daughters (list gabrielle lorna rosalind)))
        (p:amb downing-d unassigned-daughters)
        (p:amb hall-d unassigned-daughters)
        (p:amb parker-d unassigned-daughters)
        (require-distinct (list downing-d hall-d parker-d)))
               ;; Each has a yacht, named after a daughter of another.
               ;; We know the names of the yachts for each protagonist.
      (let-cells ((moore-y lorna)
                  (hood-y gabrielle)
                  (downing-y melissa)
                  (hall-y rosalind)
                  (parker-y mary-ann))
                 ;; We know that no yacht is named after its owner's
                 ;; daughter.
        (let-cells ((moore-d=y #f)
                    (downing-d=y #f)
                    (hall-d=y #f)
                    (hood-d=y #f)
                    (parker-d=y #f))
                   (p:= moore-d moore-y moore-d=y)
                   (p:= downing-d downing-y downing-d=y)
                   (p:= hall-d hall-y hall-d=y)
                   (p:= hood-d hood-y hood-d=y)
                   (p:= parker-d parker-y parker-d=y))
                 ;; We know that Gabrielle's father's yacht is named
                 ;; after Parker's daughter.  So Gabrielle's father must
                 ;; be either Moore or Downing or Hall.  But if
                 ;; Gabrielle's father is Moore then Moore's daughter is
                 ;; Gabrielle.  But Moore's daughter is Mary Ann.
          (let ((gabrielle-fy parker-d))
            (let-cells ((gfy=my #f) gfy=dy gfy=hy
                        (gabrielle gabrielle)
                        dd=g gfy=dy&dd=g
                        hd=g gfy=hy&hd=g
                        (true #t))

                       (p:= gabrielle-fy moore-y gfy=my)

                       (p:= gabrielle-fy downing-y gfy=dy)
                       (p:= downing-d gabrielle dd=g)
                       (p:and gfy=dy dd=g gfy=dy&dd=g)

                       (p:= gabrielle-fy hall-y gfy=hy)
                       (p:= hall-d gabrielle hd=g)
                       (p:and gfy=hy hd=g gfy=hy&hd=g)

                       (p:or gfy=dy&dd=g gfy=hy&hd=g true))))

      (list moore-d downing-d hall-d hood-d parker-d))))

#|
(install-arithmetic! layered-arith)     ;debugging
(install-core-propagators! merge-value-sets
                           layered-arith
                           layered-propagator-projector)


(initialize-scheduler)

(define answers (yacht-problem))
(run)
(map (lambda (cell)
       (get-base-value (cell-strongest cell)))
     answers)
;Value: (1 3 4 2 5)
;; correct!
;; (1 3 4 2 5) = (mary-ann lorna gabrielle melissa rosalind)
|#
