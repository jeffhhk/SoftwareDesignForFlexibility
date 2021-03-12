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

(define (noisy-infer-program-types expr)
  (let ((texpr (annotate-program expr)))
    (pp (simplify-annotated-program texpr))
    (let ((constraints (program-constraints texpr)))
      (for-each pp constraints)
      (let ((dict (unify-constraints constraints)))
        (if dict
            ((match:dict-substitution dict) texpr)
            '***type-error***)))))

(define (infer-program-types expr)
  (let ((texpr (annotate-program expr)))
    (let ((constraints (program-constraints texpr)))
      (let ((dict (unify-constraints constraints)))
        (if dict
            ((match:dict-substitution dict) texpr)
            '***type-error***)))))

(define (annotate-program expr)
  (annotate-expr expr (top-level-env)))

(define annotate-expr
  (simple-generic-procedure 'annotate-expr 2 #f))

(define (top-level-env)
  (list (make-top-level-env-frame)))

(define (make-top-level-env-frame)
  (let ((binary-numerical
         (let ((v (numeric-type)))
           (procedure-type (list v v) v)))
        (binary-comparator
         (let ((v (numeric-type)))
           (procedure-type (list v v) (boolean-type)))))
    (list (cons '+ binary-numerical)
          (cons '- binary-numerical)
          (cons '* binary-numerical)
          (cons '/ binary-numerical)
          (cons '= binary-comparator)
          (cons '< binary-comparator)
          (cons '> binary-comparator)
          (cons '<= binary-comparator)
          (cons '>= binary-comparator))))

#|
;; coderef: make-top-level-env-frame
(define (make-top-level-env-frame)
  (let ((binary-numerical
         (let ((v (numeric-type)))
           (procedure-type (list v v) v)))
        (binary-comparator
         (let ((v (numeric-type)))
           (procedure-type (list v v) (boolean-type)))))
    (list (cons '+ binary-numerical)
          ...
          (cons '= binary-comparator)
          (cons '< binary-comparator)
          ...)))
|#

(define (new-frame names env)
  (cons (map make-type-cell names)
        env))

(define (get-var-type name env)
  (cdr (let loop ((env env))
         (or (assq name (car env))
             (if (pair? (cdr env))
                 (loop (cdr env))
                 (let ((tcell (make-type-cell name)))
                   (set-car! env (cons tcell (car env)))
                   tcell))))))

(define (make-type-cell name)
  (cons name (type-variable name)))

(define (define-var-type name env)
  (let ((p (assq name (car env))))
    (if p
        (error "Can't redefine name:" name)))
  (let ((tcell (make-type-cell name)))
    (set-car! env (cons tcell (car env)))
    (cdr tcell)))

(define (program-constraints texpr)
  (program-constraints-1 (texpr-type texpr)
                         (texpr-expr texpr)))

(define program-constraints-1
  (simple-generic-procedure 'program-constraints-1 2 #f))

(define (constrain lhs rhs)
  `(= ,lhs ,rhs))

(define constraint-lhs cadr)
(define constraint-rhs caddr)

#|
(define (unify-constraints constraints)
  (let loop ((constraints constraints) (dict (match:new-dict)))
    (if (pair? constraints)
        (let ((dict* (unify-constraint (car constraints) dict)))
          (and dict*
               (loop (cdr constraints) dict*)))
        dict)))

(define (unify-constraint constraint dict)
  (let ((subst (match:dict-substitution dict)))
    (unify:internal (subst (constraint-lhs constraint))
                    (subst (constraint-rhs constraint))
                    dict
                    (lambda (dict) dict))))
|#

;;; combines all equations into one giant equation list

(define (unify-constraints constraints)
  (unify (map constraint-lhs constraints)
         (map constraint-rhs constraints)))

(define (simplify-annotated-program texpr)
  (simplify-annotated-program-1 (texpr-type texpr)
                                (texpr-expr texpr)))

(define simplify-annotated-program-1
  (simple-generic-procedure 'simplify-annotated-program-1 1 #f))

;;;; Typed expressions

(define (make-texpr type expr)
  `(t ,type ,expr))

(define (texpr? object)
  (and (list? object)
       (= (length object) 3)
       (eq? (car object) 't)
       (type-expression? (cadr object))))

(define (texpr-type expr)
  (cadr expr))

(define (texpr-expr expr)
  (caddr expr))

;;;; Type Expressions

(define (type-expression? object)
  (or (type-variable? object)
      (primitive-type? object)
      (parametric-type? object)))

(define (type-variable? object)
  (and (pair? object)
       (eq? (car object) '?)
       (pair? (cdr object))
       (symbol? (cadr object))
       (or (null? (cddr object))
           (and (pair? (cddr object))
                (symbol? (caddr object))
                (null? (cdddr object))))))

;; TODO: merge with pattern variable definitions
(define (type-variable #!optional root)
  `(? ,(generate-unique-name
        (if (default-object? root)
            'type
            root))))

(define generate-unique-name
  (let ((n 0))
    (lambda (prefix)
      (set! n (+ n 1))
      (symbol prefix ":" n))))

(define (type-variable-name variable)
  (cadr variable))

(define (primitive-type? object)
  (any (lambda (pred)
         (pred object))
       primitive-predicates))

(define primitive-predicates '())

(define (primitive-type name)

  (define (constructor)
    (list name))

  (define (predicate object)
    (and (pair? object)
         (eq? name (car object))
         (null? (cdr object))))

  (guarantee symbol? name)
  (set! primitive-predicates
        (cons predicate primitive-predicates))
  (values constructor predicate))

(define bottom-type)
(define bottom-type?)
(receive (constructor predicate) (primitive-type 'bottom-type)
  (set! bottom-type constructor)
  (set! bottom-type? predicate))

(define boolean-type)
(define boolean-type?)
(receive (constructor predicate) (primitive-type 'boolean-type)
  (set! boolean-type constructor)
  (set! boolean-type? predicate))

(define numeric-type)
(define numeric-type?)
(receive (constructor predicate) (primitive-type 'numeric-type)
  (set! numeric-type constructor)
  (set! numeric-type? predicate))

(define (parametric-type? object)
  (and (pair? object)
       (memq (car object) parametric-type-operators)
       (list? (cdr object))
       ;; Fails on procedure-type:
       ;;(every type-expression? (cdr object))
       ))

(define parametric-type-operators '())

(define (define-parametric-type-operator operator)
  (guarantee symbol? operator)
  (set! parametric-type-operators
        (cons operator parametric-type-operators))
  (values (lambda operands
            (cons operator operands))
          (parametric-type-predicate operator)))

(define (parametric-type-operator param)
  (car param))

(define (parametric-type-operands param)
  (cdr param))

(define ((parametric-type-predicate operator) expr)
  (and (parametric-type? expr)
       (eq? (parametric-type-operator expr) operator)))

(define procedure-type)
(define procedure-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:procedure)
  (set! procedure-type constructor)
  (set! procedure-type? predicate))

(define (procedure-type-domains expr)
  (car (parametric-type-operands expr)))

(define (procedure-type-codomain expr)
  (cadr (parametric-type-operands expr)))

;;;; Expressions

;; coderef: annotate-boolean
(define-generic-procedure-handler annotate-expr
  (match-args boolean? any-object?)
  (lambda (expr env)
    (make-texpr (boolean-type) expr)))

;; coderef: annotate-number
(define-generic-procedure-handler annotate-expr
  (match-args number? any-object?)
  (lambda (expr env)
    (make-texpr (numeric-type) expr)))

;; coderef: annotate-symbol
(define-generic-procedure-handler annotate-expr
  (match-args symbol? any-object?)
  (lambda (expr env)
    (make-texpr (get-var-type expr env) expr)))

(define-generic-procedure-handler program-constraints-1
  (match-args type-expression?
              (disjoin boolean? number? symbol?))
  (lambda (type expr)
    '()))

(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression?
              (disjoin boolean? number? symbol?))
  (lambda (type expr)
    expr))

(define (if-expr? object)
  (and (list? object)
       (= (length object) 4)
       (eq? (car object) 'if)))

(define if-predicate cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define (make-if-expr predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; coderef: annotate-if
(define-generic-procedure-handler annotate-expr
  (match-args if-expr? any-object?)
  (lambda (expr env)
    (make-texpr (type-variable)
                (make-if-expr
                 (annotate-expr (if-predicate expr) env)
                 (annotate-expr (if-consequent expr) env)
                 (annotate-expr (if-alternative expr) env)))))

;; coderef: constrain-if
(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? if-expr?)
  (lambda (type expr)
    (append
     (list (constrain (boolean-type)
                      (texpr-type (if-predicate expr)))
           (constrain type
                      (texpr-type (if-consequent expr)))
           (constrain type
                      (texpr-type (if-alternative expr))))
     (program-constraints (if-predicate expr))
     (program-constraints (if-consequent expr))
     (program-constraints (if-alternative expr)))))

;; coderef: simplify-if
(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression? if-expr?)
  (lambda (type expr)
    (make-if-expr
     (simplify-annotated-program (if-predicate expr))
     (simplify-annotated-program (if-consequent expr))
     (simplify-annotated-program (if-alternative expr)))))

(define (lambda-expr? object)
  (and (list? object)
       (= (length object) 3)
       (eq? (car object) 'lambda)
       (bvl? (cadr object))))

(define (bvl? object)
  (list-of-unique-symbols? object))

(define lambda-bvl cadr)
(define lambda-body caddr)

(define (make-lambda-expr bvl body)
  (list 'lambda bvl body))

;; coderef: annotate-lambda
(define-generic-procedure-handler annotate-expr
  (match-args lambda-expr? any-object?)
  (lambda (expr env)
    (let ((env* (new-frame (lambda-bvl expr) env)))
      (make-texpr (procedure-type (map (lambda (name)
                                         (get-var-type name env*))
                                       (lambda-bvl expr))
                                  (type-variable))
                  (make-lambda-expr (lambda-bvl expr)
                    (annotate-expr (lambda-body expr) env*))))))

;; coderef: constrain-lambda
(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? lambda-expr?)
  (lambda (type expr)
    (cons (constrain (procedure-type-codomain type)
                     (texpr-type (lambda-body expr)))
          (program-constraints (lambda-body expr)))))

;; coderef: simplify-lambda
(define-generic-procedure-handler simplify-annotated-program-1
  (match-args procedure-type? lambda-expr?)
  (lambda (type expr)
    `(lambda ,(lambda-bvl expr)
       ,@(map declare-type-expr
              (lambda-bvl expr)
              (procedure-type-domains type))
       ,@(splice-begin
          (simplify-annotated-program (lambda-body expr))))))

(define (combination-expr? object)
  (and (list? object)
       (>= (length object) 1)
       (not (memq (car object) '(if lambda define begin)))))

(define combination-operator car)
(define combination-operands cdr)

(define (make-combination-expr operator operands)
  (cons operator operands))

;; coderef: annotate-combination
(define-generic-procedure-handler annotate-expr
  (match-args combination-expr? any-object?)
  (lambda (expr env)
    (make-texpr (type-variable)
                (make-combination-expr
                 (annotate-expr (combination-operator expr) env)
                 (map (lambda (operand)
                        (annotate-expr operand env))
                      (combination-operands expr))))))

;; coderef: constrain-combination
(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? combination-expr?)
  (lambda (type expr)
    (cons (constrain (texpr-type (combination-operator expr))
                     (procedure-type 
                      (map texpr-type
                           (combination-operands expr))
                      type))
          (append (program-constraints (combination-operator expr))
                  (append-map program-constraints
                              (combination-operands expr))))))

;; coderef: simplify-combination
(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression? combination-expr?)
  (lambda (type expr)
    (make-combination-expr
     (simplify-annotated-program (combination-operator expr))
     (map simplify-annotated-program
          (combination-operands expr)))))

(define (define-expr? object)
  (and (list? object)
       (= (length object) 3)
       (eq? (car object) 'define)
       (symbol? (cadr object))))

(define define-name cadr)
(define define-value caddr)

(define (make-define-expr name value)
  (list 'define name value))

(define-generic-procedure-handler annotate-expr
  (match-args define-expr? any-object?)
  (lambda (expr env)
    (let ((name (define-name expr)))
      (let ((type (define-var-type name env)))
        (make-texpr type
                    (make-define-expr name
                                      (annotate-expr
                                       (define-value expr)
                                       env)))))))

(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? define-expr?)
  (lambda (type expr)
    (cons (constrain type (texpr-type (define-value expr)))
          (program-constraints (define-value expr)))))

(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression? define-expr?)
  (lambda (type expr)
    (make-begin-expr
     (list
      (make-define-expr (define-name expr)
        (simplify-annotated-program (define-value expr)))
      (declare-type-expr (define-name expr)
                         (texpr-type (define-value expr)))))))

(define (begin-expr? object)
  (and (list? object)
       (> (length object) 2)
       (eq? (car object) 'begin)))

(define begin-exprs cdr)

(define (make-begin-expr exprs)
  (cons* 'begin exprs))

(define-generic-procedure-handler annotate-expr
  (match-args begin-expr? any-object?)
  (lambda (expr env)
    (make-texpr (type-variable)
                (make-begin-expr
                 (map (lambda (subexpr)
                        (annotate-expr subexpr env))
                      (begin-exprs expr))))))

(define-generic-procedure-handler program-constraints-1
  (match-args type-expression? begin-expr?)
  (lambda (type expr)
    (cons (constrain type (texpr-type (last (begin-exprs expr))))
          (append-map program-constraints
                      (begin-exprs expr)))))

(define-generic-procedure-handler simplify-annotated-program-1
  (match-args type-expression? begin-expr?)
  (lambda (type expr)
    (make-begin-expr
     (append-map (lambda (x)
                   (splice-begin (simplify-annotated-program x)))
                 (begin-exprs expr)))))

(define (splice-begin s)
  (if (begin-expr? s)
      (begin-exprs s)
      (list s)))

(define (declare-type-expr name type)
  (list 'declare-type name type))

;;; Arithmetic types
#|
(define symbolic-type)
(define symbolic-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:symbolic)
  (set! symbolic-type constructor)
  (set! symbolic-type? predicate))

(define (symbolic-type-base expr)
  (car (parametric-type-operands expr)))

(define function-type)
(define function-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:function)
  (set! function-type constructor)
  (set! function-type? predicate))

(define (function-type-domain expr)
  (car (parametric-type-operands expr)))

(define (function-type-codomain expr)
  (cadr (parametric-type-operands expr)))

(define %product-type)
(define product-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:product)
  (set! %product-type constructor)
  (set! product-type? predicate))

(define (product-type . factors)
  (if (= (length factors) 1)
      (car factors)
      (apply %product-type factors)))

(define (product-type-factors expr)
  (parametric-type-operands expr))

(define %union-type)
(define union-type?)
(receive (constructor predicate)
    (define-parametric-type-operator 'type:union)
  (set! %union-type constructor)
  (set! union-type? predicate))

(define (union-type . terms)
  (if (= (length terms) 1)
      (car terms)
      (apply %union-type terms)))

(define (union-type-terms expr)
  (parametric-type-operands expr))
|#

#|
(define +-type
  (function-type (product-type (numeric-type) (numeric-type))
                 (numeric-type)))

(define +-type
  (let ((v (type-variable)))
    (function-type (product-type v v)
                   v)))

(define example-type-1
  (union-type (function-type (numeric-type) (numeric-type))
              (function-type (symbolic-type (numeric-type))
                             (symbolic-type (numeric-type)))))

(define example-type-2
  (function-type (union-type (numeric-type)
                             (symbolic-type (numeric-type)))
                 (symbolic-type (numeric-type))))


(define example-type-3
  (application-type +-type
                    (product-type example-type-1
                                  example-type-2)))

(define example-type-1
  (let ((v (type-variable)))
    (function-type v v)))

(define example-type-2
  (let ((v (type-variable)))
    (function-type (union-type v (symbolic-type v))
                   (symbolic-type v))))

(let* ((n (numeric-type))
       (s (symbolic-type n))
       (n+s (union-type n s))
       (f (function-type (type-variable) n+s)))
  (union-type (function-type n+s n+s)
              (function-type f f)))

(function-type (numeric-type) (numeric-type))
(let ((v (type-variable)))
  (function-type (symbolic-type v)
                 (symbolic-type v)))
(let ((x
       (function-type (type-variable)
                      (union (numeric-type)
                             (symbolic-type (type-variable))))))
  (function-type x x))


((+ cos (literal-function 'f)) 3)

(<= <v1> <v1>)

(iff (<= <v1> <v3>)
     (<= (function-type <v1> <v2>)
         (function-type <v3> <v2)))

(iff (<= <v1> <v3>)
     (<= (function-type <v2> <v3)
         (function-type <v2> <v1>)))

(if (<= (union <v1> <v2>) <v3>)
    (<= <v1> <v3>))

|#

;;;; examples:

#|
(pp (noisy-infer-program-types '((lambda (x y) (+ x y)) 3 4)))
(t
 (? type:8)
 ((t
   (type:procedure (? param:7) ((? x:3) (? y:4)) (? type:6))
   (lambda (x y)
     (t (? type:5)
        ((t (type:procedure (? param:2) ((? type:1) (? type:1)) (? type:1)) +) (t (? x:3) x) (t (? y:4) y)))))
  (t (numeric-type) 3)
  (t (numeric-type) 4)))
(= (type:procedure (? param:7) ((? x:3) (? y:4)) (? type:6))
   (type:procedure (? param:10) ((numeric-type) (numeric-type)) (? type:8)))
(= (? type:6) (? type:5))
(= (type:procedure (? param:2) ((? type:1) (? type:1)) (? type:1))
   (type:procedure (? param:9) ((? x:3) (? y:4)) (? type:5)))
(t
 (numeric-type)
 ((t
   (type:procedure (? param:10) ((numeric-type) (numeric-type)) (numeric-type))
   (lambda (x y)
     (t
      (numeric-type)
      ((t (type:procedure (? param:9) ((numeric-type) (numeric-type)) (numeric-type)) +) (t (numeric-type) x)
                                                                                (t (numeric-type) y)))))
  (t (numeric-type) 3)
  (t (numeric-type) 4)))
;Unspecified return value
|#

#|
(define fib-program
  '(define fib
     (lambda (n)
       (if (< n 2)
           n
           (+ (fib (- n 1))
              (fib (- n 2)))))))

(pp (noisy-infer-program-types fib-program))
(t
 (? fib:30)
 (define fib
   (t
    (type:procedure (? param:40) ((? n:31)) (? type:39))
    (lambda (n)
      (t
       (? type:38)
       (if (t
            (? type:37)
            ((t (type:procedure (? param:25) ((? type:24) (? type:24)) (boolean-type)) <) (t (? n:31) n)
                                                                                     (t (numeric-type) 2)))
           (t (? n:31) n)
           (t
            (? type:36)
            ((t (type:procedure (? param:29) ((? type:28) (? type:28)) (? type:28)) +)
             (t
              (? type:33)
              ((t (? fib:30) fib)
               (t
                (? type:32)
                ((t (type:procedure (? param:27) ((? type:26) (? type:26)) (? type:26)) -) (t (? n:31) n)
                                                                                        (t (numeric-type) 1)))))
             (t
              (? type:35)
              ((t (? fib:30) fib)
               (t
                (? type:34)
                ((t (type:procedure (? param:27) ((? type:26) (? type:26)) (? type:26)) -)
                 (t (? n:31) n)
                 (t (numeric-type) 2)))))))))))))
(= (? fib:30) (type:procedure (? param:40) ((? n:31)) (? type:39)))
(= (? type:39) (? type:38))
(= (boolean-type) (? type:37))
(= (? type:38) (? n:31))
(= (? type:38) (? type:36))
(= (type:procedure (? param:25) ((? type:24) (? type:24)) (boolean-type))
   (type:procedure (? param:46) ((? n:31) (numeric-type)) (? type:37)))
(= (type:procedure (? param:29) ((? type:28) (? type:28)) (? type:28))
   (type:procedure (? param:45) ((? type:33) (? type:35)) (? type:36)))
(= (? fib:30) (type:procedure (? param:44) ((? type:32)) (? type:33)))
(= (type:procedure (? param:27) ((? type:26) (? type:26)) (? type:26))
   (type:procedure (? param:43) ((? n:31) (numeric-type)) (? type:32)))
(= (? fib:30) (type:procedure (? param:42) ((? type:34)) (? type:35)))
(= (type:procedure (? param:27) ((? type:26) (? type:26)) (? type:26))
   (type:procedure (? param:41) ((? n:31) (numeric-type)) (? type:34)))
(t
 (type:procedure (? param:42) ((numeric-type)) (numeric-type))
 (define fib
   (t
    (type:procedure (? param:42) ((numeric-type)) (numeric-type))
    (lambda (n)
      (t
       (numeric-type)
       (if (t
            (boolean-type)
            ((t (type:procedure (? param:46) ((numeric-type) (numeric-type)) (boolean-type)) <) (t (numeric-type) n)
                                                                                       (t (numeric-type) 2)))
           (t (numeric-type) n)
           (t
            (numeric-type)
            ((t (type:procedure (? param:45) ((numeric-type) (numeric-type)) (numeric-type)) +)
             (t
              (numeric-type)
              ((t (type:procedure (? param:42) ((numeric-type)) (numeric-type)) fib)
               (t
                (numeric-type)
                ((t (type:procedure (? param:41) ((numeric-type) (numeric-type)) (numeric-type)) -) (t (numeric-type) n)
                                                                                           (t (numeric-type) 1)))))
             (t
              (numeric-type)
              ((t (type:procedure (? param:42) ((numeric-type)) (numeric-type)) fib)
               (t
                (numeric-type)
                ((t (type:procedure (? param:41) ((numeric-type) (numeric-type)) (numeric-type)) -)
                 (t (numeric-type) n)
                 (t (numeric-type) 2)))))))))))))
;Unspecified return value
|#

#|
(define fib-internal
  '(lambda (m)
     (begin
       (define fib
         (lambda (n)
           (if (< n 2)
               n
               (+ (fib (- n 1))
                  (fib (- n 2))))))
       (fib m))))

(pp (noisy-infer-program-types fib-internal))
(t
 (type:procedure (? param:22) ((? m:7)) (? type:21))
 (lambda (m)
   (t
    (? type:20)
    (begin
     (t
      (? fib:8)
      (define fib
        (t
         (type:procedure (? param:18) ((? n:9)) (? type:17))
         (lambda (n)
           (t
            (? type:16)
            (if (t (? type:15) ((t (type:procedure (? param:2) ((? type:1) (? type:1)) (boolean-type)) <) (t (? n:9) n) (t (numeric-type) 2)))
                (t (? n:9) n)
                (t
                 (? type:14)
                 ((t (type:procedure (? param:6) ((? type:5) (? type:5)) (? type:5)) +)
                  (t (? type:11) ((t (? fib:8) fib) (t (? type:10) ((t (type:procedure (? param:4) ((? type:3) (? type:3)) (? type:3)) -) (t (? n:9) n) (t (numeric-type) 1)))))
                  (t (? type:13) ((t (? fib:8) fib) (t (? type:12) ((t (type:procedure (? param:4) ((? type:3) (? type:3)) (? type:3)) -) (t (? n:9) n) (t (numeric-type) 2)))))))))))))
     (t (? type:19) ((t (? fib:8) fib) (t (? m:7) m)))))))
(= (? type:21) (? type:20))
(= (? type:20) (? type:19))
(= (? fib:8) (type:procedure (? param:18) ((? n:9)) (? type:17)))
(= (? type:17) (? type:16))
(= (boolean-type) (? type:15))
(= (? type:16) (? n:9))
(= (? type:16) (? type:14))
(= (type:procedure (? param:2) ((? type:1) (? type:1)) (boolean-type)) (type:procedure (? param:29) ((? n:9) (numeric-type)) (? type:15)))
(= (type:procedure (? param:6) ((? type:5) (? type:5)) (? type:5)) (type:procedure (? param:28) ((? type:11) (? type:13)) (? type:14)))
(= (? fib:8) (type:procedure (? param:27) ((? type:10)) (? type:11)))
(= (type:procedure (? param:4) ((? type:3) (? type:3)) (? type:3)) (type:procedure (? param:26) ((? n:9) (numeric-type)) (? type:10)))
(= (? fib:8) (type:procedure (? param:25) ((? type:12)) (? type:13)))
(= (type:procedure (? param:4) ((? type:3) (? type:3)) (? type:3)) (type:procedure (? param:24) ((? n:9) (numeric-type)) (? type:12)))
(= (? fib:8) (type:procedure (? param:23) ((? m:7)) (? type:19)))
(t
 (type:procedure (? param:22) ((numeric-type)) (numeric-type))
 (lambda (m)
   (t
    (numeric-type)
    (begin
     (t
      (type:procedure (? param:23) ((numeric-type)) (numeric-type))
      (define fib
        (t
         (type:procedure (? param:23) ((numeric-type)) (numeric-type))
         (lambda (n)
           (t
            (numeric-type)
            (if (t (boolean-type) ((t (type:procedure (? param:29) ((numeric-type) (numeric-type)) (boolean-type)) <) (t (numeric-type) n) (t (numeric-type) 2)))
                (t (numeric-type) n)
                (t
                 (numeric-type)
                 ((t (type:procedure (? param:28) ((numeric-type) (numeric-type)) (numeric-type)) +)
                  (t (numeric-type) ((t (type:procedure (? param:23) ((numeric-type)) (numeric-type)) fib) (t (numeric-type) ((t (type:procedure (? param:24) ((numeric-type) (numeric-type)) (numeric-type)) -) (t (numeric-type) n) (t (numeric-type) 1)))))
                  (t (numeric-type)
                     ((t (type:procedure (? param:23) ((numeric-type)) (numeric-type)) fib) (t (numeric-type) ((t (type:procedure (? param:24) ((numeric-type) (numeric-type)) (numeric-type)) -) (t (numeric-type) n) (t (numeric-type) 2)))))))))))))
     (t (numeric-type) ((t (type:procedure (? param:23) ((numeric-type)) (numeric-type)) fib) (t (numeric-type) m)))))))
;Unspecified return value
|#

#|
(define fact-iterative
  '(define fact
     (lambda (n)
       (begin
         (define iter
           (lambda (product counter)
             (if (> counter n)
                 product
                 (iter (* product counter)
                       (+ counter 1)))))
         (iter 1 1)))))

(define foo
  (nosiy-infer-program-types fact-iterative))
(begin
 (define fact
   (lambda (n)
     (declare-type n (? n:2))
     (define iter
       (lambda (product counter)
         (declare-type product (? product:4))
         (declare-type counter (? counter:5))
         (if (> counter n)
             product
             (iter (* product counter) (+ counter 1)))))
     (declare-type iter (type:procedure ((? product:4) (? counter:5)) (? type:11)))
     (iter 1 1)))
 (declare-type fact (type:procedure ((? n:2)) (? type:14))))
(= (? fact:1) (type:procedure ((? n:2)) (? type:14)))
(= (? type:14) (? type:13))
(= (? type:13) (? type:12))
(= (? iter:3) (type:procedure ((? product:4) (? counter:5)) (? type:11)))
(= (? type:11) (? type:10))
(= (boolean-type) (? type:9))
(= (? type:10) (? product:4))
(= (? type:10) (? type:8))
(= (type:procedure ((numeric-type) (numeric-type)) (boolean-type)) (type:procedure ((? counter:5) (? n:2)) (? type:9)))
(= (? iter:3) (type:procedure ((? type:6) (? type:7)) (? type:8)))
(= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((? product:4) (? counter:5)) (? type:6)))
(= (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) (type:procedure ((? counter:5) (numeric-type)) (? type:7)))
(= (? iter:3) (type:procedure ((numeric-type) (numeric-type)) (? type:12)))
;Value: foo

(pp foo)
(t
 (type:procedure ((numeric-type)) (numeric-type))
 (define fact
   (t
    (type:procedure ((numeric-type)) (numeric-type))
    (lambda (n)
      (t
       (numeric-type)
       (begin
        (t
         (type:procedure ((numeric-type) (numeric-type)) (numeric-type))
         (define iter
           (t
            (type:procedure ((numeric-type) (numeric-type)) (numeric-type))
            (lambda (product counter)
              (t
               (numeric-type)
               (if (t
                    (boolean-type)
                    ((t (type:procedure ((numeric-type) (numeric-type)) (boolean-type)) >) (t (numeric-type) counter)
                                                                                  (t (numeric-type) n)))
                   (t (numeric-type) product)
                   (t
                    (numeric-type)
                    ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) iter)
                     (t
                      (numeric-type)
                      ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) *) (t (numeric-type) product)
                                                                                    (t (numeric-type) counter)))
                     (t
                      (numeric-type)
                      ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) +) (t (numeric-type) counter)
                                                                                    (t (numeric-type) 1)))))))))))
        (t
         (numeric-type)
         ((t (type:procedure ((numeric-type) (numeric-type)) (numeric-type)) iter) (t (numeric-type) 1)
                                                                          (t (numeric-type) 1)))))))))

(pp (simplify-annotated-program foo))
(begin
 (define fact
   (lambda (n)
     (declare-type n (numeric-type))
     (define iter
       (lambda (product counter)
         (declare-type product (numeric-type))
         (declare-type counter (numeric-type))
         (if (> counter n)
             product
             (iter (* product counter) (+ counter 1)))))
     (declare-type iter (type:procedure ((numeric-type) (numeric-type)) (numeric-type)))
     (iter 1 1)))
 (declare-type fact (type:procedure ((numeric-type)) (numeric-type))))
|#
