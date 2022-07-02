#lang racket

(require "eopl_errors.rkt")

(define empty-env (lambda () (lambda (var) (report-no-binding-found! var))))

(define apply-env (lambda (var env) (env var)))

(define extend-env (lambda (new-var new-val env) (lambda (var) (if (equal? var new-var) new-val (apply-env var env)))))


;--------------------------------------------------------

(define the-store 'uninitialized)

(define empty-store (lambda () '()))

(define initialize-store! (lambda () (set! the-store (empty-store))))

(define get-store (lambda () the-store))

(define newref (lambda (val) (let ((ref (length the-store))) (and (set! the-store (append the-store (list val))) ref))))

(define deref (lambda (ref) (let ((size (length the-store))) (if (< ref size) (list-ref the-store ref) (report-invalid-reference!)))))

(define setref! (lambda (ref val) (let ((size (length the-store))) (if (< ref size) (set! the-store (update ref val the-store '())) (report-invalid-reference!)))))

(define update (lambda (num val store current) (cond [(= 0 num) (append current (list val) (cdr store))]
                                             [else (update (- num 1) val (cdr store) (append current (list (car store))))])))
  
(provide (all-defined-out))