#lang racket

(require (lib "eopl.ss" "eopl"))
(require "datatypes.rkt")

(define var->string (lambda (var) (cases ATOM var
                                      (id-exp (name) name)
                                      (else ""))))

(define (report-no-binding-found! var) (eopl:error 'binding-dismatch "\n\tidentifier ~s is used before its declaration!" (var->string var)))

(define (report-invalid-reference!) (eopl:error 'invalid-reference "\n\tillegal reference to memory!"))

(define (report-no-program!) (eopl:error 'bad-syntax "\n\tthe given string is not a valid program to execute!"))

(provide (all-defined-out))