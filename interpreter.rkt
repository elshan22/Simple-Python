#lang racket

(require "eopl_errors.rkt")
(require "environment.rkt")
(require "datatypes.rkt")
(require "parser.rkt")
(require "lexer.rkt")
(require (lib "eopl.ss" "eopl"))


(define lex (lambda (lexer input) (lambda () (lexer input))))
(define run (lambda (str) (value-of-program (full-parser (lex full-lexer (open-input-string str))))))

(define value-of-program (lambda (pgm) (and (initialize-store!) (cases program pgm
                                         (a-program (sttmnts) (value-of-statements sttmnts (empty-env) (empty-store)))
                                         (else (report-no-program!))))))

(define value-of-statements (lambda (sttmnts env) (cases statements sttmnts
                                                    (a-statement (sttmnt) (value-of-statement sttmnt env))
                                                    (some-statements (sttmnts sttmnt) (let ((res (value-of-statements sttmnts env))) (value-of-statement sttmnt (cdr res)))))))

(define value-of-statement (lambda (sttmnt env) (cases statement sttmnt
                                                  (compound-statement (cmp) (value-of-compound cmp env))
                                                  (simple-statement (smpl) (value-of-simple smpl env)))))

(define value-of-simple (lambda (smpl env) (cases simple smpl
                                             (assignment-statement (assign) (value-of-assignment assign env))
                                             (global-statement (glbl) (value-of-global glbl env))
                                             (return-statement (rtrn) (value-of-return rtrn env))
                                             (pass-statement () (list 'pass env))
                                             (break-statement () (list 'break env))
                                             (continue-statement () (list 'continue env)))))

(define value-of-compound (lambda (cmp env) (cases compound cmp
                                              (function-definition (func) (value-of-function func env))
                                              (if-statement (if-stmt) (value-of-if if-stmt env))
                                              (for-statement (for-stmt) (value-of-for for-stmt env)))))