#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require "lexer.rkt")

(define full-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (program ((statements) (list 'program $1)))
    (statements
     ((statement SEMICOLON) (list 'statements $1))
     ((statements statement SEMICOLON) (list 'statements $1 $2)))
    (statement
     ((comp-stmt) (list 'statement $1))
     ((smpl-stmt) (list 'statement $1)))
    (smpl-stmt
     ((assignment) (list 'smpl-stmt $1))
     ((glbl-stmt) (list 'smpl-stmt $1))
     ((rtrn-stmt) (list 'smpl-stmt $1))
     ((PASS) (list 'smpl-stmt 'pass))
     ((BREAK) (list 'smpl-stmt 'break))
     ((CONTINUE) (list 'smpl-stmt 'continue)))
    (comp-stmt
     ((func-def) (list 'comp-stmt $1))
     ((if-stmt) (list 'comp-stmt $1))
     ((for-stmt) (list 'comp-stmt $1)))
    (assignment ((ID ASSIGNMENT expression) (list 'assignment (list 'variable $1) $3)))
    (rtrn-stmt
     ((RETURN) (list 'return null))
     ((RETURN expression) (list 'return $2)))
    (glbl-stmt ((GLOBAL ID) (list 'global 'variable $2)))
    (func-def
     ((DEF ID OP CP COLON statements) (list 'function (list 'name $2) null (list 'body $6)))
     ((DEF ID OP parameters CP COLON statements) (list 'function (list 'name $2) $4 (list 'body $7))))
    (parameters
     ((assignment) (list 'params $1))
     ((parameters COMMA assignment) (list 'params $1 $3)))
    (if-stmt ((IF expression COLON statements else-stmt) (list 'if (list 'condition $2) (list 'true $4) $5)))
    (else-stmt ((ELSE COLON statements) (list 'false $3)))
    (for-stmt ((FOR ID IN expression COLON statements) (list 'for (list 'variable $2) (list 'iterator $4) (list 'body $6))))
    (expression
     ((disjunction) (list 'expression $1)))
    (disjunction
     ((conjunction) $1)
     ((disjunction OR conjunction) (list 'disjunction $1 $3)))
    (conjunction
     ((inversion) $1)
     ((conjunction AND inversion) (list 'conjunction $1 $3)))
    (inversion
     ((NOT inversion) (list 'inversion $2))
     ((comparison) $1))
    (comparison
     ((eq-sum) (list 'comparison $1))
     ((lt-sum) (list 'comparison $1))
     ((let-sum) (list 'comparison $1))
     ((gt-sum) (list 'comparison $1))
     ((get-sum) (list 'comparison $1))
     ((sum) $1))
    (eq-sum ((sum EQUALS sum) (list 'equals $1 $3)))
    (lt-sum ((sum LT sum) (list 'less-than $1 $3)))
    (let-sum ((sum LET sum) (list 'less-than-or-equal $1 $3)))
    (gt-sum ((sum GT sum) (list 'greater-than $1 $3)))
    (get-sum ((sum GET sum) (list 'greater-than-or-equal $1 $3)))
    (sum
     ((sum PLUS term) (list 'plus $1 $3))
     ((sum MINUS term) (list 'minus $1 $3))
     ((term) $1))
    (term
     ((term TIMES factor) (list 'times $1 $3))
     ((term DIVIDES factor) (list 'divides $1 $3))
     ((factor) $1))
    (factor
     ((PLUS power) (list 'plus-factor $2))
     ((MINUS power) (list 'minus-factor $2))
     ((power) $1))
    (power
     ((atom POWER factor) (list 'power $1 $3))
     ((primary) $1))
    (primary
     ((atom) $1)
     ((primary OB expression CB) (list 'primary $1 $3))
     ((primary OP CP) (list 'primary $1))
     ((primary OP arguments CP) (list 'primary $1 $3)))
    (arguments
     ((expression) (list 'arguments $1))
     ((arguments COMMA expression) (list 'arguments $1 $3)))
    (atom
     ((ID) (list 'variable $1))
     ((NUM) (list 'number $1))
     ((List) $1)
     ((TRUE) (list 'atom 'true))
     ((FALSE) (list 'atom 'false))
     ((NONE) (list 'atom 'none)))
    (List
     ((OB CB) (list 'list null))
     ((OB expressions CB) (list 'list $2)))
    (expressions
     ((expression) (list 'expressions $1))
     ((expressions COMMA expression) (list 'expressions $1 $3))))))

(provide (all-defined-out))
