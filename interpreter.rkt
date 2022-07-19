#lang racket

(require "eopl_errors.rkt")
(require "env_store.rkt")
(require "datatypes.rkt")
(require "parser.rkt")
(require "lexer.rkt")
(require (lib "eopl.ss" "eopl"))
(require try-catch)

(define is-list (lambda (val) (list? (car val))))
(define is-bool (lambda (val) (boolean? (car val))))
(define is-num (lambda (val) (number? (car val))))


(define lex (lambda (lexer input) (lambda () (lexer input))))
(define run (lambda (str) (value-of-program (full-parser (lex full-lexer (open-input-string str))))))
(define execute (lambda (file_input) (run (open-input-file file_input))))

(define value-of-program (lambda (pgm) (and (initialize-store!) (cases program pgm
                                         (a-program (sttmnts) (value-of-statements sttmnts (empty-env) (empty-store)))
                                         (else (report-no-program!))))))

(define value-of-statements (lambda (sttmnts glob_env curr_env) (cases statements sttmnts
                                                    (a-statement (sttmnt) (value-of-statement sttmnt glob_env curr_env))
                                                    (some-statements (sttmnts sttmnt) (let ((res (value-of-statements sttmnts glob_env curr_env))) (cond
                                                                                        [(list? (car res)) (if (equal? 'return (caar res)) res
                                                                                                               (value-of-statement sttmnt glob_env (cddr res)))]
                                                                                        [(equal? 'continue (car res)) res]
                                                                                        [(equal? 'break (car res)) res] ; need iterator changes
                                                                                        [else (value-of-statement sttmnt glob_env (cddr res))]))))))

(define value-of-statement (lambda (sttmnt glob_env curr_env) (cases statement sttmnt
                                                  (compound-statement (cmp) (value-of-compound cmp glob_env curr_env))
                                                  (simple-statement (smpl) (value-of-simple smpl glob_env curr_env)))))
;TODO
(define value-of-simple (lambda (smpl glob_env curr_env) (cases simple smpl
                                             (assignment-statement (assign) (value-of-assignment assign glob_env curr_env))
                                             (global-statement (glbl) (value-of-global glbl glob_env curr_env))
                                             (return-statement (rtrn) (value-of-return rtrn glob_env curr_env))
                                             (pass-statement () (list 'pass glob_env curr_env))
                                             (break-statement () (list 'break glob_env curr_env))
                                             (continue-statement () (list 'continue glob_env curr_env)))))

(define value-of-compound (lambda (cmp glob_env curr_env) (cases compound cmp
                                              (function-definition (func) (value-of-function func glob_env curr_env))
                                              (if-statement (if-stmt) (value-of-if if-stmt glob_env curr_env))
                                              (for-statement (for-stmt) (value-of-for for-stmt glob_env curr_env)))))
; needs try-catch package to run the code !
(define value-of-assignment (lambda (assign glob_env curr_env) (cases assignment assign
                                                   (an-assignment (var exp) (cases ATOM var
                                                                              (id-exp (name) (let ((val (value-of-expression exp glob_env curr_env)))
                                                                                               (try [(let ((loc (apply-env var curr_env))))
                                                                                                   (setref! loc (car val)) (list 'assignment glob_env curr_env)]
                                                                                                  [catch (void (let ((loc (newref (car val))))
                                                                                             (list 'assignment glob_env (extend-env var (car val) (cddr val)))))])))
                                                                              (else (report-invalid-lhs! var)))))))

(define value-of-global (lambda (glbl glob_env curr_env) (cases global glbl
                                                           (a-global (var) (list 'global glob_env (extend-env var (apply-env var glob_env) curr_env))))))

(define value-of-return (lambda (rtrn glob_env curr_env) (cases return rtrn
                                                           (void-return () (list (list 'return null) glob_env curr_env))
                                                           (exp-return (exp) (list (list 'return (value-of-expression exp glob_env curr_env)) glob_env curr_env)))))

; value of compound statements: TODO

(define value-of-expression (lambda (exp glob_env curr_env) (cases expression exp
                                                              (an-expression (dis) (value-of-disjunction dis)))))

(define value-of-disjunction (lambda (dis glob_env curr_env) (cases disjunction dis
                                                                (a-disjunction (dis con) (let ((res1 (value-of-disjunction dis glob_env curr_env))
                                                                                               (res2 (value-of-conjunction con glob_env curr_env)))
                                                                                           (if (is-bool res1) (if (is-bool res2)
                                                                                                  (list (or (car res1) (car res2)) glob_env curr_env)
                                                                                                  (report-nonbool-type! con)) (report-nonbool-type! dis))))
                                                                (conjunction-exp (con) (value-of-conjuncion con)))))

(define value-of-conjunction (lambda (con glob_env curr_env) (cases conjunction con
                                                               (a-conjunction (con inv) (let ((res1 (value-of-conjunction con glob_env curr_env))
                                                                                              (res2 (value-of-inversion inv glob_env curr_env)))
                                                                                          (if (is-bool res1) (if (is-bool res2)
                                                                                                   (list (and (car res1) (car res2)) glob_env curr_env)
                                                                                                   (report-nonbool-type! inv)) (report-nonbool-type! con))))
                                                               (inversion-exp (inv) (value-of-inversion inv glob_env curr_env)))))

(define value-of-inversion (lambda (inv glob_env curr_env) (cases inversion inv
                                                             (an-inversion (inv) (let ((res (value-of-inversion inv glob_env curr_env)))
                                                                                   (if (is-bool res) (list (not (car res)) glob_env curr_env)
                                                                                       (report-nonbool-type! inv))))
                                                             (comp-exp (comp) (value-of-comparison comp glob_env curr_env)))))

(define value-of-comparison (lambda (comp glob_env curr_env) (cases comparison comp
                                                               (equal-sum (eq) (value-of-eq eq-exp glob_env curr_env))
                                                               (lessthan-sum (lt) (value-of-lt lt-exp glob_env curr_env))
                                                               (lessthanorequal-sum (let) (value-of-let let-exp glob_env curr_env))
                                                               (greaterthan-sum (gt) (value-of-gt gt-exp glob_env curr_env))
                                                               (greaterthanorequal-sum (get) (value-of-get get-exp glob_env curr_env))
                                                               (sum-expression (sum) (value-of-sum sum glob_env curr_env)))))

(define value-of-eq (lambda (eq glob_env curr_env) (cases eq-exp eq
                                                     (an-eq-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                  (res2 (value-of-sum num2 glob_env curr_env)))
                                                                              (if (is-num res1) (if (is-num res2)
                                                                                          (list (= (car res1) (car res2)) glob_env curr_env)
                                                                                          (report-nan-type! num2)) (report-nan-type! num1)))))))

(define value-of-lt (lambda (lt glob_env curr_env) (cases lt-exp lt
                                                     (a-lt-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                 (res2 (value-of-sum num2 glob_env curr_env)))
                                                                             (if (is-num res1) (if (is-num res2)
                                                                                          (list (< (car res1) (car res2)) glob_env curr_env)
                                                                                          (report-nan-type! num2)) (report-nan-type! num1)))))))

(define value-of-gt (lambda (gt glob_env curr_env) (cases gt-exp gt
                                                    (a-gt-exp (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                (res2 (value-of-sum num2 glob_env curr_env)))
                                                                            (if (is-num res1) (if (is-num res2)
                                                                                          (list (> (car res1) (car res2)) glob_env curr_env)
                                                                                          (report-nan-type! num2)) (report-nan-type! num1)))))))

(define value-of-let (lambda (let glob_env curr_env) (cases let-exp let
                                                       (a-let-exp (num1 num2) (let ((res (value-of-gt (a-gt-exp num1 num2) glob_env curr_env)))
                                                                                (list (not (car res)) glob_env curr_env))))))

(define value-of-get (lambda (get glob_env curr_env) (cases get-exp get
                                                       (a-get-exp (num1 num2) (let ((res (value-of-lt (a-lt-exp num1 num2) glob_env curr_env)))
                                                                                (list (not (car res)) glob_env curr_env))))))

(define value-of-sum (lambda  (sum glob_env curr_env) (cases sum-exp sum
                                                        (plus-term (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                     (res2 (value-of-term num2 glob_env curr_env)))
                                                                                 (if (is-list res1) (if (is-list res2)
                                                                                            (list (append (car res1) (car res2)) glob_env curr_env)
                                                                                            (report-nonlist-type! num2))
                                                                                     (if (is-num res1) (if (is-num res2)
                                                                                            (list (+ (car res1) (car res2)) glob_env curr_env)
                                                                                            (report-nan-type! num2)) (report-nan-type! num1)))))
                                                        (minus-term (num1 num2) (let ((res1 (value-of-sum num1 glob_env curr_env))
                                                                                      (res2 (value-of-term num2 glob_env curr_env)))
                                                                                  (if (is-num res1) (if (is-num res2)
                                                                                            (list (- (car res1) (car res2)) glob_env curr_env)
                                                                                            (report-nan-type! num2)) (report-nan-type! num1))))
                                                        (term-expression (term) (value-of-term term glob_env curr_env)))))

(define value-of-term (lambda (term glob_env curr_env) (cases term-exp term
                                                         (times-factor (num1 num2) (let ((res1 (value-of-term num1 glob_env curr_env))
                                                                                         (res2 (value-of-factor num2 glob_env curr_env)))
                                                                                     (if (is-num res1) (if (is-num res2)
                                                                                            (list (* (car res1) (car res2)) glob_env curr_env)
                                                                                            (report-nan-type! num2)) (report-nan-type! num1))))
                                                         (divides-factor (num1 num2) (let ((res1 (value-of-term num1 glob_env curr_env))
                                                                                           (res2 (value-of-factor num2 glob_env curr_env)))
                                                                                       (if (is-num res1) (if (is-num res2)
                                                                                            (list (/ (car res1) (car res2)) glob_env curr_env)
                                                                                            (report-nan-type! num2)) (report-nan-type! num1))))
                                                         (factor-expression (factor) (value-of-factor factor glob_env curr_env)))))

(define value-of-factor (lambda (factor glob_env curr_env) (cases factor-exp factor
                                                             (plus-power (pow) (let ((res (value-of-power pow glob_env curr_env)))
                                                                                 (if (is-num res) (list (+ 0 (car res)) glob_env curr_env)
                                                                                     (report-nan-type! pow))))
                                                             (minus-power (pow) (let ((res (value-of-power pow glob_env curr_env)))
                                                                                 (if (is-num res) (list (- 0 (car res)) glob_env curr_env)
                                                                                     (report-nan-type! pow))))
                                                             (power-expression (pow) (value-of-power pow glob_env curr_env)))))

(define value-of-power (lambda (pow glob_env curr_env) (cases power-exp pow
                                                         (pow-exp (num1 num2) (let ((res1 (value-of-ATOM num1 glob_env curr_env))
                                                                                    (res2 (value-of-factor num2 glob_env curr_env)))
                                                                                (if (is-num res1) (if (is-num res2)
                                                                                     (list (expt (car res1) (car res2)) glob_env curr_env)
                                                                                     (report-nan-type! num2)) (report-nan-type! num1))))
                                                         (primary-expression (prim) (value-of-primary prim glob_env curr_env)))))

(define value-of-primary (lambda (primary glob_env curr_env) (cases primary-exp primary
                                                               (atom-exp (var) (value-of-ATOM var))
                                                               (list-idx (prim exp) (let ((res1 (value-of-primary prim glob_env curr_env))
                                                                                          (res2 (value-of-expression exp glob_env curr_env)))
                                                                                      (if (is-list res1) (if (is-num res2)
                                                                                        (list (list-ref (car res1) (car res2)) glob_env curr_env)
                                                                                        (report-nan-type! num2)) (report-nonlist-type! num1))))
                                                               (func-call (prim args) null) ;TODO
                                                               (func-call-noargs (prim) null) ;TODO
                                                               )))

(define value-of-arguments (lambda (args glob_env curr_env) (cases arguments-exp args
                                                              (an-argument (exp) (value-of-expression exp glob_env curr_env))
                                                              (some-arguments (arguments exp) (let ((res1 (value-of-arguments arguments glob_env curr_env))
                                                                                                    (res2 (value-of-expression exp glob_env curr_env)))
                                                                                                (list (append (car res1) (list (car res2))) glob_env curr_env))))))

(define value-of-ATOM (lambda (atom glob_env curr_env) (cases ATOM atom
                                                         (id-exp (name) (deref (apply-env atom curr_env)))
                                                         (num-exp (num) num)
                                                         (list-expression (l) (value-of-list l glob_env curr_env))
                                                         (true-exp () (list #t glob_env curr_env))
                                                         (false-exp () (list #f glob_env curr_env))
                                                         (none-exp () (list 'none glob_env curr_env)))))

(define value-of-list (lambda (l glob_env curr_env) (cases list-exp l
                                                      (a-list (exps) (value-of-expressions exps glob_env curr_env))
                                                      (null-list () (list null glob_env curr_env)))))

(define value-of-expressions (lambda (exps glob_env curr_env) (cases expressions exps
                                                                (an-exp (exp) (value-of-expression exp glob_env curr_env))
                                                                (some-exps (expressions exp) (let ((res1 (value-of-expressions expressions glob_env curr_env))
                                                                                                   (res2 (value-of-expression exp glob_env curr_env)))
                                                                                               (list (append (car res1) (list (car res2))) glob_env curr_env))))))