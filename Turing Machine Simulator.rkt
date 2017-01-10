#lang racket

;**********************************************************
; Name: Harmanpreet Singh
; Email address: harmanpreet.singh@yale.edu
; Topic: a simulator for Turing machines and writing Turing machine programs.
; ***************************************************************************

(provide hours
         ins ins-c-state ins-c-symbol ins-n-state ins-n-symbol ins-dir
         tm-reverse
         i-match? i-lookup
         conf conf-state conf-ltape conf-symbol conf-rtape
         halted? change-state write-symbol
	 normalize
         shift-head-left shift-head-right
         next-config
         tm-convert
	 tm-sort)

(define hours 20)

; ****************************************************************
; Here is a top-level procedure to simulate a Turing machine starting from a given configuration until 
; either it halts or it has executed n steps.
; The procedure returns the list of the successive configurations of the computation,
; starting with the initial one.
; The length of the list of configurations is one more than 
; the number of steps taken by the machine.

(define (simulate mach config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? mach config) (list config))
    (else
     (cons config
           (simulate 
            mach (next-config mach config) (- n 1))))))

; mach is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; ****************************************************************
; Turing machine representation.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a struct
; defined as follows:

(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent)
(struct conf (state ltape symbol rtape) #:transparent)

(define tm-reverse
  (list
   [ins 'q1 0 'q1 0 'R]
   [ins 'q1 1 'q1 1 'R]
   [ins 'q1 'b 'q2 'a 'L]
   [ins 'q2 'a 'q2 'a 'L]
   [ins 'q2 0 'q3 'a 'R]
   [ins 'q2 1 'q4 'a 'R]
   [ins 'q2 'b 'q5 'b 'R]
   [ins 'q3 0 'q3 0 'R]
   [ins 'q3 1 'q3 1 'R]
   [ins 'q3 'a 'q3 'a 'R]
   [ins 'q3 'b 'q6 0 'L]
   [ins 'q4 0 'q4 0 'R]
   [ins 'q4 1 'q4 1 'R]
   [ins 'q4 'a 'q4 'a 'R]
   [ins 'q4 'b 'q6 1 'L]
   [ins 'q5 'a 'q5 'b 'R]
   [ins 'q6 1 'q6 1 'L]
   [ins 'q6 0 'q6 0 'L]
   [ins 'q6 'a 'q2 'a 'L]
   [ins 'q5 1 'q7 1 'L]
   [ins 'q5 0 'q7 0 'L]
   [ins 'q7 'b 'qh 'b 'R]))

(define (i-match? state symbol inst)
  (if (equal? state (ins-c-state inst))
      (if (equal? symbol (ins-c-symbol inst))
          #t
          #f)
      #f))

(define (i-lookup state symbol mach)
  (if (empty? mach)
      #f
      (if (i-match? state symbol (car mach))
          (car mach)
          (i-lookup state symbol (cdr mach)))))

(define (halted? mach config)
  (if (false? (i-lookup (conf-state config) (conf-symbol config) mach))
      #t
      #f))

(define (change-state state config)
  (conf state
        (conf-ltape config)
        (conf-symbol config)
        (conf-rtape config)))

(define (write-symbol new-symbol config)
   (conf (conf-state config)
         (conf-ltape config)
         new-symbol
         (conf-rtape config)))

(define (norm-aux-1 tape)
      (if (empty? tape)
          '()
          (if (equal? (car tape) 'b)
              (norm-aux-1 (cdr tape))
              tape)))

(define (norm-aux-2 tape)
    (reverse (norm-aux-1 (reverse tape))))

(define (normalize config)  
  (conf (conf-state config)
        (norm-aux-1 (conf-ltape config))
        (conf-symbol config)
        (norm-aux-2 (conf-rtape config))))

(define (shift-head-left config)
  (normalize (conf (conf-state config)
                   (reverse (cdr (reverse (cons 'b (conf-ltape config)))))
                   (car (reverse (cons 'b (conf-ltape config))))
                   (cons (conf-symbol config) (conf-rtape config)))))
                   
(define (shift-head-right config)
  (normalize (conf (conf-state config)
                   (append (conf-ltape config) (list (conf-symbol config)))
                   (car (append (conf-rtape config) '(b)))
                   (cdr (append (conf-rtape config) '(b))))))

(define (changedir dir config) (
    (if (equal? (ins-dir dir) 'L)
        shift-head-left
        shift-head-right)
    (write-symbol (ins-n-symbol dir) (change-state (ins-n-state dir) config))))

(define (next-config mach config)
  (if (halted? mach config)
      config
      (changedir (i-lookup (conf-state config) (conf-symbol config) mach) config)))

(define tm-convert
  (list
   [ins 'q1 1 'q1 1 'R]
   [ins 'q1 0 'q1 0 'R]
   [ins 'q1 'b 'q2 'b 'L]
   [ins 'q2 0 'q2 1 'L]
   [ins 'q2 1 'q3 0 'L]
   [ins 'q3 0 'q3 0 'L]
   [ins 'q3 1 'q3 1 'L]
   [ins 'q3 'x 'q3 'x 'L]
   [ins 'q3 'b 'q4 'x 'R]
   [ins 'q4 'x 'q4 'x 'R]
   [ins 'q4 0 'q4 0 'R]
   [ins 'q4 1 'q1 1 'R]
   [ins 'q4 'b 'q5 'b 'L]
   [ins 'q5 0 'q5 'b 'L]
   [ins 'q5 'x 'q6 'x 'L]
   [ins 'q6 'x 'q6 'x 'L]
   [ins 'q6 'b 'qh 'b 'R]))

(define tm-sort
  (list
   [ins 'q1 0 'q1 0 'R]
   [ins 'q1 1 'q2 0 'R]
   [ins 'q2 0 'q1 1 'L]
   [ins 'q2 1 'q2 1 'R]
   [ins 'q2 'b 'q3 'b 'L]
   [ins 'q3 1 'q3 1 'L]
   [ins 'q3 0 'q4 1 'L]
   [ins 'q4 0 'q4 0 'L]
   [ins 'q4 1 'q4 1 'L]
   [ins 'q4 'b 'qh 'b 'R]))

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #f)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))

(test 'i-match? (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) #t)
(test 'i-match? (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) #f)
(test 'i-match? (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) #t)
(test 'i-lookup (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R))
(test 'i-lookup (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R))
(test 'i-lookup (i-lookup 'q3 1 tm1) #f)

(test 'halted? (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) #f)
(test 'halted? (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) #t)
(test 'change-state (change-state 'q2 (conf 'q1 '(0) 1 '())) (conf 'q2 '(0) 1 '()))
(test 'change-state (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) (conf 'q13 '(0 1 1) 'b '()))
(test 'write-symbol (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) (conf 'q5 '(0) 1 '(1 1)))
(test 'write-symbol (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) (conf 'q2 '(0 0 1) 'c '(1 1)))
(test 'write-symbol (write-symbol 'b (conf 'q3 '(1) 0 '())) (conf 'q3 '(1) 'b '()))

(test 'normalize (normalize config1) (conf 'q3 '(0 0) 1 '(1)))
(test 'normalize (normalize config2) (conf 'q6 '(1 b) 0 '()))
(test 'normalize (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) (conf 'q3 '(0) 'b '(1 1 0)))
(test 'normalize (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) (conf 'q6 '(0 b 0) 1 '(0 b 0)))
(test 'normalize (normalize (conf 'q4 '(b b) 'b '(b b b))) (conf 'q4 '() 'b '()))


(test 'shift-head-left (shift-head-left (conf 'q5 '() 'b '())) (conf 'q5 '() 'b '()))
(test 'shift-head-left (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) (conf 'q6 '(0) 0 '(1 1 1)))
(test 'shift-head-left (shift-head-left (conf 'q7 '() 0 '(1 1 0))) (conf 'q7 '() 'b '(0 1 1 0)))
(test 'shift-head-right (shift-head-right (conf 'q2 '() 'b '())) (conf 'q2 '() 'b '()))
(test 'shift-head-right (shift-head-right (conf 'q9 '() 0 '(1 1 1))) (conf 'q9 '(0) 1 '(1 1)))
(test 'shift-head-right (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) (conf 'q8 '(1 0 1 1 b) 'b '()))


(test 'next-config (next-config tm1 (conf 'q1 '() 0 '(0 1))) (conf 'q1 '(1) 0 '(1)))
(test 'next-config (next-config tm1 (conf 'q1 '(1) 0 '(1))) (conf 'q1 '(1 1) 1 '()))
(test 'next-config (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) (conf 'q2 '(1 1) 0 '()))
(test 'next-config (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) (conf 'q3 '() 1 '(1 0)))
(test 'next-config (next-config tm1 (conf 'q3 '() 1 '(1 0))) (conf 'q3 '() 1 '(1 0)))