;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")

(provide
 initial-state
 next-state
 accepting-state?
 error-state?
 )

(check-location "02" "fsm.rkt")

;; DATA DEFINITION
(define-struct mstate (value curr prev))
;; A mstate is a (make-mstate 1String Integer Integer)
;; INTERPRETATION:
;; The valid mstates for this finite mstate machine are -1
;; (no prev mstate),0,1,2,3,4,5 and 9 (error/invalid mstate).
;; value - input passed in to the machine
;; current - describes the present mstate of the system
;; prev - stores the previous mstate of the system
;; TEMPLATE:
;; mstate-fn : mstate -> ?
#;(define (mstate-fn s)
   (...
    (mstate-value s)
    (mstate-current s)
    (mstate-prev s)))

;; initial-state : Number -> mstate
;; The intial state is designed to store an empty string as value
;; with the current state set to 0 and previous state set to -1.
;; GIVEN: any number. The value doesn't matter since this just
;;        serves as an initializer.
;; RETURNS: a representation of the initial state
;; of your machine.  The given number is ignored.
;; EXAMPLES:
;; (initial-state 0) = (make-mstate "" 0 -1)
(define (initial-state ip)
  (make-mstate "" 0 -1))

;; TEST CASES
(begin-for-test
  (check-equal? (initial-state 0) (make-mstate "" 0 -1))
  (check-equal? (initial-state -1) (make-mstate "" 0 -1))
  (check-equal? (initial-state 100) (make-mstate "" 0 -1))
  )

;; next-state : State MachineInput -> State
;; A MachineInput is a 1String. The valid inputs to this machine
;; is "a", "b", "c", "d", "e" and "f". The next state is deduced
;; from the regular expression: (a|b)*c(a|b)*d(e|f)*
;; The above regular expression has been used in the following
;; form to keep track of the states:
;; (a|b)-0,c-1,(a|b)-2,d-3,(e|f)-4 
;; The Current state is also passed in to this function as a
;; mstate object.
;; STRATEGY: Using template for Mstate on s and cases on
;;           MachineInput
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should follow the given input.
;; EXAMPLES:
;; (next-state (make-mstate "" 0 -1) "a") = (make-mstate "a" 0 0)
;; (next-state (make-mstate "a" 0 0) "b") = (make-mstate "b" 0 0)
;; (next-state (make-mstate "b" 0 0) "c") = (make-mstate "c" 1 0)
;; (next-state (make-mstate "c" 1 0) "a") = (make-mstate "a" 2 1)
;; (next-state (make-mstate "a" 2 1) "d") = (make-mstate "d" 3 2)
;; (next-state (make-mstate "d" 3 2) "e") = (make-mstate "e" 4 3)
;; (next-state (make-mstate "e" 4 3) "f") = (make-mstate "f" 4 4)
;; (next-state (make-mstate "f" 4 4) "g") = (make-mstate "g" 9 4)
(define (next-state s m-ip)
  (cond
    ;; Condition 1: Input - "a", current state - 0
    ;; next state -> 0
    [(and (key=? m-ip "a") (= (mstate-curr s) 0))
     (make-mstate m-ip 0 0)]
    ;; Condition 2: Input - "b", current state - 0
    ;; next state -> 0
    [(and (key=? m-ip "b") (= (mstate-curr s) 0))
     (make-mstate m-ip 0 0)]
    ;; Condition 3: Input - "c", current state - 0
    ;; next state -> 1
    [(and (key=? m-ip "c") (= (mstate-curr s) 0))
     (make-mstate m-ip 1 0)]
    ;; Condition 4: Input - "a", current state - 1
    ;; next state -> 2
    [(and (key=? m-ip "a") (= (mstate-curr s) 1))
     (make-mstate m-ip 2 1)]
    ;; Condition 5: Input - "a", current state - 2
    ;; next state -> 2
    [(and (key=? m-ip "a") (= (mstate-curr s) 2))
     (make-mstate m-ip 2 2)]
    ;; Condition 6: Input - "b", current state - 1
    ;; next state -> 2
    [(and (key=? m-ip "b") (= (mstate-curr s) 1))
     (make-mstate m-ip 2 1)]
    ;; Condition 7: Input - "b", current state - 2
    ;; next state -> 2
    [(and (key=? m-ip "b") (= (mstate-curr s) 2))
     (make-mstate m-ip 2 2)]
    ;; Condition 8: Input - "d", current state - 2
    ;; next state -> 3
    [(and (key=? m-ip "d") (= (mstate-curr s) 2))
     (make-mstate m-ip 3 2)]
    ;; Condition 9: Input - "e", current state - 3
    ;; next state -> 4
    [(and (key=? m-ip "e") (= (mstate-curr s) 3))
     (make-mstate m-ip 4 3)]
    ;; Condition 10: Input - "e", current state - 4
    ;; next state -> 4
    [(and (key=? m-ip "e") (= (mstate-curr s) 4))
     (make-mstate m-ip 4 4)]
    ;; Condition 11: Input - "f", current state - 3
    ;; next state -> 4
    [(and (key=? m-ip "f") (= (mstate-curr s) 3))
     (make-mstate m-ip 4 3)]
    ;; Condition 12: Input - "f", current state - 4
    ;; next state -> 4
    [(and (key=? m-ip "f") (= (mstate-curr s) 4))
     (make-mstate m-ip 4 4)]
    ;; All other cases are error scenarios. So an else clause
    ;; to handle them all.
    [else (make-mstate m-ip 9 (mstate-curr s))])
  )

;; TEST CASES
(begin-for-test
  ;; CORRECT scenarios:
  (check-equal?
   (next-state (make-mstate "" 0 -1) "a")
   (make-mstate "a" 0 0))
  (check-equal?
   (next-state (make-mstate "a" 0 0) "a")
   (make-mstate "a" 0 0))
  (check-equal?
   (next-state (make-mstate "" 0 -1) "b")
   (make-mstate "b" 0 0))
  (check-equal?
   (next-state (make-mstate "b" 0 0) "b")
   (make-mstate "b" 0 0))
  (check-equal?
   (next-state (make-mstate "b" 0 0) "a")
   (make-mstate "a" 0 0))
  (check-equal?
   (next-state (make-mstate "a" 0 0) "b")
   (make-mstate "b" 0 0))
  (check-equal?
   (next-state (make-mstate "a" 0 0) "c")
   (make-mstate "c" 1 0))
  (check-equal?
   (next-state (make-mstate "" 0 -1) "c")
   (make-mstate "c" 1 0))
  (check-equal?
   (next-state (make-mstate "b" 0 0) "c")
   (make-mstate "c" 1 0))
  (check-equal?
   (next-state (make-mstate "c" 1 0) "a")
   (make-mstate "a" 2 1))
  (check-equal?
   (next-state (make-mstate "a" 2 1) "a")
   (make-mstate "a" 2 2))
  (check-equal?
   (next-state (make-mstate "b" 2 1) "a")
   (make-mstate "a" 2 2))
  (check-equal?
   (next-state (make-mstate "b" 2 2) "a")
   (make-mstate "a" 2 2))
  (check-equal?
   (next-state (make-mstate "c" 1 0) "b")
   (make-mstate "b" 2 1))
  (check-equal?
   (next-state (make-mstate "b" 2 1) "b")
   (make-mstate "b" 2 2))
  (check-equal?
   (next-state (make-mstate "a" 2 1) "b")
   (make-mstate "b" 2 2))
  (check-equal?
   (next-state (make-mstate "a" 2 2) "b")
   (make-mstate "b" 2 2))
  (check-equal?
   (next-state (make-mstate "a" 2 2) "d")
   (make-mstate "d" 3 2))
  (check-equal?
   (next-state (make-mstate "b" 2 1) "d")
   (make-mstate "d" 3 2))
  (check-equal?
   (next-state (make-mstate "d" 3 2) "e")
   (make-mstate "e" 4 3))
  (check-equal?
   (next-state (make-mstate "e" 4 3) "e")
   (make-mstate "e" 4 4))
  (check-equal?
   (next-state (make-mstate "e" 4 4) "e")
   (make-mstate "e" 4 4))
  (check-equal?
   (next-state (make-mstate "f" 4 3) "e")
   (make-mstate "e" 4 4))
  (check-equal?
   (next-state (make-mstate "d" 3 2) "f")
   (make-mstate "f" 4 3))
  (check-equal?
   (next-state (make-mstate "f" 4 3) "f")
   (make-mstate "f" 4 4))
  (check-equal?
   (next-state (make-mstate "e" 4 4) "f")
   (make-mstate "f" 4 4))
  (check-equal?
   (next-state (make-mstate "e" 4 3) "f")
   (make-mstate "f" 4 4))

  ;; ERROR scenarios
  (check-equal?
   (next-state (make-mstate "e" 4 3) "g")
   (make-mstate "g" 9 4))
  (check-equal?
   (next-state (make-mstate "a" 0 0) "d")
   (make-mstate "d" 9 0))
  (check-equal?
   (next-state (make-mstate "d" 3 2) "a")
   (make-mstate "a" 9 3))
  (check-equal?
   (next-state (make-mstate "b" 2 2) "e")
   (make-mstate "e" 9 2))
  (check-equal?
   (next-state (make-mstate "f" 4 4) "d")
   (make-mstate "d" 9 4))
  (check-equal?
   (next-state (make-mstate "a" 0 0) "A")
   (make-mstate "A" 9 0))
  )

;; accepting-state? : State -> Boolean
;; The final states that are possible are 3 and 4.
;; STRATEGY: Using template for Mstate on s
;; GIVEN: a state of the machine stored as mstate
;; RETURNS: true iff the given state is a final (accepting) state
;; EXAMPLES:
;; (accepting-state? (make-mstate "d" 3 2)) = #true
;; (accepting-state? (make-mstate "e" 4 3)) = #true
;; (accepting-state? (make-mstate "f" 4 4)) = #true
(define (accepting-state? s)
  (or (= (mstate-curr s) 4) (= (mstate-curr s) 3)))

;; TEST CASES
(begin-for-test
  (check-equal? (accepting-state? (make-mstate "d" 3 2)) #true)
  (check-equal? (accepting-state? (make-mstate "e" 4 3)) #true)
  (check-equal? (accepting-state? (make-mstate "a" 0 0)) #false)
  (check-equal? (accepting-state? (make-mstate "" 0 -1)) #false)
  (check-equal? (accepting-state? (make-mstate "g" 9 2)) #false))

;; error-state? : mstate -> Boolean
;; Every state except 0,1,2,3 and 4 are error states since there are
;; no paths to a final accpeting state.
;; STRATEGY: Using template for Mstate on s
;; GIVEN: a state of the machine
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
;; EXAMPLES:
;; (error-state? (make-mstate "d" 3 2)) = #false
;; (error-state? (make-mstate "e" 4 3)) = #false
;; (error-state? (make-mstate "f" 4 4)) = #false
;; (error-state? (make-mstate "g" 9 2)) = #true
(define (error-state? s)
  (or (> (mstate-curr s) 4) (< (mstate-curr s) 0)))

;; TEST CASES
(begin-for-test
  (check-equal? (error-state? (make-mstate "" 0 -1)) #false)
  (check-equal? (error-state? (make-mstate "" -2 -1)) #true)
  (check-equal? (error-state? (make-mstate "e" 4 3)) #false)
  (check-equal? (error-state? (make-mstate "g" 9 2)) #true))