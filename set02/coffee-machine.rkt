;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-coffee
 machine-remaining-chocolate
 machine-bank
 )

(check-location "02" "coffee-machine.rkt")

;; DATA DEFINITION:
(define-struct machineState (bank change ncoffee nhotchoc))
;; A MachineState is a (make-machineState PosInt PosInt PosInt)
;; INTERPRETATION:
;; bank - contains all the money from customer purchases, in cents
;; change - holds the money that was inserted by the customer, in cents
;; ncoffee - number of coffee cups available in the machine
;; nhotchoc - number of hot chocolate cups available in the machine
;; DESTRUCTOR TEMPLATE:
#;(define (machineState-fn s)
    (...
     (machineState-bank s)
     (machineState-change s)
     (machineState-ncoffee s)
     (machineState-nhotchoc s))
    )

;; Define machine states for testing
(define t1 (make-machineState 720 200 5 5))
(define t2 (make-machineState 720 50 5 5))
(define t3 (make-machineState 720 200 0 0))
(define t4 (make-machineState 720 50 0 0))
(define t5 (make-machineState 720 200 1 1))
(define t6 (make-machineState 720 0 1 1))

;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of cups of coffee and of hot chocolate
;; RETURNS: the state of a machine loaded with the given number of cups
;;         of coffee and of hot chocolate, with an empty bank.
;; EXAMPLE:
;; (initial-machine 10 10) = (make-machineState 0 0 10 10)
(define (initial-machine n-coffee n-hotchoc)
  (make-machineState 0 0 n-coffee n-hotchoc))

;; TESTS:
(begin-for-test
  (check-equal? (initial-machine 10 10)
                (make-machineState 0 0 10 10)))

;; new-machine : PosReal PosReal NonNegInt NonNegInt ->
;;               MachineState
;; GIVEN:
;; money in the machine (in cents), change passed in by the user
;; (in cents), number of coffee cups and number of hot chocolate cups
;; RETURNS:
;; the state of the machine loaded with the given amount in
;; bank, change, number of cups of coffee and hot chocolate
;; EXAMPLE:
;; (new-machine 10.00 2.00 10 10) =
;;    (make-machineState 10.00 2.00 10 10)
(define (new-machine bank change ncoffee nhotchoc)
  (make-machineState bank change ncoffee nhotchoc))

;; TESTS:
(begin-for-test
  (check-equal?
   (new-machine 720 200 5 5)
   (make-machineState 720 200 5 5)))

;; dispense-coffee : MachineState -> MachineState
;; If a coffee cup is dispensed by the machine, $1.50 gets added
;; to the bank and the rest of the money stays as change which
;; can be retrieved by the user.
;; STRATEGY: Using template for MachineState on s
;; GIVEN: a machine state
;; RETURNS: the machine state after dispensing one coffee
;; EXAMPLE:
;; (dispense-coffee (make-machineState 720 200 5 5)) =
;;   (make-machineState 870 50 4 5)
;; (dispense-coffee (make-machineState 720 50 5 5)) =
;;   (make-machineState 720 50 5 5)
;; (dispense-coffee (make-machineState 720 200 0 5)) =
;;   (make-machineState 720 200 0 5)
(define (dispense-coffee s)
  (new-machine
   (+ (machineState-bank s) 150)
   (- (machineState-change s) 150)
   (- (machineState-ncoffee s) 1)
   (machineState-nhotchoc s))
  )

;; TESTS:
;; Minimal test to check if the function is working as expected.
;; All the error cases are handled through the caller functions.
(begin-for-test
  (check-equal?
   (dispense-coffee t1)
   (make-machineState 870 50 4 5)))

;; dispense-hotchoc : MachineState -> MachineState
;; If a hot chocolate cup is dispensed by the machine, $0.60 gets
;; added to the bank and the rest of the money stays as change
;; which can be retrieved by the user.
;; STRATEGY: Using template for MachineState on s
;; GIVEN: a machine state
;; RETURNS: the machine state after dispensing one hot chocolate
;; EXAMPLES:
;; (dispense-hotchoc (make-machineState 720 200 5 5)) =
;;   (make-machineState 780 140 5 4)
(define (dispense-hotchoc s)
  (new-machine
   (+ (machineState-bank s) 60)
   (- (machineState-change s) 60)
   (machineState-ncoffee s)
   (- (machineState-nhotchoc s) 1))
  )

;; TESTS:
;; Once again, the error cases are handled by the caller. Hence,
;; we keep the testing minimal just be sure about its correctness.
(begin-for-test
  (check-equal?
   (dispense-hotchoc t1)
   (make-machineState 780 140 5 4)))

;; dispense-change : MachineState -> MachineState
;; Calling this function will update the amount stored in the
;; 'change' container to 0 cents.
;; STRATEGY: Using template for MachineState on s
;; GIVEN: a machine state
;; RETURNS: the machine state after dispensing the change
;; EXAMPLES:
;; (dispense-change (make-machineState 720 200 5 5)) =
;;   (make-machineState 720 0 5 5)
;; (dispense-change (make-machineState 720 0 5 5)) =
;;   (make-machineState 720 0 5 5)
(define (dispense-change s)
  (new-machine
   (machineState-bank s)
   0
   (machineState-ncoffee s)
   (machineState-nhotchoc s))
  )

;; TESTS:
;; Minimal testing to check the correctness of the function.
(begin-for-test
  (check-equal?
   (dispense-change t1)
   (make-machineState 720 0 5 5)))

;; add-change : MachineState CustomerInput -> MachineState
;; STRATEGY: Using template for MachineState on s
;; GIVEN: current machine state and inserted change, in cents
;; RETURNS: the updated machine state after adding the change
;; EXAMPLES:
;; (add-change (make-machineState 720 200 5 5) 150) =
;;   (make-machineState 720 350 5 5)
(define (add-change s ip)
  (new-machine
   (machineState-bank s)
   (+ (machineState-change s) ip)
   (machineState-ncoffee s)
   (machineState-nhotchoc s))
  )

;; TESTS:
;; Minimal testing to check the correctness of the function.
(begin-for-test
  (check-equal?
   (add-change t1 175)
   (make-machineState 720 375 5 5)))

;; machine-next-state : MachineState CustomerInput -> MachineState
;; A CustomerInput is one of
;; -- a PosInt          interp: insert the specified amount of money,
;;                              in cents
;; -- "coffee"          interp: request a coffee
;; -- "hot chocolate"   interp: request a hot chocolate
;; -- "change"          interp: return all the unspent money that the
;;                              customer has inserted
;; STRATEGY: Using template for MachineState on state and cases on
;;           CustomerInput
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (machine-next-state (make-machineState 200 200 5 5) "coffee") =
;;   (make-machineState 350 50 4 5)
;; (machine-next-state (make-machineState 200 200 5 5) "hot chocolate")
;;   = (make-machineState 260 140 5 4)
;; (machine-next-state (make-machineState 200 0 5 5) "coffee") =
;;   (make-machineState 200 0 5 5)
;; (machine-next-state (make-machineState 200 200 5 0) "hot chocolate") =
;;   (make-machineState 200 200 5 0)
;; (machine-next-state (make-machineState 200 0 0 0) "coffee") =
;;   (make-machineState 200 0 0 0)
;; (machine-next-state (make-machineState 200 200 0 0) "change") =
;;   (make-machineState 200 0 0 0)
;; (machine-next-state (make-machineState 200 0 0 0) "change") =
;;   (make-machineState 200 0 0 0)
;; (machine-next-state (make-machineState 200 200 0 0) 100) =
;;   (make-machineState 200 300 0 0)
(define (machine-next-state state ip)
  (if (integer? ip)
      (add-change state ip)
      (cond
        [(string=? "coffee" ip)
         (if (and
              ;; check if coffee is available
              (> (machineState-ncoffee state) 0)
              ;; check if the user has inserted enough money
              (>= (machineState-change state) 150))
             (dispense-coffee state)
             state)]
        [(string=? "hot chocolate" ip)
         (if (and
              ;; check if hot chocolate is available
              (> (machineState-nhotchoc state) 0)
              ;; check if enough money has been inserted
              (>= (machineState-change state) 60))
             (dispense-hotchoc state)
             state)]
        [(string=? "change" ip)
         (if (> (machineState-change state) 0)
             (dispense-change state)
             state)]
        [else state]  ;; Handle bad inputs. Machine does nothing.
        )
      )
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (machine-next-state t1 "coffee")
   (make-machineState 870 50 4 5))
  (check-equal?
   (machine-next-state t2 "coffee")
   (make-machineState 720 50 5 5))
  (check-equal?
   (machine-next-state t3 "coffee")
   (make-machineState 720 200 0 0))
  (check-equal?
   (machine-next-state t4 "coffee")
   (make-machineState 720 50 0 0))
  (check-equal?
   (machine-next-state t5 "coffee")
   (make-machineState 870 50 0 1))
  (check-equal?
   (machine-next-state t6 "coffee")
   (make-machineState 720 0 1 1))
  (check-equal?
   (machine-next-state t1 "hot chocolate")
   (make-machineState 780 140 5 4))
  (check-equal?
   (machine-next-state t2 "hot chocolate")
   (make-machineState 720 50 5 5))
  (check-equal?
   (machine-next-state t3 "hot chocolate")
   (make-machineState 720 200 0 0))
  (check-equal?
   (machine-next-state t4 "hot chocolate")
   (make-machineState 720 50 0 0))
  (check-equal?
   (machine-next-state t5 "hot chocolate")
   (make-machineState 780 140 1 0))
  (check-equal?
   (machine-next-state t6 "hot chocolate")
   (make-machineState 720 0 1 1))
  (check-equal?
   (machine-next-state t1 "change")
   (make-machineState 720 0 5 5))
  (check-equal?
   (machine-next-state t4 "change")
   (make-machineState 720 0 0 0))
  (check-equal?
   (machine-next-state t6 "change")
   (make-machineState 720 0 1 1))
  (check-equal?
   (machine-next-state t6 "change")
   (make-machineState 720 0 1 1))
  (check-equal?
   (machine-next-state t1 100)
   (make-machineState 720 300 5 5))
  (check-equal?
   (machine-next-state t1 "wrong input")
   (make-machineState 720 200 5 5))
  )

;; machine-output : MachineState CustomerInput -> MachineOutput
;; A MachineOutput is one of
;; -- "coffee"         interp: machine dispenses a cup of coffee
;; -- "hot chocolate"  interp: machine dispenses a cup of hot chocolate
;; -- "Out of Item"    interp: machine displays "Out of Item"
;; -- a PosInt         interp: machine releases the specified amount of
;;                             money, in cents
;; -- "Nothing"        interp: the machine does nothing
;; STRATEGY: Using template for MachineState on state and cases on
;;           CustomerInput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input
;; EXAMPLES:
;; (machine-output (make-machineState 200 200 5 5) "coffee") =
;;   "coffee"
;; (machine-output (make-machineState 200 200 5 5) "hot chocolate")
;;   = "hot chocolate"
;; (machine-output (make-machineState 200 0 5 5) "coffee") =
;;   "Nothing"
;; (machine-output (make-machineState 200 200 5 0) "hot chocolate") =
;;   "Out of Item"
;; (machine-output (make-machineState 200 0 0 0) "coffee") =
;;   "Out of Item"
;; (machine-output (make-machineState 200 200 0 0) "change") =
;;   "Nothing"
;; (machine-output (make-machineState 200 0 0 0) "change") =
;;   "Nothing"
;; (machine-output (make-machineState 200 200 0 0) 100) =
;;   "Nothing"
(define (machine-output state ip)
  (if (integer? ip)
      "Nothing"  ;; Money is being inserted -> Do nothing
      (if (and
           (not (string=? ip "change"))
           (equal? (machine-next-state state ip) state))
          ;; State has not changed.
          ;; If the user has entered 'coffee' or 'hot chocolate',
          ;; check if the item is available and then provide an
          ;; output.          
          (if (is-item-empty? state ip)
              "Out of Item"  ;; when the item is unavailable
              "Nothing")  ;; when the user inserts an invalid input
          ;; State has changed
          (cond
            [(string=? "coffee" ip) "coffee"]
            [(string=? "hot chocolate" ip) "hot chocolate"]
            [(string=? "change" ip) (machineState-change state)]))
      )
  )

;; TESTS:
(begin-for-test
  (check-equal? (machine-output t1 "coffee") "coffee")
  (check-equal? (machine-output t2 "coffee") "Nothing")
  (check-equal? (machine-output t3 "coffee") "Out of Item")
  (check-equal? (machine-output t4 "coffee") "Out of Item")
  (check-equal? (machine-output t5 "coffee") "coffee")
  (check-equal? (machine-output t6 "coffee") "Nothing")
  (check-equal? (machine-output t1 "hot chocolate") "hot chocolate")
  (check-equal? (machine-output t2 "hot chocolate") "Nothing")
  (check-equal? (machine-output t3 "hot chocolate") "Out of Item")
  (check-equal? (machine-output t4 "hot chocolate") "Out of Item")
  (check-equal? (machine-output t5 "hot chocolate") "hot chocolate")
  (check-equal? (machine-output t6 "hot chocolate") "Nothing")
  (check-equal? (machine-output t1 "change") 200)
  (check-equal? (machine-output t4 "change") 50)
  (check-equal? (machine-output t6 "change") 0)
  (check-equal? (machine-output t1 100) "Nothing"))
    
;; machine-remaining-coffee : MachineState -> NonNegInt
;; STRATEGY: Using template for MachineState on state
;; GIVEN: a machine state
;; RETURNS: the number of cups of coffee left in the machine
;; EXAMPLES:
;; (machine-remaining-coffee (make-machineState 200 50 5 5)) = 5
;; (machine-remaining-coffee (make-machineState 200 50 0 5)) = 0
(define (machine-remaining-coffee state)
  (machineState-ncoffee state))

;; TESTS:
;; Basic test cases that check the functionality
(begin-for-test
  (check-equal? (machine-remaining-coffee t1) 5)
  (check-equal? (machine-remaining-coffee t4) 0))

;; machine-remaining-chocolate : MachineState -> NonNegInt
;; STRATEGY: Using template for MachineState on state
;; GIVEN: a machine state
;; RETURNS: the number of cups of hot chocolate left in the machine
;; EXAMPLES:
;; (machine-remaining-chocolate (make-machineState 200 50 5 5)) = 5
;; (machine-remaining-chocolate (make-machineState 200 50 5 0)) = 0
(define (machine-remaining-chocolate state)
  (machineState-nhotchoc state))

;; TESTS:
;; Basic test cases that check the functionality
(begin-for-test
  (check-equal? (machine-remaining-chocolate t1) 5)
  (check-equal? (machine-remaining-chocolate t4) 0))

;; machine-bank : MachineState -> NonNegInt
;; STRATEGY: Using template for MachineState on state
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; (machine-bank (make-machineState 720 200 5 5)) = 720
;; (machine-bank (make-machineState 0 200 5 5)) = 0
(define (machine-bank state)
  (machineState-bank state))

;; TESTS:
;; Basic test cases that check the functionality
(begin-for-test
  (check-equal? (machine-bank t1) 720))

;; is-item-empty? : MachineState CustomerInput -> Boolean
;; STRATEGY: Using template for MachineState on state and cases
;;           on CustomerInput
;; GIVEN: a machine state and a customer input
;; RETURNS: whether the requested item is in stock or not
;; (is-item-empty? (make-machineState 50 50 5 6) "coffee" = #false
;; (is-item-empty? (make-machineState 50 50 0 6) "coffee" = #true
;; (is-item-empty? (make-machineState 50 50 5 6) "hot chocolate"
;;   = #false
;; (is-item-empty? (make-machineState 50 50 5 0) "hot chocolate"
;;   = #true
(define (is-item-empty? state ip)
  (cond
    [(string=? "coffee" ip)
     (= (machine-remaining-coffee state) 0)]
    [(string=? "hot chocolate" ip)
     (= (machine-remaining-chocolate state) 0)]))

;; TESTS:
;; Basic testing to check the correctness of the function
(begin-for-test
  (check-equal? (is-item-empty? t1 "coffee") #false)
  (check-equal? (is-item-empty? t4 "hot chocolate") #true))