;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The following program takes list of daily rainfall amounts
;; and returns average rainfall during a particular period ignoring
;; negative values and considering -999 as end of list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require "extras.rkt")

(provide rainfall)

(check-location "07" "rainfall.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define END-OF-DATA -999)
(define DEFAULT-END false)
(define DEFAULT-AVERAGE 0.0)
(define MINIMUM-VALUE 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS
;;
;; A Rainfall is a real number
;; A ListOfRainfall is either
;; -- empty
;; -- (cons Rainfall ListOfRainfall)
;;
;; Template
;; lor-fn : ListOfRainfall -> ??
#; (define (lot-fn lor)
     (cond
       [(empty? lor) ...]
       [else (... (first lor)
                  (lor-fn (rest lor)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOCAL VARIABLES FOR TESTING
(define l1 (list -1 0 -9 -999 -2 50 6))
(define l2 (list -999 0 -9 10 -2 50 6))
(define l3 (list -1 0 2 -9 24 10 -2 10 -999  -2 6))
(define l4 (list 29 11 40 0 0 -999 2 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HELPER FUNCTIONS

;; remove-negative : ListOfRainfall -> ListOfRainfall
;; GIVEN: a list of rainfall readings
;; RETURNS: a list similar to the original list but, with all the
;; numbers <= 0 removed from it
;; EXAMPLES:
;; (remove-negative (list 1 2 -5 20 0 -2)) = (list 1 2 20)
;; (remove-negative (list -2 -5 -999 -1)) = empty
;; (remove-negative (list 4 6 10)) = (list 4 6 10)
;; (remove-negative empty) = empty
;; STRATEGY: Using HOF filter on lst
(define (remove-negative lst)
  (filter
   (lambda (l) (>= l MINIMUM-VALUE))
   lst))

;; TESTS:
(begin-for-test
  (check-equal?
   (remove-negative (list 1 2 -5 20 0 -2)) (list 1 2 20 0)
   "Mixed list is not being filtered properly")
  (check-equal?
   (remove-negative (list -2 -5 -999 -1)) empty
   "List of negative values not being filtered properly")
  (check-equal?
   (remove-negative (list 4 6 10)) (list 4 6 10)
   "List of positive values not being filtered properly")
  (check-equal?
   (remove-negative empty) empty
   "Empty list not being filtered properly"))

;; average : ListOfRainfall -> PosReal
;; GIVEN: a list of rainfall readings
;; WHERE: the list has been filtered off all negative values
;; RETURNS: the average of all the values within the list
;; EXAMPLES:
;; (average (list 1 5 9)) = 5.0
;; (average empty) = empty
;; STRATEGY: Using cases on lst and combining simpler functions
(define (average lst)
  (cond
    [(empty? lst) DEFAULT-AVERAGE]
    [else(/ (foldr + DEFAULT-AVERAGE lst) (length lst))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (average (list 1 5 9)) 5.0
   "Average is not being calculated properly")
  (check-equal?
   (average empty) 0.0
   "Average of empty list is not being calculated properly"))

;; valid-sub-list : ListOfRainfall Boolean ListOfRainfall
;;                   -> ListOfRainfall
;; GIVEN: a list of real numbers
;; WHERE: lst is a sub-list of the original list of rainfall readings
;; after processing the elements marked in flst
;; RETURNS: a similar list with readings but only till -999 and with
;; all the negative values removed
;; EXAMPLES: See TESTS for filter-readings below
;; STRATEGY: Using cases on end-found and lst
(define (valid-sub-list lst end-found flst)
  (cond    
    [(equal? (first lst) END-OF-DATA)
     (remove-negative flst)]
    [else
     (valid-sub-list
      (rest lst)
      end-found
      (cons (first lst) flst))]))

;; filter-readings : ListOfRainfall Boolean ListOfRainfall -> Real
;; GIVEN: a list of Rainfall readings
;; RETURNS: a sublist of the original list of Rainfall readings till
;; -999 is found and has all it's negative values removed
;; EXAMPLES: See TESTS belows
;; STRATEGY: Using cases on lst
(define (filter-readings lst end-found flst)
  (cond
    [(empty? lst) empty]
    [else (valid-sub-list lst end-found flst)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (filter-readings l1 DEFAULT-END empty) (list 0)
   "Resultant list with only 0 is not being filtered properly")
  (check-equal?
   (filter-readings l2 DEFAULT-END empty) empty
   "List with -999 as its first element is not filtered properly")
  (check-equal?
   (filter-readings l3 DEFAULT-END empty) (list 10 10 24 2 0)
   "Rainfall readings were not filtered properly")
  (check-equal?
   (filter-readings empty DEFAULT-END empty) empty
   "Empty list is not being filtered properly"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN FUNCTION

;; rainfall : ListOfRainfall -> PosReal
;; GIVEN: a list of rainfall readings
;; WHERE: the list may contain both positive and negative real numbers
;; RETURNS: average of all the non-negative numbers in the list till
;; -999 is encountered
;; EXAMPLES: See TESTS below
;; STRATEGY: Combining simpler functions
(define (rainfall lst)
  (average (filter-readings lst DEFAULT-END empty)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rainfall l3) 9.2
   "Average rainfall calculated incorrecty")
  (check-equal?
   (rainfall empty) 0
   "Average rainfall with empty list calculated incorrectly")
  (check-equal?
   (rainfall l2) 0
   "Average rainfall with starting number as -999 incorrect")
  (check-equal?
   (rainfall l1) 0
   "Average rainfall calculated incorrectly")
  (check-equal?
   (rainfall l4) 16
   "Average rainfall for a list of positive readings is incorrect"))