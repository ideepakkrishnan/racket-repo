;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 felleisen-roster
 shivers-roster
 make-slip
 slip-color
 slip-name1
 slip-name2)

(check-location "05" "class-lists.rkt")

;; CONSTANTS
(define FELLEISEN-COLOR "yellow")
(define SHIVERS-COLOR "blue")

;; DATA DEFINITIONS:

;; A Color is one of
;; -- "yellow"
;; -- "blue"

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; INTERPRETATION:
;; color - represents the color of the slip which is used to
;; determine the professor
;; name1 - stores the first part of a student's name as String
;; name2 - stores the second part of a student's name as String
;; TEMPLATE:
;; slip-fn : Slip -> ??
#; (define (slip-fn s)
     (...
      (slip-color s)
      (slip-name1 s)
      (slip-name2 s)))

;; slip-color : Slip -> Color
;; slip-name1 : Slip -> String
;; slip-name2 : Slip -> String
;; GIVEN: a Slip
;; RETURNS: the color of the slip, first name and last name. The
;; above functions are part of Slip struct and are created
;; implicitly when an instance of this struct is created.

;; A ListOfSlip is
;; -- empty
;; -- (cons Slip ListOfSlip)
;; TEMPLATE:
;; los-fn : ListOfSlip -> ??
;; (define (los-fn los)
;;  (cond
;;   [(empty? los) ...]
;;   [else (... (first los)
;;              (los-fn (rest los)))]))

;;-------------------------------------------------------------

;; Local variables for testing

(define dk-slip (make-slip "yellow" "Deepak" "Krishnan"))
(define kd-slip (make-slip "yellow" "Krishnan" "Deepak"))
(define kn-slip (make-slip "yellow" "Kiran" "Nair"))
(define nk-slip (make-slip "yellow" "Nair" "Kiran"))
(define mr-slip (make-slip "blue" "Manoj" "Ravindran"))
(define rm-slip (make-slip "yellow" "Ravindran" "Manoj"))
(define tj-slip (make-slip "yellow" "Tenny" "Joseph"))
(define jt-slip (make-slip "blue" "Joseph" "Tenny"))
(define ga-slip (make-slip "blue" "Gautham" "Asok"))
(define ag-slip (make-slip "blue" "Asok" "Gautham"))

(define stud-lst
  (list
   dk-slip kd-slip kn-slip nk-slip mr-slip rm-slip
   tj-slip jt-slip ga-slip ag-slip))

(define dk-lst (list kd-slip))
(define no-duplicates-lst
  (list dk-slip kn-slip mr-slip tj-slip ga-slip))
(define trimmed-no-duplicates-lst
  (list kn-slip mr-slip tj-slip ga-slip))
(define felleisen-lst (list dk-slip kd-slip kn-slip nk-slip
                            rm-slip tj-slip))
(define final-felleisen-lst (list kd-slip nk-slip rm-slip
                                  tj-slip))
(define shivers-lst (list mr-slip jt-slip ga-slip ag-slip))
(define final-shivers-lst (list mr-slip jt-slip ag-slip))

;;-------------------------------------------------------------

;; GENERAL HELPER FUNCTIONS

;; filter-duplicate : ListOfSlip -> ListOfSlip
;; GIVEN: a list of Slips
;; RETURNS: a similar list with the duplicate entries for
;; each student removed
;; EXAMPLES:
;; (filter-duplicate stud-lst) =
;;  (list
;;   (make-slip "yellow" "Deepak" "Krishnan")
;;   (make-slip "yellow" "Kiran" "Nair")
;;   (make-slip "yellow" "Manoj" "Ravindran")
;;   (make-slip "blue" "Tenny" "Joseph")
;;   (make-slip "blue" "Gautham" "Asok"))
;; STRATEGY: Use HOF foldr on slips
(define (filter-duplicate slips)
  (foldr add-if-no-duplicate-present empty slips))

;; TESTS:
(begin-for-test
  (check-equal? (filter-duplicate stud-lst)
                (list kd-slip nk-slip rm-slip jt-slip ag-slip)
                "Return a list where all duplicates are removed"))

;; add-if-no-duplicate-present : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a slip and a list of slips
;; WHERE: the slip and the list of slips form independent sets
;; i.e. the slip passed in as argument is not a part of the
;; list
;; RETURNS: checks for duplicate entries of the slip by
;; comparing it with the each of the elements in the list and
;; returns a updated list which contains the slip if no duplilcate
;; entries are found
;; EXAMPLES:
;; (add-if-no-duplicate-present dk-slip dk-lst) = (list kd-slip)
;; (add-if-no-duplicate-present dk-slip trimmed-no-duplicates-lst)
;;   = trimmed-no-duplicates-lst
;; STRATEGY: Use HOF ormap on slips
(define (add-if-no-duplicate-present s slips)
  (if (ormap
       ;; Slip -> Boolean
       ;; GIVEN: a slip
       ;; RETURNS: true iff the slip contains the information for
       ;; same student as s
       (lambda (slip) (same-student? s slip))
       slips)
      slips (cons s slips)))

;; TESTS:
(begin-for-test
  (check-equal?
   (add-if-no-duplicate-present dk-slip dk-lst) (list kd-slip)
   "Add slip to list having a duplicate element")
  (check-equal?
   (add-if-no-duplicate-present dk-slip trimmed-no-duplicates-lst)
   (cons dk-slip trimmed-no-duplicates-lst)
   "Add slip to list with no duplicate elements"))

;; same-student? : Slip Slip -> Boolean
;; GIVEN: two slips
;; RETURNS: whether the two slips contain the information
;; about the same student or not
;; EXAMPLES:
;; (same-student? dk-slip kd-slip) = #true
;; (same-student? dk-slip kn-slip) = #false
;; STRATEGY: Use template for Slip on slip1 and slip2
(define (same-student? slip1 slip2)
  (or
   (and (string=? (slip-name1 slip1) (slip-name1 slip2))
        (string=? (slip-name2 slip1) (slip-name2 slip2)))
   (and (string=? (slip-name2 slip1) (slip-name1 slip2))
        (string=? (slip-name1 slip1) (slip-name2 slip2)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (same-student? dk-slip kd-slip) #true
   "Compare two slips that contain details about the same student")
  (check-equal?
   (same-student? kd-slip kd-slip) #true
   "Compare two slips that contain exact same information")
  (check-equal?
   (same-student? dk-slip kn-slip) #false
   "Compare two slips that contain details about two different students"))

;; filter-each-student : ListOfSlip Color -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips which are of the same color as
;; specified within the argument
;; EXAMPLES:
;; (filter-each-student stud-lst FELLEISEN-COLOR) =
;;  felleisen-lst
;; (filter-each-student felleisen-lst SHIVERS-COLOR) = empty
;; STRATEGY: Use HOF filter on all-slips
(define (filter-each-student all-slips color)
  (filter
   ;; Slip -> Slip
   ;; GIVEN: a slip
   ;; RETURNS: the slip iff it has the color passed as argument
   (lambda (s) (string=? (slip-color s) color))
   all-slips))

;; TESTS:
(begin-for-test
  (check-equal?
   (filter-each-student stud-lst FELLEISEN-COLOR) felleisen-lst
   "Return a list of students in Prof.Felleisen's class")
  (check-equal?
   (filter-each-student felleisen-lst SHIVERS-COLOR) empty
   "Return a list of students in Prof.Shivers' class"))

;;-------------------------------------------------------------

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen's class, without duplication.
;; EXAMPLES:
;; (felleisen-roster stud-lst) = final-felleisen-lst
;; (felleisen-roster shivers-lst) = empty
;; (felleisen-roster empty) = empty
;; STRATEGY: Call a more general function
(define (felleisen-roster all-slips)
  (filter-duplicate (filter-each-student all-slips FELLEISEN-COLOR)))

;; TESTS:
(begin-for-test
  (check-equal?
   (felleisen-roster stud-lst) final-felleisen-lst
   "Filter and remove duplicates from a list having Felleisen's students")
  (check-equal?
   (felleisen-roster shivers-lst) empty
   "Filter and remove duplicates from a list with 0 Felleisen's students")
  (check-equal?
   (felleisen-roster empty) empty
   "Filter and remove duplicates from an empty list"))

;;-------------------------------------------------------------

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' class, without duplication.
;; EXAMPLES:
;; (shivers-roster stud-lst) = final-shivers-lst
;; (shivers-roster felleisen-lst) = empty
;; (shivers-roster empty) = empty
;; STRATEGY: Call a more general function
(define (shivers-roster all-slips)
  (filter-duplicate (filter-each-student all-slips SHIVERS-COLOR)))

;; TESTS:
(begin-for-test
  (check-equal?
   (shivers-roster stud-lst) final-shivers-lst
   "Filter and remove duplicates from a list having Shivers' students")
  (check-equal?
   (shivers-roster felleisen-lst) empty
   "Filter and remove duplicates from a list with 0 Shivers' students")
  (check-equal?
   (shivers-roster empty) empty
   "Filter and remove duplicates from an empty list"))