;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 felleisen-roster
 shivers-roster)

(check-location "04" "class-lists.rkt")

;; CONSTANTS
(define FELLEISEN-COLOR "Yellow")
(define SHIVERS-COLOR "Blue")

;; DATA DEFINITIONS:

;; A Color is one of
;; -- "Yellow"
;; -- "Blue"

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; INTERPRETATION:
;; color - represents the color of the slip which is used to
;; determine the professor
;; name1 - stores the first part of a student's name as String
;; name2 - stores the second part of a student's name as String
;; TEMPLATE:
;; slip-fn : Slip -> ?
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

;;-------------------------------------------------------------

;; Local variables for testing

(define dk-slip (make-slip "Yellow" "Deepak" "Krishnan"))
(define kd-slip (make-slip "Yellow" "Krishnan" "Deepak"))
(define kn-slip (make-slip "Yellow" "Kiran" "Nair"))
(define nk-slip (make-slip "Yellow" "Nair" "Kiran"))
(define mr-slip (make-slip "Blue" "Manoj" "Ravindran"))
(define rm-slip (make-slip "Yellow" "Ravindran" "Manoj"))
(define tj-slip (make-slip "Yellow" "Tenny" "Joseph"))
(define jt-slip (make-slip "Blue" "Joseph" "Tenny"))
(define ga-slip (make-slip "Blue" "Gautham" "Asok"))
(define ag-slip (make-slip "Blue" "Asok" "Gautham"))

(define stud-lst
  (list
   dk-slip kd-slip kn-slip nk-slip mr-slip rm-slip
   tj-slip jt-slip ga-slip ag-slip))

(define dk-lst (list dk-slip kd-slip))
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
;;   (make-slip "Yellow" "Deepak" "Krishnan")
;;   (make-slip "Yellow" "Kiran" "Nair")
;;   (make-slip "Yellow" "Manoj" "Ravindran")
;;   (make-slip "Blue" "Tenny" "Joseph")
;;   (make-slip "Blue" "Gautham" "Asok"))
;; STRATEGY: Use template for ListOfSlip on slips
(define (filter-duplicate slips)
  (if (empty? slips)
      empty
      (append
       (if (duplicate-slip-present? (first slips) (rest slips))
           empty
           (list (first slips)))
       (filter-duplicate (rest slips)))))

;; TESTS:
(begin-for-test
  (check-equal? (filter-duplicate stud-lst)
                (list kd-slip nk-slip rm-slip
                      jt-slip ag-slip)))

;; duplicate-slip-present? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of slips
;; WHERE: the slip and the list of slips form independent sets
;; i.e. the slip passed in as argument is not a part of the
;; list
;; RETURNS: checks for duplicate entries of the slip by
;; comparing it with the each of the elements in the list and
;; returns true iff a duplicate entry is found
;; EXAMPLES:
;; (duplicate-slip-present? dk-slip dk-lst) = #true
;; (duplicate-slip-present? dk-slip trimmed-no-duplicates-lst)
;;   = #false
;; STRATEGY: Use template for ListOfSlip on slips
(define (duplicate-slip-present? s slips)
  (if (empty? slips)
      #false
      (or
       (same-student? s (first slips))
       (duplicate-slip-present? s (rest slips)))))

;; TESTS:
(begin-for-test
  (check-equal? (duplicate-slip-present? dk-slip dk-lst)
                #true)
  (check-equal? (duplicate-slip-present?
                 dk-slip trimmed-no-duplicates-lst)
                #false))

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
  (check-equal? (same-student? dk-slip kd-slip) #true)
  (check-equal? (same-student? dk-slip kn-slip) #false))

;;-------------------------------------------------------------

;; filter-felleisen-students : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list which contains only Prof. Felleisen's slips.
;; Note that the duplicate entries are not removed at this stage.
;; EXAMPLES:
;; (filter-felleisen-students stud-lst) = felleisen-lst
;; (filter-felleisen-students shivers-lst) = empty
(define (filter-felleisen-students all-slips)
  (if (empty? all-slips)
      empty
      (append
       (is-felleisen-student (first all-slips))
       (filter-felleisen-students (rest all-slips)))))

;; TESTS:
(begin-for-test
  (check-equal? (filter-felleisen-students stud-lst) felleisen-lst)
  (check-equal? (filter-felleisen-students shivers-lst) empty))

(define (is-felleisen-student s)
  (if (string=? (slip-color s) FELLEISEN-COLOR) (list s) empty))

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen's class, without duplication.
;; EXAMPLES:
;; (felleisen-roster stud-lst) = final-felleisen-lst
;; (felleisen-roster shivers-lst) = empty
;; (felleisen-roster empty) = empty
;; STRATEGY: Combining simpler functions
(define (felleisen-roster all-slips)
  (if (empty? all-slips)
      empty
      (filter-duplicate (filter-felleisen-students all-slips))))

;; TESTS:
(begin-for-test
  (check-equal? (felleisen-roster stud-lst) final-felleisen-lst)
  (check-equal? (felleisen-roster shivers-lst) empty)
  (check-equal? (felleisen-roster empty) empty))

;;-------------------------------------------------------------

;; filter-shivers-students : ListOfSlip -> ListofSlip
;; GIVEN: a list of slips
;; RETURNS: a list which contains only Prof. Shivers' slips.
;; Note that the duplicate entries are not removed at this stage.
;; EXAMPLES:
;; (filter-shivers-students stud-lst) = shivers-lst
;; (filter-shivers-students felleisen-lst) = empty
;; (filter-shivers-students empty) = empty
;; STRATEGY: Use template for ListOfSlip on all-slips
(define (filter-shivers-students all-slips)
  (if (empty? all-slips)
      empty
      (append
       (is-shivers-student (first all-slips))
       (filter-shivers-students (rest all-slips)))))

;; TESTS:
(begin-for-test
  (check-equal? (filter-shivers-students stud-lst)
                shivers-lst)
  (check-equal? (filter-shivers-students felleisen-lst)
                empty)
  (check-equal? (filter-shivers-students empty) empty))

(define (is-shivers-student s)
  (if (string=? (slip-color s) SHIVERS-COLOR) (list s) empty))

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' class, without duplication.
;; EXAMPLES:
;; (shivers-roster stud-lst) = final-shivers-lst
;; (shivers-roster felleisen-lst) = empty
;; (shivers-roster empty) = empty
;; STRATEGY: Use template for ListOfSlip on all-slips
(define (shivers-roster all-slips)
  (if (empty? all-slips)
      empty
      (filter-duplicate (filter-shivers-students all-slips))))

;; TESTS:
(begin-for-test
  (check-equal? (shivers-roster stud-lst) final-shivers-lst)
  (check-equal? (shivers-roster felleisen-lst) empty)
  (check-equal? (shivers-roster empty) empty))