;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rosters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 roster=?
 rosterset=?
 enrollments-to-rosters
 )

(check-location "05" "rosters.rkt")

;; A SetOfX is a list of X's without duplication.  Two SetOfX's are
;; considered equal if they have the same members.

;; Example: (list (list 1 2) (list 2 1)) is NOT a SetOfSetOfNumber,
;; because (list 1 2) and (list 2 1) represent the same set of numbers.

;; Student is an opaque datatype, but we assume that students may be
;; compared for equality with equal?

;; Class is opaque datatype, but we assume that classes may be
;; compared for equality with equal?

(define-struct enrollment (student class))
;; An Enrollment is a (make-enrollment Student Class)
;; INTERPRETATION:
;; student - represents the student. Student could be a value of any
;; datatype
;; class - represents the class the student is enrolled in. Class could
;; be of any datatype
;; (make-enrollment s c) represents the assertion that student s is
;; enrolled in class c.
;; enrollment-fn : Enrollment -> ?
#; (define (enrollment-fn e)
     (...
      (enrollment-student e)
      (enrollment-class e)))
;; enrollment-student : Enrollment -> Student
;; enrollment-student : Enrollment -> Class
;; GIVEN: an Enrollment
;; RETURNS: the specified attribute of Enrollment

(define-struct roster (classname students))
;; A ClassRoster is a (make-roster Class SetOfStudent)
;; INTERPRETATION:
;; classname - represents the name of the class. Could be of any datatype
;; students - represents the list of students who are enrolled to this
;; class. It is a SetOfX which means that no entry is duplicated within
;; the list.
;; (make-roster c ss) represents that the students in class c are exactly
;; the students in set ss.
;; roster-fn : Roster -> ?
#; (define (roster r)
     (...
      (roster-classname r)
      (roster-students r)))
;; roster-classname : Roster -> Class
;; roster-students : Roster -> SetOfStudents
;; GIVEN: a Roster
;; RETURNS: the specified attribute of the Roster

;; Two ClassRosters are equal if they have the same class and equal
;; sets of students.

;; A ListOfRoster is
;; -- empty
;; -- (cons Roster ListOfRoster)
;; TEMPLATE:
;; lor-fn : ListOfRoster -> ??
;; (define (lor-fn lor)
;;  (cond
;;   [(empty? lor) ...]
;;   [else (... (first lor)
;;              (lor-fn (rest lor)))]))

;; A ListOfEnrollment is
;; -- empty
;; -- (cons Enrollment ListOfEnrollment)
;; TEMPLATE:
;; loe-fn : ListOfEnrollment -> ??
;; (define (loe-fn loe)
;;  (cond
;;   [(empty? loe) ...]
;;   [else (... (first loe)
;;              (loe-fn (rest loe)))]))

;; element-present? : X ListOfX -> Boolean
;; GIVEN: X and a list of X where the datatype is unspecified
;; WHERE: we assume that X can be compared using equal?
;; RETURNS: true iff X is a part of ListOfX
;; EXAMPLES:
;; (element-present? 1 (list 1 2)) = #true
;; (element-present? 3 (list 1 2)) = #false
;; (element-present? 1 empty) = #false
;; STRATEGY: Use HOF ormap on loe
(define (element-present? element loe)
  (ormap
   ;; X -> Boolean
   ;; RETURNS: true iff X is same as element
   (lambda (e) (equal? e element))
   loe))

;; TESTS:
(begin-for-test
  (check-true (element-present? 1 (list 1 2))
              "Element is present within the list")
  (check-false (element-present? 3 (list 1 2))
               "Element is not present within the list")
  (check-false (element-present? 1 empty)
               "Empty list passed. No element found"))

;; subset? : SetOfX SetOfX -> Boolean
;; GIVEN: two lists having elements with unspecified datatype with
;; no duplicate elements
;; WHERE: we assume that the elements can be compared using equal?
;; RETURNS: true iff list2 is a subset of list1
;; EXAMPLES:
;; (subset? (list 1 2) (list 1)) = #true
;; (subset? (list 1 2) (list 1 2)) = #true
;; (subset? (list 1 2) (list 1 3)) = #false
;; (subset? (list 1 2) empty) = #true
;; (subset? empty (list 1 2)) = #false
;; STRATEGY: Use HOF andmap on list2
(define (subset? list1 list2)
  (if (empty? list2)
      #true
      (andmap
       ;; X -> Boolean
       ;; RETURNS: true iff X is present in list1
       (lambda (e) (element-present? e list1))
       list2)))

;; TESTS:
(begin-for-test
  (check-true (subset? (list 1 2) (list 1))
              "List 2 is subset of List 1")
  (check-true (subset? (list 1 2) (list 1 2))
              "List 1 and List 2 are the same")
  (check-false (subset? (list 1 2) (list 1 3))
               "List 2 is not a subset of List 1")
  (check-true (subset? (list 1 2) empty)
              "List 2 is empty and hence the subset of List 1")
  (check-false (subset? empty (list 1 2))
               "List 1 is empty and hence List 2 cannot be a subset"))

;; list=? : SetOfX SetOfX -> Boolean
;; GIVEN: two lists of unspecified datatypes with no duplicated elements
;; WHERE: the elements within the list is assumed to be comparable
;; with equal?
;; RETURNS: true iff both the lists contain the same set of elements
;; EXAMPLES:
;; (list=? (list 1 2) (list 1 2)) = #true
;; (list=? (list 1 2) (list 1 3)) = #false
;; (list=? (list 1 2) (list 1)) = #false
;; (list=? (list 1 2) empty) = #false
;; (list=? empty (list 1 2)) = #false
;; (list=? empty empty) = #false
;; STRATEGY: Combining simpler functions
(define (list=? list1 list2)
  (and (subset? list1 list2)
       (subset? list2 list1)))

;; TESTS:
(begin-for-test
  (check-true (list=? (list 1 2) (list 1 2))
              "List 1 and List 2 are the same")
  (check-false (list=? (list 1 2) (list 1 3))
               "List 1 and List 2 have same length but different elements")
  (check-false (list=? (list 1 2) (list 1))
               "List 1 and List 2 have different lengths")
  (check-false (list=? (list 1 2) empty)
               "List 2 is empty and hence not same as List 1")
  (check-false (list=? empty (list 1 2))
               "List 1 is empty and hence not same as List 2")
  (check-true (list=? empty empty)
              "Both the lists are empty and hence the same"))

;; roster=? : ClassRoster ClassRoster -> Boolean
;; GIVEN: two rosters
;; RETURNS: true iff the two arguments represent the same roster
;; EXAMPLES:
;; (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
;;           (make-roster "PDP" (list "John" "Feng" "Amy"))) = #true
;; (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
;;           (make-roster "HCI" (list "John" "Feng" "Amy"))) = #false
;; (roster=? (make-roster "PDP" (list "John" "Feng"))
;;           (make-roster "PDP" (list "John" "Feng" "Amy"))) = #false
;; (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
;;           (make-roster "PDP" (list "John" "Feng"))) = #false
;; (roster=? (make-roster "HCI" empty)
;;           (make-roster "HCI" empty)) = #true
;; STRATEGY: Combining simpler functions
(define (roster=? roster1 roster2)
  (and (equal? (roster-classname roster1) (roster-classname roster2))
       (list=? (roster-students roster1) (roster-students roster2))))

;; TESTS:
(begin-for-test
  (check-true
   (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
             (make-roster "PDP" (list "John" "Feng" "Amy")))
   "Comparing two rosters which are the same")
  (check-false
   (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
             (make-roster "HCI" (list "John" "Feng" "Amy")))
   "Rosters which have same length but different students")
  (check-false
   (roster=? (make-roster "PDP" (list "John" "Feng"))
             (make-roster "PDP" (list "John" "Feng" "Amy")))
   "Rosters with same class but lesser number of students in list 1")
  (check-false
   (roster=? (make-roster "PDP" (list "John" "Feng" "Amy"))
             (make-roster "PDP" (list "John" "Feng")))
   "Rosters with same class but lesser number of students in list 2")
  (check-true
   (roster=? (make-roster "HCI" empty)
             (make-roster "HCI" empty))
   "Rosters with no students"))

;; roster-present? : Roster SetOfClassRoster -> Boolean
;; GIVEN: a roster and a set of rosters
;; RETURNS: true iff the roster is a part of the set of rosters
;; EXAMPLES:
;; (roster-present? (make-roster 1 (list 1 2 3))
;;                  (list (make-roster 1 (list 1 2 3))
;;                        (make-roster 2 (list 3 5 6))) = #t
;; (roster-present? (make-roster 3 (list 1 2 3))
;;                  (list (make-roster 1 (list 1 2 3))
;;                        (make-roster 2 (list 3 5 6))) = #f
;; (roster-present? (make-roster 3 (list 1 2 3)) empty) = #f
;; STRATEGY: Using HOF ormap on lor
(define (roster-present? roster lor)
  (ormap
   ;; Roster -> Boolean
   ;; RETURNS: true iff r is same as roster 
   (lambda (r) (roster=? roster r))
   lor))

;; TESTS:
(begin-for-test
  (check-true
   (roster-present? (make-roster 1 (list 1 2 3))
                    (list (make-roster 1 (list 1 2 3))
                          (make-roster 2 (list 3 5 6))))
   "Roster present in the set of rosters")
  (check-false
   (roster-present? (make-roster 3 (list 1 2 3))
                    (list (make-roster 1 (list 1 2 3))
                          (make-roster 2 (list 3 5 6))))
   "Roster is not present in the set of rosters")
  (check-false
   (roster-present? (make-roster 3 (list 1 2 3)) empty)
   "Roster is nor present in the set. Set of rosters is empty"))

;; rosterset=? : SetOfClassRoster SetOfClassRoster -> Boolean
;; GIVEN: two sets of rosters
;; RETURNS: true iff the two arguments represent the same set of rosters
;; EXAMPLES:
;; (rosterset=? (list (make-roster 1 (list 1 2 3))
;;                    (make-roster 2 (list 3 5 6)))
;;              (list (make-roster 1 (list 1 2 3))
;;                    (make-roster 2 (list 3 5 6)))) = #t
;; (rosterset=? (list (make-roster 1 (list 1 2 3))
;;                    (make-roster 3 (list 3 5 6)))
;;              (list (make-roster 2 (list 4 5 3))
;;                    (make-roster 3 (list 3 5 6)))) = #f
;; (rosterset=? empty empty) = #t
;; STRATEGY: Use HOF andmap on soc2
(define (rosterset=? soc1 soc2)
  (andmap
   ;; Roster -> Boolean
   ;; RETURNS: true iff r is present in soc1
   (lambda (r) (roster-present? r soc1))
   soc2))

;; TESTS:
(begin-for-test
  (check-true
   (rosterset=? (list (make-roster 1 (list 1 2 3))
                      (make-roster 2 (list 3 5 6)))
                (list (make-roster 1 (list 1 2 3))
                      (make-roster 2 (list 3 5 6))))
   "Comparing two sets containing the same rosters")
  (check-false
   (rosterset=? (list (make-roster 1 (list 1 2 3))
                      (make-roster 3 (list 3 5 6)))
                (list (make-roster 2 (list 4 5 3))
                      (make-roster 3 (list 3 5 6))))
   "Comparing two sets which have different rosters")
  (check-true (rosterset=? empty empty)
              "Comparing two empty sets of rosters"))

;; class-roster-available? : Class SetOfClassRoster -> Boolean
;; GIVEN: a class name and a list of rosters
;; RETURNS: true iff the class has an existing roster within the list
;; EXAMPLES:
;; (class-roster-available? 1 (list (make-roster 1 (list 1 2 3))
;;                                  (make-roster 2 (list 3 5 6))) = #t
;; (class-roster-available? 3 (list (make-roster 1 (list 1 2 3))
;;                                  (make-roster 2 (list 3 5 6))) = #f
;; STRATEGY: Using HOF ormap on lor
(define (class-roster-available? class lor)
  (ormap
   ;; Roster -> Boolean
   ;; RETURNS: true iff Roster has the same class name as the
   ;; argument class
   (lambda (r) (equal? (roster-classname r) class))
   lor))

;; TESTS:
(begin-for-test
  (check-true
   (class-roster-available? "PDP"
                            (list (make-roster "PDP" (list 1 2 3))
                                  (make-roster "HCI" (list 3 5 6))))
   "Class roster is available with the set of rosters")
  (check-false
   (class-roster-available? 3 (list (make-roster 1 (list 1 2 3))
                                    (make-roster 2 (list 3 5 6))))
   "Class roster is not available within the set of rosters"))

;; class-roster : Class SetOfClassRoster -> Roster
;; GIVEN: a class and a set of rosters
;; WHERE: a roster is available for the class specified
;; RETURNS: the roster for the class specified within the argument
;; EXAMPLES:
;; (class-roster 1 (list (make-roster 1 (list 1 2 3))
;;                       (make-roster 2 (list 3 5 6)))) =
;; (make-roster 1 (list 1 2 3))
;; STRATEGY: Use HOF filter on soc
(define (class-roster class soc)
  (first (filter
          ;; Roster -> Roster
          ;; RETURNS: roster iff the roster's class is same as
          ;; mentioned in the argument class
          (lambda (r) (equal? (roster-classname r) class))
          soc)))

;; TESTS:
(begin-for-test
  (check
   roster=?
   (class-roster 1 (list (make-roster 1 (list 1 2 3))
                         (make-roster 2 (list 3 5 6))))
   (make-roster 1 (list 2 1 3))
   "Roster is available within the set of rosters"))

;; lor-with-class-removed : Class SetOfClassRoster -> SetOfClassRoster
;; GIVEN: a class and a set of rosters
;; WHERE: a roster is available for the class specified as argument
;; RETURNS: a set of rosters with the roster for the class specified
;; as argument removed from it
;; EXAMPLES:
;; (lor-with-class-removed 1 (list (make-roster 1 (list 1 2 3))
;;                                 (make-roster 2 (list 3 5 6))))
;;  = (list (make-roster 2 (list 3 5 6)))
;; STRATEGY: Use HOF filter on soc
(define (lor-with-class-removed class soc)
  (filter
   ;; Roster -> Roster
   ;; RETURNS: the roster iff the roster's class is not same as
   ;; mentioned in the argument class
   (lambda (r) (not (equal? (roster-classname r) class)))
   soc))

;; TESTS:
(begin-for-test
  (check
   rosterset=?
   (lor-with-class-removed 1 (list (make-roster 1 (list 1 2 3))
                                   (make-roster 2 (list 3 5 6))))
   (list (make-roster 2 (list 5 6 3)))
   "Available roster is removed from the set of rosters"))

;; new-roster : Enrollment -> Roster
;; GIVEN: an enrollment
;; WHERE: a roster doesn't exist for the class specified within the
;; enrollment
;; RETURNS: a roster for the new class with the student added to it
;; EXAMPLES:
;; (new-roster (make-enrollment "John" "PDP")) =
;;  (make-roster "PDP" (list "John"))
;; STRATEGY: Use template for Enrollment on e
(define (new-roster e)
  (make-roster (enrollment-class e) (list (enrollment-student e))))

;; TESTS:
(begin-for-test
  (check
   roster=?
   (new-roster (make-enrollment "John" "PDP"))
   (make-roster "PDP" (list "John"))
   "Create a new roster for the enrollment"))

;; add-to-roster : Enrollment Roster -> Roster
;; GIVEN: an enrollment and a roster
;; WHERE: the roster is for the class specified within the enrollment
;; RETURNS: an updated roster with the student specified within the
;; enrollment added to it
;; EXAMPLES:
;; (add-to-roster (make-enrollment "John" "PDP")
;;                (make-roster "PDP" (list "Amy" "Feng")))
;;  = (make-roster "PDP" (list "John" "Amy" "Feng"))
;; STRATEGY: Use template for Enrollment on e and template for
;; Roster on r
(define (add-to-roster e r)
  (make-roster (enrollment-class e)
               (cons (enrollment-student e)
                     (roster-students r))))

;; TESTS:
(begin-for-test
  (check
   roster=?
   (add-to-roster (make-enrollment "John" "PDP")
                  (make-roster "PDP" (list "Feng" "Amy")))
   (make-roster "PDP" (list "John" "Amy" "Feng"))
   "Add the student to the roster passed as argument"))

;; new-enrollment-in-roster : Enrollment SetOfClassRoster -> Roster
;; GIVEN: an enrollment and a set of rosters
;; RETURNS: an updated roster with the new student added to it if
;; the roster already exists for the specified class, else a new
;; roster
;; EXAMPLES:
;; (new-enrollment-in-roster
;;   (make-enrollment "John" "PDP")
;;   (list (make-roster "PDP" (list "Amy" "Feng"))))
;;  = (make-roster "PDP" (list "John" "Amy" "Feng"))
;; (new-enrollment-in-roster (make-enrollment "John" "PDP") empty)
;;  = (make-roster "PDP" (list "John"))
;; STRATEGY: Cases on if a roster is available for the class specified
;; within e
(define (new-enrollment-in-roster e soc)
  (if (class-roster-available? (enrollment-class e) soc)
      (add-to-roster e (class-roster (enrollment-class e) soc))
      (new-roster e)))

;; TESTS:
(begin-for-test
  (check
   roster=?
   (new-enrollment-in-roster
    (make-enrollment "John" "PDP")
    (list (make-roster "PDP" (list "Amy" "Feng"))))
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   "Add the enrollment to an existing roster and return the roster")
  (check
   roster=?
   (new-enrollment-in-roster (make-enrollment "John" "PDP") empty)
   (make-roster "PDP" (list "John"))
   "Add the enrollment to a new roster and return the roster"))

;; lor-with-new-enrollment : Enrollment SetOfClassRoster ->
;;  SetOfClassRoster
;; GIVEN: an enrollment and a set of rosters
;; RETURNS: an updated set of rosters with the new student enrolled
;; to the class specified
;; EXAMPLES:
;; (lor-with-new-enrollment
;;  (make-enrollment "John" "PDP")
;;  (list (make-roster "PDP" (list "Amy" "Feng"))
;;        (make-roster "HCI" (list "Kathryn" "Amy")))) =
;;  (list (make-roster "PDP" (list "John" "Amy" "Feng"))
;;        (make-roster "HCI" (list "Kathryn" "Amy")))
;; (lor-with-new-enrollment (make-enrollment "John" "PDP") empty)
;;  = (list (make-roster "PDP" (list "John")))
;; STRATEGY: Use template for Enrollment on e
(define (lor-with-new-enrollment e soc)
  (cons (new-enrollment-in-roster e soc)
        (lor-with-class-removed (enrollment-class e) soc)))

;; TESTS:
(begin-for-test
  (check
   rosterset=?
   (lor-with-new-enrollment
    (make-enrollment "John" "PDP")
    (list (make-roster "PDP" (list "Amy" "Feng"))
          (make-roster "HCI" (list "Kathryn" "Amy"))))
   (list (make-roster "HCI" (list "Amy" "Kathryn"))
         (make-roster "PDP" (list "Amy" "John" "Feng")))
   "Add the enrollment to an existing roster and return all rosters")
  (check
   rosterset=?
   (lor-with-new-enrollment (make-enrollment "John" "PDP") empty)
   (list (make-roster "PDP" (list "John")))
   "Add the enrollment to a new roster and return all rosters"))

;; enrollments-to-rosters: SetOfEnrollment -> SetOfClassRoster
;; GIVEN: a set of enrollments
;; RETURNS: the set of class rosters for the given enrollments
;; EXAMPLE:
;; (enrollments-to-rosters
;;    (list (make-enrollment "John" "PDP")
;;          (make-enrollment "Kathryn" "Networks")
;;          (make-enrollment "Feng" "PDP")
;;          (make-enrollment "Amy" "PDP")
;;          (make-enrollment "Amy" "Networks")))
;; =>
;; (list
;;   (make-roster "PDP" (list "John" "Feng" "Amy"))
;;   (make-roster "Networks" (list "Kathryn" "Amy")))
;; STRATEGY: Use HOF foldr on soe
(define (enrollments-to-rosters soe)
  (foldr lor-with-new-enrollment empty soe))

;; TESTS:
(define soe (list (make-enrollment "John" "PDP")
                  (make-enrollment "Kathryn" "Networks")
                  (make-enrollment "Feng" "PDP")
                  (make-enrollment "Amy" "PDP")
                  (make-enrollment "Amy" "Networks")))

(define sor (list (make-roster "PDP" (list "John" "Feng" "Amy"))
                  (make-roster "Networks" (list "Kathryn" "Amy"))))

(define soe-numbers
  (list (make-enrollment 1 5010)
        (make-enrollment 2 5010)
        (make-enrollment 1 5340)
        (make-enrollment 3 5340)
        (make-enrollment 4 5011)))

(define sor-numbers
  (list (make-roster 5010 (list 2 1))
        (make-roster 5340 (list 1 3))
        (make-roster 5011 (list 4))))

(begin-for-test
  (check rosterset=?
         (enrollments-to-rosters soe) sor
         "Datatype:String-Convert set of enrollments -> set of rosters")
  (check rosterset=?
         (enrollments-to-rosters soe-numbers) sor-numbers
         "Datatype:Integer-Convert set of enrollments -> set of rosters"))