;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

(check-location "07" "outlines.rkt")

;;---------------------------------------------------------------------
;; DATA DEFINITIONS

;; A NaturalNumber is a non-negative integer

;; A ListOfNaturalNumber is a list of non-negative integers
;; A ListOfNatualNumber can be either
;; -- empty
;; -- (cons NaturalNumber ListOfNaturalNumber)
;; TEMPLATE:
;; lon-fn : ListOfNaturalNumber -> ??
#; (define (lon-fn lon)
     (cond
       [(empty? lon) ...]
       [else (... (first lon)
                  (lon-fn (rest lon)))]))

(define-struct line (sec-no text))
;; A Line is a (make-line ListOfNaturalNumber String)
;; INTERPRETATION:
;; (make-line ListOfNaturalNumber String) is a Line where:
;;  -- sec-no: represents the section number of the current line as a
;;             ListOfNaturalNumber
;;  -- text: represents the actual text in the current line
;; TEMPLATE:
;; line-fn : Line -> ??
#; (define (line-fn l)
     (...
      (lon-fn (line-sec-no l))
      (line-text l)))

;; line-sec-no : Line -> ListOfNaturalNumber
;; line-text : Line -> String
;; GIVEN: a Line
;; RETURNS: the specified attribute of the Line
;; The above functions are part of line struct and are created
;; implicitly when an instance of this struct is created

;; A ListOfLine is either:
;; -- empty
;; -- (cons Line ListOfLine)
;; TEMPLATE:
;; lol-fn : ListOfLine -> ??
#; (define (lol-fn lol)
     (cond
       [(empty? lol) ...]
       [else (... (first lol)
                  (lol-fn (rest lol)))]))

(define-struct section (str secs))
;; A Section is a (make-section String ListOfSection)
;; INTERPRETATION:
;; (make-section str secs) is a section where
;;  -- str : header text of the section
;;  -- secs : list of subsections of the section
;; TEMPLATE:
;; section-fn : Section -> ??
#; (define (section-fn s)
     (...
      (section-str s)
      (los-fn (section-secs s))))

;; A ListOfSection is either:
;; -- empty
;; -- (cons Section ListOfSection)
;; TEMPLATE:
;; los-fn : ListOfSection -> ??
#; (define (los-fn los)
     (cond
       [(empty? los) ...]
       [else (... (section-fn (first los))
                  (los-fn (rest los)))]))

;; section-str : Section -> String
;; section-secs : Section -> ListOfSection
;; GIVEN: a Section
;; RETURNS: the specified attribute of the Section
;; The above functions are part of line struct and are created
;; implicitly when an instance of this struct is created

;; An Outline is a ListOfSection

;;---------------------------------------------------------------------
;; TEST CASE VARIABLE DEFINITIONS

(define sec-1 (make-line (list 1)
                         "The first section"))
(define sec-1-1 (make-line (list 1 1)
                           "A subsection with no subsections"))
(define sec-1-2 (make-line (list 1 2)
                           "Another subsection"))
(define sec-1-2-1 (make-line (list 1 2 1)
                             "This is a subsection of 1.2"))
(define sec-1-2-2 (make-line (list 1 2 2)
                             "This is another subsection of 1.2"))
(define sec-1-3 (make-line (list 1 3)
                           "The last subsection of 1"))
(define sec-2 (make-line (list 2)
                         "Another section"))
(define sec-2-1 (make-line (list 2 1)
                           "More stuff"))
(define sec-2-2 (make-line (list 2 2)
                           "Still more stuff"))
(define para (list sec-1 sec-1-1 sec-1-2 sec-1-2-1
                   sec-1-2-2 sec-1-3
                   sec-2 sec-2-1 sec-2-2))
(define para2 (list sec-1  sec-1-2-1))
(define tree-input
  (list 
   (make-section
    "The first section"
    (list
     (make-section
      "A subsection with no subsections" empty)
     (make-section
      "Another subsection"
      (list
       (make-section
        "This is a subsection of 1.2" empty)
       (make-section
        "This is another subsection of 1.2" empty)))
     (make-section
      "The last subsection of 1" empty)))
   (make-section
    "Another section"
    (list
     (make-section "More stuff" empty)
     (make-section "Still more stuff" empty)))))

;;---------------------------------------------------------------------
;; HELPER FUNCTIONS

;; add-subsection : ListOfNaturalNumber -> ListOfNaturalNumber
;; GIVEN: a list of NaturalNumber which represents a section number
;; RETURNS: a list similar to the one which is passed in, but with
;; a new section number appended to it's end
;; EXAMPLES:
;; (add-subsection empty) = (list 0)
;; (add-subsection (list 1 2)) = (list 1 2 0)
;; STRATEGY: Combining simpler functions
(define (add-subsection lon)
  (append lon (list 0)))

;; TESTS:
(begin-for-test
  (check-equal? (add-subsection empty) (list 0))
  (check-equal? (add-subsection (list 1 2)) (list 1 2 0)))

;; update-subsection : ListOfNaturalNumber NaturalNumber
;;                      -> ListOfNaturalNumber
;; GIVEN: a list of NaturalNumber which represents a section number and
;; the subsection number which needs to be updated
;; WHERE: lon is a sublist of some list lon0 and level is the position
;; of the last element of the previous list sublist
;; RETURNS: a list similar to the one passed, but with the last
;; sub-section incremented by 1
;; EXAMPLES:
;; (update-subsection (list 1 1) 2) = (list 1 2)
;; STRATEGY: Use cases on if level is equal to 1
(define (update-subsection lon level)
  (if (equal? level 1)
      (list (+ 1 (first lon)))
      (cons (first lon)
            (update-subsection (rest lon) (- level 1)))))

;; TESTS:
(begin-for-test
  (check-equal? (update-subsection (list 1 1) 2) (list 1 2)))

;; remove-subsection : ListOfNaturalNumber ListOfNaturalNumber
;;                      NaturalNumber -> ListOfNaturalNumber
;; GIVEN: two lists that represents section numbers and the sub-section
;; level after which the elements are to be removed
;; WHERE: lon is a sublist of some list lon0 and level is a counter
;; which when becomes 0, new-lon would be the required section number
;; RETURNS: a list similar to lon which has all the subsections after
;; the specified level removed
;; EXAMPLES:
;; (remove-subsection (list 1 2 2) empty 2) = (list 1 2)
;; STRATEGY: Using cases on if level > 0
(define (remove-subsection lon new-lon level)
  (if (> level 0)
      (remove-subsection
       (rest lon)
       (append new-lon (list (first lon)))
       (- level 1))
      new-lon))

;; TESTS:
(begin-for-test
  (check-equal?
   (remove-subsection (list 1 2 2) empty 2) (list 1 2)))

;; same-sub-exp? : ListOfNaturalNumber ListOfNaturalNumber -> Boolean
;; GIVEN: two lists that represent the section numbers to be compared
;; RETURNS: true iff all the corresponding elements of the two lists
;; are equal
;; EXAMPLES:
;; (same-sub-exp? (list 1 2) (list 1 2)) = true
;; (same-sub-exp? (list 1 2) (list 1 3)) = false
;; (same-sub-exp? (list 1 2) (list 3 2)) = false
;; (same-sub-exp? (list 1 2) (list 3 4)) = false
;; STRATEGY: Combining simpler functions
(define (same-sub-exp? curr-lon expected-lon)
  (cond
    [(empty? curr-lon) true]
    [else
     (and (equal? (first curr-lon) (first expected-lon))
          (same-sub-exp? (rest curr-lon) (rest expected-lon)))]))

;; TESTS:
(begin-for-test
  (check-true (same-sub-exp? (list 1 2) (list 1 2)))
  (check-false (same-sub-exp? (list 1 2) (list 1 3)))
  (check-false (same-sub-exp? (list 1 2) (list 3 2)))
  (check-false (same-sub-exp? (list 1 2) (list 3 4))))

;; validate-sub-section : Line ListOfNaturalNumber -> Boolean
;; GIVEN: a line and a list of NaturalNumber
;; WHERE: curr-line is the sublist of some list lst0 and prev-lon is
;; the section number of the previous line
;; RETURNS: true iff the section number of curr-line is same as the
;; section number which follows the previous section number
;; EXAMPLES:
;; (validate-sub-section sec-1 (list 0)) = true
;; (validate-sub-section sec-1-2 (list 1 1)) = true
;; (validate-sub-section sec-1-3 (list 1 2 2)) = true
;; (validate-sub-section sec-2 (list 4)) = false
;; STRATEGY: Using cases on length of the current section number and
;; previous section number
(define (validate-sub-section curr-line prev-lon)
  (cond    
    [(equal? (length (line-sec-no curr-line))
             (length prev-lon))
     (same-sub-exp?
      (line-sec-no curr-line)
      (update-subsection prev-lon (length prev-lon)))]
    [(= (length (line-sec-no curr-line)) (+ (length prev-lon) 1))
     (validate-sub-section
      curr-line (add-subsection prev-lon))]
    [(< (length (line-sec-no curr-line)) (length prev-lon))
     (validate-sub-section
      curr-line
      (remove-subsection
       prev-lon
       empty
       (length (line-sec-no curr-line))))]
    [else false]))

;; TESTS:
(begin-for-test
  (check-true (validate-sub-section sec-1 (list 0)))
  (check-true (validate-sub-section sec-1-1 (list 1)))
  (check-true (validate-sub-section sec-1-2 (list 1 1)))
  (check-true (validate-sub-section sec-1-2-1 (list 1 2)))
  (check-true (validate-sub-section sec-1-2-2 (list 1 2 1)))
  (check-true (validate-sub-section sec-1-3 (list 1 2 2)))
  (check-true (validate-sub-section sec-2 (list 1 3)))
  (check-true (validate-sub-section sec-2 (list 1 2 2)))
  (check-false (validate-sub-section sec-2 (list 4))))

;; validate-all-subsections : ListOfLine ListOfNaturalNumber -> Boolean
;; GIVEN: a list of lines and the previous section number
;; WHERE: lol is a sublist of some list lol0 and prev-lon represents
;; the previous section number
;; RETURNS: true iff the section numbers of all the lines in the
;; list of lines are in the expected order
;; EXAMPLES:
;; (validate-all-subsections para (list 0)) = true
;; STRATEGY: Use template for Line/ListOfLine
(define (validate-all-subsections lol prev-lon)
  (cond
    [(empty? lol) true]
    [else
     (and (validate-sub-section (first lol) prev-lon)
          (validate-all-subsections (rest lol)
                                    (line-sec-no (first lol))))]))

;; TESTS:
(begin-for-test
  (check-true (validate-all-subsections para (list 0))))

;; flat-rep-for-sec : Section ListOfNaturalNumber -> FlatRep
;; GIVEN : a section and a ListOfNumber which contains previous
;; section number
;; RETURNS : the flat representation of the outline
;; EXAMPLES :
;; (flat-rep-for-sec
;;  (make-section "A subsection with no subsections" empty) (list 0))
;;  = (list (make-line (list 1) "A subsection with no subsections"))
;; STRATEGY : Using template for Section on curr-sec
(define (flat-rep-for-sec curr-sec prev-secno)
  (cons
   (make-line
    (update-subsection prev-secno (length prev-secno))
    (section-str curr-sec))
   (flat-rep-for-all-secs (section-secs curr-sec)
                          (add-subsection
                           (update-subsection
                            prev-secno
                            (length prev-secno))))))

;; flat-rep-for-all-secs : Outline ListOfNaturalNumber -> FlatRep
;; GIVEN : a list of sections and the previous section number
;; WHERE: secs is a sublist of some list secs0 and prev-secno is the
;; last generated section number
;; RETURNS : the flat representation of the outline
;; EXAMPLES :
;; (flat-rep-for-all-secs tree-input (list 0))= para
;; STRATEGY : Using cases on secs
(define (flat-rep-for-all-secs secs prev-secno)
  (cond
    [(empty? secs) empty]
    [else
     (append (flat-rep-for-sec (first secs) prev-secno)
             (flat-rep-for-all-secs
              (rest secs)
              (update-subsection prev-secno (length prev-secno))))]))

;;---------------------------------------------------------------------
;; PROVIDED FUNCTIONS

;; legal-flat-rep? : ListOfLine -> Boolean
;; GIVEN: a list of lines
;; RETURNS: true iff it is a legal flat representation of an outline
;; EXAMPLES:
;; (legal-flat-rep? para)= true
;; (legal-flat-rep? para2)= false
;; STRATEGY: Calling a more general function
(define (legal-flat-rep? lol)
  (validate-all-subsections lol (list 0)))

;; TESTS
(begin-for-test
  (check-equal? (legal-flat-rep? para) true)
  (check-equal? (legal-flat-rep? para2) false))

;; tree-rep-to-flat-rep : Outline -> FlatRep
;; GIVEN: a outline which is a list of sections
;; RETURNS: the equivalent flat representation of the outline
;; EXAMPLES:
;; (tree-rep-to-flat-rep tree-input) = para
;; STRATEGY: Calling a more general function
(define (tree-rep-to-flat-rep outline)
  (flat-rep-for-all-secs outline (list 0)))

;; TESTS
(begin-for-test
  (check-equal? (tree-rep-to-flat-rep tree-input) para)
  (check-equal? (flat-rep-for-all-secs tree-input (list 0)) para)
  (check-equal?
   (flat-rep-for-sec
    (make-section "A subsection with no subsections" empty) (list 0))
   (list (make-line (list 1) "A subsection with no subsections"))))