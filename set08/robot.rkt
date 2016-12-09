;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide
 path
 eval-plan)

(check-location "08" "robot.rkt")

;;---------------------------------------------------------------------
;; CONSTANTS

(define NE "ne")
(define NW "nw")
(define SE "se")
(define SW "sw")
(define X "x")
(define Y "y")
(define STEP 1)

;;---------------------------------------------------------------------
;; DATA DEFINITIONS

;; A Direction is one of
;; -- NE
;; -- SD
;; -- SW
;; -- NW

;; A ListOfX is
;; -- empty
;; -- (cons X ListOfX)
;; TEMPLATE:
;; list-fn : ListOfX -> ??
;; (define (list-fn lst)
;;  (cond
;;   [(empty? lst) ...]
;;   [else (... (first lst)
;;              (list-fn (rest lst)))]))

;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y).
;; Note: this is not to be confused with the built-in data type Posn.

;; A ListOfPosition is
;; -- empty
;; -- (cons Position ListOfPosition)
;; TEMPLATE:
;; lop-fn : ListOfPosition -> ??
;; (define (lop-fn lop)
;;  (cond
;;   [(empty? lop) ...]
;;   [else (... (first lop)
;;              (lop-fn (rest lop)))]))

;; A Move is a (list Direction PosInt)
;; Interp: a move of the specified number of steps in the indicated
;; direction. 

;; A ListOfMove is
;; -- empty
;; -- (cons Move ListOfMove)
;; TEMPLATE:
;; lom-fn : ListOfMove -> ??
;; (define (lom-fn lom)
;;  (cond
;;   [(empty? lom) ...]
;;   [else (... (first lom)
;;              (lom-fn (rest lom)))]))

;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.

;;---------------------------------------------------------------------
;; LOCAL VARIABLES FOR TESTING

(define pos-lst (list (list 0 0) (list -1 1) (list 0 2)
                      (list 1 3) (list 2 2)))
(define combined-mov-lst (list (list NW 1) (list NE 2)
                               (list SE 1)))
(define exploded-mov-lst (list (list NW 1) (list NE 1)
                               (list NE 1) (list SE 1)))
(define blocks (list (list 0 1) (list 1 1) (list 0 -1)
                     (list 1 -1) (list -1 0)))
(define wall (list (list -1 1) (list 1 1) (list -1 -1) (list 1 -1)))
(define expanded-blocks (list (list 0 1) (list 1 1) (list 0 -1)
                              (list 1 -1) (list -1 0) (list 2 4)
                              (list 2 2)))
(define expanded-path '((0 0) (-1 1) (0 2) (1 3) (0 4) (1 5) (2 6)
                              (3 5) (4 4) (3 3)))
(define super-expanded-blocks
  (list (list 0 1) (list 1 1) (list 0 -1) (list 1 -1) (list -1 0)
        (list 2 2) (list 2 4) (list 0 4)))
(define super-expanded-path
  (list (list 0 0) (list -1 1) (list 0 2) (list -1 3) (list -2 2)
        (list -3 3) (list -2 4) (list -1 5) (list 0 6) (list 1 5)
        (list 2 6) (list 3 5) (list 4 4) (list 3 3)))
(define dest-wall (list (list 2 2) (list 4 2) (list 2 4) (list 4 4)))

;;---------------------------------------------------------------------
;; HELPER FUNCTIONS FOR path

;; last : ListOfX -> X
;; GIVEN: a list of X
;; RETURNS: the last element in the list
;; EXAMPLES:
;; (last (list 1 2 3)) = 3
;; (last (list "a" "b" "c")) = "c"
;; (last (list (list 1 2) (list 2 3) (list 3 4))) = (list 3 4)
;; STRATEGY: Combining simpler functions
(define (last some-list)
  (first (reverse some-list)))

;; TESTS:
(begin-for-test
  (check-equal? (last (list 1 2 3)) 3
                "Last element for list of integers is wrong")
  (check-equal? (last (list "a" "b" "c")) "c"
                "Last element for list of strings is wrong")
  (check-equal? (last (list (list 1 2) (list 2 3) (list 3 4)))
                (list 3 4)
                "Last element for list of lists is wrong"))

;; movement-in-curr-dir? : Move Move -> Boolean
;; GIVEN: two moves
;; WHERE: both the Move-s are non-empty lists
;; RETURNS: true iff both the moves are in the same direction
;; EXAMPLES:
;; (movement-in-curr-dir? (list NW 1) (list NE 1)) = false
;; (movement-in-curr-dir? (list NE 2) (list NE 1)) = true
;; STRATEGY: Combining simpler functions
(define (movement-in-curr-dir? curr-move prev-move)
  (equal? (first curr-move) (first prev-move)))

;; TESTS:
(begin-for-test
  (check-false
   (movement-in-curr-dir? (list NW 1) (list NE 1))
   "Issue with consecutive Move-s in different directions")
  (check-true
   (movement-in-curr-dir? (list NE 2) (list NE 1))
   "Issue with consecutive Move-s in the same direction"))

;; update-move : Move -> Move
;; GIVEN: a move
;; RETURNS: a move with a step added in the direction as that of
;; the Move passed in to this function
;; EXAMPLES:
;; (update-move (list NE 3)) = (list NE 4)
;; STRATEGY: Combining simpler functions
(define (update-move this-move)
  (list (first this-move) (+ (second this-move) STEP)))

;; TESTS:
(begin-for-test
  (check-equal? (update-move (list NE 3)) (list NE 4)
                "The Move is not being updated properly"))

;; combine-move : Move ListOfMove -> ListOfMove
;; GIVEN: current Move and the previous list of Move traversed
;; RETURNS: an updated list of Move after combining the current move
;; with the previous move if they both are in the same direction. If
;; not, the current Move is appended to the end of the list as a new
;; Move
;; EXAMPLES:
;; (combine-move (list NE 1) (list (list NW 1) (list NE 1)))
;;  = (list (list NW 1) (list NE 2))
;; (combine-move (list NE 1) (list (list NW 1) (list SE 1)))
;;  = (list (list NW 1) (list SE 1) (list NE 1))
;; (combine-move (list NW 1) empty) = (list (list NW 1)) 
;; STRATEGY: Using cases on result-sofar and curr-move
(define (combine-move curr-move result-sofar)
  (local ((define reversed-res (reverse result-sofar)))
    (cond
      [(empty? result-sofar) (list curr-move)]
      [(movement-in-curr-dir? curr-move (first reversed-res))
       (reverse
        (cons (update-move (first reversed-res))
             (rest reversed-res)))]
      [else
       (append result-sofar (list curr-move))])))

;; TESTS:
(begin-for-test
  (check-equal?
   (combine-move (list NE 1) (list (list NW 1) (list NE 1)))
   (list (list NW 1) (list NE 2))
   "Moves in same the direction are not being combined properly")
  (check-equal?
   (combine-move (list NE 1) (list (list NW 1) (list SE 1)))
   (list (list NW 1) (list SE 1) (list NE 1))
   "Moves in different directions not handled properly")
  (check-equal?
   (combine-move (list NW 1) empty)
   (list (list NW 1)) 
   "First move is not being handled properly"))

;; combine-moves : ListOfMove -> ListOfMove
;; GIVEN: a list of Move where each move represents one step in a
;; particular direction
;; HALTING MEASURE: the length of lom
;; TERMINATION ARGUMENT: lom is empty (i.e. length of lom is 0)
;; RETURNS: an updated list of Move where subsequent moves in the
;; same direction is combined together into a single move
;; EXAMPLES:
;; (combine-moves exploded-mov-lst empty) = combined-mov-lst
;; (combine-moves empty empty) = empty
;; STRATEGY: Using cases on if lom is empty
(define (combine-moves lom combined-lom-sofar)
  (cond
    [(empty? lom) combined-lom-sofar]
    [else
     (combine-moves
      (rest lom)
      (combine-move (first lom) combined-lom-sofar))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (combine-moves exploded-mov-lst empty) combined-mov-lst
   "Exploded list of Move-s not being combined properly")
  (check-equal?
   (combine-moves empty empty) empty
   "Empty list of moves is not being handled properly"))

;; relative-direction : Position Position -> Direction
;; GIVEN: the current position and the next position of the probe
;; RETURNS: the direction in which the probe will move to reach
;; the next position
;; EXAMPLES:
;; (relative-direction (list 2 -2) (list 1 -1)) = NW
;; (relative-direction (list 2 -2) (list 3 -1)) = NE
;; (relative-direction (list 2 -2) (list 1 -3)) = SW
;; (relative-direction (list 2 -2) (list 3 -3)) = SE
;; STRATEGY: Using cases on curr-pos and next-pos
(define (relative-direction curr-pos next-pos)
  (cond
    [(and
      (equal? (- (first next-pos) (first curr-pos)) -1)
      (equal? (- (second next-pos) (second curr-pos)) 1))
     NW]
    [(and
      (equal? (- (first next-pos) (first curr-pos)) 1)
      (equal? (- (second next-pos) (second curr-pos)) 1))
     NE]
    [(and
      (equal? (- (first next-pos) (first curr-pos)) -1)
      (equal? (- (second next-pos) (second curr-pos)) -1))
     SW]
    [(and
      (equal? (- (first next-pos) (first curr-pos)) 1)
      (equal? (- (second next-pos) (second curr-pos)) -1))
     SE]))

;; TESTS:
(begin-for-test
  (check-equal? (relative-direction (list 2 -2) (list 1 -1)) NW
                "North-West movement is not handled properly")
  (check-equal? (relative-direction (list 2 -2) (list 3 -1)) NE
                "North-East movement is not handled properly")
  (check-equal? (relative-direction (list 2 -2) (list 1 -3)) SW
                "South-West movement is not handled properly")
  (check-equal? (relative-direction (list 2 -2) (list 3 -3)) SE
                "South-East movement is not handled properly"))

;; exploded-listOfMoves : ListOfPosition -> ListOfMove
;; GIVEN: a list of Position
;; HALTING MEASURE: length of lop
;; TERMINATION ARGUMENT: if lop is empty (i.e. length of lop is 0)
;; RETURNS: a list of Move which take one step at a time and are
;; yet to be combined based on the direction
;; EXAMPLES:
;; (exploded-listOfMoves (rest pos-lst) (first pos-lst)) =
;;  exploded-mov-lst
;; (exploded-listOfMoves (list (list -1 1)) (list 0 0)) =
;;  (list (list NW 1))
;; STRATEGY: Using template for ListOfPosition on lop
(define (exploded-listOfMoves lop prev-pos)
  (cond
    [(empty? lop) empty]
    [else
     (cons
      (list (relative-direction prev-pos (first lop)) STEP)
      (exploded-listOfMoves (rest lop) (first lop)))]))

;; TEST:
(begin-for-test
  (check-equal?
   (exploded-listOfMoves (list (list -1 1)) (list 0 0))
   (list (list NW STEP))
   "Step in NE direction is not being converted to a Move properly")
  (check-equal?
   (exploded-listOfMoves (rest pos-lst) (first pos-lst))
   exploded-mov-lst
   "Issue while converting ListOfPos to exploded ListOfMove"))

;; listOfPos->listOfMove : ListOfPosition -> ListOfMove
;; The list of Position passed in as argument is the path to be
;; followed from source to reach the destination. This function
;; converts it into a list of Move where each Move combines the
;; consequent Positions in a particular direction
;; GIVEN: a list of Position
;; RETURNS: a list of Move which stores the equivalent path from
;; source to destination after combining subsequent steps in a
;; particular direction
;; EXAMPLES:
;; (listOfPos->listOfMove pos-lst) = combined-mov-lst
;; STRATEGY: Combining simpler functions
(define (listOfPos->listOfMove lop)
  (combine-moves
   (exploded-listOfMoves (rest lop) (first lop)) empty))

;; TESTS:
(begin-for-test
  (check-equal?
   (listOfPos->listOfMove pos-lst) combined-mov-lst
   "ListOfPosition not being converted to ListOfMove properly"))

;; same-position? : Position Position -> Boolean
;; GIVEN: two Positions
;; RETURNS: true iff both the Positions are the same
;; EXAMPLES:
;; (same-position? (list 1 1) (list 1 1)) = true
;; (same-position? (list 1 1) (list 0 0)) = false
;; STRATEGY: Combining simpler functions
(define (same-position? pos1 pos2)
  (and (equal? (first pos1) (first pos2))
       (equal? (second pos1) (second pos2))))

;; TESTS:
(begin-for-test
  (check-true (same-position? (list 1 1) (list 1 1))
              "Issue comparing same positions")
  (check-false (same-position? (list 1 1) (list 0 0))
              "Issue comparing different positions"))

;; is-element-present? : Position ListOfPosition -> Boolean
;; GIVEN: a Position and a list of Position-s
;; RETURNS: true iff the Position is an element of the list of
;; Position-s
;; EXAMPLES:
;; (is-element-present? (list 1 1) (list (list 0 0) (list 1 1)))
;;  = true
;; (is-element-present? (list 2 2) (list (list 0 0) (list 1 1)))
;;  = false
;; STRATEGY: Using HOF ormap on lop
(define (is-element-present? new-pos lop)
  (ormap
   ;; Position -> Boolean
   ;; GIVEN: the current Position from the list being processed
   ;; RETURNS: true iff the current Position from the list is
   ;; same as the new position for the probe
   (lambda (pos) 
     (same-position? pos new-pos))
   lop))

;; TESTS:
(begin-for-test
  (check-true
   (is-element-present? (list 1 1) (list (list 0 0) (list 1 1)))
   "Existing position is not identified properly")
  (check-false
   (is-element-present? (list 2 2) (list (list 0 0) (list 1 1)))
   "New position is not being identified properly"))

;; surrounding-positions : Position -> ListOfPosition
;; GIVEN: a Position
;; RETURNS: a list of Positions that surround the current Position in
;; NE-NW and SE-SW directions
;; EXAMPLES:
;; (surrounding-positions (list -1 1)) =
;;  (list (list 0 2) (list -2 2) (list 0 0) (list -2 0))
;; STRATEGY: Combining simpler functions
(define (surrounding-positions curr-pos)
  (list
   ;; North-East Position
   (list (+ (first curr-pos) STEP) (+ (second curr-pos) STEP))
   ;; North-West Position
   (list (- (first curr-pos) STEP) (+ (second curr-pos) STEP))
   ;; South-East Position
   (list (+ (first curr-pos) STEP) (- (second curr-pos) STEP))
   ;; South-West Position
   (list (- (first curr-pos) STEP) (- (second curr-pos) STEP))))

;; TESTS:
(begin-for-test
  (check-equal?
   (surrounding-positions (list -1 1))
   (list (list 0 2) (list -2 2) (list 0 0) (list -2 0))
   "Surrounding positions are not being calculated properly"))

;; next-positions : Position ListOfPosition ListOfPosition
;;                   -> ListOfPosition
;; GIVEN: a Position, a list of Position-s which have been traversed
;; by the probe and a list of Position-s where the blocks are present
;; RETURNS: the next set of Position-s that the probe can take
;; EXAMPLES:
;; (next-positions (list -1 1) (list (list 0 0)) blocks) =
;;  (list (list -2 2) (list 0 2) (list -2 0))
;; (next-positions (list 0 0) empty blocks) =
;;  (list (list -1 1) (list -1 -1))
;; STRATEGY: Use HOF filter on surrounding positions for curr-pos
(define (next-positions curr-pos traversed-positions blocks)
  (filter
   ;; Position -> Boolean
   ;; GIVEN: a Position
   ;; RETURNS: false iff the Position holds a block or has already
   ;; been traversed
   (lambda (pos)
     (not (or (is-element-present? pos traversed-positions)
              (is-element-present? pos blocks))))
   (surrounding-positions curr-pos)))

;; TESTS:
(begin-for-test
  (check-equal?
   (next-positions (list -1 1) (list (list 0 0)) blocks)
   (list (list 0 2) (list -2 2) (list -2 0))
   "Possible positions are not being calculated properly")
  (check-equal?
   (next-positions (list 0 0) empty blocks)
   (list (list -1 1) (list -1 -1))
   "Issue with possible positions when traversed positions is empty"))

;; distance-between-positions : Position Position -> PosReal
;; The distance is calculated using the distance formula which is
;; ((x2-x1)^2 + (y2-y1)^2)^(1/2)
;; GIVEN: two Position-s
;; RETURNS: the distance between the two Position-s
;; EXAMPLES:
;; (distance-between-positions (list -2 2) (list 2 2)) = 4
;; STRATEGY: Combining simpler functions
(define (distance-between-positions pos1 pos2)
  (sqrt (+ (sqr (- (first pos2) (first pos1)))
           (sqr (- (second pos2) (second pos1))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (distance-between-positions (list -2 2) (list 2 2)) 4
   "Distance from random position is not calculated properly"))

;; sorted-next-positions : ListOfPosition Position -> ListOfPosition
;; GIVEN: a list of possible next Position and destination Position
;; WHERE: the list of possible next Position-s is not empty
;; RETURNS: a similar list to the one passed where the elements are
;; sorted based on their increasing distance to the destination
;; EXAMPLES:
;; (sorted-next-positions (list (list -2 2) (list 0 2) (list -2 0))
;;  (list 2 2)) = (list (list 0 2) (list -2 2) (list -2 0))
;; STRATEGY: Combining simpler functions
(define (sorted-next-positions possible-positions destination)
  (sort possible-positions
        ;; Position Position -> Boolean
        ;; GIVEN: two positions
        ;; RETURNS: true iff first Position is closer to the
        ;; destination than the second Position
        (lambda (pos1 pos2)
          (<= (distance-between-positions pos1 destination)
              (distance-between-positions pos2 destination)))))

;; TESTS:
(begin-for-test
  (check-equal?
   (sorted-next-positions (list (list -2 2) (list 0 2) (list -2 0))
                          (list 2 2))
   (list (list 0 2) (list -2 2) (list -2 0))
   "Positions are not being sorted properly based on the distance"))
   
;; calculate-path : Position Position ListOfPosition ListOfPosition
;;                   ListOfPosition ListOfPosition -> ListOfPosition
;; GIVEN: the current Position, the destination Position, the list of
;; Position-s where the blocks are present, the list of Position-s
;; already traversed by the probe and the list of possible Position-s
;; that the probe can take in the previous step
;; HALTING MEASURE: length of prev-next-positions and nxt-pos-lst
;; TERMINATION ARGUMENT: either curr-pos is same as the destination
;; or both prev-next-positions & nxt-pos-lst become empty
;; RETURNS: list of positions to be followed to reach the destination
;; EXAMPLES:
;; (calculate-path (list 0 0) (list 2 2) blocks empty empty empty) =
;;  (list (list 0 0) (list -1 1) (list 0 2) (list 1 3) (list 2 2))
;; (calculate-path (list 0 0) (list 2 2) wall empty empty empty)=false
;; STRATEGY: Using cases on curr-pos, destination and the list of
;; probable positions the robot can take in it's next step
(define (calculate-path curr-pos destination block-positions
                        traversed-pos-lst path-sofar
                        prev-next-positions)
  (local ((define nxt-pos-lst
            (sorted-next-positions
             (next-positions
              curr-pos traversed-pos-lst block-positions)
             destination)))
  (cond    
    [(same-position? curr-pos destination)
     (append path-sofar (list curr-pos))]
    [(and (empty? prev-next-positions)
          (empty? nxt-pos-lst))
     false]
    [(empty? nxt-pos-lst)
     (calculate-path
      (first prev-next-positions) destination block-positions
      traversed-pos-lst path-sofar (rest prev-next-positions))]
    [else
     (calculate-path
      (first nxt-pos-lst) destination block-positions
      (cons curr-pos traversed-pos-lst)
      (append path-sofar (list curr-pos)) (rest nxt-pos-lst))])))

;; TESTS:
(begin-for-test
  (check-equal?
   (calculate-path (list 0 0) (list 2 2) blocks empty empty empty)
   (list (list 0 0) (list -1 1) (list 0 2) (list 1 3) (list 2 2))
   "Existent path was not calculated properly")
  (check-false
   (calculate-path (list 0 0) (list 2 2) wall empty empty empty)
   "Source surrounded by a wall is not being handled properly")
  (check-equal?
   (calculate-path (list 0 0) (list 3 3) expanded-blocks
                   empty empty empty)
   expanded-path
   "Expanded path not calculated properly")
  (check-equal?
   (calculate-path '(0 0) '(3 3) super-expanded-blocks
                   empty empty empty)
   super-expanded-path
   "Super expanded path not calculated properly"))

;; get-wall-corners : Position PosInt -> ListOfPosition
;; GIVEN: a position
;; RETURNS: the four corners of the wall at the specified number of
;; steps from the position
;; EXAMPLES:
;; (get-wall-corners (list 0 0) 1) =
;;  (list (list 1 1) (list -1 1) (list -1 -1) (list 1 -1))
;; (get-wall-corners (list -2 2) 2) =
;;  (list (list 0 4) (list -4 4) (list -4 0) (list 0 0))
;; STRATEGY: Combining simpler functions
(define (get-wall-corners curr-pos level)
  (local ((define x-pos (first curr-pos))
          (define y-pos (second curr-pos)))
    (list
     (list (+ x-pos level) (+ y-pos level))    ;; NE
     (list (- x-pos level) (+ y-pos level))    ;; NW
     (list (- x-pos level) (- y-pos level))    ;; SW
     (list (+ x-pos level) (- y-pos level))))) ;; SE

;; TESTS:
(begin-for-test
  (check-equal?
   (get-wall-corners (list 0 0) 1)
   (list (list 1 1) (list -1 1) (list -1 -1) (list 1 -1))
   "Issue while generating the first level walls from (0,0)")
  (check-equal?
   (get-wall-corners (list -2 2) 2)
   (list (list 0 4) (list -4 4) (list -4 0) (list 0 0))
   "Issue while generating the first level walls from (-2,2)"))

;; step-through-positions : Position ListOfPosition String X Position
;;                           -> ListOfString
;; GIVEN: some Position, the list of Positions in the wall that
;; have already been discovered, the coordinate in which we are
;; moving, the destination and some function to be performed which
;; is either addition (+) or subtraction (-)
;; HALTING MEASURE: curr-pos
;; TERMINATION ARGUMENT: when curr-pos is same as dest
;; RETURNS: a list of Positions which form the wall between the
;; given source and destination in the specified coordinate
;; EXAMPLES:
;; (step-through-positions (list 1 1) empty Y - (list 1 -1)) =
;;  (list (list 1 1))
;; STRATEGY: Using cases on if curr-pos is same as dest
(define (step-through-positions curr-pos lop-sofar coord dir dest)
  (if (same-position? curr-pos dest)
      lop-sofar               
      (local ((define next-pos
                (if (equal? coord Y) 
                    (list (first curr-pos) (dir (second curr-pos) 2))
                    (list (dir (first curr-pos) 2) (second curr-pos)))))
        (step-through-positions
         next-pos (append lop-sofar (list curr-pos))
         coord dir dest))))

;; TESTS:
(begin-for-test
  (check-equal?
   (step-through-positions (list 1 1) empty Y - (list 1 -1))
   (list (list 1 1))
   "Issue while calculating wall positions in specified coordinate"))

;; wall-positions : Position PosInt -> ListOfPosition
;; GIVEN: a Position
;; RETURNS: the positions covered by the wall at the specified level
;; EXAMPLES:
;; (wall-positions (list 0 0) 1) =
;;  (list (list 1 1) (list 1 -1) (list -1 -1) (list -1 1))
;; STRATEGY: Combining simpler functions
(define (wall-positions curr-pos level)
  (local ((define corners (get-wall-corners curr-pos level))
          (define ne-pos (first corners))
          (define nw-pos (second corners))
          (define sw-pos (third corners))
          (define se-pos (fourth corners)))
    (append
     (step-through-positions ne-pos empty Y - se-pos)
     (step-through-positions se-pos empty X - sw-pos)
     (step-through-positions sw-pos empty Y + nw-pos)
     (step-through-positions nw-pos empty X + ne-pos))))

;; TESTS:
(begin-for-test
  (check-equal?
   (wall-positions (list 0 0) 1)
   (list (list 1 1) (list 1 -1) (list -1 -1) (list -1 1))
   "Issue while calculating wall positions for level 1 of (0,0)"))

;; subset? : ListOfPosition ListOfPosition -> Boolean
;; GIVEN: two lists of Position-s
;; RETURNS: true iff first list is a sub-set of second list
;; EXAMPLES:
;; (subset? (list (list 1 1) (list 3 3)) (list (list 1 1) (list 2 2)
;;  (list 3 3) (list 4 4))) = true
;; (subset? (list (list 1 1) (list 5 5)) (list (list 1 1) (list 2 2)
;;  (list 3 3) (list 4 4))) = false
;; STRATEGY: Using HOF andmap on list1
(define (subset? list1 list2)
  (andmap
   ;; Position -> Boolean
   ;; GIVEN: a Position
   ;; RETURNS: true iff the Position is present in list2
   (lambda (element)
     (is-element-present? element list2))
   list1))

;; TESTS:
(begin-for-test
  (check-true
   (subset? (list (list 1 1) (list 3 3))
            (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
   "Issue while processing correct subset")
  (check-false
   (subset? (list (list 1 1) (list 5 5))
            (list (list 1 1) (list 2 2) (list 3 3) (list 4 4)))
   "Issue while processing incorrect subset")
  (check-true
   (subset? (list (list 4 4) (list 4 2) (list 2 2) (list 2 4))
            dest-wall)
   "Equal sets being processed incorrectly"))

;; dest-discovereable? : Position Position ListOfPosition PosInt
;;                               -> Boolean
;; GIVEN: the source position, destination position and the list of
;; position of the blocks
;; HALTING MEASURE: source, dest, source-wall and dest-wall
;; TERMINATION ARGUMENT: either source is a part of dest-wall and
;; dest is a part of source-wall or source-wall is a subset of
;; lob and dest-wall is a subset of lob
;; RETURNS: true iff the destination is reachable from the source
;; EXAMPLES:
;; (dest-discovereable? (list 0 0) (list 3 3) wall 0) = false
;; (dest-discovereable? (list 0 0) (list 3 3) blocks 0) = true
;; (dest-discovereable? (list 0 0) (list 3 3) dest-wall 0) = false
;; STRATEGY: Using cases on source, dest, source-wall and dest-wall
(define (dest-discovereable? source dest lob level)
  (local ((define source-wall (wall-positions source level))
          (define dest-wall (wall-positions dest level)))
    (cond
      [(and (is-element-present? source dest-wall)
            (is-element-present? dest source-wall))
       true]
      [(or (subset? source-wall lob)
           (subset? dest-wall lob))
       false]
      [else
       (dest-discovereable? source dest lob (+ level 1))])))

;; TESTS:
(begin-for-test
  (check-false (dest-discovereable? (list 0 0) (list 3 3) wall 1)
               "Blocked source being handled incorrectly")
  (check-true (dest-discovereable? (list 0 0) (list 3 3) blocks 1)
              "Reachable destination being handled incorrectly")
  (check-false (dest-discovereable? (list 0 0) (list 3 3) dest-wall 1)
               "Blocked destination being handled incorrectly"))

;; reachable-destination? : Position Position -> Boolean
;; This function doesn't take the blocks into consideration. It checks
;; if the destination is reachable in a normal configuration where
;; there are no blocks present in the infinite chess-board
;; GIVEN: the source Position and the destination Position
;; RETURNS: true iff the destination is reachable from the source
;; EXAMPLES:
;; (reachable-destination? '(0 0) '(3 3)) = true
;; (reachable-destination? '(1 0) '(2 2)) = false
;; (reachable-destination? '(-1 -1) '(-3 -5)) = true
;; (reachable-destination? '(3 3) '(-3 -5)) = true
;; (reachable-destination? '(4 2) '(-2 3)) = false
;; STRATEGY: Combining simpler functions
(define (reachable-destination? source dest)
  (local ((define source-xy-sum (+ (first source) (second source)))
          (define dest-xy-sum (+ (first dest) (second dest))))
    (or (and (even? source-xy-sum) (even? dest-xy-sum))
        (and (odd? source-xy-sum) (odd? dest-xy-sum)))))

;; TESTS:
(begin-for-test
  (check-true (reachable-destination? '(0 0) '(3 3))
              "(3,3) should be reachable from (0,0)")
  (check-false (reachable-destination? '(1 0) '(2 2))
               "(2,2) shouldn't be reachable from (1,0)")
  (check-true (reachable-destination? '(-1 -1) '(-3 -5))
              "(-3,-5) should be reachable from (-1,-1)")
  (check-true (reachable-destination? '(3 3) '(-3 -5))
              "(-3,-5) should be reachable from (3,3)")
  (check-false (reachable-destination? '(4 2) '(-2 3))
               "(-2,3) shouldn't be reachable from (4,2)"))

;;---------------------------------------------------------------------
;; HELPER FUNCTIONS FOR eval-plan

;; relative-position : Direction Position -> Position
;; GIVEN: the relative Direction of the next Position and the previous
;; Position
;; RETURNS: the next Position for the proble
;; EXAMPLES:
;; (relative-position NE (list 0 0)) = (list 1 1)
;; (relative-position NW (list 0 0)) = (list -1 1)
;; (relative-position SE (list 0 0)) = (list 1 -1)
;; (relative-position SW (list 0 0)) = (list -1 -1)
;; STRATEGY: Using cases on direction
(define (relative-position direction prev-pos)
  (cond
    [(equal? direction NE)
     (list (+ (first prev-pos) 1) (+ (second prev-pos) 1))]
    [(equal? direction NW)
     (list (- (first prev-pos) 1) (+ (second prev-pos) 1))]
    [(equal? direction SE)
     (list (+ (first prev-pos) 1) (- (second prev-pos) 1))]
    [(equal? direction SW)
     (list (- (first prev-pos) 1) (- (second prev-pos) 1))]))

;; TESTS:
(begin-for-test
  (check-equal? (relative-position NE (list 0 0)) (list 1 1)
                "Issue while calculating NE direction")
  (check-equal? (relative-position NW (list 0 0)) (list -1 1)
                "Issue while calculating NW direction")
  (check-equal? (relative-position SE (list 0 0)) (list 1 -1)
                "Issue while calculating SE direction")
  (check-equal? (relative-position SW (list 0 0)) (list -1 -1)
                "Issue while calculating SW direction"))

;; explode-move : Direction PosInt ListOfPosition -> ListOfPosition
;; GIVEN: a Direction, the number of steps the probe has to move in
;; the specified direction and the list of positions that the probe
;; has moved through so far
;; HALTING MEASURE: steps
;; TERMINATION ARGUMENT: if steps become 0
;; RETURNS: a list of Position-s that the probe moves through to
;; satisfy an equivalent Move
;; EXAMPLES:
;; (explode-move NE 2 (list (list 0 0))) =
;;  (list (list 1 1) (list 2 2))
;; (explode-move NW 1 (list (list 0 0))) = (list (list -1 1))
;; (explode-move SE 1 (list (list 0 0) (list 1 1))) =
;;  (list (list 2 0))
;; (explode-move SW 2 (list (list 0 0) (list -1 -1))) =
;;  (list (list -2 -2) (list -3 -3))
;; STRATEGY: Using cases on steps
(define (explode-move dir steps lop-sofar)
  (cond
    [(> steps 0)
     (explode-move
      dir (- steps 1)
      (append
       lop-sofar (list (relative-position dir (last lop-sofar)))))]
    [else lop-sofar]))

;; TESTS:
(begin-for-test
  (check-equal? (explode-move NE 2 (list (list 0 0)))
                (list (list 0 0) (list 1 1) (list 2 2))
                "Issue generating positions in NE direction")
  (check-equal? (explode-move NW 1 (list (list 0 0)))
                (list (list 0 0) (list -1 1))
                "Issue generating positions in NW direction")
  (check-equal? (explode-move SE 1 (list (list 0 0) (list 1 1)))
                (list (list 0 0) (list 1 1) (list 2 0))
                "Issue generating positions in SE direction")
  (check-equal? (explode-move SW 2 (list (list 0 0) (list -1 -1)))
                (list (list 0 0) (list -1 -1) (list -2 -2) (list -3 -3))
                "Issue generating positions in SW direction"))

;; listOfMove->listOfPosition : ListOfMove ListOfPosition
;;                               -> ListOfPosition
;; GIVEN: a list of moves and a list which stores the generated
;; Position-s
;; HALTING MEASURE: length of lom
;; TERMINATION ARGUMENT: length of lom becomes 0 (i.e. lom is empty)
;; RETURNS: the equivalent list of Position-s for the initial list
;; of Moves passed in as argument
;; EXAMPLES:
;; (listOfMove->listOfPosition combined-mov-lst '((0 0))) = pos-lst
;; STRATEGY: Using cases on lom
(define (listOfMove->listOfPosition lom lop-sofar)
  (cond
    [(empty? lom) lop-sofar]
    [else
     (listOfMove->listOfPosition
      (rest lom)
      (explode-move
       (first (first lom))
       (second (first lom))
       lop-sofar))]))

;; TESTS:
(begin-for-test
  (check-equal?
   (listOfMove->listOfPosition combined-mov-lst '((0 0))) pos-lst
   "Move-s are not being exploded to Position-s properly"))

;; validate-positions : ListOfPosition ListOfPosition -> MaybePosition
;; GIVEN: a list of Positions that the robot traverses through and a
;; list of positions where the blocks are present
;; RETURNS: the final Position if every Position is valid or false
;; if a block is present in any of the Position-s
;; EXAMPLES:
;; (validate-positions pos-lst blocks) = pos-lst
;; (validate-positions pos-lst wall) = false
;; STRATEGY: Using HOF ormap on positions followed by cases on
;; path-is-blocked
(define (validate-positions positions block-positions)
  (local
    ((define path-is-blocked
       (ormap
        ;; Position -> Boolean
        ;; GIVEN: a Position
        ;; RETURNS: true iff the Position has a block present in it
        (lambda (pos) (is-element-present? pos block-positions))
        positions)))
    (if (not path-is-blocked)
        (last positions)
        false)))

;; TESTS:
(begin-for-test
  (check-equal? (validate-positions pos-lst blocks) (last pos-lst)
                "Valid path is not being handled properly")
  (check-false (validate-positions pos-lst wall)
               "Blocked path is not being handled properly"))

;;---------------------------------------------------------------------
;; PROVIDED FUNCTIONS

;; path : Position Position ListOfPosition -> MaybePlan
;; NOTE: If source and destination are the same, then this function
;; returns false
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from
;; the starting position to the target position without passing over any
;; of the blocks, or false if no such sequence of moves exists.
;; EXAMPLES:
;; (path (list 0 0) (list 3 3) expanded-blocks) =
;;  (list (list NW 1) (list NE 2) (list NW 1) (list NE 2) (list SE 2)
;;        (list SW 1))
;; (path (list 0 0) (list 2 2) blocks) = combined-mov-lst
;; (path (list -1 3) (list 4 -5) blocks) = false
;; STRATEGY: Using cases on source and destination
(define (path source destination blocks)
  (cond
    [(same-position? source destination) empty]
    [(and (reachable-destination? source destination)
          (dest-discovereable? source destination blocks 1))
     (listOfPos->listOfMove
      (calculate-path source destination blocks empty empty empty))]
    [else #f]))

;; TESTS:
(begin-for-test
  (check-equal?
   (path (list 0 0) (list 3 3) expanded-blocks)
   (list (list NW 1) (list NE 2) (list NW 1) (list NE 2)
         (list SE 2) (list SW 1))
   "Issue while calculating the set of Move-s to reach (3,3)")
  (check-equal?
   (path (list 0 0) (list 2 2) blocks) combined-mov-lst
   "Issue while calculating the set of Move-s to reach (2,2)")
  (check-false
   (path (list -1 3) (list 4 -5) blocks)
   "Issue while calculating the Move-s to reach (4,-5)")
  (check-false
   (path (list 0 0) (list 3 3) dest-wall)
   "Issue while calculating Move-s when destination has a wall around")
  (check-equal?
   (path (list 1 1) (list 1 1) blocks) empty
   "Issue while calculating plan when source is same as destination"))

;; eval-plan : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. A list of the blocks on the board
;; 3. A plan for the robot's motion
;; RETURNS:
;; The position of the robot at the end of executing the plan, or false
;; if the plan sends the robot to or  through any block.
;; EXAMPLES:
;; (eval-plan '(0 0) blocks combined-mov-lst) = pos-lst
;; STRATEGY: Combining simpler functions
(define (eval-plan source block-positions plan)
  (validate-positions
   (listOfMove->listOfPosition plan (list source))
   block-positions))

;; TESTS:
(begin-for-test
  (check-equal?
   (eval-plan '(0 0) blocks combined-mov-lst) (last pos-lst)
   "Move-s are not being evaluated to Position-s properly"))