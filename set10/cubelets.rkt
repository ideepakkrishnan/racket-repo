#lang racket

(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(provide
 Cube%
 Block<%>
 make-block)

;;--------------------------------------------------------------------
;; CONSTANTS

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))

(define CUBE-SIDE 20)
(define CUBE-HALF-SIDE (/ CUBE-SIDE 2))
(define CUBE-DEFAULT-MX 0)
(define CUBE-DEFAULT-MY 0)
(define CUBE-STYLE "outline")
(define CUBE-DEFAULT-COLOR "green")
(define CUBE-SELECTED-COLOR "red")

(define DEFAULT-CUBE-IMG (rectangle CUBE-SIDE CUBE-SIDE
                                    CUBE-STYLE CUBE-DEFAULT-COLOR))
(define SELECTED-CUBE-IMG (rectangle CUBE-SIDE CUBE-SIDE
                                     CUBE-STYLE CUBE-SELECTED-COLOR))

(define NEW-CUBE-KEY "b")

;;--------------------------------------------------------------------
;; INTERFACES

;; The Block<%> interface extends the SWidget<%> interface from
;; WidgetWorks file
(define Block<%>
  (interface (SWidget<%>)
    ;; get-team : -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    get-team

    ;; add-teammate: Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    add-teammate

    ;; block-x : -> Integer
    ;; block-y : -> Integer
    ;; RETURNS: the x or y coordinates of this block
    block-x
    block-y))

;;--------------------------------------------------------------------
;; HELPER FUNCTIONS

;; click-inside-cube? : NonNegInt NonNegInt NonNegInt NonNegInt
;;                       NonNegInt -> Boolean
;; GIVEN: x & y coordinates of the cube, the x & y coordinate
;; of a mouse event and the half of the cube's side
;; RETURNS: whether the event occurred inside the cube or not
;; EXAMPLES & TESTS: See Test Cases below
;; STRATEGY: Combining simpler functions
(define (click-inside-cube? x y mx my ds)
  (and
    (<= (- x ds) mx (+ x ds))
    (<= (- y ds) my (+ y ds))))

;; same-cube? : Cube Cube -> Boolean
;; GIVEN: two Cube-s
;; RETURNS: true iff all the atributes of the two Cube-s match
;; EXAMPLES & TESTS: See Test Cases below
;; STRATEGY: Combining simpler functions
(define (same-cube? cube1 cube2)
  (and
   (equal? (send cube1 block-x) (send cube2 block-x))
   (equal? (send cube1 block-y) (send cube2 block-y))
   (equal? (send cube1 cube-mdx) (send cube2 cube-mdx))
   (equal? (send cube1 cube-mdy) (send cube2 cube-mdy))
   (equal? (send cube1 cube-selected?) (send cube2 cube-selected?))))

;; cube-is-member? : Cube ListOfCube -> Boolean
;; GIVEN: a Cube and a list of Cube-s
;; RETURNS: true iff the Cube is a member of the list of Cube-s
;; EXAMPLES & TESTS: See Test Cases below
;; STRATEGY: Using HOF ormap on team
(define (cube-is-member? cube team)
  (ormap
   ;; Cube -> Boolean
   ;; GIVEN: a Cube
   ;; RETURNS: true iff this Cube is same as the one stored in cube
   (lambda (c)
     (same-cube? c cube))
   team))

;; cube-overlaps-member? : Cube ListOfCube -> Boolean
;; GIVEN: a cube and a list of Cube-s
;; RETURNS: true iff the Cube overlaps any member of list of Cube-s
;; EXAMPLES & TESTS: See Test Cases below
;; STRATEGY: Using HOF ormap on sel-cubes
(define (cube-overlaps-member? c loc)
  (ormap
   ;; Cube -> Boolean
   ;; GIVEN: a Cube
   ;; RETURNS: true iff this Cube overlaps the Cube stored in c
   (lambda (cube)
     (send c overlap? cube))
   loc))

;;--------------------------------------------------------------------
;; CLASSES

;; PlaygroundState% is a class that represents the Playground state
;; object
;; A Playground state object is a
;; (new PlaygroundState%
;;   [teams ListOfListOfCube]
;;   [curr-x PosInt]
;;   [curr-y PosInt])
(define PlaygroundState%
  (class* object% (SWidget<%>)
    
    (init-field [teams empty]) ;; a list which stores all the teams
                               ;; in the current PlaygroundState
    
    ;; The x and y coordinate of the location where new cubes are to
    ;; be created
    (init-field [curr-x CANVAS-CENTER-X])
    (init-field [curr-y CANVAS-CENTER-Y])
    
    (super-new)
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick.
    (define/public (after-tick)
      this)

    ;; reset-teams : ListOfListOFCube -> Void
    ;; EFFECT: Updates the teams in this state
    (define/public (reset-teams lot)
      (set! teams lot))

    ;; new-team : ListOfCube -> Void
    ;; GIVEN: a list of Cube-s
    ;; EFFECT: Updates the list of teams in this playground state
    ;; with the new team
    (define/public (new-team t)
      (set! teams (cons t teams)))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the mouse down event at given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (after-button-down mx my)
      (begin
        ;; Update the x and y coordinates of the position where new
        ;; Cube-s are to be created
        (set! curr-x mx)
        (set! curr-y my)
        ;; Update the state of the cubes to the state that should
        ;; follow the mouse down event
        (for-each
         ;; ListOfCube -> Void
         ;; GIVEN: a team (which is a list of Cube-s)
         ;; EFFECT: the list of Cube-s after the mouse-down
         ;; event
         (lambda (team)
           (for-each
            ;; Cube -> Void
            ;; GIVEN: a cube
            ;; EFFECT: the cube after the mouse-down event
            (lambda (c) (send c after-button-down mx my))
            team))
         teams)))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the mouse up event at given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (after-button-up mx my)
      (for-each
       ;; ListOfCube -> Void
       ;; GIVEN: a team (which is a list of Cube-s)
       ;; EFFECT: the list of Cube-s after the mouse-up
       ;; event
       (lambda (team)
         (for-each
          ;; Cube -> Void
          ;; GIVEN: a cube
          ;; EFFECT: the cube after the mouse-up event
          (lambda (c) (send c after-button-up mx my))
          team))
       teams))

    ;; move-all-cubes : PosInt PosInt -> Void
    ;; GIVEN: the current mouse event position
    ;; EFFECT: the Cube-s are updated to the state that should this
    ;; mouse event
    (define (move-all-cubes mx my)
      (for-each
       ;; ListOfCube -> Void
       ;; GIVEN: a team (which is a list of Cube-s)
       ;; EFFECT: the list of Cube-s after the mouse-drag
       ;; event
       (lambda (team)
         (for-each
          ;; Cube -> Void
          ;; GIVEN: a cube
          ;; EFFECT: the cube after the mouse-drag event
          (lambda (c) (send c after-drag mx my))
          team))
       teams))

    ;; clear-rel-mov-flags : -> Void
    ;; EFFECT: clears the relative movement of all the Cube-s in
    ;; this Playground state
    (define (clear-rel-mov-flags)
      (for-each
       ;; ListOfCube -> Void
       ;; GIVEN: a team (which is a list of Cube-s)
       ;; EFFECT: clears the relative movement for all the members
       ;; of this team
       (lambda (team)
         (for-each
          ;; Cube -> Void
          ;; GIVEN: a cube
          ;; EFFECT: the cube after the mouse-drag event
          (lambda (c) (send c clear-relative-move-flag))
          team))
       teams))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the mouse drag event at given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (after-drag mx my)      
      (begin
        (move-all-cubes mx my)
        (clear-rel-mov-flags)))
    
    ;; KeyEvent : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using cases on kev
    (define/public (after-key-event kev)
      (set! teams
            (cond
              [(equal? kev NEW-CUBE-KEY)
               (cons
                (list (new Cube% [x curr-x] [y curr-y]))
                teams)]
              [else teams])))
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using HOF foldr on teams
    (define/public (add-to-scene scene)
      (foldr
       ;; ListOfCube Image -> Image
       ;; GIVEN: a team (which is a list of Cube-s) and the current
       ;; state of the scene
       ;; RETURNS: the scene with all elements of the current team
       ;; rendered on it
       (lambda (team curr-scene)
         (overlay
          (foldr
           ;; Cube Image -> Image
           ;; GIVEN: a Cube and current image to be rendered
           ;; RETURNS: an image with this Cube rendered on it
           (lambda (c img) 
             (overlay (send c add-to-scene img) img))
           curr-scene
           team)
          curr-scene))
       scene
       teams))
    
    ;; independent-teams : Cube -> ListOfCube
    ;; GIVEN: a Cube
    ;; RETURNS: a list of teams which doesn't contain this Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using HOF filter on teams
    (define/public (independent-teams c)
      (filter
       ;; ListOfCube -> Boolean
       ;; GIVEN: a list of Cube-s
       ;; RETURNS: true iff c is not a member of this team
       (lambda (team)
         (not (cube-is-member? c team)))
       teams))
    
    ;; team-for-cube : Cube -> ListOfCube
    ;; GIVEN: a Cube
    ;; RETURNS: the team of this Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using HOF filter on teams
    (define/public (team-for-cube c)
      (first
       (filter
        ;; ListOfCube -> Boolean
        ;; GIVEN: a list of Cube-s
        ;; RETURNS: true iff c is a member of team
        (lambda (team)
          (cube-is-member? c team))
        teams)))
    
    ;; update-cube-team : Cube Cube -> Void
    ;; GIVEN: a cube and a new team mate for this Cube
    ;; EFFECT: the team of this Cube is updated with the new member
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (update-cube-team c mate)
      (local ((;; List of teams which are not affected by Cube c
               define other-teams (independent-teams c)))
        (set! teams
              (cons
               (cons mate (team-for-cube c))
               other-teams))))
    
    ;; teams-with-overlapping-members : Cube -> ListOfListOfCube
    ;; GIVEN: a Cube
    ;; RETURNS: a list of teams which have atleast one of their member
    ;; overlapping the given Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using HOF filter on teams
    (define (teams-with-overlapping-members c)
      (filter
       ;; ListOfCube -> Boolean
       ;; GIVEN: a list of Cube-s
       ;; RETURNS: true iff the Cube stored in c overlaps any of the
       ;; members of this team
       (lambda (team)
         (cube-overlaps-member? c team))
       teams))
    
    ;; teams-with-no-overlapping-members : Cube -> ListOfListOfCube
    ;; GIVEN: a Cube
    ;; RETURNS: a list of teams which have none of their member
    ;; overlapping the given Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using HOF filter on teams
    (define (teams-with-no-overlapping-members c)
      (filter
       ;; ListOfCube -> Boolean
       ;; GIVEN: a list of Cube-s
       ;; RETURNS: true iff the Cube stored in c doesn't overlap any
       ;; of the members of this team
       (lambda (team)
         (not (cube-overlaps-member? c team)))
       teams))
    
    ;; merge-affected-teams : Cube -> Void
    ;; GIVEN: a Cube
    ;; EFFECT: merges the teams which are affected by Cube c and
    ;; updates the list stored in PlaygroundState
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (merge-affected-teams c)
      (local (;; team which contains the Cube c
              (define my-team
                (send PLAYGROUND-STATE team-for-cube c))
              ;; teams which doesn't contain the Cube c
              (define other-teams
                (send PLAYGROUND-STATE independent-teams c))
              ;; team which doesn't contain the Cube c, but have
              ;; atleast one member overlapping c
              (define affected-teams
                (teams-with-overlapping-members c))
              ;; teams which are left absolutely untouched by c
              (define unaffected-teams
                (teams-with-no-overlapping-members c)))
        (set! teams (cons (flatten affected-teams) unaffected-teams))))
    
    ;; playground-teams : -> ListOfListOfCube
    ;; RETURNS: the current list of teams in the playground state
    ;; EXAMPLES & TESTS: See Test Cases below
    (define/public (playground-teams)
      teams)

    ;; test:curr-x : -> PosInt
    ;; RETURNS: the current X coordinate for initializing a new Cube
    (define/public (test:curr-x)
      curr-x)

    ;; test:curr-y : -> PosInt
    ;; RETURNS: the current Y coordinate for initializing a new Cube
    (define/public (test:curr-y)
      curr-y)

    ;; test:clear-teams : -> Void
    ;; EFFECT: Cleare
    (define/public (test:clear-teams)
      (begin
        (set! teams empty)
        (set! curr-x CANVAS-CENTER-X)
        (set! curr-y CANVAS-CENTER-Y)))))

;; Global variable which stores the playground state
(define PLAYGROUND-STATE (new PlaygroundState%))

;; Cube% is a class that represents the Cubelet object
;; A Cubelet object is a
;; (new Cube%
;;   [x PosInt] [y PosInt]
;;   [selected? Boolean]
;;   [mdx Integer] [mdy Integer])
(define Cube%
  (class* object% (Block<%>)

    (init-field x y) ;; x & y coordinates of the center of this cube
    (init-field [selected? false]) ;; true iff this Cube is selected

    ;; Relative position of the mouse click position to the center of
    ;; the cube
    (init-field
     [mdx CUBE-DEFAULT-MX]
     [mdy CUBE-DEFAULT-MY])

    ;; Private variable storing the image of the cube for rendering
    (field [cube-img DEFAULT-CUBE-IMG])

    ;; Private variable which stores a flag whether the Cube has
    ;; already been moved relatively
    (field [moved-relatively? false])

    (super-new)

    ;; block-x : -> PosInt
    ;; RETURNS: the x position of the center of this Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (block-x)
      x)

    ;; block-y : -> PosInt
    ;; RETURNS: the y position of the center of this Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (block-y)
      y)

    ;; cube-selected? : -> Boolean
    ;; RETURNS: the current selection of the Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (cube-selected?)
      selected?)

    ;; cube-mdx : -> Integer
    ;; RETURNS: the current relative x position of the Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (cube-mdx)
      mdx)

    ;; cube-mdy : -> Integer
    ;; RETURNS: the current relative y position of the Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (cube-mdy)
      mdy)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this Cube to the state it should have
    ;; following a tick.
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)
      this)

    ;; PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this Cube to the state it should have
    ;; following the mouse click event at the given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; within the cube or not
    (define/public (after-button-down mx my)
      (if (click-inside-cube? x y mx my CUBE-HALF-SIDE)
          (begin
            (set! mdx (- x mx))
            (set! mdy (- y my))
            (set! selected? true)
            (set! cube-img SELECTED-CUBE-IMG))
          (begin
            (set! mdx CUBE-DEFAULT-MX)
            (set! mdy CUBE-DEFAULT-MY)
            (set! selected? false))))

    ;; PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this Cube to the state it should have
    ;; following the mouse button up event at the given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (after-button-up mx my)
      (begin
        (set! mdx CUBE-DEFAULT-MX)
        (set! mdy CUBE-DEFAULT-MY)
        (set! selected? false)
        (set! moved-relatively? false)
        (set! cube-img DEFAULT-CUBE-IMG)))

    ;; PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this Cube to the state it should have
    ;; following the mouse drag event at the given location
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using cases on if this Cube is selected
    (define/public (after-drag mx my)
      (local (;; next X coordinate pos for this Cube
              (define new-x (- mx mdx))
              ;; next Y coordinate pos for this Cube
              (define new-y (- my mdy))
              ;; relative movement of this Cube in X direction
              (define dx (- new-x x))
              ;; relative movement of this Cube in Y direction
              (define dy (- new-y y)))
        (if selected?
            (begin
              ;; Update the x and y positions of this Cube
              (set! x new-x)
              (set! y new-y)
              ;; Move the team members of this Cube in the same
              ;; direction
              (for-each
               ;; Cube -> Void
               ;; GIVEN: a cube
               ;; EFFECT: updates the position of each cube to its
               ;; new position
               (lambda (c)
                 (send c update-position dx dy))
               (send PLAYGROUND-STATE team-for-cube this))
              ;; Add additional Cubes which touch this Cube to the
              ;; team
              (send PLAYGROUND-STATE merge-affected-teams this))
            (begin
              (set! x x)
              (set! y y)))))

    ;; KeyEvent : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this Cube to the state it should have
    ;; following the given key event
    ;; EXAMPLES & TESTS: See Test Cases below
    (define/public (after-key-event kev)
      this)

    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image cube-img x y scene))

    ;; get-team : -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (get-team)
      (send PLAYGROUND-STATE team-for-cube this))

    ;; add-teammate: Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (add-teammate c)
      (local ((define team-for-c (send c get-team))
              (define independent-teams-for-c
                (send PLAYGROUND-STATE independent-teams c))
              (define other-teams
                (begin
                  (send PLAYGROUND-STATE reset-teams independent-teams-for-c)
                  (send PLAYGROUND-STATE independent-teams this))))
      (send PLAYGROUND-STATE
            reset-teams
            (cons (flatten (list (get-team) team-for-c))
                           other-teams))))

    ;; update-position : Integer Integer -> Void
    ;; GIVEN: the relative distance by which the center of the current
    ;; cube needs to be moved in x and y coordinates
    ;; EFFECT: updates the position of the Cube to the position
    ;; which should follow after the drag event
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Using cases on if this Cube is selected
    (define/public (update-position dx dy)
      (if (or selected? moved-relatively?)
          ;; Do not move if the Cube is selected since it has already
          ;; been moved by after-drag event
          (begin        
            (set! x x)
            (set! y y))
          ;; Move the Cube by relative position if it is not selected
          (begin        
            (set! x (+ x dx))
            (set! y (+ y dy))
            (set! moved-relatively? true))))

    ;; clear-relative-move-flag : -> Void
    ;; EFFECT: Clears the relative movement flag set for this Cube
    (define/public (clear-relative-move-flag)
      (set! moved-relatively? false))

    ;; overlap? : Cube% -> Boolean
    ;; GIVEN: a Cube
    ;; WHERE: the Cube passed as argument is currently selected
    ;; RETURNS: True iff any edge of this Cube overlaps with the
    ;; selected Cube
    ;; EXAMPLES & TESTS: See Test Cases below
    ;; STRATEGY: Combining simpler functions
    (define/public (overlap? selected-cube)
      (local
        (;; x/y position of the center of selected cube
         (define sc-x (send selected-cube block-x))
         (define sc-y (send selected-cube block-y)))
        ;; Check if any edge of this Cube overlaps with the selected
        ;; cube
        (and (<= (- CUBE-SIDE) (- x sc-x) CUBE-SIDE)
             (<= (- CUBE-SIDE) (- y sc-y) CUBE-SIDE))))))

;;--------------------------------------------------------------------
;; PROVIDED FUNCTIONS

;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the
;; playground.
;; RETURNS: a new block, at the given position, with no teammates
;; NOTE: it is up to you as to whether you use the third argument or
;; not. Some implementations may use the third argument; others may not
;; EXAMPLES & TESTS: See Test Cases below
;; STRATEGY: Combining simpler functions
(define (make-block x-pos y-pos loc)
  (local ((define new-cube (new Cube% [x x-pos] [y y-pos])))
    (begin
      (send PLAYGROUND-STATE new-team (list new-cube))
      new-cube)))

;;--------------------------------------------------------------------
;; TEST CASES

;; STEP 1
;; Create a new Cube at it's default location and move it to (200,190)
(define (test:flow1-step1)
  (begin
    (send PLAYGROUND-STATE after-key-event "b")
    (send PLAYGROUND-STATE after-button-down 300 250)
    (send PLAYGROUND-STATE after-drag 200 190)
    (send PLAYGROUND-STATE after-button-up 200 190)
    PLAYGROUND-STATE))

;; STEP 2
;; Create a new Cube at the current init position and move it to
;; (280,230) such that it touches the first Cube
(define (test:flow1-step2)
  (begin
    (send PLAYGROUND-STATE after-key-event "b")
    (send PLAYGROUND-STATE after-button-down 200 190)
    (send PLAYGROUND-STATE after-drag 275 225)
    PLAYGROUND-STATE))

;; STEP 3
;; Move the first cube to (280,230) such that it touches the second
;; cube and becomes a team
(define (test:flow1-step3)
  (begin
    (send PLAYGROUND-STATE after-drag 280 230)
    PLAYGROUND-STATE))

;; STEP 4
;; Now move the first cube to (300,275) such that new team moves
;; relative to this position
(define (test:flow1-step4)
  (begin
    (send PLAYGROUND-STATE after-drag 300 275)
    PLAYGROUND-STATE))

;; STEP 5
;; Rendering this scene
(define EMPTY-CANVAS (empty-scene 600 500))
(define (test:flow1-step5)
  (send PLAYGROUND-STATE add-to-scene EMPTY-CANVAS))

;; STEP 6
;; Invalid key event handling
(define (test:flow1-step6)
  (begin
    (send PLAYGROUND-STATE after-key-event "s")
    PLAYGROUND-STATE))

;; STEP 7
;; Clear all test variables from the Playground state
(define (test:clear-playgroundState)
  (begin
    (send PLAYGROUND-STATE test:clear-teams)
    PLAYGROUND-STATE))

(begin-for-test
  ;; Test cases for STEP 1
  (check-equal?
   (send (first (first (send (test:flow1-step1) playground-teams)))
         block-x)
   200
   "Wrong X position for the first cube after drag to (200,190)")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   190
   "Wrong Y position for the first cube after drag to (200,190)")
  (check-false
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Selection was not cleared for first cube after drag to (200,190)")

  ;; Test cases for STEP 2
  (check-equal?
   (send (first (first (send (test:flow1-step2) playground-teams)))
         block-x)
   275
   "Wrong X position for first cube during drag to (275,225)")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   225
   "Wrong Y position for first cube during drag to (275,225)")
  (check-true
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Wrong selection for first cube during drag to (275,225)")
  (check-equal?
   (send (first (second (send PLAYGROUND-STATE playground-teams)))
         block-x)
   300
   "Wrong X position for second cube during initialization")
  (check-equal?
   (send (first (second (send PLAYGROUND-STATE playground-teams)))
         block-y)
   250
   "Wrong Y position for second cube during initialization")
  (check-false
   (send (first (second (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Wrong selection for second cube during initialization")

  ;; Test cases for STEP 3
  (check-equal?
   (send (first (first (send (test:flow1-step3) playground-teams)))
         block-x)
   280
   "Wrong X position for first cube during drag to (280,230)")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   230
   "Wrong Y position for first cube during drag to (280,230)")
  (check-true
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Wrong selection for first cube during drag to (280,230)")
  (check-equal?
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         block-x)
   300
   "Wrong X position for second cube while teaming up with cube 1")
  (check-equal?
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   250
   "Wrong Y position for second cube while teaming up with cube 1")
  (check-false
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Wrong selection for second cube while teaming up with cube 1")

  ;; Test cases for STEP 4
  (check-equal?
   (send (first (first (send (test:flow1-step4) playground-teams)))
         block-x)
   300
   "Wrong X position for the first cube after drag to (300,275)")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   275
   "Wrong Y position for the first cube after drag to (300,275)")
  (check-true
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Selection was not set for first cube during drag to (300,275)")
  (check-equal?
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         block-x)
   320
   "Wrong X position for the second cube after drag to (320,295)")
  (check-equal?
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         block-y)
   295
   "Wrong Y position for the second cube after drag to (320,295)")
  (check-false
   (send (second (first (send PLAYGROUND-STATE playground-teams)))
         cube-selected?)
   "Wrong selection for second cube during drag to (300,275)")

  ;; Test cases for STEP 5
  (check-equal?
   (test:flow1-step5)
   (place-image
    (rectangle 20 20 "outline" "red") 300 275
    (place-image
      (rectangle 20 20 "outline" "green") 320 295 EMPTY-CANVAS))
   "Scene was not rendered properly in STEP 5")

  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         get-team)
   (first (send PLAYGROUND-STATE playground-teams))
   "Invalid team mates found for first cube")

  (check-equal?
   (begin
     (send (first (first (send PLAYGROUND-STATE playground-teams)))
           add-teammate (make-block 280 250 empty))
     (send (first (first (send PLAYGROUND-STATE playground-teams)))
           get-team))
   (first (send PLAYGROUND-STATE playground-teams))
   "Issue while adding a new team mate")

  (check-equal? (send PLAYGROUND-STATE test:curr-x) 200
                "Wrong X coordinate for last mouse down event")
  (check-equal? (send PLAYGROUND-STATE test:curr-y) 190
                "Wrong Y coordinate for last mouse down event")
  (check-equal? (send PLAYGROUND-STATE after-tick) PLAYGROUND-STATE
                "Wrong Playground state after tick event")
  (check-equal? (send (test:flow1-step6) playground-teams)
                (send PLAYGROUND-STATE playground-teams)
                "Invalid key events are not handled properly")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         after-key-event "b")
   (first (first (send PLAYGROUND-STATE playground-teams)))
   "Cubes are not handling the key events properly")
  (check-equal?
   (send (first (first (send PLAYGROUND-STATE playground-teams)))
         after-tick)
   (first (first (send PLAYGROUND-STATE playground-teams)))
   "Cubes are not handling the tick events properly")
  (check-equal?
   (send (test:clear-playgroundState) playground-teams) empty
   "Playground teams are not being cleared properly"))