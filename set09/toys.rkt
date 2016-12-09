#lang racket

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 make-world
 run
 make-square-toy
 make-throbber
 make-clock
 make-football
 PlaygroundState<%>
 Toy<%>)

(check-location "09" "toys.rkt")

;; CONSTANTS:
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define CANVAS-LEFT 0)
(define CANVAS-RIGHT CANVAS-WIDTH)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define NEW-SQUARE-TOY-EVENT "s")
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "w")
(define NEW-FOOTBALL-EVENT "f")

(define SQUARE-TOY-WIDTH 40)
(define SQUARE-TOY-HEIGHT 40)
(define SQUARE-TOY-HALF-WIDTH (/ SQUARE-TOY-WIDTH 2))
(define SQUARE-TOY-HALF-HEIGHT (/ SQUARE-TOY-HEIGHT 2))
(define SQUARE-TOY-MIN-X (+ CANVAS-LEFT SQUARE-TOY-HALF-WIDTH))
(define SQUARE-TOY-MAX-X (- CANVAS-RIGHT SQUARE-TOY-HALF-WIDTH))
(define SQUARE-TOY-STYLE "outline")
(define SQUARE-TOY-COLOR "red")

(define CLOCK-RADIUS 20)
(define CLOCK-STYLE "outline")
(define CLOCK-COLOR "red")
(define CLOCK-TEXT-COLOR "red")
(define CLOCK-TEXT-SIZE 11)
(define CLOCK-INIT-TICK 0)

(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define THROBBER-INIT-STEP 5)
(define THROBBER-STYLE "solid")
(define THROBBER-COLOR "green")

(define FOOTBALL-INIT-SCALE 0.4)
(define FOOTBALL-IMG-PATH "football.png")
(define FOOTBALL-SCALE-DOWN-VAR 0.75)

(define TARGET-RADIUS 10)
(define TARGET-STYLE "outline")
(define TARGET-COLOR "black")
(define TARGET-INIT-X (/ CANVAS-WIDTH 2))
(define TARGET-INIT-Y (/ CANVAS-HEIGHT 2))
(define TARGET-INIT-SELECTION false)

(define DEFAULT-MDX 0)
(define DEFAULT-MDY 0)
(define DEFAULT-SELECTION false)

;=================
;; DATA DEFINITIONS

;; Interfaces:

;; Every object that lives in the world must implement the Widget<%>
;; interface.

(define Widget<%>
  (interface ()

    ; after-tick : -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; after-button-down : Integer Integer -> Widget
    ; after-button-up : Integer Integer -> Widget
    ; after-drag : Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag

    ; add-to-scene : Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; The World implements the WorldState<%> interface
;; WorldState<%> contains all the methods needed by big-bang

(define WorldState<%>
  (interface ()

    ; after-tick : -> WorldState<%>
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick
    after-tick          

    ; after-mouse-event : Integer Integer MouseEvent-> WorldState<%>
    ; GIVEN: a location
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event


    ; after-key-event : KeyEvent -> WorldState<%>
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event
    after-key-event     

    ; to-scene : -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;; Playground state operates on the target and the list of toys
(define PlaygroundState<%>
  (interface (WorldState<%>) ;; this means: include all the methods in
                             ;; WorldState<%>.
    
    ;; target-x : -> Integer
    ;; target-y : -> Integer
    ;; RETURN: the x and y coordinates of the target
    target-x
    target-y

    ;; target-selected? : -> Boolean
    ;; Is the target selected?
    target-selected?

    ;; get-toys : -> ListOfToy<%>
    get-toys

))

;; A Toy is an interface to an object that is present in PlaygroundState<%>
(define Toy<%> 
  (interface (Widget<%>)  ;; this means: include all the methods in
                          ;;  Widget<%>. 
 
    ;; toy-x : -> Int
    ;; toy-y : -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y

    ;; toy-data : -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data))

;; SquareToy% is a class that represents the Square Toy object
;; A Square Toy object is a
;; (new SquareToy% [x Integer] [y Integer] [speed Integer]
;;   [mdx Integer] [mdy Integer] [selected? boolean])
(define SquareToy%
  (class* object% (Toy<%>)
    (init-field
     x ; PosInt, x-position of the center of the Toy
     y ; PosInt, y-position of the center of the Toy
     speed) ; Integer, speed at which the Toy moves in pixels/tick

    ;; Relative x and y position of the center of the Toy from the
    ;; current mouse x position. Both these values are 0 by default
    ;; and is updated when the Toy is selected
    (init-field mdx) ; Integer
    (init-field mdy) ; Integer

    ;; Whether the toy is selected?, False by default
    (init-field selected?) ; Boolean

    ;; Private variable which stores the SquareToy image
    (field [SQUARE-TOY-IMG
            (rectangle SQUARE-TOY-WIDTH SQUARE-TOY-HEIGHT
                       SQUARE-TOY-STYLE SQUARE-TOY-COLOR)])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Toy
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Toy
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> PosInt
    ;; RETURNS: the speed of the Toy in pixels/tick
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      speed)

    ;; after-tick : -> SquareToy
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time t+1
    ;; STRATEGY: Using cases on if the Toy is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (new SquareToy%
               [x (next-x-position)]
               [y y]
               [mdx mdx]
               [mdy mdy]
               [speed (update-speed)]
               [selected? selected?])))

    ;; after-button-down : PosInt PosInt -> SquareToy
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; inside the square toy or not
    (define/public (after-button-down mx my)
      (if (click-inside-rect? x y mx my SQUARE-TOY-HALF-WIDTH
                              SQUARE-TOY-HALF-HEIGHT)
          (new SquareToy%
               [x x]
               [y y]
               [mdx (- x mx)]
               [mdy (- y my)]
               [speed speed]
               [selected? (not selected?)])
          this))

    ;; after-button-up : PosInt PosInt -> SquareToy
    ;; GIVEN: the x and y coordinates of the mouse button up event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse button up event
    ;; STRATEGY: Using cases on if the SquareToy was already selected
    ;; or not
    (define/public (after-button-up mx my)
      (if selected?
          (new SquareToy%
               [x x]
               [y y]
               [mdx DEFAULT-MDX]
               [mdy DEFAULT-MDY]
               [speed speed]
               [selected? (not selected?)])
          this))

    ;; after-drag : PosInt PosInt -> SquareToy
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse drag event
    ;; STRATEGY: Using cases on if the SquareToy is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new SquareToy%
               [x (+ mx mdx)]
               [y (+ my mdy)]
               [mdx mdx]
               [mdy mdy]
               [speed speed]
               [selected? selected?])
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image SQUARE-TOY-IMG x y scene))

    ;; Private helper functions for Sqaure Toy

    ;; square-toy-hits-right-wall? : -> Boolean
    ;; RETURNS: true iff toy hits the right wall of the canvas
    ;; STRATEGY: Combining simpler functions
    (define (square-toy-hits-right-wall?)
      (< CANVAS-RIGHT (+ x speed SQUARE-TOY-HALF-WIDTH)))

    ;; square-toy-hits-left-wall? : -> Boolean
    ;; RETURNS: true iff toy hits the left wall of the canvas
    ;; STRATEGY: Combining simpler functions
    (define (square-toy-hits-left-wall?)
      (> CANVAS-LEFT (+ (- x SQUARE-TOY-HALF-WIDTH) speed)))

    ;; next-x-position : -> PosInt
    ;; RETURNS: the position of the center of the Toy at next tick
    ;; STRATEGY: Using cases on if the next x position is outside the
    ;; canvas or not
    (define (next-x-position)
      (cond
        [(square-toy-hits-right-wall?) SQUARE-TOY-MAX-X]
        [(square-toy-hits-left-wall?) SQUARE-TOY-MIN-X]
        [else (+ x speed)]))

    ;; update-speed : -> Integer
    ;; RETURNS: the velocity with which the toy moves in x-direction
    ;; STRATEGY: Using cases on if the next x position is outside the
    ;; canvas or not
    (define (update-speed)
      (if (or (square-toy-hits-right-wall?)
              (square-toy-hits-left-wall?))
          (- speed)
          speed))))

;; TESTS:
(begin-for-test
  (check-equal? (send test-sqt1 toy-x)
                 120)
  (check-equal? (send test-sqt1 toy-y)
                 70)
  (check-equal? (send test-sqt1 toy-data)
                 10)
  (check-equal? (send (send test-sqt1 after-tick) toy-x)
                 130)
  (check-equal? (send (send test-sqt1 after-tick) toy-y)
                 70)
  (check-equal? (send (send test-sqt1 after-tick) toy-data)
                 10)
  (check-equal? (send (send test-sqt1 after-tick) toy-data)
                 10)
  (check-equal? (send
                 (send
                 (send test-sqt1 after-button-down 119 71) after-drag 150 150)
                 toy-x)
                 151)
  (check-equal? (send
                 (send
                 (send test-sqt1 after-button-down 119 71) after-tick)
                 toy-x)
                 (send test-sqt1 toy-x))
  (check-equal? (send
                 (send
                 (send test-sqt1 after-button-down 119 71) after-drag 150 150)
                 toy-y)
                 149)
  (check-equal? (send
                 (send test-sqt1 after-drag 150 150)
                 toy-y)
                (send test-sqt1 toy-y))
  (check-equal? (send
                 (send
                 (send test-sqt1 after-button-down 119 71) after-button-up 5 5)
                 toy-x)
                 120)
  (check-equal? (send (send test-sqt1 after-button-down 150 150) toy-x)
                (send test-sqt1 toy-x))
  (check-equal? (send (send test-sqt1 after-button-up 150 150) toy-x)
                (send test-sqt1 toy-x))
  (check-equal? (send
                 (send
                  (send
                   (send
                    (send test-sqt1 after-button-down 119 71)
                    after-drag SQUARE-TOY-MAX-X 150)
                   after-button-up SQUARE-TOY-MAX-X 150)
                  after-tick)
                 toy-data)
                (- (send test-sqt1 toy-data)))
  (check-equal? (send
                 (send
                  (send
                   (send
                    (send test-sqt1 after-button-down 119 71)
                    after-drag 0 150)
                   after-button-up 0 150)
                  after-tick)
                 toy-x)
                SQUARE-TOY-MIN-X)
  (check-equal? (send test-sqt1 add-to-scene EMPTY-CANVAS)
                (place-image (rectangle SQUARE-TOY-WIDTH SQUARE-TOY-HEIGHT
                       SQUARE-TOY-STYLE SQUARE-TOY-COLOR) 120 70 EMPTY-CANVAS)))

;; A Throbber is
;; (new Throbber% [x PosInt][y PosInt]
;;     [radius PosInt]
;;     [step Integer]
;;     [mdx Integer]
;;     [mdy Integer]
;;     [selected? Boolean])
(define Throbber%
  (class* object% (Toy<%>)
    (init-field
     x ; PosInt, x-position of the center of the Throbber
     y) ; PosInt, y-position of the center of the Throbber

    ;; current radius of the Throbber in pixels, default being 0
    (init-field radius) ; PosInt

    ;; the step by which radius should change in current tick,
    ;; default value for a new instance is THROBBER-INIT-STEP
    (init-field step) ; Integer

    ;; Relative x and y position of center of the Throbber from the
    ;; current mouse x position. Both these values are 0 by default
    ;; and is updated when the Throbber is selected
    (init-field mdx) ; Integer
    (init-field mdy) ; Integer

    ;; Whether the Throbber is selected?, False by default
    (init-field selected?) ; Boolean

    ;; Private variable which stores the Throbber image
    (field [THROBBER-IMG (circle radius
                                 THROBBER-STYLE
                                 THROBBER-COLOR)])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Throbber
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Throbber
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> PosInt
    ;; RETURNS: the current radius of the Throbber in pixels
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      radius)

    ;; after-tick : -> Throbber
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time t+1
    ;; STRATEGY: Using cases on if the Throbber is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (new Throbber%
               [x x]
               [y y]
               [mdx mdx]
               [mdy mdy]
               [radius (+ radius step)]
               [step (update-step)]
               [selected? selected?])))

    ;; PosInt PosInt -> Throbber
    ;; GIVEN: the x and y coordinate of the mouse click event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; within the Throbber or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my radius)
          (new Throbber%
               [x x]
               [y y]
               [mdx (- x mx)]
               [mdy (- y my)]
               [radius radius]
               [step step]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Throbber
    ;; GIVEN: the x and y coordinate of the mouse button up event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse button up event
    ;; STRATEGY: Using cases on if the Throbber is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Throbber%
               [x x]
               [y y]
               [mdx DEFAULT-MDX]
               [mdy DEFAULT-MDY]
               [radius radius]
               [step step]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Throbber
    ;; GIVEN: the x and y coordinate of the mouse drag event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse drag event
    ;; STRATEGY: Using cases on if the Throbber is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (+ mx mdx)]
               [y (+ my mdy)]
               [mdx mdx]
               [mdy mdy]
               [radius radius]
               [step step]
               [selected? selected?])
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image THROBBER-IMG x y scene))

    ;; Private helper functions

    ;; throbber-at-max-size? : -> Boolean
    ;; RETURNS: true iff the radius of the throbber reaches maximum
    ;; STRATEGY: Combining simpler functions
    (define (throbber-at-max-size?)
      (>= (+ radius step) THROBBER-MAX-RADIUS))

    ;; throbber-at-min-size? : -> Boolean
    ;; RETURNS: true iff radius of throbber reaches minimum
    ;; STRATEGY: Combining simpler functions
    (define (throbber-at-min-size?)
      (<= (+ radius step) THROBBER-MIN-RADIUS))

    ;; update-step : -> Integer
    ;; RETURNS: the steps to be taken by the throbber in next tick
    ;; STRATEGY: Using cases on if throbber radius has reached the
    ;; minimum or maximum allowed value
    (define (update-step)
      (if (or (throbber-at-max-size?)
              (throbber-at-min-size?))
          (- step)
          step))))

;; TESTS:
(begin-for-test
  (check-equal? (send test-thrb1 toy-x)
                 140)
  (check-equal? (send test-thrb1 toy-y)
                 80)
  (check-equal? (send test-thrb1 toy-data)
                 5)
  (check-equal? (send
                 (send
                  (send
                   (send
                    (send test-thrb1 after-tick) after-tick)
                   after-tick)
                  after-tick)
                 toy-data)
                15)
  (check-equal? (send (send test-thrb1 after-tick) toy-x)
                 140)
  (check-equal? (send (send test-thrb1 after-tick) toy-y)
                 80)
  (check-equal? (send (send test-thrb1 after-tick) toy-data)
                 10)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 141 81) after-drag 170 150)
                 toy-x)
                 169)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 141 81) after-tick)
                 toy-x)
                 140)
  (check-equal? (send
                 (send test-thrb1 after-button-down 200 200)
                 toy-x)
                 140)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 200 200)
                 after-button-up 200 200)
                 toy-x)
                 140)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 200 200)
                 after-drag 250 250)
                 toy-x)
                 140)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 141 81) after-drag 170 150)
                 toy-y)
                 149)
  (check-equal? (send
                 (send
                 (send test-thrb1 after-button-down 142 83) after-button-up 5 5)
                 toy-x)
                 140)
  (check-equal? (send test-thrb1 add-to-scene EMPTY-CANVAS)
                (place-image (circle 5
                                 THROBBER-STYLE
                                 THROBBER-COLOR) 140 80 EMPTY-CANVAS)))
;; A Clock is
;; (new Clock% [x PosInt][y PosInt]
;;     [ticks PosInt]
;;     [mdx Integer]
;;     [mdy Integer]
;;     [selected? Boolean])
(define Clock%
  (class* object% (Toy<%>)
    (init-field
     x ; PosInt, x-position of the center of the Clock
     y) ; PosInt, y-position of the center of the Clock

    ;; Current value of the clock, default value is 0
    (init-field ticks) ; PosInt

    ;; Relative x and y position of the center of the Clock from the
    ;; current mouse x position. Both these values are 0 by default
    ;; and is updated when the Toy is selected
    (init-field mdx) ; Integer
    (init-field mdy) ; Integer

    ;; Whether the toy is selected?, False by default
    (init-field selected?) ; Boolean

    ;; Private variables
    ;; Stores the Clock image
    (field [clock-img (circle CLOCK-RADIUS
                              CLOCK-STYLE
                              CLOCK-COLOR)])

    ;; Stores the number of ticks since the Clock was created
    (field [clock-text (text (number->string ticks)
                             CLOCK-TEXT-SIZE
                             CLOCK-TEXT-COLOR)])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Clock
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Clock
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> PosInt
    ;; RETURNS: the number of ticks since the Clock was created
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      ticks)

    ;; after-tick : -> Clock
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time t+1
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)      
      (new Clock%
           [x x]
           [y y]
           [ticks (+ ticks 1)]
           [mdx mdx]
           [mdy mdy]
           [selected? selected?]))

    ;; PosInt PosInt -> Clock
    ;; GIVEN: x and y coordinates of the mouse click event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; inside the Clock or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my CLOCK-RADIUS)
          (new Clock%
               [x x]
               [y y]
               [ticks ticks]
               [mdx (- x mx)]
               [mdy (- y my)]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Clock
    ;; GIVEN: x and y coordinates of the mouse button up event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse button up event
    ;; STRATEGY: Using cases on if the Clock is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Clock%
               [x x]
               [y y]
               [ticks ticks]
               [mdx DEFAULT-MDX]
               [mdy DEFAULT-MDY]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Clock
    ;; GIVEN: x and y coordinates of the mouse drag event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse drag event
    ;; STRATEGY: Using cases on if the Clock is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (+ mx mdx)]
               [y (+ my mdy)]
               [ticks ticks]
               [mdx mdx]
               [mdy mdy]
               [selected? selected?])
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image clock-text x y
                   (place-image clock-img x y scene)))))
;; TESTS:
(begin-for-test
  (check-equal? (send test-clk1 toy-x)
                 20)
  (check-equal? (send test-clk1 toy-y)
                 200)
  (check-equal? (send test-clk1 toy-data)
                 0)
  (check-equal? (send (send test-clk1 after-tick) toy-x)
                 20)
  (check-equal? (send (send test-clk1 after-tick) toy-y)
                 200)
  (check-equal? (send (send test-clk1 after-tick) toy-data)
                 1)
  (check-equal? (send
                 (send
                 (send test-clk1 after-button-down 18 198) after-drag 100 210)
                 toy-x)
                 102)
  (check-equal? (send
                 (send
                 (send test-clk1 after-button-down 18 198)
                 after-button-up 18 198)
                 toy-x)
                 20)
  (check-equal? (send
                 (send
                 (send test-clk1 after-button-down 18 198) after-drag 100 210)
                 toy-y)
                 212)
  (check-equal? (send
                 (send
                 (send test-clk1 after-button-down 200 200)
                 after-drag 250 250)
                 toy-x)
                 20)
  (check-equal? (send
                 (send
                 (send test-clk1 after-button-down 142 83) after-button-up 5 5)
                 toy-x)
                 20)
  (check-equal? (send test-clk1 add-to-scene EMPTY-CANVAS)
                (place-image (text (number->string 0)
                             CLOCK-TEXT-SIZE
                             CLOCK-TEXT-COLOR) 20 200
                   (place-image (circle CLOCK-RADIUS
                              CLOCK-STYLE
                              CLOCK-COLOR) 20 200 EMPTY-CANVAS))))

;; A Football is
;; (new Football% [x PosInt][y PosInt]
;;     [scale PosReal]
;;     [mdx Integer] [mdy Integer]
;;     [selected? Boolean])
(define Football%
  (class* object% (Toy<%>)
    (init-field
     x ; PosInt, x-position of the center of the Football image
     y) ; PosInt, y-position of the center of the Football image

    ;; Current value of the scale, default value is 0.4
    (init-field scale) ; PosReal

    ;; Relative x and y position of the center of the Football from
    ;; the current mouse position. Both these values are 0 by
    ;; default and is updated when the Football is selected
    (init-field mdx) ; Integer
    (init-field mdy) ; Integer

    ;; Whether the Football is selected?, False by default
    (init-field selected?) ; Boolean

    ;; Private variable which stores the Football image
    (field [football-img
            (scale/xy
             scale scale
             (bitmap/file FOOTBALL-IMG-PATH))])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> PosReal
    ;; RETURNS: the current size of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      (* (image-width football-img)
         (image-height football-img)))

    ;; after-tick : -> Football
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time t+1
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)    
      (new Football%
           [x x]
           [y y]
           [scale (update-scale)]
           [mdx mdx]
           [mdy mdy]
           [selected? selected?]))

    ;; PosInt PosInt -> Football
    ;; GIVEN: the x and y coordinate of the mouse click event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; within the Football image or not
    (define/public (after-button-down mx my)
      (local
        ((define img-half-width (/ (image-width football-img) 2))
         (define img-half-height (/ (image-height football-img) 2)))
        (if (click-inside-rect? x y mx my img-half-width
                                img-half-height)
            (new Football%
                 [x x]
                 [y y]
                 [scale scale]
                 [mdx (- x mx)]
                 [mdy (- y my)]
                 [selected? (not selected?)])
            this)))

    ;; PosInt PosInt -> Football
    ;; GIVEN: the x and y coordinate of the mouse button up event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse button up event
    ;; STRATEGY: Using cases on if the Football is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Football%
               [x x]
               [y y]
               [scale scale]
               [mdx DEFAULT-MDX]
               [mdy DEFAULT-MDY]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Football
    ;; GIVEN: the x and y coordinate of the mouse drag event
    ;; RETURNS: the state of this object that should follow the
    ;; mouse drag event
    ;; STRATEGY: Using cases on if the Football is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Football%
               [x (+ mx mdx)]
               [y (+ my mdy)]
               [scale scale]
               [mdx mdx]
               [mdy mdy]
               [selected? selected?])
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image football-img x y scene))

    ;; Private methods
    ;; update-scale : -> PosReal
    ;; RETURNS: the current scaling of the Football image
    ;; STRATEGY: Using cases on if the scale is > 0.001
    (define (update-scale)
      (local ((define new-scale (* scale FOOTBALL-SCALE-DOWN-VAR)))
        (if (> new-scale 0.001)
            new-scale
            scale)))))
;; TESTS:
(begin-for-test
  (check-equal? (send test-ftbl1 toy-x)
                 50)
  (check-equal? (send test-ftbl1 toy-y)
                 50)
  (check-equal? (send test-ftbl1 toy-data)
                 76800)
  (check-equal? (send (send test-ftbl1 after-tick) toy-x)
                 50)
  (check-equal? (send (send test-ftbl1 after-tick) toy-y)
                 50)
  (check-equal? (send (send test-ftbl1 after-tick) toy-data)
                 43200)
  (check-equal? (send
                 (send
                 (send test-ftbl1 after-button-down 51 49) after-drag 100 100)
                 toy-x)
                 99)
  (check-equal? (send
                 (send test-ftbl1 after-button-down 200 200)
                 toy-x)
                50)
  (check-equal? (send
                 (send
                 (send test-ftbl1 after-button-down 51 49) after-drag 100 100)
                 toy-y)
                 101)
  (check-equal? (send
                 (send
                  (send test-ftbl1 after-button-down 200 200)
                  after-drag 250 250)
                 toy-x)
                50)
  (check-equal? (send
                 (send test-ftbl2 after-tick) toy-x) 50)
  (check-equal? (send
                 (send
                 (send test-ftbl1 after-button-down 51 49) after-button-up 5 5)
                 toy-x)
                 50)
  (check-equal? (send
                 (send
                 (send test-ftbl1 after-button-down 200 200)
                 after-button-up 200 200)
                 toy-x)
                 50)
  (check-equal? (send test-ftbl1 add-to-scene EMPTY-CANVAS)
                (place-image (scale/xy
             0.4 0.4
             (bitmap/file FOOTBALL-IMG-PATH)) 50 50 EMPTY-CANVAS)))

;; A Football is
;; (new Target% [x PosInt][y PosInt]
;;     [mdx Integer] [mdy Integer]
;;     [selected? Boolean])
(define Target%
  (class* object% (Toy<%>)
    (init-field
     ;; x and y position of the center of the Target
     x          ; PosInt
     y          ; PosInt
     ;; Relative x and y position of the center of the Target from
     ;; the current mouse x position. Both these values are 0 by
     ;; default and is updated when the Target is selected
     mdx        ; Integer
     mdy        ; Integer
     ;; Whether the toy is selected?, False by default
     selected?) ; Boolean
    
    ;; Private variable which stores the Target image
    (field [target-img (circle TARGET-RADIUS
                               TARGET-STYLE
                               TARGET-COLOR)])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Target
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Target
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> Boolean
    ;; RETURNS: true iff the Target is selected
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      selected?)

    ;; after-tick : -> Target
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at
    ;; time t+1
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)
      this)

    ;; PosInt PosInt -> Target
    ;; GIVEN: a location
    ;; RETURNS: the state of this object that should follow the
    ;; mouse click event
    ;; STRATEGY: Using cases on if the mouse click event happened
    ;; within the Target or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my TARGET-RADIUS)
          (new Target%
               [x x]
               [y y]
               [mdx (- x mx)]
               [mdy (- y my)]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Target
    ;; GIVEN: a location
    ;; RETURNS: the state of this object that should follow the
    ;; mouse button up event
    ;; STRATEGY: Using cases on if the Target is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (new Target%
               [x x]
               [y y]
               [mdx DEFAULT-MDX]
               [mdy DEFAULT-MDY]
               [selected? (not selected?)])
          this))

    ;; PosInt PosInt -> Target
    ;; GIVEN: a location
    ;; RETURNS: the state of this object that should follow the
    ;; mouse drag event
    ;; STRATEGY: Using cases on if the Target is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (new Target%
               [x (+ mx mdx)]
               [y (+ my mdy)]
               [mdx mdx]
               [mdy mdy]
               [selected? selected?])
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    (define/public (add-to-scene scene)
      (place-image target-img x y scene))))

;; TESTS:
(begin-for-test
  (check-equal? (send test-trgt1 toy-x)
                 250)
  (check-equal? (send test-trgt1 toy-y)
                 250)
  (check-equal? (send test-trgt1 toy-data)
                 false)
  (check-equal? (send (send test-trgt1 after-tick) toy-x)
                 250)
  (check-equal? (send (send test-trgt1 after-tick) toy-y)
                 250)
  (check-equal? (send (send test-trgt1 after-tick) toy-data)
                 false)
  (check-equal? (send
                 (send
                 (send test-trgt1 after-button-down 250 250) after-drag 100 100)
                 toy-x)
                 100)
  (check-equal? (send
                 (send test-trgt1 after-button-down 300 300)
                 toy-x)
                 250)
  (check-equal? (send
                 (send
                 (send test-trgt1 after-button-down 250 250) after-drag 100 100)
                 toy-y)
                 100)
  (check-equal? (send
                 (send
                 (send test-trgt1 after-button-down 300 300)
                 after-drag 100 100)
                 toy-x)
                 250)
  (check-equal? (send
                 (send
                 (send test-trgt1 after-button-down 250 250) after-button-up 5 5)
                 toy-x)
                 250)
  (check-equal? (send
                 (send
                 (send test-trgt1 after-button-down 300 300)
                 after-button-up 300 300)
                 toy-x)
                 250)
  (check-equal? (send test-trgt1 add-to-scene EMPTY-CANVAS)
                (place-image (circle TARGET-RADIUS
                               TARGET-STYLE
                               TARGET-COLOR) 250 250 EMPTY-CANVAS)))
;; A PlaygroundState is
;; (new PlaygroundState% [objs ListOfToy][speed speed])
(define PlaygroundState%
  (class* object% (PlaygroundState<%>)

    (init-field objs) ; ListOfToy    
    (init-field speed) ; PosInt

    (super-new)

    (define target (last objs)) ; Target

    ;; target-x : -> PosInt
    ;; RETURNS: the x-coordinate position of the target
    ;; STRATEGY: Combining simpler functions
    (define/public (target-x)
      (send target toy-x))

    ;; target-y : -> PosInt
    ;; RETURNS: the y-coordinate position of the target
    ;; STRATEGY: Combining simpler functions
    (define/public (target-y)
      (send target toy-y))

    ;; target-selected? : -> Boolean
    ;; RETURNS: true iff the target is currently selected
    ;; STRATEGY: Combining simpler functions
    (define/public (target-selected?)
      (send target toy-data))

    ;; get-toys : -> ListofToy
    ;; RETURNS: the list of toys in the current state
    ;; STRATEGY: Combining simpler functions
    (define/public (get-toys)
      objs)

    ;; after-tick : -> World
    ;; RETURNS: the state which follows the current state at time t+1
    ;; STRATEGY: Useing HOF map on the Toy-s in this World
    (define/public (after-tick)
      (make-playgrd-state
       (map
        ;; Toy -> Toy
        ;; GIVEN: a toy
        ;; RETURNS: the state of the toy at time t+1
        (lambda (toy) (send toy after-tick))
        objs)
       speed))

    ;; to-scene : -> Scene
    ;; RETURNS: a Scene updated with all the Toys rendered on it
    ;; STRATEGY: Use HOF foldr on the Toy-s in this World
    (define/public (to-scene)
      (foldr
       ;; Toy Scene -> Scene
       ;; GIVEN: a toy and a scene
       ;; RETURNS: the scene updated with the toy
       (lambda (toy scene)
         (send toy add-to-scene scene))
       EMPTY-CANVAS
       objs))

    ;; after-key-event : KeyEvent -> WorldState
    ;; GIVEN: a key event
    ;; RETURNS: the world state that follows the key event
    ;; STRATEGY: Cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-SQUARE-TOY-EVENT)
         (make-playgrd-state (cons
            (make-square-toy (send this target-x)
                             (send this target-y)
                             speed)
            objs) speed)]
        [(key=? kev NEW-THROBBER-EVENT)
         (make-playgrd-state
           (cons (make-throbber (send this target-x)
                                (send this target-y))
                 objs)
           speed)]
        [(key=? kev NEW-CLOCK-EVENT)
         (make-playgrd-state
           (cons (make-clock (send this target-x)
                             (send this target-y))
                 objs)
           speed)]
        [(key=? kev NEW-FOOTBALL-EVENT)
         (make-playgrd-state
           (cons (make-football (send this target-x)
                                (send this target-y))
                 objs)
           speed)]
        [else
         this]))

    ;; world-after-mouse-event : PosInt PosInt MouseEvent
    ;;                            -> WorldState
    ;; GIVEN: the x and y coordinates of the mouse event and the
    ;; mouse event that occurred
    ;; RETURNS: the world after the mouse event
    ;; STRATEGY: Using cases on mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev "button-down")
         (world-after-button-down mx my)]
        [(mouse=? mev "drag")
         (world-after-drag mx my)]
        [(mouse=? mev "button-up")
         (world-after-button-up mx my)]
        [else this]))

    ;; Local functions

    ;; world-after-button-down : PosInt PosInt -> WorldState
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; RETURNS: the world state after the mouse click event
    ;; STRATEGY: Using HOF map on objs
    (define (world-after-button-down mx my)
      (make-playgrd-state
       (map
        ;; Toy -> Toy
        ;; GIVEN: a Toy
        ;; RETURNS: the toy after mouse click event
        (lambda (toy) (send toy after-button-down mx my))
        objs)
       speed))

    ;; world-after-button-up : PosInt PosInt -> WorldState
    ;; GIVEN: the x and y coordinates of the mouse button up event
    ;; RETURNS: the world state after the mouse button up event
    ;; STRATEGY: Using HOF map on objs
    (define (world-after-button-up mx my)
      (make-playgrd-state
        (map
         ;; Toy -> Toy
         ;; GIVEN: a Toy
         ;; RETURNS: the Toy after mouse button up event
         (lambda (toy) (send toy after-button-up mx my))
         objs)
        speed))

    ;; world-after-drag : PosInt PosInt -> WorldState
    ;; GIVEN: the x and y coordinates of the mouse drag event
    ;; RETURNS: the world state after the mouse drag event
    ;; STRATEGY: Using HOF map on objs
    (define (world-after-drag mx my)
      (make-playgrd-state
       (map
        ;; Toy -> Toy
        ;; GIVEN: a Toy
        ;; RETURNS: the Toy after mouse drag event
        (lambda (toy) (send toy after-drag mx my))
        objs)
       speed))))
;; TESTS:
(begin-for-test
  (check-equal? (send test-plgrd target-x)
                 250)
  (check-equal? (send test-plgrd target-y)
                 300)
  (check-equal? (send test-plgrd target-selected?)
                 false)
  (check-equal? (send (send test-plgrd after-tick) target-x)
                 250)
  (check-equal? (send (send test-plgrd after-tick) target-y)
                 300)
  (check-equal? (send (send test-plgrd after-tick) target-selected?)
                 false)
;  (check-equal? (send (send test-plgrd after-key-event "s") get-toys)
;                 (list (make-square-toy 250 300 10) (make-target)))
  (check-equal? (send
                 (send
                  (send test-plgrd after-mouse-event 251 301 "button-down")
                  after-mouse-event 100 100 "drag")
                 target-x)
                 99)
  (check-equal? (send
                 (send test-plgrd after-mouse-event 251 301 "move")
                 target-x)
                 250)
  (check-equal? (send
                 (first (send
                         (send test-plgrd after-key-event "s")
                         get-toys)) toy-x)
                250)
  (check-equal? (send
                 (first (send
                         (send test-plgrd after-key-event "t")
                         get-toys)) toy-x)
                250)
  (check-equal? (send
                 (first (send
                         (send test-plgrd after-key-event "f")
                         get-toys)) toy-x)
                250)
  (check-equal? (send
                 (first (send
                         (send test-plgrd after-key-event "w")
                         get-toys)) toy-x)
                250)
  (check-false (send
                (first (send
                        (send test-plgrd after-key-event "a")
                        get-toys)) toy-data))
  (check-equal? (send
                 (send
                  (send test-plgrd after-mouse-event 251 301 "button-down")
                  after-mouse-event 100 100 "button-up")
                 target-x)
                 250)
  (check-equal? (send test-plgrd to-scene)
                (place-image (circle TARGET-RADIUS
                               TARGET-STYLE
                               TARGET-COLOR) 250 300 EMPTY-CANVAS)))

;; click-inside-circle? : NonNegInt NonNegInt NonNegInt NonNegInt
;;                         NonNegInt -> Boolean
;; GIVEN: the x & y coordinate of the center of a circle, the x & y
;; coordinate of the mouse click event and the radius of the circle
;; RETURNS: true iff the click happened inside the circle
;; STRATEGY: Combining simpler functions
(define (click-inside-circle? x y mx my radius)
  (<= (sqrt (+ (sqr (- mx x)) (sqr (- my y)))) radius))

;; click-inside-rect? : NonNegInt NonNegInt NonNegInt NonNegInt
;;                       NonNegInt NonNegInt -> Boolean
;; GIVEN: x & y coordinates of the rectangle, the x & y coordinate
;; of a mouse event and the half of the rectangle's height & width
;; RETURNS: whether the event occurred inside the rectangle or not
;; STRATEGY: Combining simpler functions
(define (click-inside-rect? x y mx my dx dy)
  (and
    (<= (- x dx) mx (+ x dx))
    (<= (- y dy) my (+ y dy))))

;; make-world : PosInt -> PlaygroundState<%>
;; GIVEN: the speed at which the SquareToy moves
;; RETURNS: a world with a target, but no toys, and in which any
;; square toys created in the future will travel at the given speed (in
;; pixels/tick)
;; STRATEGY: Combining simpler functions
(define (make-world speed)
  (make-playgrd-state (list(make-target)) speed))

;; make-playgrd-state : PosInt -> PlaygroundState<%>
;; GIVEN: the speed at which the SquareToy moves
;; RETURNS: a Playground state
;; STRATEGY: Combining simpler functions
(define (make-playgrd-state lst speed)
  (new PlaygroundState% [objs lst][speed speed]))

;; run : PosNum PosInt -> PlaygroundState<%> 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed
;; (in pixels/tick), creates and runs a world in which square
;; toys travel at the given speed.
;; RETURNS: the final state of the world
;; STRATEGY: Combine simpler functions
(define (run rate square-speed)
  (big-bang (make-world square-speed)
    (on-tick
      (lambda (w) (send w after-tick))
      rate)
    (on-draw
      (lambda (w) (send w to-scene)))
    (on-key
      (lambda (w kev)
        (send w after-key-event kev)))
    (on-mouse
      (lambda (w mx my mev)
        (send w after-mouse-event mx my mev)))))

;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; STRATEGY: Combining simpler functions
(define (make-square-toy x y speed)
  (new SquareToy% [x x][y y][speed speed]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))

;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: the x and y position
;; RETURNS: an object representing a throbber at the given position.
;; STRATEGY: Combining simpler functions
(define (make-throbber x y)
  (new Throbber% [x x][y y]
       [radius THROBBER-MIN-RADIUS]
       [step THROBBER-INIT-STEP]
       [mdx DEFAULT-MDX]
       [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))

;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position
;; STRATEGY: Combining simpler functions
(define (make-clock x y)
  (new Clock% [x x][y y]
       [ticks CLOCK-INIT-TICK]
       [mdx DEFAULT-MDX]
       [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; STRATEGY: Combining simpler functions
(define (make-football x y)
  (new Football% [x x][y y]
       [scale FOOTBALL-INIT-SCALE]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))

;; make-target : -> Target
;; GIVEN: no arguments
;; RETURNS: an object representing a Target at the given position
;; STRATEGY: Combining simpler functions
(define (make-target)
  (new Target% [x TARGET-INIT-X][y TARGET-INIT-Y]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))


;=================
; TEST VARIABLES
;=================
(define test-sqt1 (make-square-toy 120 70 10))
(define test-thrb1 (make-throbber 140 80))
(define test-clk1 (make-clock 20 200))
(define test-ftbl1 (make-football 50 50))
(define test-ftbl2 (new Football% [x 50][y 50]
       [scale 0.0001]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))
(define test-trgt1 (new Target% [x 250][y 250]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))
(define test-plgrd (make-world 10))
