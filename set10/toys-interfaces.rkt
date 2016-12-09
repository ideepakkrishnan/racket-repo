#lang racket

(require "WidgetWorks.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 PlaygroundState<%>
 Toy<%>
 click-inside-rect?
 click-inside-circle?
 CANVAS-WIDTH
 CANVAS-HEIGHT
 CANVAS-LEFT
 CANVAS-RIGHT
 EMPTY-CANVAS
 SQUARE-TOY-WIDTH
 SQUARE-TOY-HEIGHT
 SQUARE-TOY-HALF-WIDTH
 SQUARE-TOY-HALF-HEIGHT
 SQUARE-TOY-MIN-X
 SQUARE-TOY-MAX-X
 SQUARE-TOY-STYLE
 SQUARE-TOY-COLOR
 DEFAULT-MDX
 DEFAULT-MDY
 DEFAULT-SELECTION
 THROBBER-MIN-RADIUS
 THROBBER-MAX-RADIUS 
 THROBBER-INIT-STEP
 THROBBER-STYLE
 THROBBER-COLOR
 CLOCK-RADIUS
 CLOCK-STYLE
 CLOCK-COLOR
 CLOCK-TEXT-COLOR
 CLOCK-TEXT-SIZE
 CLOCK-INIT-TICK
 FOOTBALL-INIT-SCALE
 FOOTBALL-IMG-PATH
 FOOTBALL-SCALE-DOWN-VAR
 TARGET-RADIUS
 TARGET-STYLE
 TARGET-COLOR
 TARGET-INIT-X
 TARGET-INIT-Y
 TARGET-INIT-SELECTION
 NEW-SQUARE-TOY-EVENT
 NEW-THROBBER-EVENT
 NEW-CLOCK-EVENT
 NEW-FOOTBALL-EVENT)

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

;; Playground state operates on the target and the list of toys
(define PlaygroundState<%>
  (interface (SWidget<%>) ;; this means: include all the methods in
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
  (interface (SWidget<%>)  ;; this means: include all the methods in
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

;======================
;; COMMON FUNCTIONS
;======================

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

;; click-inside-circle? : NonNegInt NonNegInt NonNegInt NonNegInt
;;                         NonNegInt -> Boolean
;; GIVEN: the x & y coordinate of the center of a circle, the x & y
;; coordinate of the mouse click event and the radius of the circle
;; RETURNS: true iff the click happened inside the circle
;; STRATEGY: Combining simpler functions
(define (click-inside-circle? x y mx my radius)
  (<= (sqrt (+ (sqr (- mx x)) (sqr (- my y)))) radius))