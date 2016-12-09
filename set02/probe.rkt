;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/image)

(provide
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?
 )

(check-location "02" "probe.rkt")

;; GLOBALS/CONSTANTS
(define SCENE-SIDE 347)
(define PBODY (circle 20 "solid" "Orange")) ;; Probe body
(define TRAP (empty-scene SCENE-SIDE SCENE-SIDE)) ;; Plutonian trap

;; DATA DEFINITIONS
(define-struct probe (body x y dir))
;; A Probe is a (make-probe PosInt PosInt String)
;; INTERPRETATION:
;; body - actual body of the probe
;; x - x coordinate (horizontal) position of the probe in cm
;; y - y coordinate (vertical) position of the probe in cm
;; dir - the direction which the probe is facing. Could take the
;;       values "north", "east", "south" or "west"
;; DESTRUCTOR TEMPLATE:
#; (define (probe-fn p)
     (...
      (probe-body p)
      (probe-x p)
      (probe-y p)
      (probe-dir p))
     )

;; Coordinate system being used:
;;
;;   a-----b
;;   |     |
;;   |  o  |
;;   |     |
;;   c-----d
;;
;; where:
;; a -> (-173.5, -173.5)
;; b -> (173.5, -173.5)
;; o -> (0, 0)
;; c -> (-173.5, 173.5)
;; d -> (173.5, 173.5)

;; math-coordinate : Integer -> PosInt
;; Since the trap is a square, the relative positions can be
;; calculated by adding the trap's side length to the coordinates.
;; STRATEGY: Combining simple functions
;; GIVEN: coordinate in graphics-style coordinate system
;; RETURNS: coordinate in standard coordinate system
;; EXAMPLES:
;; (math-coordinate 0) = 173.5
;; (math-coordinate -173.5) = 0
;; (math-coordinate 173.5) = 347
(define (math-coordinate x)
  (+ x (/ SCENE-SIDE 2)))

;; TESTS:
(begin-for-test
  (check-equal? (math-coordinate 0) 173.5)
  (check-equal? (math-coordinate -173.5) 0)
  (check-equal? (math-coordinate 173.5) 347))

;; graphics-coordinate : PosInt -> Integer
;; STRATEGY: Combining simple functions
;; GIVEN: coordinate in math-style coordinate system
;; RETURNS: coordinate in graphics-style coordinate system
;; EXAMPLES:
;; (graphics-coordinate 173.5) = 0
;; (graphics-coordinate 0) = -173.5
;; (graphics-coordinate 347) = 173.5
(define (graphics-coordinate x)
  (- x (/ SCENE-SIDE 2)))

;; TESTS:
(begin-for-test
  (check-equal? (graphics-coordinate 173.5) 0)
  (check-equal? (graphics-coordinate 0) -173.5)
  (check-equal? (graphics-coordinate 347) 173.5))

;; probe-at : Integer Integer -> Probe
;; STRATEGY: Combining simple functions and use template for probe
;; on the generated output
;; GIVEN: an x-coordinate and a y-coordinate
;; WHERE: these coordinates leave the robot entirely inside the trap
;; RETURNS: a probe with its center at those coordinates, facing north
;; EXAMPLES:
;; (probe-at 0 0) = (make-probe (circle 20 "solid" "Orange")
;;                    0 0 "north"))
;; (probe-at -10 -25) = (make-probe (circle 20 "solid" "Orange")
;;                        -10 -25 "north"))
(define (probe-at x y)
  (make-probe PBODY x y "north"))

;; TESTS:
(begin-for-test
  (check-equal? (probe-at 0 0)
                (make-probe PBODY 0 0 "north"))
  (check-equal? (probe-at -10 -25)
                (make-probe PBODY -10 -25 "north"))
  )

;; left-direction : Probe -> String
;; STRATEGY: Combining template for Probe on p and using cases
;;           on p
;; GIVEN: a probe
;; RETURNS: the new direction of the probe when it turns left
;; EXAMPLES:
;; (left-direction (make-probe PBODY 0 0 "north")) = "west"
;; (left-direction (make-probe PBODY 0 0 "west")) = "south"
;; (left-direction (make-probe PBODY 0 0 "south")) = "east"
;; (left-direction (make-probe PBODY 0 0 "east")) = "north"
(define (left-direction p)
  (cond
    [(probe-north? p) "west"]
    [(probe-south? p) "east"]
    [(probe-east? p) "north"]
    [(probe-west? p) "south"])
  )

;; TESTS:
(begin-for-test
  (check-equal? (left-direction
                 (make-probe PBODY 0 0 "north"))
                "west")
  (check-equal? (left-direction
                 (make-probe PBODY 0 0 "west"))
                "south")
  (check-equal? (left-direction
                 (make-probe PBODY 0 0 "south"))
                "east")
  (check-equal? (left-direction
                 (make-probe PBODY 0 0 "east"))
                "north")
  )

;; right-direction : Probe -> String
;; STRATEGY: Combining template for Probe on p and using cases
;;           on p
;; GIVEN: a probe
;; RETURNS: the new direction of the probe when it turns right
;; EXAMPLES:
;; (right-direction (make-probe PBODY 0 0 "north")) = "east"
;; (right-direction (make-probe PBODY 0 0 "east")) = "south"
;; (right-direction (make-probe PBODY 0 0 "south")) = "west"
;; (right-direction (make-probe PBODY 0 0 "west")) = "north"
(define (right-direction p)
  (cond
    [(probe-north? p) "east"]
    [(probe-south? p) "west"]
    [(probe-east? p) "south"]
    [(probe-west? p) "north"])
  )

;; TESTS
(begin-for-test
  (check-equal? (right-direction
                 (make-probe PBODY 0 0 "north"))
                "east")
  (check-equal? (right-direction
                 (make-probe PBODY 0 0 "east"))
                "south")
  (check-equal? (right-direction
                 (make-probe PBODY 0 0 "south"))
                "west")
  (check-equal? (right-direction
                 (make-probe PBODY 0 0 "west"))
                "north")
  )

;; probe-turned-left : Probe -> Probe
;; STRATEGY: Use template for Probe on p
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees left
;; EXAMPLES:
;; (probe-turned-left (make-probe PBODY 0 0 "north")) = "west"
;; (probe-turned-left (make-probe PBODY 0 0 "west")) = "south"
;; (probe-turned-left (make-probe PBODY 0 0 "south")) = "east"
;; (probe-turned-left (make-probe PBODY 0 0 "east")) = "north"
(define (probe-turned-left p)
  (make-probe
   (probe-body p)
   (probe-x p)
   (probe-y p)
   (left-direction p))
  )

;; TESTS:
(begin-for-test
  (check-equal? (probe-turned-left
                 (make-probe PBODY 0 0 "north"))
                (make-probe PBODY 0 0 "west"))
  (check-equal? (probe-turned-left
                 (make-probe PBODY 0 0 "west"))
                (make-probe PBODY 0 0 "south"))
  (check-equal? (probe-turned-left
                 (make-probe PBODY 0 0 "south"))
                (make-probe PBODY 0 0 "east"))
  (check-equal? (probe-turned-left
                 (make-probe PBODY 0 0 "east"))
                (make-probe PBODY 0 0 "north"))
  )

;; probe-turned-right : Probe -> Probe
;; STRATEGY: Use template for Probe on p
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90 degrees right
;; (probe-turned-right (make-probe PBODY 0 0 "north")) = "east"
;; (probe-turned-right (make-probe PBODY 0 0 "east")) = "south"
;; (probe-turned-right (make-probe PBODY 0 0 "south")) = "west"
;; (probe-turned-right (make-probe PBODY 0 0 "west")) = "north"
(define (probe-turned-right p)
  (make-probe
   (probe-body p)
   (probe-x p)
   (probe-y p)
   (right-direction p))
  )

;; TESTS
(begin-for-test
  (check-equal? (probe-turned-right
                 (make-probe PBODY 0 0 "north"))
                (make-probe PBODY 0 0 "east"))
  (check-equal? (probe-turned-right
                 (make-probe PBODY 0 0 "east"))
                (make-probe PBODY 0 0 "south"))
  (check-equal? (probe-turned-right
                 (make-probe PBODY 0 0 "south"))
                (make-probe PBODY 0 0 "west"))
  (check-equal? (probe-turned-right
                 (make-probe PBODY 0 0 "west"))
                (make-probe PBODY 0 0 "north"))
  )

;; new-position : Integer Integer -> Integer
;; If any move of the probe would cause the probe to run into
;; the wall of the trap, then the probe will move forward until
;; another 1cm step would take it past the wall, and then stop.
;; STRATEGY: Using cases on x and dx
;; GIVEN: Position and distance to be moved
;; RETURNS: new position of the probe
;; EXAMPLES:
;; (new-position 10 10) = 20
;; (new-position -10 -10) = -20
;; (new-position 10 170) = 173
;; ()
(define (new-position x dx)
  (cond
    ;; handle cases where movement takes the probe past the wall
    [(> (+ x dx) (/ SCENE-SIDE 2))
     (quotient SCENE-SIDE 2)]
    [(< (+ x dx) (/ SCENE-SIDE -2))
     (quotient SCENE-SIDE -2)]
    ;; normal movement
    [else (+ x dx)])
  )

;; TESTS:
(begin-for-test
  (check-equal? (new-position 10 10) 20)
  (check-equal? (new-position 10 170) 173)
  (check-equal? (new-position -10 -163) -173)
  (check-equal? (new-position -173 -1) -173))

;; move-sides : Probe PosInt -> Probe
;; STRATEGY: Using template for Probe on p
;; GIVEN: a probe and the horizontal distance to be moved in cm
;; RETURNS: the new position of the probe
;; EXAMPLES:
;; (move-sides (make-probe PBODY 0 0 "west") 10) =
;;   (make-probe PBODY -10 0 "west")
;; (move-sides (make-probe PBODY 173 0 "east") 10) =
;;   (make-probe PBODY 173 0 "east")
(define (move-sides p dx)
  (make-probe  ;; move the probe in horizontal direction
   (probe-body p)
   (new-position (probe-x p) dx)
   (probe-y p)
   (probe-dir p))
  )

;; move-up-down : Probe Integer -> Probe
;; STRATEGY: Using template for probe on p
;; GIVEN: a probe and the vertical distance to be moved in cm
;; RETURNS: the new position of the probe
;; EXAMPLES:
;; (move-sides (make-probe PBODY 0 0 "north") 173) =
;;   (make-probe PBODY 0 -173 "north")
;; (move-sides (make-probe PBODY 0 0 "south") 174) =
;;   (make-probe PBODY 0 173 "south")
(define (move-up-down p dy)
  (make-probe  ;; move the probe in vertical direction
   (probe-body p)
   (probe-x p)
   (new-position (probe-y p) dy)
   (probe-dir p))
  )

;; probe-forward : Probe PosInt -> Probe
;; The system uses a graphics-style coordinate system with the
;; center of the plutonian trap taking the origin. Hence, we need
;; to update the relative signs of the displacement to keep it in
;; terms with the coordinate system.
;; STRATEGY: Using template for Probe on p and using cases on p
;; GIVEN: a probe and a distance in cm
;; RETURNS: a probe like the given one, but moved forward by the
;; specified distance.  If moving forward the specified distance would
;; cause the probe to hit any wall of the trap, then the probe should 
;; move as far as it can inside the trap, and then stop.
;; EXAMPLES:
;; (probe-forward (make-probe PBODY 0 0 "west") 10) =
;;   (make-probe PBODY -10 0 "west")
;; (probe-forward (make-probe PBODY 0 0 "north") 173) =
;;   (make-probe PBODY 0 -173 "north")
;; (probe-forward (make-probe PBODY 0 0 "south") 174) =
;;   (make-probe PBODY 0 173 "south")
;; (probe-forward (make-probe PBODY 173 0 "east") 10) =
;;   (make-probe PBODY 173 0 "east")
(define (probe-forward p dx)
  (cond
    [(probe-north? p)
     (move-up-down p (- 0 dx))] ;; changing signs to match direction
    [(probe-south? p)
     (move-up-down p dx)]
    [(probe-east? p)
     (move-sides p dx)]
    [(probe-west? p)
     (move-sides p (- 0 dx))])  ;; changing signs to match direction
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (probe-forward (make-probe PBODY 0 0 "west") 10)
   (make-probe PBODY -10 0 "west"))
  (check-equal?
   (probe-forward (make-probe PBODY 0 0 "north") 173)
   (make-probe PBODY 0 -173 "north"))
  (check-equal?
   (probe-forward (make-probe PBODY 0 0 "south") 174)
   (make-probe PBODY 0 173 "south"))
  (check-equal?
   (probe-forward (make-probe PBODY 173 0 "east") 10)
   (make-probe PBODY 173 0 "east"))
  )

;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> Boolean
;; STRATEGY: Combining template for Probe on p
;; GIVEN: a probe
;; ANSWERS: whether the probe is facing in the specified direction.
(define (probe-north? p)
  (string=? (probe-dir p) "north"))

(define (probe-south? p)
  (string=? (probe-dir p) "south"))

(define (probe-east? p)
  (string=? (probe-dir p) "east"))

(define (probe-west? p)
  (string=? (probe-dir p) "west"))