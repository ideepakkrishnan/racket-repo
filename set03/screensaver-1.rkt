;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)
(require rackunit)
(require "extras.rkt")

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1
 world-rect2
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 )

(check-location "03" "screensaver-1.rkt")

;; CONSTANTS
(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 50)
(define RECTANGLE-STYLE "outline")
(define RECTANGLE-COLOR "Blue")
(define RECTANGLE-Y-OFFSET (/ RECTANGLE-HEIGHT 2))
(define RECTANGLE-X-OFFSET (/ RECTANGLE-WIDTH 2))

(define TEXT-COLOR "Blue")
(define TEXT-SIZE 11)

(define PAUSE-KEY " ")
(define SCREENSAVER-PAUSED? #false)

(define CANVAS-TOP 0)
(define CANVAS-BOTTOM 300)
(define CANVAS-LEFT 0)
(define CANVAS-RIGHT 400)
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

(define MIN-POS-IN-X-COORD (+ CANVAS-LEFT RECTANGLE-X-OFFSET))
(define MAX-POS-IN-X-COORD (- CANVAS-RIGHT RECTANGLE-X-OFFSET))
(define MIN-POS-IN-Y-COORD (+ CANVAS-TOP RECTANGLE-Y-OFFSET))
(define MAX-POS-IN-Y-COORD (- CANVAS-BOTTOM RECTANGLE-Y-OFFSET))

;;--------------------------------------------------------------
;; DATA DEFINITIONS

(define-struct rect (x y vx vy))
;; A Rect is a (make-rect NonNegInt NonNegInt Integer Integer)
;; INTERPRETATION:
;; x: x coordinate position of the rectangle in pixels
;; y: y coordinate position of the rectangle in pixels
;; vx: Velocity with which the rectangle moves in the x
;;        coordinate in pixels/tick
;; vy: Velocity with which the rectangle moves in the y
;;        coordinate in pixels/tick
;; TEMPLATE:
;; rect-fn : Rect -> ?
#; (define (rect-fn r)
     (...
      (rect-x r)
      (rect-y r)
      (rect-vx r)
      (rect-vy r)
      )
     )

;; rect-x : Rectangle -> NonNegInt
;; rect-y : Rectangle -> NonNegInt
;; rect-vx : Rectangle -> Int
;; rect-vy : Rectangle -> Int
;; GIVEN: Rectangle
;; RETURNS: the coordinates of the center of the rectangle and
;; its velocity in the x- and y- directions.
;; The above functions are part of rect struct and are created
;; implicitly when an instance of this struct is created

(define-struct world (rect1 rect2 paused?))
;; A World is a (make-world Rectangle Rectangle Boolean)
;; INTERPRETATION:
;; rect1 : Stores the properties of the first rectangle in the
;;         scene
;; rect2 : Stores the properties of the second rectangle in the
;;         scene
;; paused? : Describes whether or not the screensaver is paused
;;           as a boolean
;; TEMPLATE:
#; (define (world-fn w)
     (...
      (world-rect1 w)
      (world-rect2 w)
      (world-paused? w)
      )
     )

;; world-rect1 : WorldState -> Rectangle
;; world-rect2 : WorldState -> Rectangle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState

;;--------------------------------------------------------------

;; Local variables for testing
(define r1 (make-rect 200 100 -12 20))
(define r2 (make-rect 100 25 -12 20))
(define r3 (make-rect 30 25 -12 20))
(define r4 (make-rect 88 15 -12 20))
(define r5 (make-rect 112 30 23 -14))
(define r6 (make-rect 200 275 -12 20))
(define r7 (make-rect 200 275 23 -14))
(define r8 (make-rect 375 295 23 -14))
(define paused-world (make-world r1 r2 #true))
(define unpaused-world (make-world r1 r2 #false))

;;--------------------------------------------------------------

;; MAIN FUNCTION.

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state
;; as specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver speed)
  (big-bang (initial-world 0)
            (on-tick world-after-tick speed)
            (on-draw world->scene)
            (on-key world-after-key-event)))

;;--------------------------------------------------------------

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLE:
;; (initial-world 0) =
;;     (make-world RECT1 RECT2 SCREENSAVER-PAUSED?)
;; STRATEGY: Call a more general function
(define (initial-world val)
  (make-world RECT1 RECT2 SCREENSAVER-PAUSED?))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world 0)
   (make-world RECT1 RECT2 SCREENSAVER-PAUSED?))
  )

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLES:
;; (world-after-tick paused-world) = paused-world
;; (world-after-tick unpaused-world) =
;;     (make-world
;;       (rect-moved-forward (world-rect1 unpaused-world))
;;       (rect-moved-forward (world-rect2 unpaused-world))
;;       (world-paused? unpaused-world))
;; STRATEGY: Using template for World on w
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rect-moved-forward (world-rect1 w))
       (rect-moved-forward (world-rect2 w))
       (world-paused? w)))
  )

;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick paused-world) paused-world)
  (check-equal?
   (world-after-tick unpaused-world)
   (make-world
    (rect-moved-forward (world-rect1 unpaused-world))
    (rect-moved-forward (world-rect2 unpaused-world))
    (world-paused? unpaused-world)))
  )

;; rect-top-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the y coordinate of the rectangle's top edge
;; EXAMPLES:
;; (rect-top-edge r1) = 75
;; (rect-top-edge r4) = 0
;; (rect-top-edge r5) = 5
;; (rect-top-edge r8) = 270
;; STRATEGY: combining simpler functions
(define (rect-top-edge r)
  (- (rect-y r) RECTANGLE-Y-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-top-edge r1) 75)
  (check-equal? (rect-top-edge r4) -10)
  (check-equal? (rect-top-edge r5) 5)
  (check-equal? (rect-top-edge r8) 270)
  )

;; rect-top-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the y coordinate of the rectangle's top edge
;; EXAMPLES:
;; (rect-bottom-edge r1) = 125
;; (rect-bottom-edge r6) = 300
;; (rect-bottom-edge r8) = 320
;; STRATEGY: combining simpler functions
(define (rect-bottom-edge r)
  (+ (rect-y r) RECTANGLE-Y-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-bottom-edge r1) 125)
  (check-equal? (rect-bottom-edge r6) 300)
  (check-equal? (rect-bottom-edge r8) 320)
  )

;; rect-left-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate marking the left edge of the rectangle
;; EXAMPLES:
;; (rect-left-edge r1) = 170
;; (rect-left-edge r3) = 0
;; (rect-left-edge r8) = 345
;; STRATEGY: combining simpler functions
(define (rect-left-edge r)
  (- (rect-x r) RECTANGLE-X-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-left-edge r1) 170)
  (check-equal? (rect-left-edge r3) 0)
  (check-equal? (rect-left-edge r8) 345)
  )

;; rect-right-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate marking the right edge of the rectangle
;; EXAMPLES:
;; (rect-right-edge r1) = 230
;; (rect-right-edge r3) = 60
;; (rect-right-edge r8) = 405
;; STRATEGY: combining simpler functions
(define (rect-right-edge r)
  (+ (rect-x r) RECTANGLE-X-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-right-edge r1) 230)
  (check-equal? (rect-right-edge r3) 60)
  (check-equal? (rect-right-edge r8) 405)
  )

;; rect-hits-top-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          top wall of the canvas when it is moved by vy pixels
;;          in the y direction
;; EXAMPLES:
;; (rect-hits-top-wall? r1) = #false
;; (rect-hits-top-wall? r2) = #false
;; (rect-hits-top-wall? r3) = #false
;; (rect-hits-top-wall? r4) = #false
;; (rect-hits-top-wall? r5) = #true
;; (rect-hits-top-wall? r6) = #false
;; (rect-hits-top-wall? r7) = #false
;; STRATEGY: combining simpler functions
(define (rect-hits-top-wall? r)
  (<= (+ (rect-top-edge r) (rect-vy r)) CANVAS-TOP))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-top-wall? r1) #false)
  (check-equal? (rect-hits-top-wall? r2) #false)
  (check-equal? (rect-hits-top-wall? r3) #false)
  (check-equal? (rect-hits-top-wall? r4) #false)
  (check-equal? (rect-hits-top-wall? r5) #true)
  (check-equal? (rect-hits-top-wall? r6) #false)
  (check-equal? (rect-hits-top-wall? r7) #false))

;; rect-hits-bottom-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          bottom wall of the canvas when it is moved by vy
;;          pixels in the y direction
;; EXAMPLES:
;; (rect-hits-bottom-wall? r1) = #false
;; (rect-hits-bottom-wall? r2) = #false
;; (rect-hits-bottom-wall? r3) = #false
;; (rect-hits-bottom-wall? r4) = #false
;; (rect-hits-bottom-wall? r5) = #false
;; (rect-hits-bottom-wall? r6) = #true
;; (rect-hits-bottom-wall? r7) = #false
;; STRATEGY: combining simpler functions
(define (rect-hits-bottom-wall? r)
  (>= (+ (rect-bottom-edge r) (rect-vy r)) CANVAS-BOTTOM))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-bottom-wall? r1) #false)
  (check-equal? (rect-hits-bottom-wall? r2) #false)
  (check-equal? (rect-hits-bottom-wall? r3) #false)
  (check-equal? (rect-hits-bottom-wall? r4) #false)
  (check-equal? (rect-hits-bottom-wall? r5) #false)
  (check-equal? (rect-hits-bottom-wall? r6) #true)
  (check-equal? (rect-hits-bottom-wall? r7) #false))

;; rect-hits-left-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          left wall of the canvas when it is moved by vx
;;          pixels in the x direction
;; EXAMPLES:
;; (rect-hits-left-wall? r1) = #false
;; (rect-hits-left-wall? r2) = #false
;; (rect-hits-left-wall? r3) = #true
;; (rect-hits-left-wall? r4) = #false
;; (rect-hits-left-wall? r5) = #false
;; (rect-hits-left-wall? r6) = #false
;; (rect-hits-left-wall? r7) = #false
;; STRATEGY: combining simpler functions
(define (rect-hits-left-wall? r)
  (<= (+ (rect-left-edge r) (rect-vx r)) CANVAS-LEFT))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-left-wall? r1) #false)
  (check-equal? (rect-hits-left-wall? r2) #false)
  (check-equal? (rect-hits-left-wall? r3) #true)
  (check-equal? (rect-hits-left-wall? r4) #false)
  (check-equal? (rect-hits-left-wall? r5) #false)
  (check-equal? (rect-hits-left-wall? r6) #false)
  (check-equal? (rect-hits-left-wall? r7) #false))

;; rect-hits-right-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          right wall of the canvas when it is moved by vx
;;          pixels in the x direction
;; EXAMPLES:
;; (rect-hits-right-wall? r1) = #false
;; (rect-hits-right-wall? r2) = #false
;; (rect-hits-right-wall? r3) = #false
;; (rect-hits-right-wall? r4) = #false
;; (rect-hits-right-wall? r5) = #false
;; (rect-hits-right-wall? r6) = #true
;; (rect-hits-right-wall? r7) = #false
;; STRATEGY: combining simpler functions
(define (rect-hits-right-wall? r)
  (>= (+ (rect-right-edge r) (rect-vx r)) CANVAS-RIGHT))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-right-wall? r1) #false)
  (check-equal? (rect-hits-right-wall? r2) #false)
  (check-equal? (rect-hits-right-wall? r3) #false)
  (check-equal? (rect-hits-right-wall? r4) #false)
  (check-equal? (rect-hits-right-wall? r5) #false)
  (check-equal? (rect-hits-right-wall? r6) #false)
  (check-equal? (rect-hits-right-wall? r7) #false)
  (check-equal? (rect-hits-right-wall? r8) #true))

;; displace-x-coord : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate moved by vx pixels in the direction
;;          the rectangle is moving. The direction changes if
;;          the next step causes the rectangle to move out of
;;          the canvas
;; EXAMPLES:
;; (displace-x-coord r1) = 188
;; (displace-x-coord r2) = 88
;; (displace-x-coord r3) = 30
;; (displace-x-coord r4) = 76
;; (displace-x-coord r5) = 135
;; (displace-x-coord r6) = 188
;; (displace-x-coord r7) = 223
;; (displace-x-coord r8) = 370
;; STRATEGY: Combining simpler functions
(define (displace-x-coord r)
  (cond
    [(rect-hits-left-wall? r) MIN-POS-IN-X-COORD]
    [(rect-hits-right-wall? r) MAX-POS-IN-X-COORD]
    [else (+ (rect-x r) (rect-vx r))])
  )

;; TESTS:
(begin-for-test
  (check-equal? (displace-x-coord r1) 188)
  (check-equal? (displace-x-coord r2) 88)
  (check-equal? (displace-x-coord r3) 30)
  (check-equal? (displace-x-coord r4) 76)
  (check-equal? (displace-x-coord r5) 135)
  (check-equal? (displace-x-coord r6) 188)
  (check-equal? (displace-x-coord r7) 223)
  (check-equal? (displace-x-coord r8) 370))

;; displace-y-coord : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the y coordinate moved by vy in the direction
;;          the rectangle is moving. The direction changes
;;          if the next step causes the rectangle to move
;;          out of the canvas
;; EXAMPLES:
;; (displace-y-coord r1) = 120
;; (displace-y-coord r2) = 45
;; (displace-y-coord r3) = 45
;; (displace-y-coord r4) = 35
;; (displace-y-coord r5) = 25
;; (displace-y-coord r6) = 275
;; (displace-y-coord r7) = 261
;; (displace-y-coord r8) = 275
;; STRATEGY: Divide into cases on r
(define (displace-y-coord r)
  (cond
    [(rect-hits-top-wall? r) MIN-POS-IN-Y-COORD]
    [(rect-hits-bottom-wall? r) MAX-POS-IN-Y-COORD]
    [else (+ (rect-y r) (rect-vy r))])
  )

;; TESTS:
(begin-for-test
  (check-equal? (displace-y-coord r1) 120)
  (check-equal? (displace-y-coord r2) 45)
  (check-equal? (displace-y-coord r3) 45)
  (check-equal? (displace-y-coord r4) 35)
  (check-equal? (displace-y-coord r5) 25)
  (check-equal? (displace-y-coord r6) 275)
  (check-equal? (displace-y-coord r7) 261)
  (check-equal? (displace-y-coord r8) 275))

;; toggle-direction : Integer -> Integer
;; GIVEN: a rectangle
;; RETURNS: the velocity with which the rectangle should
;;          move after hitting the wall. This is calculated
;;          by flipping the negative sign on current velocity
;; EXAMPLES:
;; (toggle-direction (rect-vx r1)) = 12
;; (toggle-direction (rect-vx r5)) = -23
;; (toggle-direction (rect-vy r1)) = -20
;; (toggle-direction (rect-vy r5)) = 14
;; STRATEGY: Combine simpler functions
(define (toggle-direction vel)
  (- 0 vel))

;; TESTS:
(begin-for-test
  (check-equal? (toggle-direction (rect-vx r1)) 12)
  (check-equal? (toggle-direction (rect-vx r5)) -23)
  (check-equal? (toggle-direction (rect-vy r1)) -20)
  (check-equal? (toggle-direction (rect-vy r5)) 14)
  )

;; updated-vx : Rectangle -> Integer
;; GIVEN: a rectangle
;; RETURNS: the velocity at which the rectangle moves in the
;;          x coordinate
;; EXAMPLES:
;; (updated-vx r1) = -12
;; (updated-vx r2) = -12
;; (updated-vx r3) = 12
;; (updated-vx r4) = -12
;; (updated-vx r5) = 23
;; (updated-vx r6) = -12
;; (updated-vx r7) = 23
;; (updated-vx r8) = -23
;; STRATEGY: Using cases on r
(define (updated-vx r)
  (if (or (rect-hits-left-wall? r)
           (rect-hits-right-wall? r))
      (toggle-direction (rect-vx r))
      (rect-vx r))
  )

;; TESTS:
(begin-for-test
  (check-equal? (updated-vx r1) -12)
  (check-equal? (updated-vx r2) -12)
  (check-equal? (updated-vx r3) 12)
  (check-equal? (updated-vx r4) -12)
  (check-equal? (updated-vx r5) 23)
  (check-equal? (updated-vx r6) -12)
  (check-equal? (updated-vx r7) 23)
  (check-equal? (updated-vx r8) -23))

;; updated-vy : Rectangle -> Integer
;; GIVEN: a rectangle
;; RETURNS: the velocity at which the rectangle moves in the
;;          y coordinate
;; EXAMPLES:
;; (updated-vy r1) = 20
;; (updated-vy r2) = 20
;; (updated-vy r3) = 20
;; (updated-vy r4) = 20
;; (updated-vy r5) = 14
;; (updated-vy r6) = -20
;; (updated-vy r7) = -14
;; (updated-vy r8) = 14
;; STRATEGY: Using cases on r
(define (updated-vy r)
  (if (or (rect-hits-top-wall? r)
           (rect-hits-bottom-wall? r))
      (toggle-direction (rect-vy r))
      (rect-vy r))
  )

;; TESTS:
(begin-for-test
  (check-equal? (updated-vy r1) 20)
  (check-equal? (updated-vy r2) 20)
  (check-equal? (updated-vy r3) 20)
  (check-equal? (updated-vy r4) 20)
  (check-equal? (updated-vy r5) 14)
  (check-equal? (updated-vy r6) -20)
  (check-equal? (updated-vy r7) -14)
  (check-equal? (updated-vy r8) 14))

;; rect-moved-forward : Rectangle -> Rectangle
;; GIVEN: a rectangle
;; RETURNS: the rectangle with updated x and y coordinates and
;;          x and y coordinate velocities
;; EXAMPLES:
;; (rect-moved-forward r1) = (make-rect 188 120 -12 20)
;; (rect-moved-forward r2) = (make-rect 88 45 -12 20)
;; (rect-moved-forward r3) = (make-rect 30 45 12 20)
;; (rect-moved-forward r4) = (make-rect 76 35 -12 20)
;; (rect-moved-forward r5) = (make-rect 135 25 23 14)
;; (rect-moved-forward r6) = (make-rect 188 275 -12 -20)
;; (rect-moved-forward r7) = (make-rect 223 261 23 -14)
;; (rect-moved-forward r8) = (make-rect 370 275 -23 14)
;; STRATEGY: Call a more general function
(define (rect-moved-forward r)
  (make-rect
   (displace-x-coord r)
   (displace-y-coord r)
   (updated-vx r)
   (updated-vy r))
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-moved-forward r1) (make-rect 188 120 -12 20))
  (check-equal?
   (rect-moved-forward r2) (make-rect 88 45 -12 20))
  (check-equal?
   (rect-moved-forward r3) (make-rect 30 45 12 20))
  (check-equal?
   (rect-moved-forward r4) (make-rect 76 35 -12 20))
  (check-equal?
   (rect-moved-forward r5) (make-rect 135 25 23 14))
  (check-equal?
   (rect-moved-forward r6) (make-rect 188 275 -12 -20))
  (check-equal?
   (rect-moved-forward r7) (make-rect 223 261 23 -14))
  (check-equal?
   (rect-moved-forward r8) (make-rect 370 275 -23 14))
  )

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state and key event
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; (world-after-key-event paused-world PAUSE-KEY) = unpaused-world
;; (world-after-key-event unpaused-world PAUSE-KEY) = paused-world
;; (world-after-key-event paused-world "\n") = paused-world
;; (world-after-key-event unpaused-world "\r") = unpaused-world
;; STRATEGY: using cases on kev
(define (world-after-key-event w kev)
  (if (key=? kev PAUSE-KEY)
      (world-with-paused-toggled w)
      w)
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event paused-world PAUSE-KEY)
   unpaused-world)
  (check-equal?
   (world-after-key-event unpaused-world PAUSE-KEY)
   paused-world)
  (check-equal?
   (world-after-key-event paused-world "\n")
   paused-world)
  (check-equal?
   (world-after-key-event unpaused-world "\r")
   unpaused-world)
  )

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLES:
;; (world-with-paused-toggled paused-world) = unpaused-world
;; (world-with-paused-toggled unpaused-world) = paused-world
;; STRATEGY: Use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w)))
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (world-with-paused-toggled paused-world)
   unpaused-world)
  (check-equal?
   (world-with-paused-toggled unpaused-world)
   paused-world))

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLES:
;; (new-rectangle 200 100 -12 20) = r1
;; (new-rectangle 112 30 23 -14) = r5
;; (new-rectangle 375 295 23 -14) = r8
;; STRATEGY: call a more general function
(define (new-rectangle x-pos y-pos x-vel y-vel)
  (make-rect x-pos y-pos x-vel y-vel))

;; TESTS:
(begin-for-test
  (check-equal? (new-rectangle 200 100 -12 20) r1)
  (check-equal? (new-rectangle 112 30 23 -14) r5)
  (check-equal? (new-rectangle 375 295 23 -14) r8))

;; Variables storing the properties of the two rectangles
;; rendered within the screensaver
(define RECT1 (new-rectangle 200 100 -12 20))
(define RECT2 (new-rectangle 200 200 23 -14))

;; pos-as-text : Rectangle -> String
;; GIVEN: a rectangle
;; RETURNS: a string which renders the current velocity of the
;;          rectangle in the format -> (vx,vy) where vx is the
;;          velocity in x direction and vy is the velocity in y
;;          direction
;; EXAMPLES:
;; (pos-as-text r1) = "(-12,20)"
;; (pos-as-text r5) = "(23,-14)"
;; (pos-as-text r8) = "(23,-14)"
;; STRATEGY: Use template for Rect on r
(define (pos-as-text r)
  (string-append
   "("
   (number->string (rect-vx r))
   ","
   (number->string (rect-vy r))
   ")")
  )

;; render-rect : Rectangle -> Image
;; GIVEN: a rectangle
;; RETURNS: an image which renders the rectangle based on the
;; dimensions specified, overlaying the velocities with which it
;; is moving in the x and y directions of the canvas
;; EXAMPLES:
;; (render-rect r1) =
;;    (overlay
;;      (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;                 RECTANGLE-STYLE RECTANGLE-COLOR)
;;      (text "(-12,20)" TEXT-SIZE TEXT-COLOR))
;; (render-rect r7) =
;;    (overlay
;;      (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;                 RECTANGLE-STYLE RECTANGLE-COLOR)
;;      (text "23,-14" TEXT-SIZE TEXT-COLOR))
;; STRATEGY: Combine simpler functions
(define (render-rect r)
  (overlay   
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
              RECTANGLE-STYLE RECTANGLE-COLOR)
   (text (pos-as-text r) TEXT-SIZE TEXT-COLOR))
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (render-rect r1)
   (overlay
    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
               RECTANGLE-STYLE RECTANGLE-COLOR)
    (text "(-12,20)" TEXT-SIZE TEXT-COLOR)))
  (check-equal?
   (render-rect r7)
   (overlay
    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
               RECTANGLE-STYLE RECTANGLE-COLOR)
    (text "(23,-14)" TEXT-SIZE TEXT-COLOR)))
  )

;; render-posn : Rectangle -> Posn
;; GIVEN: a rectangle
;; RETURNS: the coordinates of the rectangle converted into a
;; Posn object
;; EXAMPLES:
;; (render-posn r1) = (make-posn 200 100)
;; (render-posn r6) = (make-posn 200 275)
;; STRATEGY: Use template for Rect on r
(define (render-posn r)
  (make-posn (rect-x r) (rect-y r)))

;; TESTS:
(begin-for-test
  (check-equal? (render-posn r1) (make-posn 200 100))
  (check-equal? (render-posn r6) (make-posn 200 275))
  )

;; world->scene : World -> Scene
;; GIVEN: a world
;; RETURNS: a scene that portrays the current world
;; EXAMPLES:
;; (world->scene unpaused-world) =
;;    (place-images
;;      (list (render-rect (world-rect1 UNPAUSED-WORLD))
;;            (render-rect (world-rect2 UNPAUSED-WORLD)))
;;      (list (render-posn (world-rect1 UNPAUSED-WORLD))
;;            (render-posn (world-rect2 UNPAUSED-WORLD)))
;;      EMPTY-CANVAS)
;; STRATEGY: Use template of World on w
(define (world->scene w)
  (place-images
   (list (render-rect (world-rect1 w))
         (render-rect (world-rect2 w)))
   (list (render-posn (world-rect1 w))
         (render-posn (world-rect2 w)))
   EMPTY-CANVAS)
  )

;; TESTS:
(begin-for-test
  (check-equal?
   (world->scene unpaused-world)
   (place-images
    (list (render-rect (world-rect1 unpaused-world))
          (render-rect (world-rect2 unpaused-world)))
    (list (render-posn (world-rect1 unpaused-world))
          (render-posn (world-rect2 unpaused-world)))
    EMPTY-CANVAS))
  )