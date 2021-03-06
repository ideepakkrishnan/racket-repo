;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname screensaver-5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
 world-rects
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 world-after-mouse-event
 rect-after-mouse-event
 rect-after-key-event
 rect-selected?
 rect-pen-down?
 )

(check-location "05" "screensaver-5.rkt")

;; CONSTANTS
(define INITIAL-RECT-LIST empty)
(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 50)
(define RECTANGLE-STYLE "outline")
(define RECTANGLE-Y-OFFSET (/ RECTANGLE-HEIGHT 2))
(define RECTANGLE-X-OFFSET (/ RECTANGLE-WIDTH 2))

(define MOUSE-CLICK-POS-MARKER (circle 5 "outline" "Red"))
(define MOUSE-INITIALIZE-X 0)
(define MOUSE-INITIALIZE-Y 0)
(define DEFAULT-DX 0)
(define DEFAULT-DY 0)

(define TEXT-SIZE 11)

(define SELECTED-ITEM-COLOR "Red")
(define UNSELECTED-ITEM-COLOR "Blue")

(define PAUSE-KEY " ")
(define NEW-RECTANGLE-KEY "n")
(define DECREASE-Y-VEL "up")
(define INCREASE-Y-VEL "down")
(define INCREASE-X-VEL "right")
(define DECREASE-X-VEL "left")
(define DROP-PEN-KEY "d")
(define LIFT-PEN-KEY "u")
(define SCREENSAVER-PAUSED? #true)
(define PEN-TIP (circle 1 "solid" "Black"))
(define INIT-PEN-DOWN? #false)
(define PEN-DOWN #true)
(define PEN-UP #false)

(define X-VEL-INCREASE-STEP 2)
(define X-VEL-DECREASE-STEP 2)
(define Y-VEL-INCREASE-STEP 2)
(define Y-VEL-DECREASE-STEP 2)

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

(define RECTANGLE-INIT-X (/ CANVAS-RIGHT 2))
(define RECTANGLE-INIT-Y (/ CANVAS-BOTTOM 2))
(define RECTANGLE-INIT-VX 0)
(define RECTANGLE-INIT-VY 0)
(define RECTANGLE-INIT-MDX 0)
(define RECTANGLE-INIT-MDY 0)
(define RECTANGLE-INIT-SELECTION #false)
(define RECTANGLE-INIT-PEN-DOWN #false)
(define RECTANGLE-INIT-POS-LST empty)

;;--------------------------------------------------------------
;; DATA DEFINITIONS

(define-struct rect (x y vx vy mdx mdy selected? pen-down? pos-lst))
;; A Rect is a (make-rect NonNegInt NonNegInt Integer Integer
;; Integer Integer Boolean Booleam ListOfPosn)
;; INTERPRETATION:
;; x: x coordinate position of the rectangle in pixels
;; y: y coordinate position of the rectangle in pixels
;; vx: Velocity with which the rectangle moves in the x
;;        coordinate in pixels/tick
;; vy: Velocity with which the rectangle moves in the y
;;        coordinate in pixels/tick
;; mdx: Distance between the x coordinate of the rectangle and
;;      the x coordinate of the mouse event in pixels
;; mdy: Distance between the y coordinate of the rectangle and
;;      the y coordinate of the mouse event in pixels
;; selected?: whether the rectangle is selected or not
;; pen-down?: whether the pen is currently on the canvas or lifted up
;; pos-list: a list of Posns which the rectangle has traversed after
;;           the pen touched the canvas
;; TEMPLATE:
;; rect-fn : Rect -> ??
#; (define (rect-fn r)
     (...
      (rect-x r)
      (rect-y r)
      (rect-vx r)
      (rect-vy r)
      (rect-mdx r)
      (rect-mdy r)
      (rect-selected? r)
      (rect-pen-down? r)
      (rect-pos-lst r)))

;; rect-x : Rectangle -> NonNegInt
;; rect-y : Rectangle -> NonNegInt
;; rect-vx : Rectangle -> Int
;; rect-vy : Rectangle -> Int
;; rect-mdx : Rectangle -> Int
;; rect-mdy : Rectangle -> Int
;; rect-selected? : Rectangle -> Boolean
;; rect-pen-down? : Rectangle -> Boolean
;; rect-pos-list : Rectangle -> ListOfPosn
;; GIVEN: Rectangle
;; RETURNS: the specified attribute of the rectangle
;; The above functions are part of rect struct and are created
;; implicitly when an instance of this struct is created

(define-struct world (rects mx my paused?))
;; A World is a (make-world ListOfRect NonNegInt NonNegInt Boolean)
;; INTERPRETATION:
;; rects : Stores a list of rectangles and their properties
;;         that are to be rendered on the scene
;; mx : x coordinate position of the mouse event
;; my : y coordinate position of the mouse event
;; paused? : whether the screensaver is paused or not
;; TEMPLATE:
;; world-fn : World -> ??
#; (define (world-fn w)
     (...
      (world-rects w)
      (world-mx w)
      (world-my w)
      (world-paused? w)))

;; world-rects : WorldState -> ListOfRectangle
;; world-mx : WorldState -> NonNegInt
;; world-my : WorldState -> NonNegInt
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState

;; A ListOfRect is
;; -- empty
;; -- (cons Rect ListOfRect)
;; TEMPLATE:
;; lor-fn : ListOfRect -> ??
;; (define (lor-fn lor)
;;  (cond
;;   [(empty? lor) ...]
;;   [else (... (first lor)
;;              (lor-fn (rest lor)))]))

;; A ListOfPosn is
;; -- empty
;; -- (cons Posn ListOfPosn)
;; TEMPLATE:
;; lop-fn : ListOfPosn -> ??
;; (define (lop-fn lop)
;;  (cond
;;   [(empty? lop) ...]
;;   [else (... (first lop)
;;              (lop-fn (rest lop)))]))

;; A ListOfImage is
;; -- empty
;; -- (cons Image ListOfImage)
;; TEMPLATE:
;; loi-fn : ListOfImage -> ??
;; (define (loi-fn loi)
;;  (cond
;;   [(empty? loi) ...]
;;   [else (... (first loi)
;;              (loi-fn (rest loi)))]))

;;--------------------------------------------------------------

;; Constants to store the default objects

(define DEFAULT-RECTANGLE (make-rect RECTANGLE-INIT-X
                                     RECTANGLE-INIT-Y
                                     RECTANGLE-INIT-VX 
                                     RECTANGLE-INIT-VY
                                     RECTANGLE-INIT-MDX
                                     RECTANGLE-INIT-MDY
                                     RECTANGLE-INIT-SELECTION
                                     RECTANGLE-INIT-PEN-DOWN
                                     RECTANGLE-INIT-POS-LST))

(define INITIAL-WORLD (make-world INITIAL-RECT-LIST
                                  MOUSE-INITIALIZE-X
                                  MOUSE-INITIALIZE-Y
                                  SCREENSAVER-PAUSED?))

;;--------------------------------------------------------------

;; Local variables for testing
(define r1 (make-rect 200 100 -12 20 0 0 #false #true empty))
(define r2 (make-rect 100 25 -12 20 0 0 #false #false empty))
(define r3 (make-rect 30 25 -12 20 0 0 #false #false empty))
(define r4 (make-rect 88 15 -12 20 0 0 #false #false empty))
(define r5 (make-rect 112 30 23 -14 0 0 #true #false empty))
(define r6 (make-rect 200 275 -12 20 0 0 #false #false empty))
(define r7 (make-rect 200 275 23 -14 0 0 #false #false empty))
(define r8 (make-rect 375 295 23 -14 0 0 #false #false empty))
(define r9 (make-rect 200 150 23 -14 0 0 #true #true empty))
(define r10 (make-rect 20 15 -30 -20 10 5 #true #false empty))
(define paused-world (make-world (list r1 r2) 0 0 #true))
(define unpaused-world (make-world (list r1 r2) 0 0 #false))
(define selected-world (make-world (list r1 r5) 0 0 #false))
(define selected-world-inc-in-x
  (make-world (list r1 (make-rect 112 30 25 -14 0 0 #true
                                  #false empty))
                    0 0 #false))
(define selected-world-dec-in-x
  (make-world (list r1 (make-rect 112 30 21 -14 0 0 #true
                                  #false empty))
                    0 0 #false))
(define selected-world-inc-in-y
  (make-world (list r1 (make-rect 112 30 23 -12 0 0 #true
                                  #false empty))
                    0 0 #false))
(define selected-world-dec-in-y
  (make-world (list r1 (make-rect 112 30 23 -16 0 0 #true
                                  #false empty))
                    0 0 #false))
(define selected-world-pen-up
  (make-world (list r5 r2) 0 0 #false))
(define selected-world-pen-down
  (make-world (list (make-rect 112 30 23 -14 0 0
                               #true #true empty)
                    r2) 0 0 #false))
(define selected-rect-with-pen-down
  (make-rect 112 30 23 -14 0 0 #true #true
             (list (make-posn 112 30) (make-posn 89 44))))
(define selected-rect-with-pen-up
  (make-rect 112 30 23 -14 0 0 #true #false
             (list (make-posn 112 30) (make-posn 89 44))))
(define unselected-rect-with-pen-down
  (make-rect 200 150 23 -14 0 0 #false #true
             (list (make-posn 200 150) (make-posn 177 164))))
(define world-with-pen-down
  (make-world (list selected-rect-with-pen-down
                    unselected-rect-with-pen-down)
              0 0 #false))
(define world-with-pen-up
  (make-world (list selected-rect-with-pen-up
                    unselected-rect-with-pen-down)
              0 0 #false))

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
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

;;--------------------------------------------------------------

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLE:
;; (initial-world 0) = INITIAL-WORLD
;; STRATEGY: Combining simpler functions
(define (initial-world val)
  INITIAL-WORLD)

;; TESTS:
(begin-for-test
  (check-equal? (initial-world 0) INITIAL-WORLD
                "Create initial world"))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: a world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLES:
;; (world-after-tick paused-world) = paused-world
;; (world-after-tick unpaused-world) =
;;     (make-world
;;       (rects-moved-forward (world-rects unpaused-world))
;;       0 0
;;       (world-paused? unpaused-world))
;; STRATEGY: Cases on if the world is paused or not
(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rects-moved-forward (world-rects w))       
       (world-mx w) (world-my w)
       (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal? (world-after-tick paused-world) paused-world
                "Paused world after tick")
  (check-equal?
   (world-after-tick unpaused-world)
   (make-world
    (rects-moved-forward (world-rects unpaused-world))
    MOUSE-INITIALIZE-X MOUSE-INITIALIZE-Y
    (world-paused? unpaused-world))
   "Unpaused world after tick"))

;; rect-top-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the y coordinate of the rectangle's top edge
;; EXAMPLES:
;; (rect-top-edge r1) = 75
;; (rect-top-edge r4) = 0
;; (rect-top-edge r5) = 5
;; (rect-top-edge r8) = 270
;; STRATEGY: Using template for Rect on r
(define (rect-top-edge r)
  (- (rect-y r) RECTANGLE-Y-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-top-edge r1) 75
                "Top edge of rectangle inside scene")
  (check-equal? (rect-top-edge r4) -10
                "Top edge of rectangle extending outside top edge")
  (check-equal? (rect-top-edge r5) 5
                "Top edge of rectangle near the top edge")
  (check-equal? (rect-top-edge r8) 270
                "Top edge of rectangle near bottom edge"))

;; rect-top-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the y coordinate of the rectangle's top edge
;; EXAMPLES:
;; (rect-bottom-edge r1) = 125
;; (rect-bottom-edge r6) = 300
;; (rect-bottom-edge r8) = 320
;; STRATEGY: Using template for Rect on r
(define (rect-bottom-edge r)
  (+ (rect-y r) RECTANGLE-Y-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-bottom-edge r1) 125
                "Bottom edge for rectangle inside scene")
  (check-equal? (rect-bottom-edge r6) 300
                "Bottom edge for rectangle touching bottom edge")
  (check-equal? (rect-bottom-edge r8) 320
                "Bottom edge for rectanlge extending outside"))

;; rect-left-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate marking the left edge of the rectangle
;; EXAMPLES:
;; (rect-left-edge r1) = 170
;; (rect-left-edge r3) = 0
;; (rect-left-edge r8) = 345
;; STRATEGY: Using template for Rect on r
(define (rect-left-edge r)
  (- (rect-x r) RECTANGLE-X-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-left-edge r1) 170
                "Left edge for rectangle inside the scene")
  (check-equal? (rect-left-edge r3) 0
                "Left edge for rectangle touching left edge")
  (check-equal? (rect-left-edge r8) 345
                "Left edge for rectangle near scene's right edge"))

;; rect-right-edge : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate marking the right edge of the rectangle
;; EXAMPLES:
;; (rect-right-edge r1) = 230
;; (rect-right-edge r3) = 60
;; (rect-right-edge r8) = 405
;; STRATEGY: Using template for Rect on r
(define (rect-right-edge r)
  (+ (rect-x r) RECTANGLE-X-OFFSET))

;; TESTS:
(begin-for-test
  (check-equal? (rect-right-edge r1) 230
                "Right edge for rectangle inside the scene")
  (check-equal? (rect-right-edge r3) 60
                "Right edge for rectangle near left edge")
  (check-equal?
   (rect-right-edge r8) 405
   "Right edge for rectangle extending outside scene's right edge"))

;; rect-hits-top-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          top wall of the canvas when it is moved by vy pixels
;;          in the y direction
;; EXAMPLES:
;; (rect-hits-top-wall? r1) = #false
;; (rect-hits-top-wall? r5) = #true
;; STRATEGY: Using template for Rect on r
(define (rect-hits-top-wall? r)
  (<= (+ (rect-top-edge r) (rect-vy r)) CANVAS-TOP))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-top-wall? r1) #false
                "Rectangle inside scene hits top wall?")
  (check-equal? (rect-hits-top-wall? r5) #true
                "Rectangle touching the top edge hits top wall?"))

;; rect-hits-bottom-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          bottom wall of the canvas when it is moved by vy
;;          pixels in the y direction
;; EXAMPLES:
;; (rect-hits-bottom-wall? r1) = #false
;; (rect-hits-bottom-wall? r6) = #true
;; STRATEGY: Using template for Rect on r
(define (rect-hits-bottom-wall? r)
  (>= (+ (rect-bottom-edge r) (rect-vy r)) CANVAS-BOTTOM))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-bottom-wall? r1) #false
                "Rectangle inside the scene hits bottom wall?")
  (check-equal?
   (rect-hits-bottom-wall? r6) #true
   "Rectangle extending outside the scene hits bottom wall?"))

;; rect-hits-left-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          left wall of the canvas when it is moved by vx
;;          pixels in the x direction
;; EXAMPLES:
;; (rect-hits-left-wall? r1) = #false
;; (rect-hits-left-wall? r3) = #true
;; STRATEGY: Using template for Rect on r
(define (rect-hits-left-wall? r)
  (<= (+ (rect-left-edge r) (rect-vx r)) CANVAS-LEFT))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-left-wall? r1) #false
                "Rectangle inside the scene hits left wall?")
  (check-equal?
   (rect-hits-left-wall? r3) #true
   "Rectangle extending outside scene's left edge hits left wall?"))

;; rect-hits-right-wall? : Rectangle -> Boolean
;; GIVEN: a rectangle
;; RETURNS: whether or not an edge of a rectangle touches the
;;          right wall of the canvas when it is moved by vx
;;          pixels in the x direction
;; EXAMPLES:
;; (rect-hits-right-wall? r1) = #false
;; (rect-hits-right-wall? r8) = #true
;; STRATEGY: Using template for Rect on r
(define (rect-hits-right-wall? r)
  (>= (+ (rect-right-edge r) (rect-vx r)) CANVAS-RIGHT))

;; TESTS:
(begin-for-test
  (check-equal? (rect-hits-right-wall? r1) #false
                "Rectangle inside the scene hits right wall?")
  (check-equal?
   (rect-hits-right-wall? r8) #true
   "Rectangle extending outside right edge hits right wall?"))

;; displace-x-coord : Rectangle -> NonNegInt
;; GIVEN: a rectangle
;; RETURNS: the x coordinate moved by vx pixels in the direction
;;          the rectangle is moving. If the next step causes the
;;          the rectangle to move out of the canvas, it shortens
;;          its step so that it doesn't go out of the canvas
;; EXAMPLES:
;; (displace-x-coord r1) = 188
;; (displace-x-coord r2) = 88
;; (displace-x-coord r3) = 30
;; (displace-x-coord r4) = 76
;; (displace-x-coord r5) = 135
;; (displace-x-coord r6) = 188
;; (displace-x-coord r7) = 223
;; (displace-x-coord r8) = 370
;; STRATEGY: Divide into cases on r and using template for Rect on r
(define (displace-x-coord r)
  (cond
    [(rect-hits-left-wall? r) MIN-POS-IN-X-COORD]
    [(rect-hits-right-wall? r) MAX-POS-IN-X-COORD]
    [else
     ;; Displace only if the rectangle is unselected
     (if (not (rect-selected? r))
         (+ (rect-x r) (rect-vx r))
         (rect-x r))]))

;; TESTS:
(begin-for-test
  (check-equal? (displace-x-coord r1) 188
                "Displace X coordinate for r1")
  (check-equal? (displace-x-coord r2) 88
                "Dispalce X coordinate for r2")
  (check-equal? (displace-x-coord r3) 30
                "Dispalce X coordinate for r3")
  (check-equal? (displace-x-coord r4) 76
                "Displace X coordinate for r4")
  (check-equal? (displace-x-coord r5) 112
                "Displace X coordinate for r5")
  (check-equal? (displace-x-coord r6) 188
                "Displace X coordinate for r6")
  (check-equal? (displace-x-coord r7) 223
                "Displace X coordinate for r7")
  (check-equal? (displace-x-coord r8) 370
                "Displace X coordinate for r8"))

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
;; STRATEGY: Divide into cases on r and using template for
;; Rect on r
(define (displace-y-coord r)
  (cond
    [(rect-hits-top-wall? r) MIN-POS-IN-Y-COORD]
    [(rect-hits-bottom-wall? r) MAX-POS-IN-Y-COORD]
    [else
     (if (not (rect-selected? r))
         (+ (rect-y r) (rect-vy r))
         (rect-y r))]))

;; TESTS:
(begin-for-test
  (check-equal? (displace-y-coord r1) 120
                "Displace y coordinate for r1")
  (check-equal? (displace-y-coord r2) 45
                "Displace y coordinate for r2")
  (check-equal? (displace-y-coord r3) 45
                "Displace y coordinate for r3")
  (check-equal? (displace-y-coord r4) 35
                "Displace y coordinate for r4")
  (check-equal? (displace-y-coord r5) 25
                "Displace y coordinate for r5")
  (check-equal? (displace-y-coord r6) 275
                "Displace y coordinate for r6")
  (check-equal? (displace-y-coord r7) 261
                "Displace y coordinate for r7")
  (check-equal? (displace-y-coord r8) 275
                "Displace y coordinate for r8")
  (check-equal? (displace-y-coord r9) 150
                "Displace y coordinate for r9"))

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
  (check-equal? (toggle-direction (rect-vx r1)) 12
                "Toggle direction for vx on r1")
  (check-equal? (toggle-direction (rect-vx r5)) -23
                "Toggle direction for vx on r5")
  (check-equal? (toggle-direction (rect-vy r1)) -20
                "Toggle direction for vy on r1")
  (check-equal? (toggle-direction (rect-vy r5)) 14
                "Toggle direction for vy on r5"))

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
;; STRATEGY: Cases on if r hits the left or right wall
(define (updated-vx r)
  (if (or (rect-hits-left-wall? r)
           (rect-hits-right-wall? r))
      (toggle-direction (rect-vx r))
      (rect-vx r)))

;; TESTS:
(begin-for-test
  (check-equal? (updated-vx r1) -12
                "Update velocity vx on r1")
  (check-equal? (updated-vx r2) -12
                "Update velocity vx on r2")
  (check-equal? (updated-vx r3) 12
                "Update velocity vx on r3")
  (check-equal? (updated-vx r4) -12
                "Update velocity vx on r4")
  (check-equal? (updated-vx r5) 23
                "Update velocity vx on r5")
  (check-equal? (updated-vx r6) -12
                "Update velocity vx on r6")
  (check-equal? (updated-vx r7) 23
                "Update velocity vx on r7")
  (check-equal? (updated-vx r8) -23
                "Update velocity vx on r8"))

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
;; STRATEGY: Cases on if the r hits the top or bottom wall
(define (updated-vy r)
  (if (or (rect-hits-top-wall? r)
           (rect-hits-bottom-wall? r))
      (toggle-direction (rect-vy r))
      (rect-vy r)))

;; TESTS:
(begin-for-test
  (check-equal? (updated-vy r1) 20
                "Update velocity vy for r1")
  (check-equal? (updated-vy r2) 20
                "Update velocity vy for r2")
  (check-equal? (updated-vy r3) 20
                "Update velocity vy for r3")
  (check-equal? (updated-vy r4) 20
                "Update velocity vy for r4")
  (check-equal? (updated-vy r5) 14
                "Update velocity vy for r5")
  (check-equal? (updated-vy r6) -20
                "Update velocity vy for r6")
  (check-equal? (updated-vy r7) -14
                "Update velocity vy for r7")
  (check-equal? (updated-vy r8) 14
                "Update velocity vy for r8"))

;; rect-moved-forward : Rectangle -> Rectangle
;; GIVEN: a rectangle
;; RETURNS: the rectangle with updated x and y coordinates and
;;          x and y coordinate velocities
;; EXAMPLES:
;; (rect-moved-forward r1) = (make-rect 188 120 -12 20 0 0)
;; (rect-moved-forward r2) = (make-rect 88 45 -12 20 0 0)
;; (rect-moved-forward r3) = (make-rect 30 45 12 20 0 0)
;; (rect-moved-forward r4) = (make-rect 76 35 -12 20 0 0)
;; (rect-moved-forward r5) = (make-rect 135 25 23 14 0 0)
;; (rect-moved-forward r6) = (make-rect 188 275 -12 -20 0 0)
;; (rect-moved-forward r7) = (make-rect 223 261 23 -14 0 0)
;; (rect-moved-forward r8) = (make-rect 370 275 -23 14 0 0)
;; STRATEGY: Cases on if r is selected or not
(define (rect-moved-forward r)
  (if (rect-selected? r)
      r
      (make-rect
       (displace-x-coord r)
       (displace-y-coord r)
       (updated-vx r)
       (updated-vy r)
       (rect-mdx r)
       (rect-mdy r)
       (rect-selected? r)
       (rect-pen-down? r)
       (update-rect-pos-lst r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-moved-forward r1)
   (make-rect 188 120 -12 20 0 0 #false #true
              (list (make-posn 200 100)))
   "Rectangle r1 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r2)
   (make-rect 88 45 -12 20 0 0 #false #false empty)
   "Rectangle r2 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r3)
   (make-rect 30 45 12 20 0 0 #false #false empty)
   "Rectangle r3 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r4)
   (make-rect 76 35 -12 20 0 0 #false #false empty)
   "Rectangle r4 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r5)
   (make-rect 112 30 23 -14 0 0 #true #false empty)
   "Rectangle r5 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r6)
   (make-rect 188 275 -12 -20 0 0 #false #false empty)
   "Rectangle r6 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r7)
   (make-rect 223 261 23 -14 0 0 #false #false empty)
   "Rectangle r7 moved forward in x and y directions")
  (check-equal?
   (rect-moved-forward r8)
   (make-rect 370 275 -23 14 0 0 #false #false empty)
   "Rectangle r8 moved forward in x and y directions"))

;; rects-moved-forward : ListOfRectangle -> ListOfRectangle
;; GIVEN: a list of rectangles
;; RETURNS: the list of rectangles after moving each rectangle to
;;          their new position based on their individual velocities
;; EXAMPLES:
;; (rects-moved-forward (list r1 r2)) =
;;   (list (make-rect 188 120 -12 20 0 0 #false)
;;         (make-rect 88 45 -12 20 0 0 #false))
;; (rects-moved-forward (list r5 r6 r8)) =
;;   (list (make-rect 112 30 23 -14 0 0 #true)
;;         (make-rect 188 275 -12 -20 0 0 #false)
;;         (make-rect 370 275 -23 14 0 0 #false))
;; STRATEGY: Use HOF map on rects
(define (rects-moved-forward rects)
  (map rect-moved-forward rects))

;; TESTS:
(begin-for-test
  (check-equal?
   (rects-moved-forward (list r1 r2))
   (list (make-rect 188 120 -12 20 0 0 #false #true
                    (list (make-posn 200 100)))
         (make-rect 88 45 -12 20 0 0 #false #false empty))
   "Rectangles r1 and r2 moved forward in x and y directions")
  (check-equal?
   (rects-moved-forward (list r5 r6 r8))
   (list (make-rect 112 30 23 -14 0 0 #true #false empty)
         (make-rect 188 275 -12 -20 0 0 #false #false empty)
         (make-rect 370 275 -23 14 0 0 #false #false empty))
   "Rectangles r5 r6 and r8 moved forward in x and y directions"))

;; update-rect-pos-lst : Rectangle -> ListOfPosn
;; GIVEN: a rectangle
;; WHERE: the pen is down, touching the canvas
;; RETURNS: the list of Posn-s through which the rectangle has
;; travelled after the pen started touching the canvas including
;; the current position
;; EXAMPLES:
;; (update-rect-pos-lst r1) =
;;   (list (make-posn (rect-x r1) (rect-y r1)))
;; (update-rect-pos-lst r2) = (rect-pos-lst r2)
;; STRATEGY: Cases on if the pen is down or not
(define (update-rect-pos-lst r)
  (if (rect-pen-down? r)
      (cons
       (make-posn (rect-x r) (rect-y r))
       (rect-pos-lst r))
      (rect-pos-lst r)))

;; TESTS:
(begin-for-test
  (check-equal? (update-rect-pos-lst r1)
                (list (make-posn (rect-x r1) (rect-y r1)))
                "Update posn list for r1")
  (check-equal? (update-rect-pos-lst r2) (rect-pos-lst r2)
                "Update posn list for r2"))

;; add-new-rect : ListOfRect -> ListOfRect
;; GIVEN: a list of rectangles
;; RETURNS: similar list of rectangles with a new rectangle,
;; positioned at the center of the canvas, appended to it
;; EXAMPLES:
;; (add-new-rect INITIAL-RECT-LIST) = (list DEFAULT-RECTANGLE)
;; STRATEGY: Combining simpler functions
(define (add-new-rect rects)
  (cons DEFAULT-RECTANGLE rects))

;; TESTS:
(begin-for-test
  (check-equal? (add-new-rect INITIAL-RECT-LIST)
                (list DEFAULT-RECTANGLE)
                "Adding new rectangle to a list of rectangles"))

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; GIVEN: a world state and key event
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLES:
;; (world-after-key-event paused-world PAUSE-KEY) = unpaused-world
;; (world-after-key-event unpaused-world PAUSE-KEY) = paused-world
;; (world-after-key-event (initial-world "any") NEW-RECTANGLE-KEY)
;;  = new-scene-with-rect
;; (world-after-key-event selected-world INCREASE-X-VEL)
;;  = selected-world-inc-in-x
;; (world-after-key-event selected-world DECREASE-X-VEL)
;;  = selected-world-dec-in-x
;; (world-after-key-event selected-world INCREASE-Y-VEL)
;;  = selected-world-inc-in-y
;; (world-after-key-event selected-world DECREASE-Y-VEL)
;;  = selected-world-dec-in-y
;; (world-after-key-event selected-world-pen-up DROP-PEN-KEY)
;;  = selected-world-pen-down
;; (world-after-key-event world-with-pen-down LIFT-PEN-KEY)
;;  = world-with-pen-up
;; (world-after-key-event paused-world "\n") = paused-world
;; (world-after-key-event unpaused-world "\r") = unpaused-world
;; STRATEGY: using cases on kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev PAUSE-KEY) (world-with-paused-toggled w)]
    [(key=? kev NEW-RECTANGLE-KEY) (world-with-new-rectangle w)]
    [else
     (make-world
      (rects-after-key-event (world-rects w) kev)
      (world-mx w) (world-my w) (world-paused? w))]))

;; TESTS:
;; local variable for testing
(define new-scene-with-rect
  (make-world (add-new-rect INITIAL-RECT-LIST)
              MOUSE-INITIALIZE-X MOUSE-INITIALIZE-Y
              SCREENSAVER-PAUSED?))
;; Test cases
(begin-for-test
  (check-equal?
   (world-after-key-event paused-world PAUSE-KEY)
   unpaused-world
   "Unpaused world after hitting pause key")
  (check-equal?
   (world-after-key-event unpaused-world PAUSE-KEY)
   paused-world
   "Paused world after hitting pause key")
  (check-equal?
   (world-after-key-event (initial-world "any")
                          NEW-RECTANGLE-KEY)
                          new-scene-with-rect
                          "World after hitting new rectangle key")
  (check-equal?
   (world-after-key-event selected-world
                          INCREASE-X-VEL)
                          selected-world-inc-in-x
                          "World after hitting increase x vel key")
  (check-equal?
   (world-after-key-event selected-world
                          DECREASE-X-VEL)
                          selected-world-dec-in-x
                          "World after hitting decrease x vel key")
  (check-equal?
   (world-after-key-event selected-world
                          INCREASE-Y-VEL)
                          selected-world-inc-in-y
                          "World after hitting increase y vel key")
  (check-equal?
   (world-after-key-event selected-world
                          DECREASE-Y-VEL)
                          selected-world-dec-in-y
                          "World after hitting decrease y vel key")
  (check-equal?
   (world-after-key-event selected-world-pen-up DROP-PEN-KEY)
   selected-world-pen-down
   "World after hitting the drop pen key")
  (check-equal?
   (world-after-key-event world-with-pen-down LIFT-PEN-KEY)
   world-with-pen-up
   "World after hitting the lift pen key")
  (check-equal?
   (world-after-key-event paused-world "\n")
   paused-world
   "World after hitting the new line key")
  (check-equal?
   (world-after-key-event unpaused-world "\r")
   unpaused-world
   "World after hitting the tab key"))

;; rect-after-pen-down : Rect -> Rect
;; GIVEN: a rectangle
;; RETURNS: the rectangle with its pen touching the canvas
;; if the rectangle is selected
;; EXAMPLES:
;; (rect-after-pen-down r5) =
;;  (make-rect 112 30 23 -14 0 0 #true #true empty)
;; (rect-after-pen-down r2) = r2
;; STRATEGY: Cases on if r is selected or not
(define (rect-after-pen-down r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r) (rect-y r)
       (rect-vx r) (rect-vy r)
       (rect-mdx r) (rect-mdy r)
       (rect-selected? r) PEN-DOWN
       (update-rect-pos-lst r))
      r))

;; TESTS:
(begin-for-test
  (check-equal? (rect-after-pen-down r5)
                (make-rect 112 30 23 -14 0 0 #true #true
                           empty)
                "Selected rectangle after hitting pen down key")
  (check-equal? (rect-after-pen-down r2) r2
                "Unselected rectangle after hitting pen down key"))

;; rect-after-pen-lift : Rectangle -> Rectangle
;; GIVEN: a rectangle
;; RETURNS: an updated rectangle with its pen lifted up from
;; the canvas if the rectangle is selected
;; EXAMPLES:
;; (rect-after-pen-lift selected-rect-with-pen-down) =
;;  selected-rect-with-pen-up
;; (rect-after-pen-lift unselected-rect-with-pen-down) =
;;  unselected-rect-with-pen-down
;; STRATEGY: Cases on if r is selected or not
(define (rect-after-pen-lift r)
  (if (rect-selected? r)
      (make-rect
       (rect-x r) (rect-y r)
       (rect-vx r) (rect-vy r)
       (rect-mdx r) (rect-mdy r)
       (rect-selected? r) PEN-UP
       (rect-pos-lst r))
      r))

;; TESTS:
(begin-for-test
  (check-equal? (rect-after-pen-lift selected-rect-with-pen-down)
                selected-rect-with-pen-up
                "Selected rectangle with pen down after pen up event")
  (check-equal? (rect-after-pen-lift unselected-rect-with-pen-down)
                unselected-rect-with-pen-down
                "Unselected rectangle after pen up key event"))

;; world-with-new-rectangle : World -> World
;; GIVEN: a world
;; RETURNS: a new world with a new rectangle added to its
;; existing list of rectangles at the center of the canvas
;; EXAMPLES:
;; (world-with-new-rectangle initial-world) =
;;  selected-world-inc-in-x
;; STRATEGY: Using template for World on w
(define (world-with-new-rectangle w)
  (make-world
   (add-new-rect (world-rects w))
   (world-mx w)
   (world-my w)
   (world-paused? w)))

;; TESTS:
(begin-for-test
  (check-equal? (world-with-new-rectangle (initial-world "any"))
                new-scene-with-rect
                "Initial world with new rectangle added"))

;; rect-with-updated-x-vel : Rect KeyEvent -> Rect
;; GIVEN: a rectangle and a key event
;; WHERE: the rectangle is selected
;; RETURNS: the same rectangle with the x velocity updated
;; EXAMPLES:
;; (rect-with-updated-x-vel r5 INCREASE-X-VEL) =
;;   (make-rect 112 30 25 -14 0 0 #true #false empty)
;; (rect-with-updated-x-vel r5 DECREASE-X-VEL) =
;;   (make-rect 112 30 21 -14 0 0 #true #false empty)
;; STRATEGY: Use template for Rect on r
(define (rect-with-updated-x-vel r kev)
  (make-rect
   (rect-x r)
   (rect-y r)
   (if (equal? kev INCREASE-X-VEL)
       (+ (rect-vx r) X-VEL-INCREASE-STEP)
       (- (rect-vx r) X-VEL-DECREASE-STEP))
   (rect-vy r)
   (rect-mdx r)
   (rect-mdy r)
   (rect-selected? r)
   (rect-pen-down? r)
   (rect-pos-lst r)))

;; TESTS:
(begin-for-test
  (check-equal? (rect-with-updated-x-vel r5 INCREASE-X-VEL)
                (make-rect 112 30 25 -14 0 0 #true #false empty)
                "Selected rectangle with x velocity increased")
  (check-equal? (rect-with-updated-x-vel r5 DECREASE-X-VEL)
                (make-rect 112 30 21 -14 0 0 #true #false empty)
                "Selected rectangle with x velocity decreased"))

;; rect-with-updated-y-vel : Rect KeyEvent -> Rect
;; GIVEN: a rectangle and the key event
;; WHERE: the rectangle is selected
;; RETURNS: the same rectangle with the updated y velocity
;; EXAMPLES:
;; (rect-with-updated-y-vel r5 INCREASE-Y-VEL) =
;;   (make-rect 112 30 23 -12 0 0 #true #false empty)
;; (rect-with-updated-y-vel r5 DECREASE-Y-VEL) =
;;   (make-rect 112 30 23 -16 0 0 #true #false empty)
;; STRATEGY: Use template for Rect on r
(define (rect-with-updated-y-vel r kev)
  (make-rect
   (rect-x r)
   (rect-y r)
   (rect-vx r)
   (if (equal? kev INCREASE-Y-VEL)
       (+ (rect-vy r) Y-VEL-INCREASE-STEP)
       (- (rect-vy r) Y-VEL-DECREASE-STEP))
   (rect-mdx r)
   (rect-mdy r)
   (rect-selected? r)
   (rect-pen-down? r)
   (rect-pos-lst r)))

;; TESTS:
(begin-for-test
  (check-equal? (rect-with-updated-y-vel r5 INCREASE-Y-VEL)
                (make-rect 112 30 23 -12 0 0 #true #false empty)
                "Selected rectangle with y velocity increased")
  (check-equal? (rect-with-updated-y-vel r5 DECREASE-Y-VEL)
                (make-rect 112 30 23 -16 0 0 #true #false empty)
                "Selected rectangle with y velocity decreased"))

;; world-with-paused-toggled : World -> World
;; GIVEN: a world
;; RETURNS: a world just like the given one, but with paused? toggled
;; EXAMPLES:
;; (world-with-paused-toggled paused-world) = unpaused-world
;; (world-with-paused-toggled unpaused-world) = paused-world
;; STRATEGY: Use template for World on w
(define (world-with-paused-toggled w)
  (make-world
   (world-rects w)
   (world-mx w) (world-my w)
   (not (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-with-paused-toggled paused-world)
   unpaused-world
   "Paused world after the pause is toggled")
  (check-equal?
   (world-with-paused-toggled unpaused-world)
   paused-world
   "Unpaused world after the pause is toggled"))

;; rect-after-key-event : Rect KeyEvent -> Rect
;; GIVEN: a rectangle and a key event
;; RETURNS: the rectangle that should follow the given key event
;; EXAMPLES:
;; (rect-after-key-event r5 DECREASE-Y-VEL) =
;;  (make-rect 112 30 23 -16 0 0 #true #false empty)
;; (rect-after-key-event r5 INCREASE-Y-VEL) =
;;  (make-rect 112 30 23 -12 0 0 #true #false empty)
;; (rect-after-key-event r5 DECREASE-X-VEL) =
;;  (make-rect 112 30 21 -14 0 0 #true #false empty)
;; (rect-after-key-event r5 INCREASE-X-VEL) =
;;  (make-rect 112 30 25 -14 0 0 #true #false empty)
;; (rect-after-key-event selected-rect-with-pen-up DROP-PEN-KEY)
;;  = selected-rect-with-pen-down
;; (rect-after-key-event selected-rect-with-pen-down LIFT-PEN-KEY)
;;  = selected-rect-with-pen-up
;; STRATEGY: Divide into cases on kev
(define (rect-after-key-event r kev)
  (if (rect-selected? r)
      (cond    
        [(key=? kev INCREASE-X-VEL) (rect-with-updated-x-vel r kev)]
        [(key=? kev DECREASE-X-VEL) (rect-with-updated-x-vel r kev)]
        [(key=? kev INCREASE-Y-VEL) (rect-with-updated-y-vel r kev)]
        [(key=? kev DECREASE-Y-VEL) (rect-with-updated-y-vel r kev)]
        [(key=? kev DROP-PEN-KEY) (rect-after-pen-down r)]
        [(key=? kev LIFT-PEN-KEY) (rect-after-pen-lift r)]
        [else r])
      r))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-key-event r5 DECREASE-Y-VEL)
   (make-rect 112 30 23 -16 0 0 #true #false empty)
   "Selected rectangle after hitting decrease y velocity key")
  (check-equal?
   (rect-after-key-event r5 INCREASE-Y-VEL)
   (make-rect 112 30 23 -12 0 0 #true #false empty)
   "Selected rectangle after hitting increase y velocity key")
  (check-equal?
   (rect-after-key-event r5 DECREASE-X-VEL)
   (make-rect 112 30 21 -14 0 0 #true #false empty)
   "Selected rectangle after hitting decrease x velocity key")
  (check-equal?
   (rect-after-key-event r5 INCREASE-X-VEL)
   (make-rect 112 30 25 -14 0 0 #true #false empty)
   "Selected rectangle after hitting increase x velocity key")
  (check-equal?
   (rect-after-key-event selected-rect-with-pen-up
                         DROP-PEN-KEY)
   selected-rect-with-pen-down
   "Selected rectangle after hitting drop pen key")
  (check-equal?
   (rect-after-key-event selected-rect-with-pen-down
                         LIFT-PEN-KEY)
   selected-rect-with-pen-up
   "Selected rectangle after hitting lift pen key")
  (check-equal?
   (rect-after-key-event selected-rect-with-pen-down
                         "\n")
   selected-rect-with-pen-down
   "Selected rectangle after hitting the enter key"))

;; rects-after-key-event : ListOfRect KeyEvent -> ListOfRect
;; GIVEN: a list of rectangles and the key event
;; RETURNS: the list of rectangles that should follow the key
;; event
;; EXAMPLES:
;; (rects-after-key-event (list r2 r5) DECREASE-Y-VEL) =
;;  (list r2 (make-rect 112 30 23 -16 0 0 #true #false empty))
;; (rects-after-key-event (list r2 r5) INCREASE-Y-VEL) =
;;  (list r2 (make-rect 112 30 23 -12 0 0 #true #false empty))
;; (rects-after-key-event (list r2 r5) DECREASE-X-VEL) =
;;  (list r2 (make-rect 112 30 21 -14 0 0 #true #false empty))
;; (rects-after-key-event (list r2 r5) INCREASE-X-VEL) =
;;  (list r2 (make-rect 112 30 25 -14 0 0 #true #false empty))
;; (rects-after-key-event
;;  (list selected-rect-with-pen-down unselected-rect-with-pen-down)
;;  LIFT-PEN-KEY) =
;;  (list selected-rect-with-pen-up unselected-rect-with-pen-down)
;; (rects-after-key-event (list r5 r2) DROP-PEN-KEY) =
;;  (list (make-rect 112 30 23 -14 0 0 #true #true empty) r2)
;; STRATEGY: Use HOF map on rects
(define (rects-after-key-event rects kev)
  (map
   ;; Rect -> ListOfRect
   ;; GIVEN: a rectangle
   ;; RETURNS: the state of the rectangle after the key event
   (lambda (rect) (rect-after-key-event rect kev))
   rects))

;; TESTS:
(begin-for-test
  (check-equal? (rects-after-key-event (list r2 r5) DECREASE-Y-VEL)
                (list r2 (make-rect 112 30 23 -16 0 0 #true
                                    #false empty))
                "Rectangles after hitting decrease y velocity key")
  (check-equal? (rects-after-key-event (list r2 r5) INCREASE-Y-VEL)
                (list r2 (make-rect 112 30 23 -12 0 0 #true
                                    #false empty))
                "Rectangles after hitting increase y velocity key")
  (check-equal? (rects-after-key-event (list r2 r5) DECREASE-X-VEL)
                (list r2 (make-rect 112 30 21 -14 0 0 #true
                                    #false empty))
                "Rectangles after hitting decrease x velocity key")
  (check-equal? (rects-after-key-event (list r2 r5) INCREASE-X-VEL)
                (list r2 (make-rect 112 30 25 -14 0 0 #true
                                    #false empty))
                "Rectangles after hitting increase x velocity key")
  (check-equal? (rects-after-key-event
                 (list
                  selected-rect-with-pen-down
                  unselected-rect-with-pen-down)
                 LIFT-PEN-KEY)
                (list
                 selected-rect-with-pen-up
                 unselected-rect-with-pen-down)
                "Rectangles after hitting lift pen key")
  (check-equal? (rects-after-key-event (list r5 r2) DROP-PEN-KEY)
                (list
                 (make-rect 112 30 23 -14 0 0 #true #true empty)
                 r2)
                "Rectangles after hitting drop pen key"))

;; new-rectangle : NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLES:
;; (new-rectangle 88 15 -12 20) = r4
;; (new-rectangle 375 295 23 -14) = r8
;; STRATEGY: Combine simpler functions
(define (new-rectangle x-pos y-pos x-vel y-vel)
  (make-rect x-pos y-pos x-vel y-vel
             RECTANGLE-INIT-MDX RECTANGLE-INIT-MDY
             RECTANGLE-INIT-SELECTION
             RECTANGLE-INIT-PEN-DOWN
             RECTANGLE-INIT-POS-LST))

;; TESTS:
(begin-for-test
  (check-equal? (new-rectangle 88 15 -12 20) r4
                "New rectangle at (88,15) with velocity (-12,20)")
  (check-equal? (new-rectangle 375 295 23 -14) r8
                "New rectangle at (375,295) with velocity (23,-14)"))

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
   ")"))

;; render-color : Rectangle -> String
;; GIVEN: a rectangle
;; RETURNS: the color in which the rectangle is to be rendered.
;;          The color is 'Red' if the rectangle is selected and
;;          'Blue' if unselected.
;; EXAMPLES:
;; (rect-color r1) = "Blue"
;; (rect-color r5) = "Red"
;; STRATEGY: Cases on if r is selected or not
(define (render-color r)
  (if (rect-selected? r)
      SELECTED-ITEM-COLOR
      UNSELECTED-ITEM-COLOR))

;; TESTS:
(begin-for-test
  (check-equal? (render-color r1) "Blue"
                "Render color for unselected rectangle")
  (check-equal? (render-color r5) "Red"
                "Render color for selected rectangle"))

;; render-rect : Rectangle -> Image
;; GIVEN: a rectangle
;; RETURNS: an image which renders the rectangle based on the
;; dimensions specified, overlaying the velocities with which it
;; is moving in the x and y directions of the canvas
;; EXAMPLES:
;; (render-rect r1) =
;;    (overlay
;;      (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;                 RECTANGLE-STYLE UNSELECTED-ITEM-COLOR)
;;      (text "(-12,20)" TEXT-SIZE UNSELECTED-ITEM-COLOR))
;; (render-rect r5) =
;;    (overlay
;;      (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;                 RECTANGLE-STYLE SELECTED-ITEM-COLOR)
;;      (text "23,-14" TEXT-SIZE SELECTED-ITEM-COLOR))
;; STRATEGY: Combine simpler functions
(define (render-rect r)
  (overlay   
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
              RECTANGLE-STYLE (render-color r))
   (text (pos-as-text r) TEXT-SIZE (render-color r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (render-rect r1)
   (overlay
    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
               RECTANGLE-STYLE UNSELECTED-ITEM-COLOR)
    (text "(-12,20)" TEXT-SIZE UNSELECTED-ITEM-COLOR))
   "Render unselected rectangle")
  (check-equal?
   (render-rect r5)
   (overlay
    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
               RECTANGLE-STYLE SELECTED-ITEM-COLOR)
    (text "(23,-14)" TEXT-SIZE SELECTED-ITEM-COLOR))
   "Render selected rectangle"))

;; render-rects : ListOfRectangle -> ListOfImage
;; GIVEN: a list of rectangles
;; RETURNS: a list of images where every single rectangle is rendered
;;          based on their current position and selection
;; EXAMPLES:
;; (render-rects (list r1 r5)) =
;;  (list
;;   (overlay
;;    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;               RECTANGLE-STYLE UNSELECTED-ITEM-COLOR)
;;    (text "(-12,20)" TEXT-SIZE UNSELECTED-ITEM-COLOR))
;;   (overlay
;;    (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
;;               RECTANGLE-STYLE SELECTED-ITEM-COLOR)
;;    (text "(23,-14)" TEXT-SIZE SELECTED-ITEM-COLOR)))
;; STRATEGY: Use HOF map on rects
(define (render-rects rects)
  (map render-rect rects))

;; TESTS:
(begin-for-test
  (check-equal?
   (render-rects (list r1 r5))
   (list
    (overlay
     (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
                RECTANGLE-STYLE UNSELECTED-ITEM-COLOR)
     (text "(-12,20)" TEXT-SIZE UNSELECTED-ITEM-COLOR))
    (overlay
     (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT
                RECTANGLE-STYLE SELECTED-ITEM-COLOR)
     (text "(23,-14)" TEXT-SIZE SELECTED-ITEM-COLOR)))
   "Render a list of rectangles"))

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
  (check-equal? (render-posn r1) (make-posn 200 100)
                "Posn object for r1")
  (check-equal? (render-posn r6) (make-posn 200 275)
                "Posn object for r6"))

;; render-posns : ListOfRectangle -> ListOfPosn
;; GIVEN: a list of rectangles
;; RETURNS: a list of Posn objects that stores the position of
;;          each rectangle in the coordinate plain
;; EXAMPLES:
;; (render-posns (list r1 r6)) =
;;  (list (make-posn 200 100) (make-posn 200 275))
;; STRATEGY: Use HOF map on rects
(define (render-posns rects)
  (map render-posn rects))

;; TESTS:
(begin-for-test
  (check-equal? (render-posns (list r1 r6))
                (list (make-posn 200 100) (make-posn 200 275))
                "List of Posn objects to render the rectangles"))

;; any-rect-selected? : ListOfRect -> Boolean
;; GIVEN: a list of rectangles
;; RETURNS: true if one or more of the rectangles is selected
;; EXAMPLES:
;; (any-rect-selected? (list r2 r5)) = #true
;; (any-rect-selected? (list r1 r2)) = #false
;; STRATEGY: Use HOF foldr on rects
(define (any-rect-selected? rects)
  (foldr
   ;; Rectangle ListOfRect -> Boolean
   ;; GIVEN: a rectangle and a list of rectangles
   ;; RETURNS: true iff one of the rectangles is selected
   (lambda (r lor) (rect-is-selected r lor))
   #f rects))

;; TESTS:
(begin-for-test
  (check-equal? (any-rect-selected? (list r2 r5)) #true
                "One of the rectangles in the list is selected")
  (check-equal? (any-rect-selected? (list r1 r2)) #false
                "None of the rectangles in the list are selected"))

;; rect-is-selected : Rectangle ListOfRectangle -> Boolean
;; GIVEN: a rectangle and a list of rectangles
;; RETURNS: true iff r or one of the rectangles in lor is selected
;; EXAMPLES:
;; (rect-is-selected r2 (list r5)) = #true
;; (rect-is-selected r2 (list r1)) = #false
;; STRATEGY: Combining simpler functions
(define (rect-is-selected r lor)
  (or (rect-selected? r) lor))

;; render-pen-track-for-rects : ListOfRect -> ListOfImage
;; GIVEN: a list of rectangles
;; RETURNS: a list of images to render the path followed by
;; the rectangles in the list. The returned list contains only
;; circles which are placed at their respective positions while
;; rendering the whole scene
;; EXAMPLES:
;; (render-pen-track-for-rects (list selected-rect-with-pen-down))
;;  = (list PEN-TIP PEN-TIP)
;; STRATEGY: Use HOF foldr on rects
(define (render-pen-track-for-rects rects)
  (foldr
   ;; Rectangle ListOfRect -> ListOfImage
   ;; GIVEN: a rectangle and a list of rectangles
   ;; RETURNS: a list of images
   (lambda (r lor) (render-pen-track-for-rect r lor))
   empty rects))

;; TESTS:
(begin-for-test
  (check-equal? (render-pen-track-for-rects
                 (list selected-rect-with-pen-down))
                (list PEN-TIP PEN-TIP)
                "List of circles to track the path of rectangles"))

;; render-pen-track-for-rect : Rectangle ListOfRect -> ListOfImage
;; GIVEN: a rectangle and a list of rectangles
;; WHERE: the rectangle has its pen touching the canvas
;; RETURNS: the circles to trace the track followed by the
;; rectangle as saved in it's pos-lst attribute
;; EXAMPLES:
;; (render-pen-track-for-rect selected-rect-with-pen-down) =
;;  (list PEN-TIP PEN-TIP)
;; STRATEGY: Use template for Rect on r
(define (render-pen-track-for-rect r lor)
  (append (mark-all-pos (rect-pos-lst r)) lor))

;; TESTS:
(begin-for-test
  (check-equal? (render-pen-track-for-rect
                 selected-rect-with-pen-down empty)
                (list PEN-TIP PEN-TIP)
                "List of cirlces to track the path of a rectangle"))

;; mark-all-pos : ListOfPosn -> ListOfImage
;; GIVEN: a list of Posn-s
;; RETURNS: a list of images to mark each position the rectangle
;; travelled through after it's pen touched the canvas
;; EXAMPLES:
;; (mark-all-pos (rect-pos-lst selected-rect-with-pen-down))
;;  = (list PEN-TIP PEN-TIP)
;; STRATEGY: Use HOF map on pos-lst
(define (mark-all-pos pos-lst)
  (map mark-pos pos-lst))

;; TESTS:
(begin-for-test
  (check-equal? (mark-all-pos
                 (rect-pos-lst
                  selected-rect-with-pen-down))
                (list PEN-TIP PEN-TIP)
                "List of circles to mark the positions in the list"))

;; mark-pos : Posn -> Image
;; GIVEN: a Posn
;; RETURNS: a circle to mark the position on the canvas
;; EXAMPLES:
;; (mark-pos (make-posn 200 100)) = PEN-TIP
;; STRATEGY: Combine simpler functions
(define (mark-pos pos)
  PEN-TIP)

;; TESTS:
(begin-for-test
  (check-equal? (mark-pos (make-posn 200 100)) PEN-TIP
                "A circle to mark the position passed as argument"))

;; marked-pos-lst-for-rects : ListOfRect -> ListofPosn
;; GIVEN: a list of rectangles
;; RETURNS: a combined list of all the Posn which have to be
;; marked on the canvas
;; EXAMPLES:
;; (marked-pos-lst-for-rects
;;  (list selected-rect-with-pen-down
;;        unselected-rect-with-pen-down)) =
;;   (append (rect-pos-lst selected-rect-with-pen-down)
;;           (rect-pos-lst unselected-rect-with-pen-down))
;; STRATEGY: Use HOF foldr on rects
(define (marked-pos-lst-for-rects rects)
  (foldr
   ;; Rectangle ListOfRect -> ListOfPosn
   ;; GIVEN: the current rectangle and the rest of the rectangles
   ;; RETURNS: a list of Posn objects which contains all the positions
   ;; to be marked on the scene with the marker
   (lambda (r lor) (append (rect-pos-lst r) lor))
   empty rects))

;; TESTS:
(begin-for-test
  (check-equal?
   (marked-pos-lst-for-rects
    (list selected-rect-with-pen-down unselected-rect-with-pen-down))
   (append (rect-pos-lst selected-rect-with-pen-down)
           (rect-pos-lst unselected-rect-with-pen-down))
   "Posn list for all the selected rectangles in the world"))

;; mouse-pointer-marker : World -> ListOfImage
;; GIVEN: a world
;; WHERE: any one of the rectangles inside the world is selected
;; RETURNS: the mouse pointer marker in the form of an image stored
;; as the only element inside a list
;; EXAMPLES:
;; (mouse-pointer-marker selected-world) =
;;   (list MOUSE-CLICK-POS-MARKER)
;; (mouse-pointer-marker unpaused-world) = empty
;; STRATEGY: Cases on if any of the rectangles is selected or not
(define (mouse-pointer-marker w)
  (if (any-rect-selected? (world-rects w))
      (list MOUSE-CLICK-POS-MARKER)
      empty))

;; TESTS:
(begin-for-test
  (check-equal?
   (mouse-pointer-marker selected-world) (list MOUSE-CLICK-POS-MARKER)
   "Mouse pointer marker for selected world")
  (check-equal?
   (mouse-pointer-marker unpaused-world) empty
   "Mouse pointer marker for world with no selected rectangles"))

;; mouse-pointer-pos : World -> ListOfPosn
;; GIVEN: a world
;; WHERE: any one of the rectangles inside the world is selected
;; RETURNS: the mouse position as a Posn object stored inside a list
;; EXAMPLES:
;; (mouse-pointer-pos selected-world) = (list (make-posn 0 0))
;; (mouse-pointer-pos unpaused-world) = empty
;; STRATEGY: Use template for World on w
(define (mouse-pointer-pos w)
  (if (any-rect-selected? (world-rects w))
      (list (make-posn (world-mx w) (world-my w)))
      empty))

;; TESTS:
(begin-for-test
  (check-equal?
   (mouse-pointer-pos selected-world) (list (make-posn 0 0))
   "Mouse position for selected world")
  (check-equal?
   (mouse-pointer-pos unpaused-world) empty
   "Mouse position for world with no selected rectangles"))

;; world->scene : World -> Scene
;; While rendering the current world, the function checks whether
;; any of the two rectangles is selected. If so, it renders all the
;; elements in red and a marker circle around the current position
;; of the mouse.
;; GIVEN: a world
;; RETURNS: a scene that portrays the current world
;; EXAMPLES:
;; (world->scene unpaused-world) =
;;    (place-images
;;     (render-rects (world-rects unpaused-world))
;;     (render-posns (world-rects unpaused-world))
;;     EMPTY-CANVAS)
;; (world->scene selected-world) =
;;    (place-images
;;     (append
;;      (render-rects (world-rects selected-world))
;;      (list MOUSE-CLICK-POS-MARKER))
;;     (append
;;      (render-posns (world-rects selected-world))
;;      (list (make-posn 0 0)))
;;     EMPTY-CANVAS)
;; (world->scene (initial-world 1)) =
;;     (place-images
;;      (render-rects (world-rects (initial-world 1)))
;;      (render-posns (world-rects (initial-world 1)))
;;      EMPTY-CANVAS)
;; STRATEGY: Cases on if the list of rectangles within the world
;; is empty or not
(define (world->scene w)
  (if (empty? (world-rects w))
      EMPTY-CANVAS
      (place-images
       (append
        (render-rects (world-rects w))
        (mouse-pointer-marker w)
        (render-pen-track-for-rects (world-rects w)))
       (append
        (render-posns (world-rects w))
        (mouse-pointer-pos w)
        (marked-pos-lst-for-rects (world-rects w)))
       EMPTY-CANVAS)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world->scene unpaused-world)
   (place-images
    (render-rects (world-rects unpaused-world))
    (render-posns (world-rects unpaused-world))
    EMPTY-CANVAS)
   "Unpaused world rendered on the canvas")
  (check-equal?
   (world->scene selected-world)
   (place-images
    (append
     (render-rects (world-rects selected-world))
     (list MOUSE-CLICK-POS-MARKER))
    (append
     (render-posns (world-rects selected-world))
     (list (make-posn 0 0)))
    EMPTY-CANVAS)
   "Selected world rendered on the canvas")
  (check-equal?
   (world->scene (initial-world 1))
   (place-images
    (render-rects (world-rects (initial-world 1)))
    (render-posns (world-rects (initial-world 1)))
    EMPTY-CANVAS)
   "Initial world rendered on the canvas"))

;; click-inside-rect? : Rectangle NonNegInt NonNegInt ->
;;                      Boolean
;; GIVEN: a rectangle and the x & y coordinate of a mouse
;;        event
;; RETURNS: whether the event occurred inside the rectangle or not
;; EXAMPLE:
;; (click-inside-rect? r1 215 110) = #true
;; (click-inside-rect? r2 215 200) = #false
(define (click-inside-rect? r mx my)
  (and
    (<= 
      (- (rect-x r) RECTANGLE-X-OFFSET)
      mx
      (+ (rect-x r) RECTANGLE-X-OFFSET))
    (<= 
      (- (rect-y r) RECTANGLE-Y-OFFSET)
      my
      (+ (rect-y r) RECTANGLE-Y-OFFSET))))

;; TESTS:
(begin-for-test
  (check-equal? (click-inside-rect? r1 215 110) #true
                "Click event inside rectangle boundary")
  (check-equal? (click-inside-rect? r2 215 200) #false
                "Click event outside rectangle boundary"))

;; displace-selected-rect-x-coord : Rectangle NonNegInt ->
;;                                  NonNegInt
;; GIVEN: A rectangle and the x coordinate of the mouse up event
;; RETURNS: the new position of the rectangle after the mouse
;;          button is released
;; EXAMPLES:
;; (displace-selected-rect-x-coord r1 200) = 200
;; (displace-selected-rect-x-coord r4 88) = 88
;; (displace-selected-rect-x-coord r8 375) = 375
;; STRATEGY: Divide into cases on r
(define (displace-selected-rect-x-coord r mx)
  (cond
    [(rect-hits-left-wall? r) MIN-POS-IN-X-COORD]
    [(rect-hits-right-wall? r) MAX-POS-IN-X-COORD]
    [else     
     (+ (rect-mdx r) mx)]))

;; TESTS:
(begin-for-test
  (check-equal? (displace-selected-rect-x-coord r1 200) 200
                "Displace x coordinate for unselected rectangle")
  (check-equal? (displace-selected-rect-x-coord r10 10) 30
                "Displace x coordinate for selected rectangle")
  (check-equal?
   (displace-selected-rect-x-coord r8 375) 370
   "Displace x coordinate for selected rectangle crossing right wall"))

;; displace-selected-rect-y-coord : Rectangle NonNegInt ->
;;                                  NonNegInt
;; GIVEN: A rectangle and the y coordinate of the mouse up event
;; RETURNS: the new position of the rectangle after the mouse
;;          button is released
;; EXAMPLES:
;; (displace-selected-rect-y-coord r8 297) = 275
;; (displace-selected-rect-y-coord r4 10) = 25
;; (displace-selected-rect-y-coord r1 120) = 100
;; STRATEGY: Divide into cases on r
(define (displace-selected-rect-y-coord r my)
  (cond
    [(rect-hits-top-wall? r) MIN-POS-IN-Y-COORD]
    [(rect-hits-bottom-wall? r) MAX-POS-IN-Y-COORD]
    [else
     (+ (rect-mdy r) my)]))

;; TESTS:
(begin-for-test
  (check-equal?
   (displace-selected-rect-y-coord r8 297) 275
   "Displace y coordinate for selected rectangle crossing bottom wall")
  (check-equal?
   (displace-selected-rect-y-coord r4 10) 10
   "Displace y coordinate for unselected rectangle crossing top wall")
  (check-equal? (displace-selected-rect-y-coord r1 120) 120
                "Displace y coordinate for unselected rectangle"))

;; rect-after-mouse-down : Rectangle NonNegInt NonNegInt
;;                          -> Rectangle
;; GIVEN: A rectangle, x and y coordinate of the mouse event
;; RETURNS: the rectangle that should follow the given rectangle
;;          after the mouse down event
;; EXAMPLES:
;; (rect-after-mouse-down r1 215 90) =
;;   (make-rect 200 100 -12 20 15 -10 #true)
;; (rect-after-mouse-down r1 100 75) =
;;   (make-rect 200 100 -12 20 -15 10 #false)
;; STRATEGY: Cases on if the mouse click event is within the rectangle
(define (rect-after-mouse-down r mx my)
  (if (click-inside-rect? r mx my)
      (make-rect (rect-x r) (rect-y r)
                 (rect-vx r) (rect-vy r)
                 (- (rect-x r) mx)
                 (- (rect-y r) my)
                 (not (rect-selected? r))
                 (rect-pen-down? r)
                 (rect-pos-lst r))
      r))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-mouse-down r1 215 90)
   (make-rect 200 100 -12 20 -15 10 #true #true empty)
   "Unselected rectangle after mouse down event inside its boundary")
  (check-equal?
   (rect-after-mouse-down r1 100 75)
   (make-rect 200 100 -12 20 0 0 #false #true empty)
   "Unselected rectangle after mouse down event outside its boundary"))

;; rect-after-mouse-drag : Rectangle NonNegInt NonNegInt
;;                         -> Rectangle
;; GIVEN: a rectangle, x and y coordinates of the mouse event
;; RETURNS: returns the rectangle that should follow the current
;;          rectangle after the mouse-drag event
;; EXAMPLES:
;; (rect-after-mouse-drag r1 215 115) = r1
;; (rect-after-mouse-drag r5 120 25) = (make-rect 120 25 23 -14
;;                                      #true)
;; STRATEGY: Cases on if r is selected or not
(define (rect-after-mouse-drag r mx my)
  (if (rect-selected? r)
      (make-rect (+ (rect-mdx r) mx)
                 (+ (rect-mdy r) my)
                 (rect-vx r) (rect-vy r)
                 (rect-mdx r) (rect-mdy r)
                 (rect-selected? r)
                 (rect-pen-down? r)
                 (rect-pos-lst r))
      r))

;; TESTS:
(begin-for-test
  (check-equal? (rect-after-mouse-drag r1 200 100) r1
                "Unselected rectangle after mouse drag")
  (check-equal?
   (rect-after-mouse-drag
    (make-rect 200 100 -12 20 -15 10 #true #false empty) 185 110)
   (make-rect 170 120 -12 20 -15 10 #true #false empty)
   "Selected rectangle after mouse drag"))

;; rect-after-mouse-up : Rectangle NonNegInt NonNegInt -> Rectangle
;; GIVEN: a rectangle, x and y coordinates of the mouse event,
;; RETURNS: returns the rectangle that should follow the current
;;          rectangle after the mouse-up event
;; EXAMPLES:
;; (rect-after-mouse-up r1 215 115) = r1
;; (rect-after-mouse-up r5 115 20) = (make-rect 115 20 (rect-vx r)
;;                                   (rect-vy r) #true)
;; STRATEGY: Cases on if r is selected or not
(define (rect-after-mouse-up r mx my)
  (if (rect-selected? r)
      (make-rect (displace-selected-rect-x-coord r mx)
                 (displace-selected-rect-y-coord r my)
                 (updated-vx r) (updated-vy r)
                 DEFAULT-DX DEFAULT-DY
                 (not (rect-selected? r))
                 (rect-pen-down? r)
                 (rect-pos-lst r))
      r))

;; TESTS:
(begin-for-test
  (check-equal? (rect-after-mouse-up r1 215 115) r1
                "Unselected rectangle after mouse up event")
  (check-equal? (rect-after-mouse-up r5 115 20)
                (make-rect 115 25 23 14 0 0 #false #false empty)
                "Selected rectangle after mouse mouse up event"))

;; world-after-mouse-event
;;  : WorldState Int Int MouseEvent -> WorldState 
;; GIVEN: A World, the x- and y-coordinates of a mouse event,
;; and the mouse event
;; RETURNS: the world that should follow the given world after
;; the given mouse event
;; EXAMPLES:
;; (world-after-mouse-event unpaused-world 0 0 "button-down") =
;;  (make-world (world-rects unpaused-world) 0 0 #false)
;; (world-after-mouse-event unpaused-world 210 103 "button-down") =
;;   (make-world (list (make-rect (rect-x r1) (rect-y r1) 
;;                          (rect-vx r1) (rect-vy r1)
;;                          -10 -3 #true)
;;                          r2) 210 103 #false)
;; STRATEGY: Use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world
   (rects-after-mouse-event (world-rects w) mx my mev)   
   mx  my (world-paused? w)))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event unpaused-world 0 0 "button-down")
   (make-world (world-rects unpaused-world) 0 0 #false)
   "Unpaused world after mouse down event in free space")
  (check-equal?
   (world-after-mouse-event unpaused-world 210 103 "button-down")
   (make-world
    (list (make-rect (rect-x r1) (rect-y r1) (rect-vx r1) (rect-vy r1)
                     -10 -3 #true #true empty) r2) 210 103 #false)
   "Unpaused world after mouse down event inside a rectangle"))

;; rect-after-mouse-event :  Rectangle Int Int MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; EXAMPLES:
;; (rect-after-mouse-event r1 210 103 "button-down") =
;;   (make-rect 200 100 -12 20 -10 -3 #true)
;; (rect-after-mouse-event r5 110 25 "drag") =
;;   (make-rect 110 25 23 -14 0 0 #true)
;; (rect-after-mouse-event r5 110 25 "button-up") =
;;   (make-rect 110 25 23 -14 0 0 #false)
;; (rect-after-mouse-event r1 110 25 "move") = r1
;; STRATEGY: Divide into cases on mev
(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev "button-down") (rect-after-mouse-down r mx my)]
    [(mouse=? mev "drag") (rect-after-mouse-drag r mx my)]
    [(mouse=? mev "button-up") (rect-after-mouse-up r mx my)]
    [else r]))

;; rects-after-mouse-event :  ListOfRect Int Int MouseEvent -> ListOfRect
;; GIVEN: A list of rectangles, the x- and y-coordinates of a mouse event, and the
;; and the mouse event
;; RETURNS: the rectangles that should follow the given rectangles after
;; the given mouse event
;; EXAMPLES:
;; (rects-after-mouse-event (list r1 r2) 210 103 "button-down") =
;;   (list (make-rect 200 100 -12 20 -10 -3 #true #true empty) r2)
;; (rects-after-mouse-event (list r5 r6) 110 25 "drag") =
;;   (list (make-rect 110 25 23 -14 0 0 #true #false empty) r6)
;; (rects-after-mouse-event (list r5 r6) 110 25 "button-up") =
;;   (list (make-rect 110 25 23 -14 0 0 #false #false empty) r6)
;; (rects-after-mouse-event (list r1 r2) 110 25 "move") = (list r1 r2)
;; STRATEGY: Use HOF map on rects
(define (rects-after-mouse-event rects mx my mev)
  (map
   ;; Rect -> ListOfRect
   ;; GIVEN: a rectangle
   ;; RETURNS: the rectangle after the mouse event
   (lambda (r) (rect-after-mouse-event r mx my mev))
   rects))

;; TESTS:
(begin-for-test
  (check-equal?
   (rects-after-mouse-event (list r1 r2) 210 103 "button-down")
   (list (make-rect 200 100 -12 20 -10 -3 #true #true empty) r2)
   "Rectangles after mouse down event")
  (check-equal?
   (rects-after-mouse-event (list r5 r6) 110 25 "drag")
   (list (make-rect 110 25 23 -14 0 0 #true #false empty) r6)
   "Rectangles after mouse drag event")
  (check-equal?
   (rects-after-mouse-event (list r5 r6) 110 25 "button-up")
   (list (make-rect 110 25 23 14 0 0 #false #false empty) r6)
   "Rectangles after mouse button up event")
  (check-equal?
   (rects-after-mouse-event (list r1 r2) 110 25 "move")
   (list r1 r2)
   "Rectangles after move event")
  (check-equal?
   (rects-after-mouse-event empty 110 25 "button-down") empty
   "Empty set of rectangles after mouse button down event"))