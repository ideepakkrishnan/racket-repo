#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "interfaces.rkt")
(require "extras.rkt")
(require "FieldController.rkt")
(require "PerfectBounce.rkt")

(provide YController%)

;; a YController% is a (new YController% [model Model<%>])

(define YController%
  (class* FieldController% (Controller<%>)

    (inherit-field model)  ; the model

    ;; the x and y coordinate of the center of this controller
    (inherit-field x y)

    (inherit-field width height)

    ;; field where the ball is supposed to bounce 
    (inherit-field wall-width)
    (inherit-field wall-height)
    (inherit-field wall-half-width)
    (inherit-field wall-half-height)

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    
    ;; inherit the handle selected flag from DraggableController%
    (inherit-field handle-selected?)

    ;; fields for selection and updating the position of the particle
    (inherit-field selected?)

    ;; Relative distance of the controller from mouse click position
    (inherit-field saved-mx)
    (inherit-field saved-my)

    ;; field to check whether the click happened inside the ball's
    ;; canvas
    (field [ball-selected? false])

    (field [bdx 0])
    (field [bdy 0])

    (super-new)

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: Nothing. The XY controller doesn't respond to key events
    (define/override (after-key-event kev)
      2345)

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in this object
    (define/override (after-button-down mx my)
      (if (send this in-this? mx my)
        (begin
          (set! selected? true)
          (set! handle-selected? (send this in-handle? mx my))
          (set! ball-selected? (inside-wall? mx my))
          (set! bdx particle-x)
          (set! bdy (- my particle-y))
          (set! saved-mx (- mx x))
          (set! saved-my (- my y))
          ;; Since the mouse click occurred inside the controller,
          ;; pause the movement of the ball in each tick
          (send model execute-command (make-pause-movement true)))
        3742))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    (define/override (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! handle-selected? false)
        (set! ball-selected? false)
        ;; Clear the ball movement flag so that it can move freely
        ;; at each tick
        (send model execute-command (make-pause-movement false))))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    ;; HBD : ONLY X movement here, may need a new struct
    (define/override (after-drag mx my)
      (cond
        ;; Controller's handle is selected. So move the controller
        ;; relative to mouse drag
        [handle-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my))
           ;; Now register the new position of the bounding wall with
           ;; the model
           )] ;; bottom edge
        
        ;; Only the ball is selected. So move the ball around in its
        ;; canvas relative to mouse drag
        [ball-selected?
         (local
           (;; New X position for the ball
            (define new-px (- mx bdx))
            ;; New Y position for the ball
            (define new-py (- my bdy))
            ;; Min X position the ball can take
            (define min-px (- x wall-half-width))
            ;; Max X position the ball can take
            (define max-px (+ x wall-half-width))
            ;; Min Y position the ball can take
            (define min-py (- y wall-half-height))
            ;; Max Y position the ball can take
            (define max-py (+ y wall-half-height)))
           ;; Send the new  and Y coordinates for the ball
           (if (inside-wall-relative? (viewX bdx) (viewY new-py))
               (send model execute-command
                 (make-set-position bdx new-py))
               1257))]
        
        ;; Do nothing
        [else 2744]))

    ;; inside-wall? : PosInt PosInt -> Boolean
    ;; GIVEN: the x and y coordinates of the mouse event
    ;; RETURNS: true iff the mouse event occurred within the walls
    ;; STRATEGY: Combining simpler functions
    (define (inside-wall? mx my)
      (and
        (<= (- x wall-half-width) mx (+ x wall-half-width))
        (<= (- y wall-half-height) my (+ y wall-half-height))))

    (define (inside-wall-relative? gmx gmy)
      (and
        (<= (- wall-half-width) gmx wall-half-width)
        (<= (- wall-half-height) gmy wall-half-height)))

    (define (Xmodelplane)
      (- 150 0))
    (define (Xviewplane)
      (- wall-half-width (- wall-half-width)))
    (define (viewX px)
      (- (* px (/ (Xviewplane) (Xmodelplane))) wall-half-width))

    (define (Ymodelplane)
      (- 100 0))
    (define (Yviewplane)
      (- wall-half-height (- wall-half-height)))
    (define (viewY py)
      (- (* py (/ (Yviewplane) (Ymodelplane))) wall-half-height))
    
    (define/override (viewer-image)
      (overlay/offset
       (super viewer-image)
       0 (viewY particle-y) 
       (overlay
          (circle 1 "solid" "black")
          (circle 5 "solid" "red"))))))

;; TESTS
(require "Model.rkt")
(define m (new Model%))
(define xy1 (new YController% [width 50] [height 100] [model m]))

(define (test:step1)
  (begin    
    (send xy1 after-key-event "b")
    (send xy1 after-button-down 300 250)
    (send xy1 after-button-up 300 250)
    (send xy1 after-button-down 280 205)
    (send xy1 after-drag 285 210)
    (send xy1 after-button-up 285 210)
    (send xy1 after-drag 10 10)
    (send xy1 after-button-down 10 10)
    (send xy1 after-button-down 305 255)
    (send xy1 after-drag 310 260)
    (send xy1 after-button-down 310 260)
    (send xy1 after-drag 10 10)
    (send xy1 viewer-image)))

(define (test:output)
  (send xy1 viewer-image))

(begin-for-test
  (check-equal? (test:step1) (test:output)
                "Wrong output rendered"))