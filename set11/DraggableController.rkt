#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(require "interfaces.rkt")

(provide DraggableController%)

;; A Signal is either:
;;  -- (make-report-position PosInt PosInt)
;;  -- (make-report-velocity Integer Integer)

;; a DraggableController% is a (new DraggableController% [model Model<%>])
(define DraggableController%
  (class* object% (Controller<%>)

    (init-field model)  ; the model

    ; Nats -- the position of the center of the controller
    (init-field [x CONTROLLER-INIT-X] [y CONTROLLER-INIT-Y])

    (init-field [width 120][height 50])

    ;; field where the ball is supposed to bounce 
    (init-field [wall-width (- width 20)])
    (init-field [wall-height (- height 20)])
    (init-field [wall-half-width (/ wall-width 2)])
    (init-field [wall-half-height (/ wall-height 2)])

    (field [half-width  (/ width  2)])
    (field [half-height (/ height 2)])

    ;; the position of the particle
    (field [particle-x 0])
    (field [particle-y 0])

    ;; the velocity of the particle in x and y coordinates
    (field [particle-vx 0])
    (field [particle-vy 0])

    ;; fields for selection and updating the position of the particle
    (field [selected? false])

    ;; fields for dragging
    ;; if selected? then position of last button-down relative to
    ;; center of viewer; else any value
    (field [handle-selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])

    (super-new)

    (send model register this)
    
    ;; receive-signal: Signal -> Void
    ;; GIVEN: a signal
    ;; EFFECT: decodes signal and updates local data based on the
    ;; type of signal
    ;; STRATEGY: Using cases on sig
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (begin
           (set! particle-x (report-position-pos-x sig))
           (set! particle-y (report-position-pos-y sig)))]
        [(report-velocity? sig)
         (begin
           (set! particle-vx (report-velocity-vx sig))
           (set! particle-vy (report-velocity-vy sig)))]))

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: makes the viewer selected
    ; STRATEGY: Cases on whether the event is in this object
    (define/public (after-button-down mx my)
      (if (in-this? mx my)
        (begin
          (set! selected? true)
          (set! handle-selected? (in-handle? mx my))
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        3742))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    (define/public (after-button-up mx my)
      (begin
        (set! selected? false)
        (set! handle-selected? false)))
      

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether this is selected.
    ; If it is selected, move it so that the vector from its position to
    ; the drag event is equal to saved-mx.  Report the new position to
    ; the registered balls.
    (define/public (after-drag mx my)
      (if handle-selected?
        (begin
          (set! x (- mx saved-mx))
          (set! y (- my saved-my)))
        2744))

    ;; in-this? : PosInt PosInt -> Boolean
    ;; GIVEN: the X and Y coordinates of the mouse click event
    ;; RETURNS: true iff the click event happened anywhere inside this
    ;; controller
    ;; STRATEGY: Combining simpler functions
    (define/public (in-this? other-x other-y)
      (and
        (<= (- x half-width) other-x (+ x half-width))
        (<= (- y half-height) other-y (+ y half-height))))

    ;; in-handle? : PosInt PosInt -> Boolean
    ;; Every controller has a 10x10 square on the top left corner which
    ;; acts as a handle. The controller gets selected when the mouse
    ;; click event happens inside this handle
    ;; GIVEN: the X and Y coordinates of the mouse click event
    ;; RETURNS: true iff the click event happened inside the handle
    ;; of this controller
    ;; STRATEGY: Combining simpler functions
    (define/public (in-handle? other-x other-y)
      (local
        (;; Left edge of this controller's handle
         (define handle-left-edge (- x half-width))
         ;; Right edge of this controller's handle
         (define handle-right-edge
           (+ (- x half-width) CONTROLLER-HANDLE-SIDE))
         ;; Top edge of this controller's handle
         (define handle-top-edge (- y half-height))
         ;; Bottom edge of this controller's handle
         (define handle-bottom-edge
           (+ (- y half-height) CONTROLLER-HANDLE-SIDE)))
      (and
        (<= handle-left-edge other-x handle-right-edge)
        (<= handle-top-edge other-y handle-bottom-edge))))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with this wall painted
    ;; on it.
    ;; STRATEGY: place the image centered at x y
    (define/public (add-to-scene scene)
      (place-image (send this viewer-image) x y scene))

    ;; abstract methods which will be implemented by classes which
    ;; extend this class
    (abstract after-tick)
    (abstract after-key-event)

    ;; TEST methods
    (define/public (test:x)
      x)

    (define/public (test:y)
      y)))