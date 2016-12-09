#lang racket

(require rackunit)
(require "extras.rkt")
(require "toys-interfaces.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-target)

;; A Target is
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
     [mdx DEFAULT-MDX]        ; Integer
     [mdy DEFAULT-MDY]        ; Integer
     ;; Whether the toy is selected?, False by default
     [selected? DEFAULT-SELECTION]) ; Boolean
    
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

    ;; toy-data : -> Int
    ;; RETURNS: Not used
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      1)

    ;; toy-selected? : -> Boolean
    ;; RETURNS: true iff toy is selected
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-selected?)
      selected?)

    ;; after-key-event : -> Void
    ;; RETURNS: Void, Nothing occurs on keyevent
    ;; STRATEGY: Combining simpler functions
    (define/public (after-key-event)
      this)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)
      this)

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; STRATEGY: Using cases on if the mouse click event happened
    ;; within the Target or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my TARGET-RADIUS)
          (begin
            (set! mdx (- x mx))
            (set! mdy (- y my))
            (set! selected? (not selected?)))
          this))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; STRATEGY: Using cases on if the Target is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! mdx DEFAULT-MDX)
            (set! mdy DEFAULT-MDY)
            (set! selected? (not selected?)))
          this))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: a location
    ;; STRATEGY: Using cases on if the Target is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ mx mdx))
            (set! y (+ my mdy)))
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    (define/public (add-to-scene scene)
      (place-image target-img x y scene))))

;; make-target : Int Int -> Toy<%>
;; GIVEN: The coordinates of the target
;; RETURNS: an object representing a Target at the given position
;; STRATEGY: Combining simpler functions
(define (make-target x y)
  (new Target% [x x][y y]))

;; TESTS:
(begin-for-test
  (check-equal? (send test-trgt1 toy-x)
                 250)
  (check-equal? (send test-trgt1 toy-y)
                 250)
  (check-equal? (send test-trgt1 toy-selected?)
                 false)
  (check-equal? (begin
                  (send test-trgt1 after-tick)
                  (send test-trgt1 toy-x))
                 250)
  (check-equal? (send test-trgt1 toy-y)
                 250)
  (check-equal? (send test-trgt1 toy-selected?)
                 false)
  (check-equal? (begin
                  (send test-trgt1 after-button-down 250 250)
                  (send test-trgt1 after-drag 100 100)
                  (send test-trgt1 toy-x))
                 100)
  (check-equal? (send test-trgt1 toy-y)
                 100)
  (check-equal? (begin
                  (send test-trgt2 after-button-down 300 300)
                  (send test-trgt2 toy-selected?))
                false)
  (check-equal? (begin
                  (send test-trgt2 after-button-down 300 300)
                  (send test-trgt2 after-drag 100 100)
                  (send test-trgt2 toy-x))
                 250)
  (check-equal? (begin
                  (send test-trgt3 after-button-down 250 250)
                  (send test-trgt3 after-button-up 5 5)
                  (send test-trgt3 toy-x))
                 250)
  (check-equal? (begin
                  (send test-trgt3 after-button-down 300 300)
                  (send test-trgt3 after-button-up 300 300)
                  (send test-trgt3 toy-x))
                 250)
  (check-equal? (send test-trgt4 add-to-scene EMPTY-CANVAS)
                (place-image (circle TARGET-RADIUS
                               TARGET-STYLE
                               TARGET-COLOR) 250 250 EMPTY-CANVAS)))

;=================
; TEST VARIABLES
;=================
(define test-trgt1 (make-target 250 250))
(define test-trgt2 (make-target 250 250))
(define test-trgt3 (make-target 250 250))
(define test-trgt4 (make-target 250 250))