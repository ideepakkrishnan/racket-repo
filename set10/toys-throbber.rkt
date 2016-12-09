#lang racket

(require rackunit)
(require "extras.rkt")
(require "toys-interfaces.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-throbber)

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
    (init-field [radius THROBBER-MIN-RADIUS]) ; PosInt

    ;; the step by which radius should change in current tick,
    ;; default value for a new instance is THROBBER-INIT-STEP
    (init-field [step THROBBER-INIT-STEP]) ; Integer

    ;; Relative x and y position of center of the Throbber from the
    ;; current mouse x position. Both these values are 0 by default
    ;; and is updated when the Throbber is selected
    (init-field [mdx DEFAULT-MDX]) ; Integer
    (init-field [mdy DEFAULT-MDY]) ; Integer

    ;; Whether the Throbber is selected?, False by default
    (init-field [selected? DEFAULT-SELECTION]) ; Boolean

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

    ;; after-key-event : -> Void
    ;; RETURNS: Void, Nothing occurs on keyevent
    ;; STRATEGY: Combining simpler functions
    (define/public (after-key-event)
      this)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; STRATEGY: Using cases on if the Throbber is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (begin
            (set! radius (+ radius step))
            (set! step (update-step))
            (set! THROBBER-IMG (circle radius
                                 THROBBER-STYLE
                                 THROBBER-COLOR)))))

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; within the Throbber or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my radius)
          (begin
            (set! mdx (- x mx))
            (set! mdy (- y my))
            (set! selected? (not selected?)))
          this))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse button up event
    ;; STRATEGY: Using cases on if the Throbber is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! mdx DEFAULT-MDX)
            (set! mdy DEFAULT-MDY)
            (set! selected? (not selected?)))
          this))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse drag event
    ;; STRATEGY: Using cases on if the Throbber is selected or not
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
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image THROBBER-IMG x y scene))

    ;; Private helper functions

    ;; throbber-at-max-size? : -> Boolean
    ;; RETURNS: true iff the radius of the throbber reaches maximum
    ;; STRATEGY: Combining simpler functions
    (define (throbber-at-max-size?)
      (= radius THROBBER-MAX-RADIUS))

    ;; throbber-at-min-size? : -> Boolean
    ;; RETURNS: true iff radius of throbber reaches minimum
    ;; STRATEGY: Combining simpler functions
    (define (throbber-at-min-size?)
      (= radius THROBBER-MIN-RADIUS))

    ;; update-step : -> Integer
    ;; RETURNS: the steps to be taken by the throbber in next tick
    ;; STRATEGY: Using cases on if throbber radius has reached the
    ;; minimum or maximum allowed value
    (define (update-step)
      (if (or (throbber-at-max-size?)
              (throbber-at-min-size?))
          (- step)
          step))))

;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: the x and y position
;; RETURNS: an object representing a throbber at the given position.
;; STRATEGY: Combining simpler functions
(define (make-throbber x y)
  (new Throbber% [x x][y y]))

;; TESTS:
(begin-for-test
  (check-equal? (send test-thrb1 toy-x)
                 140)
  (check-equal? (send test-thrb1 toy-y)
                 80)
  (check-equal? (send test-thrb1 toy-data)
                 5)
  (check-equal? (begin
                  (send test-thrb1 after-tick)
                  (send test-thrb1 after-tick)
                  (send test-thrb1 after-tick)
                  (send test-thrb1 toy-data))
                20)
  (check-equal? (begin
                  (send test-thrb2 after-tick)
                  (send test-thrb2 toy-x))
                 140)
  (check-equal? (begin
                  (send test-thrb2 after-tick)
                  (send test-thrb2 toy-y))
                 80)
  (check-equal? (begin
                  (send test-thrb3 after-tick)
                  (send test-thrb3 toy-data))
                 10)
  (check-equal? (begin
                  (send test-thrb4 after-button-down 141 81)
                  (send test-thrb4 after-drag 170 150)
                  (send test-thrb4 toy-x))
                 169)
  (check-equal? (begin
                  (send test-thrb5 after-button-down 141 81)
                  (send test-thrb5 after-tick)
                  (send test-thrb5 toy-x))
                 140)
  (check-equal? (begin
                  (send test-thrb5 after-button-down 200 200)
                  (send test-thrb5 toy-x))
                 140)
  (check-equal? (begin
                  (send test-thrb5 after-button-down 200 200)
                  (send test-thrb5 after-button-up 200 200)
                  (send test-thrb5 toy-x))
                 140)
  (check-equal? (begin
                  (send test-thrb5 after-button-down 200 200)
                  (send test-thrb5 after-drag 250 250)
                  (send test-thrb5 toy-x))
                 140)
  (check-equal? (begin
                  (send test-thrb5 after-button-down 141 81)
                  (send test-thrb5 after-drag 170 150)
                  (send test-thrb5 toy-y))
                 149)
  (check-equal? (begin
                  (send test-thrb6 after-button-down 142 83)
                  (send test-thrb6 after-button-up 5 5)
                  (send test-thrb6 toy-x))
                 140)
  (check-equal? (send test-thrb7 add-to-scene EMPTY-CANVAS)
                (place-image (circle 5
                                 THROBBER-STYLE
                                 THROBBER-COLOR) 140 80 EMPTY-CANVAS)))

;=================
; TEST VARIABLES
;=================
(define test-thrb1 (make-throbber 140 80))
(define test-thrb2 (make-throbber 140 80))
(define test-thrb3 (make-throbber 140 80))
(define test-thrb4 (make-throbber 140 80))
(define test-thrb5 (make-throbber 140 80))
(define test-thrb6 (make-throbber 140 80))
(define test-thrb7 (make-throbber 140 80))