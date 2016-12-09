#lang racket

(require rackunit)
(require "extras.rkt")
(require "toys-interfaces.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-clock)

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
    (init-field [ticks CLOCK-INIT-TICK]) ; PosInt

    ;; Relative x and y position of the center of the Clock from the
    ;; current mouse x position. Both these values are 0 by default
    ;; and is updated when the Toy is selected
    (init-field [mdx DEFAULT-MDX]) ; Integer
    (init-field [mdy DEFAULT-MDY]) ; Integer

    ;; Whether the toy is selected?, False by default
    (init-field [selected? DEFAULT-SELECTION]) ; Boolean

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

    ;; after-key-event : -> Void
    ;; RETURNS: Void, Nothing occurs on keyevent
    ;; STRATEGY: Combining simpler functions
    (define/public (after-key-event)
      this)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; STRATEGY: Combining simpler functions
    (define/public (after-tick)
      (begin
        (set! ticks (+ ticks 1))
        (set! clock-text (text (number->string ticks)
                             CLOCK-TEXT-SIZE
                             CLOCK-TEXT-COLOR))))

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: x and y coordinates of the mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; inside the Clock or not
    (define/public (after-button-down mx my)
      (if (click-inside-circle? x y mx my CLOCK-RADIUS)
          (begin
            (set! mdx (- x mx))
            (set! mdy (- y my))
            (set! selected? (not selected?)))
          this))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: x and y coordinates of the mouse button up event
    ;; STRATEGY: Using cases on if the Clock is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! mdx DEFAULT-MDX)
            (set! mdy DEFAULT-MDY)
            (set! selected? (not selected?)))
          this))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: x and y coordinates of the mouse drag event
    ;; STRATEGY: Using cases on if the Clock is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (+ mx mdx))
            (set! y (+ my mdy)))
          this))

    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it
    ;; STRATEGY: Combining simpler functions
    (define/public (add-to-scene scene)
      (place-image clock-text x y
                   (place-image clock-img x y scene)))))

;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position
;; STRATEGY: Combining simpler functions
(define (make-clock x y)
  (new Clock% [x x][y y]))


;; TESTS:
(begin-for-test
  (check-equal? (send test-clk1 toy-x)
                 20)
  (check-equal? (send test-clk1 toy-y)
                 200)
  (check-equal? (send test-clk1 toy-data)
                 0)
  (check-equal? (begin
                  (send test-clk1 after-tick)
                  (send test-clk1 toy-x))
                 20)
  (check-equal? (send test-clk1 toy-y)
                 200)
  (check-equal? (send test-clk1 toy-data)
                 1)
  (check-equal? (begin
                  (send test-clk2 after-button-down 18 198)
                  (send test-clk2 after-drag 100 210)
                  (send test-clk2 toy-x))
                 102)
  (check-equal? (begin
                  (send test-clk3 after-button-down 18 198)
                  (send test-clk3 after-button-up 18 198)
                  (send test-clk3 toy-x))
                 20)
  (check-equal? (begin
                  (send test-clk4 after-button-down 18 198)
                  (send test-clk4 after-drag 100 210)
                  (send test-clk4 toy-y))
                 212)
  (check-equal? (begin
                  (send test-clk5 after-button-down 200 200)
                  (send test-clk5 after-drag 250 250)
                  (send test-clk5 toy-x))
                 20)
  (check-equal? (begin
                  (send test-clk6 after-button-down 142 83)
                  (send test-clk6 after-button-up 5 5)
                  (send test-clk6 toy-x))
                 20)
  (check-equal? (send test-clk7 add-to-scene EMPTY-CANVAS)
                (place-image (text (number->string 0)
                             CLOCK-TEXT-SIZE
                             CLOCK-TEXT-COLOR) 20 200
                   (place-image (circle CLOCK-RADIUS
                              CLOCK-STYLE
                              CLOCK-COLOR) 20 200 EMPTY-CANVAS))))

;=================
; TEST VARIABLES
;=================
(define test-clk1 (make-clock 20 200))
(define test-clk2 (make-clock 20 200))
(define test-clk3 (make-clock 20 200))
(define test-clk4 (make-clock 20 200))
(define test-clk5 (make-clock 20 200))
(define test-clk6 (make-clock 20 200))
(define test-clk7 (make-clock 20 200))