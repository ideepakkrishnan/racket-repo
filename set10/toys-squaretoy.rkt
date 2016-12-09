#lang racket

(require rackunit)
(require "extras.rkt")
(require "toys-interfaces.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-square-toy)

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
    (init-field [mdx DEFAULT-MDX]) ; Integer
    (init-field [mdy DEFAULT-MDY]) ; Integer

    ;; Whether the toy is selected?, False by default
    (init-field [selected? DEFAULT-SELECTION]) ; Boolean

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

    ;; after-key-event : -> Void
    ;; RETURNS: Void, Nothing occurs on keyevent
    ;; STRATEGY: Combining simpler functions
    (define/public (after-key-event)
      this)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; STRATEGY: Using cases on if the Toy is selected or not
    (define/public (after-tick)
      (if selected?
          this
          (begin
            (set! x (next-x-position))
            (set! speed (update-speed)))))

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; inside the square toy or not
    (define/public (after-button-down mx my)
      (if (click-inside-rect? x y mx my SQUARE-TOY-HALF-WIDTH
                              SQUARE-TOY-HALF-HEIGHT)
          (begin
            (set! mdx (- x mx))
            (set! mdy (- y my))
            (set! selected? (not selected?)))
          this))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse button up event
    ;; STRATEGY: Using cases on if the SquareToy was already selected
    ;; or not
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! mdx DEFAULT-MDX)
            (set! mdy DEFAULT-MDY)
            (set! selected? (not selected?)))
          this))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; STRATEGY: Using cases on if the SquareToy is selected or not
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

;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
;; STRATEGY: Combining simpler functions
(define (make-square-toy x y speed)
  (new SquareToy% [x x][y y][speed speed]))

;; TESTS:
(begin-for-test
  (check-equal? (send test-sqt1 toy-x)
                 120)
  (check-equal? (send test-sqt1 toy-y)
                 70)
  (check-equal? (send test-sqt1 toy-data)
                 10)
  (check-equal? (begin
                  (send test-sqt1 after-tick)
                  (send test-sqt1 toy-x))
                 130)
  (check-equal? (begin
                  (send test-sqt2 after-tick)
                  (send test-sqt2 toy-y))
                 70)
  (check-equal? (begin
                  (send test-sqt3 after-tick)
                  (send test-sqt3 toy-data))
                 10)
  (check-equal? (begin
                  (send test-sqt4 after-button-down 119 71)
                  (send test-sqt4 after-drag 150 150)
                  (send test-sqt4 toy-x))
                 151)
  (check-equal? (begin
                  (send test-sqt5 after-button-down 119 71)
                  (send test-sqt5 after-tick)
                  (send test-sqt5 toy-x))
                 (send test-sqt5 toy-x))
  (check-equal? (begin
                  (send test-sqt6 after-button-down 119 71)
                  (send test-sqt6 after-drag 150 150)
                  (send test-sqt6 toy-y))
                 149)
  (check-equal? (begin
                  (send test-sqt7 after-drag 150 150)
                  (send test-sqt7 toy-y))
                (send test-sqt1 toy-y))
  (check-equal? (begin
                  (send test-sqt8 after-button-down 119 71)
                  (send test-sqt8 after-button-up 5 5)
                  (send test-sqt8 toy-x))
                 120)
  (check-equal? (begin
                  (send test-sqt9 after-button-down 150 150)
                  (send test-sqt9 toy-x))
                (send test-sqt9 toy-x))
  (check-equal? (begin
                  (send test-sqt10 after-button-up 150 150)
                  (send test-sqt10 toy-x))
                (send test-sqt10 toy-x))
  (check-equal? (begin
                  (send test-sqt11 after-button-down 119 71)
                  (send test-sqt11 after-drag SQUARE-TOY-MAX-X 150)
                  (send test-sqt11 after-button-up SQUARE-TOY-MAX-X 150)
                  (send test-sqt11 after-tick)
                  (send test-sqt11 toy-data))
                -10)
  (check-equal? (begin
                  (send test-sqt12 after-button-down 119 71)
                  (send test-sqt12 after-drag 0 150)
                  (send test-sqt12 after-button-up 0 150)
                  (send test-sqt12 after-tick)
                  (send test-sqt12 toy-x))
                SQUARE-TOY-MIN-X)
  (check-equal? (send test-sqt13 add-to-scene EMPTY-CANVAS)
                (place-image
                 (rectangle SQUARE-TOY-WIDTH SQUARE-TOY-HEIGHT
                       SQUARE-TOY-STYLE SQUARE-TOY-COLOR)
                 120 70 EMPTY-CANVAS))
  (check-equal? (send test-sqt14 after-key-event)
                test-sqt14))

;=================
; TEST VARIABLES
;=================
(define test-sqt1 (make-square-toy 120 70 10))
(define test-sqt2 (make-square-toy 120 70 10))
(define test-sqt3 (make-square-toy 120 70 10))
(define test-sqt4 (make-square-toy 120 70 10))
(define test-sqt5 (make-square-toy 120 70 10))
(define test-sqt6 (make-square-toy 120 70 10))
(define test-sqt7 (make-square-toy 120 70 10))
(define test-sqt8 (make-square-toy 120 70 10))
(define test-sqt9 (make-square-toy 120 70 10))
(define test-sqt10 (make-square-toy 120 70 10))
(define test-sqt11 (make-square-toy 120 70 10))
(define test-sqt12 (make-square-toy 120 70 10))
(define test-sqt13 (make-square-toy 120 70 10))
(define test-sqt14 (make-square-toy 120 70 10))
