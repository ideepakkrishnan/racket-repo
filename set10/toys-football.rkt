#lang racket

(require rackunit)
(require "extras.rkt")
(require "toys-interfaces.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide make-football)

;; A Football is
;; (new Football% [x PosInt][y PosInt]
;;     [scale PosReal]
;;     [mdx Integer] [mdy Integer]
;;     [selected? Boolean])
(define Football%
  (class* object% (Toy<%>)
    (init-field
     x ; PosInt, x-position of the center of the Football image
     y) ; PosInt, y-position of the center of the Football image

    ;; Current value of the scale, default value is 0.4
    (init-field [scale FOOTBALL-INIT-SCALE]) ; PosReal

    ;; Relative x and y position of the center of the Football from
    ;; the current mouse position. Both these values are 0 by
    ;; default and is updated when the Football is selected
    (init-field [mdx DEFAULT-MDX]) ; Integer
    (init-field [mdy DEFAULT-MDY]) ; Integer

    ;; Whether the Football is selected?, False by default
    (init-field [selected? DEFAULT-SELECTION]) ; Boolean

    ;; Private variable which stores the Football image
    (field [football-img
            (scale/xy
             scale scale
             (bitmap/file FOOTBALL-IMG-PATH))])

    (super-new)

    ;; toy-x : -> PosInt
    ;; RETURNS: the x-position of the center of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-x)
      x)

    ;; toy-y : -> PosInt
    ;; RETURNS: the y-position of the center of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-y)
      y)

    ;; toy-data : -> PosReal
    ;; RETURNS: the current size of the Football image
    ;; STRATEGY: Combining simpler functions
    (define/public (toy-data)
      (* (image-width football-img)
         (image-height football-img)))

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
        (set! scale (update-scale))
        (set! football-img (scale/xy
             scale scale
             (bitmap/file FOOTBALL-IMG-PATH)))))

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse click event
    ;; STRATEGY: Using cases on if the mouse click event occurred
    ;; within the Football image or not
    (define/public (after-button-down mx my)
      (local
        ((define img-half-width (/ (image-width football-img) 2))
         (define img-half-height (/ (image-height football-img) 2)))
        (if (click-inside-rect? x y mx my img-half-width
                                img-half-height)
            (begin
              (set! mdx (- x mx))
              (set! mdy (- y my))
              (set! selected? (not selected?)))
            this)))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse button up event
    ;; STRATEGY: Using cases on if the Football is selected or not
    (define/public (after-button-up mx my)
      (if selected?
          (begin
            (set! mdx DEFAULT-MDX)
            (set! mdy DEFAULT-MDY)
            (set! selected? (not selected?)))
          this))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinate of the mouse drag event
    ;; STRATEGY: Using cases on if the Football is selected or not
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
      (place-image football-img x y scene))

    ;; Private methods
    ;; update-scale : -> PosReal
    ;; RETURNS: the current scaling of the Football image
    ;; STRATEGY: Using cases on if the scale is > 0.001
    (define (update-scale)
      (local ((define new-scale (* scale FOOTBALL-SCALE-DOWN-VAR)))
        (if (> new-scale 0.001)
            new-scale
            scale)))))

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
;; STRATEGY: Combining simpler functions
(define (make-football x y)
  (new Football% [x x][y y]))

;; TESTS:
(begin-for-test
  (check-equal? (send test-ftbl1 toy-x)
                 50)
  (check-equal? (send test-ftbl1 toy-y)
                 50)
  (check-equal? (send test-ftbl1 toy-data)
                 76800)
  (check-equal? (begin
                  (send test-ftbl1 after-tick)
                  (send test-ftbl1 toy-x))
                 50)
  (check-equal? (send test-ftbl1 toy-y)
                 50)
  (check-equal? (send test-ftbl1 toy-data)
                 43200)
  (check-equal? (begin
                  (send test-ftbl3 after-button-down 51 49)
                  (send test-ftbl3 after-drag 100 100)
                  (send test-ftbl3 toy-x))
                 99)
  (check-equal? (begin
                  (send test-ftbl4 after-button-down 200 200)
                  (send test-ftbl4 toy-x))
                50)
  (check-equal? (send test-ftbl3 toy-y)
                 101)
  (check-equal? (begin
                  (send test-ftbl4 after-drag 250 250)
                  (send test-ftbl4 toy-x))
                50)
  (check-equal? (begin
                  (send test-ftbl2 after-tick)
                  (send test-ftbl2 toy-x))
                50)
  (check-equal? (begin
                  (send test-ftbl5 after-button-down 51 49)
                  (send test-ftbl5 after-button-up 5 5)
                  (send test-ftbl5 toy-x))
                50)
  (check-equal? (begin
                  (send test-ftbl5 after-button-down 200 200)
                  (send test-ftbl5 after-button-up 200 200)
                  (send test-ftbl5 toy-x))
                 50)
  (check-equal? (send test-ftbl6 add-to-scene EMPTY-CANVAS)
                (place-image (scale/xy
             0.4 0.4
             (bitmap/file FOOTBALL-IMG-PATH)) 50 50 EMPTY-CANVAS)))

;=================
; TEST VARIABLES
;=================
(define test-ftbl1 (make-football 50 50))
(define test-ftbl2 (new Football% [x 50][y 50]
       [scale 0.0001]
       [mdx DEFAULT-MDX] [mdy DEFAULT-MDY]
       [selected? DEFAULT-SELECTION]))
(define test-ftbl3 (make-football 50 50))
(define test-ftbl4 (make-football 50 50))
(define test-ftbl5 (make-football 50 50))
(define test-ftbl6 (make-football 50 50))
