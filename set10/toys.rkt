#lang racket

(require rackunit)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "toys-interfaces.rkt")
(require "toys-clock.rkt")
(require "toys-squaretoy.rkt")
(require "toys-football.rkt")
(require "toys-throbber.rkt")
(require "toys-target.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide
 run
 make-playground)

;; A PlaygroundState is
;; (new PlaygroundState% [tgt Target] [objs ListOfToy] [speed speed])
(define PlaygroundState%
  (class* object% (PlaygroundState<%>)
    (init-field speed) ; PosInt
    (init-field [tgt (make-target (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))])
    (init-field [objs empty]) ; ListOfToy    
    
    (super-new)

    ;; target-x : -> PosInt
    ;; RETURNS: the x-coordinate position of the target
    ;; STRATEGY: Combining simpler functions
    (define/public (target-x)
      (send tgt toy-x))

    ;; target-y : -> PosInt
    ;; RETURNS: the y-coordinate position of the target
    ;; STRATEGY: Combining simpler functions
    (define/public (target-y)
      (send tgt toy-y))

    ;; target-selected? : -> Boolean
    ;; RETURNS: true iff the target is currently selected
    ;; STRATEGY: Combining simpler functions
    (define/public (target-selected?)
      (send tgt toy-selected?))

    ;; get-toys : -> ListofToy
    ;; RETURNS: the list of toys in the current state
    ;; STRATEGY: Combining simpler functions
    (define/public (get-toys)
      objs)

    ;; process-toys : (X -> Y) -> Void
    ;; GIVEN : A function
    ;; STRATEGY : Using HOF for-each on objs and tgt
    (define (process-toys fn)
       (for-each fn (cons tgt objs)))

    ;; after-tick : -> Void
    ;; STRATEGY: Useing HOF map on the Toy-s in this World
    (define/public (after-tick)
      (process-toys
        ;; -> Void
        ;; RETURNS: The processed toy
        (lambda (toy) (send toy after-tick))))

    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a Scene updated with all the Toys rendered on it
    ;; STRATEGY: Use HOF foldr on the Toy-s in this World
    (define/public (add-to-scene scene)
      (foldr
       ;; Toy Scene -> Scene
       ;; GIVEN: a toy and a scene
       (lambda (toy base-scene)
         (send toy add-to-scene base-scene))
       scene
       (cons tgt objs)))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; STRATEGY: Cases on kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-SQUARE-TOY-EVENT)
         (set! objs
               (cons
                (make-square-toy
                 (send this target-x)
                 (send this target-y)
                 speed)
            objs))]
        [(key=? kev NEW-THROBBER-EVENT)
         (set! objs
           (cons (make-throbber
                  (send this target-x)
                  (send this target-y))
                 objs))]
        [(key=? kev NEW-CLOCK-EVENT)
         (set! objs
           (cons
            (make-clock
             (send this target-x)
             (send this target-y))
            objs))]
        [(key=? kev NEW-FOOTBALL-EVENT)
         (set! objs
               (cons
                (make-football
                 (send this target-x)
                 (send this target-y))
                 objs))]
        [else
         this]))

    ;; after-button-down : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse click event
    ;; STRATEGY: Calling simpler functions
    (define/public (after-button-down mx my)
      (process-toys
        ;; -> Void
        (lambda (toy) (send toy after-button-down mx my))))

    ;; after-button-up : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse button up event
    ;; STRATEGY: Calling simpler functions
    (define/public (after-button-up mx my)
      (process-toys
        ;; -> Void
        (lambda (toy) (send toy after-button-up mx my))))

    ;; after-drag : PosInt PosInt -> Void
    ;; GIVEN: the x and y coordinates of the mouse drag event
    ;; STRATEGY: Calling simpler functions
    (define/public (after-drag mx my)
      (process-toys
        ;; -> Void
        (lambda (toy) (send toy after-drag mx my))))))

;; make-playground : PosInt -> PlaygroundState<%>
;; GIVEN: the speed at which the SquareToy moves
;; RETURNS: a Playground state
;; STRATEGY: Combining simpler functions
(define (make-playground speed)
  (new PlaygroundState% [speed speed]))

;; TESTS:
(begin-for-test
  (check-equal? (send test-plgrd target-x)
                 250)
  (check-equal? (send test-plgrd target-y)
                 300)
  (check-equal? (send test-plgrd target-selected?)
                 false)
  (check-equal? (begin
                  (send test-plgrd after-tick)
                  (send test-plgrd target-x))
                 250)
  (check-equal? (send test-plgrd target-y)
                 300)
  (check-equal? (send test-plgrd target-selected?)
                 false)
  (check-equal? (begin
                  (send test-plgrd after-button-down 251 301 )
                  (send test-plgrd after-drag 100 100)
                  (send test-plgrd after-button-up 100 100)
                  (send test-plgrd target-x))
                 99)
  (check-equal? (begin
                  (send test-plgrd2 after-key-event "s")
                  (send test-plgrd2 target-x))
                250)
  (check-equal? (begin
                  (send test-plgrd2 after-key-event "t")
                  (send test-plgrd2 target-x))
                250)
  (check-equal? (begin
                  (send test-plgrd2 after-key-event "f")
                  (send test-plgrd2 target-x))
                250)
  (check-equal? (begin
                  (send test-plgrd2 after-key-event "w")
                  (send test-plgrd2 target-x))
                250)
  (check-equal? (begin
                  (send test-plgrd2 after-key-event "a")
                  (send test-plgrd2 get-toys))
               (send test-plgrd2 get-toys))
  (check-equal? (send test-plgrd3 add-to-scene EMPTY-CANVAS)
                (place-image (circle TARGET-RADIUS
                               TARGET-STYLE
                               TARGET-COLOR) 250 300 EMPTY-CANVAS)))

;; run : PosNum PosInt -> Void 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed
;; (in pixels/tick), creates and runs a world in which square
;; toys travel at the given speed.
;; STRATEGY: Combine simpler functions
(define (run rate square-speed)
  (local
    ((define wrld (make-world CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
     (send wrld add-stateful-widget (make-playground square-speed))
     (send wrld run rate))))


;=================
; TEST VARIABLES
;=================
(define test-plgrd (make-playground 10))
(define test-plgrd2 (make-playground 10))
(define test-plgrd3 (make-playground 10))

