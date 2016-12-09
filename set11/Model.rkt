#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=200.  It accepts commands and reports when its status changes

(require "extras.rkt")
(require "interfaces.rkt")
(require "PerfectBounce.rkt")
(require rackunit)
(require "extras.rkt")

(provide Model%)

;; A Command is either:
;;  -- (make-set-position PosInt PosInt)
;;  -- (make-incr-velocity Integer Integer)

;; A ListOfController<%> is either:
;;  -- empty
;;  -- (cons Controller<%> ListOfController<%>)

;; A Model is a (new Model% (new Model% [point-x PosInt] [point-y PosInt]
;;                                      [speed-vx Integer] [speed-vy Integer]
;;                                      [controllers ListOfController<%>]))
(define Model%
  (class* object% (Model<%>)

    ;; position of the particle
    (init-field [point-x 75])
    (init-field [point-y 50])

    ;; velocity of the particle
    (init-field [speed-vx PARTICLE-INIT-VX])
    (init-field [speed-vy PARTICLE-INIT-VY])

    ;; whether the ball movement is paused by a user event
    (init-field [paused? false])

    ; ListOfController<%>
    (init-field [controllers empty])

    ;; boundaries of the wall where the ball is bouncing
    (field [wall-left-edge 0])
    (field [wall-right-edge 150])
    (field [wall-top-edge 0])
    (field [wall-bottom-edge 100])

    (super-new)

    ;; after-tick: -> Void
    ;; EFFECT: moves the particle by speed-vx and speed-vy values
    ;; stored in this Model
    ;; EXAMPLES:
    ;; if the resulting x is >= 200 or <= 0
    ;; reports x at ever tick
    ;; reports velocity only when it changes
    (define/public (after-tick)
      (if (not paused?)
          (local
            (;; Current particle as a struct
             (define curr-particle (make-particle point-x point-y
                                                  speed-vx speed-vy))
             ;; Current field where the particle is bouncing
             (define curr-rect (make-rect wall-left-edge wall-right-edge
                                          wall-top-edge wall-bottom-edge))
             ;; The particle after tick
             (define new-particle
               (particle-after-tick curr-particle curr-rect)))
            (begin
              (set! point-x (particle-x new-particle))
              (set! point-y (particle-y new-particle))
              (publish-position)
              (set! speed-vx (particle-vx new-particle))
              (set! speed-vy (particle-vy new-particle))
              (publish-velocity))
            "model after-tick handler")
      "Particle movement is paused"))

    ;; register: Controller -> Void
    ;; GIVEN: a controller
    ;; EFFECT: register the new controller and shares the current
    ;; position and velocity of the particle
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        ;; publish the current ball position to this controller
        (send c receive-signal
              (make-report-position point-x point-y))
        ;; publish the current ball velocity to this controller
        (send c receive-signal
              (make-report-velocity speed-vx speed-vy))))

    ;; register-wall : PosInt PosInt PosInt PosInt -> Void
    ;; GIVEN: the four edges of a wall
    ;; EFFECT: Updates the four edges of the wall bounding the particle
    ;; in this Model
    (define/public (register-wall left-edge right-edge
                                  top-edge bottom-edge)
      (begin
        (set! wall-left-edge left-edge)
        (set! wall-right-edge right-edge)
        (set! wall-top-edge top-edge)
        (set! wall-bottom-edge bottom-edge))
      "Updated the edges of bounding wall")

    ;; execute-command: Command -> Void
    ;; GIVEN: a Command
    ;; EFFECT: decodes the command, executes it, and sends updates to
    ;; the controllers. 
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           ;; Update X position of the particle
           (set! point-x (set-position-pos-x cmd))
           ;; Update Y position of the particle
           (set! point-y (set-position-pos-y cmd))
           ;; Publish the new particle position to all controllers
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           ;; Update velocity in X coordinate
           (set! speed-vx (+ speed-vx (incr-velocity-dvx cmd)))
           ;; Update velocity in Y coordinate
           (set! speed-vy (+ speed-vy (incr-velocity-dvy cmd)))
           ;; Publish the new velocities to all controllers
           (publish-velocity))]
        [(pause-movement? cmd)
         (set! paused? (pause-movement-flag cmd))]))

    ;; publish-position: -> Void
    ;; EFFECT: publishes the current position of the Particle to
    ;; all the controllers registered to this Model
    (define (publish-position)
      (let ((msg (make-report-position point-x point-y)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)
        ))

    ;; publish-velocity -> Void
    ;; EFFECT: publishes the current velocity of the Particle to
    ;; all the controllers registered to this Model
    (define (publish-velocity)
      (let ((msg (make-report-velocity speed-vx speed-vy)))
        (for-each
          (lambda (obs) (send obs receive-signal msg))
          controllers)))

    ;; TEST Methods
    (define/public (test:point-x)
      point-x)

    (define/public (test:point-y)
      point-y)

    (define/public (test:controllers)
      controllers)

    (define/public (test:speed-vx)
      speed-vx)

    (define/public (test:speed-vy)
      speed-vy)))

;; TESTS:
(define m1 (new Model% [point-x 100] [point-y 100] [speed-vx 5]
                [speed-vy 5] [controllers empty]))
(define m2 (new Model% [paused? true]))

(define (test:step1)
  (begin
    (send m1 after-tick)
    (send m2 after-tick)
    (send m1 register-wall 0 100 0 50)
    (send m1 execute-command (make-incr-velocity 10 20))
    (send m1 execute-command (make-set-position 125 150))
    (send m1 execute-command (make-pause-movement true))
    (send m1 test:point-x)))

(begin-for-test
  (check-equal? (test:step1) 125 "Wrong X position after tick")
  (check-equal? (send m1 test:point-y) 150
                "Wrong Y position after tick")
  (check-equal? (send m2 test:controllers) empty
                "Wrong list of controllers received for m2")
  (check-equal? (send m1 test:speed-vx) 15 "Wrong x-velocity for m1")
  (check-equal? (send m1 test:speed-vy) 25 "Wrong y-velocity for m1"))