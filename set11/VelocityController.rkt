#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "DataController.rkt")

(provide VelocityController%)

;; a VelocityController% is a (new VelocityController% [model Model<%>])

(define VelocityController%
  (class* DataController% (Controller<%>)

    (inherit-field model)  ; the model

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    
    (inherit-field particle-vx)
    (inherit-field particle-vy)

    ;; fields for selection and updating the position of the particle
    (inherit-field selected?)

    (super-new)

    ;; send up, down, left and right buttons to move the particle in
    ;; the corresponding direction inside the canvas
    (define/override (after-key-event kev)
      (if selected?
        (cond
          ;; move the particle right
          [(key=? "right" kev)
           (send model execute-command
             (make-incr-velocity
               5 0))]
          ;; move the particle left
          [(key=? "left" kev)
           (send model execute-command
             (make-incr-velocity
               -5 0))]
          ;; move the particle up
          [(key=? "up" kev)
           (send model execute-command
             (make-incr-velocity
               0 -5))]
          ;; move the particle down
          [(key=? "down" kev)
           (send model execute-command
             (make-incr-velocity
               0 5))])
        2345))
    ))