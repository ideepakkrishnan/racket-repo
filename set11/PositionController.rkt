#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require "interfaces.rkt")
(require "DataController.rkt")

(provide PositionController%)

;; a PositionController% is a (new PositionController% [model Model<%>])

(define PositionController%
  (class* DataController% (Controller<%>)

    (inherit-field model)  ; the model

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)

    ;; fields for selection and updating the position of the particle
    (inherit-field selected?)

    (super-new)

    (define/public (test:particle-x)
      particle-x)

    (define/public (test:particle-y)
      particle-y)

    ;; send up, down, left and right buttons to move the particle in
    ;; the corresponding direction inside the canvas
    (define/override (after-key-event kev)
      (if selected?
        (cond
          ;; move the particle right
          [(key=? "right" kev)
           (send model execute-command
             (make-set-position
               (+ particle-x 5) particle-y))]
          ;; move the particle left
          [(key=? "left" kev)
           (send model execute-command
             (make-set-position
               (- particle-x 5) particle-y))]
          ;; move the particle up
          [(key=? "up" kev)
           (send model execute-command
             (make-set-position
               particle-x (- particle-y 5)))]
          ;; move the particle down
          [(key=? "down" kev)
           (send model execute-command
             (make-set-position
               particle-x (+ particle-y 5)))])
        2345))))