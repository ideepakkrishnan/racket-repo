#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "interfaces.rkt")
(require "DraggableController.rkt")
(require "PerfectBounce.rkt")

(provide FieldController%)

;; A FieldController% is a (new FieldController% [model Model<%>])
(define FieldController%
  (class* DraggableController% (Controller<%>)

    (inherit-field model)  ; the model

    ;; the x and y coordinate of the center of this controller
    (inherit-field x y)

    (inherit-field width height)

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)

    ;; the velocity of the particle in x and y coordinates
    (inherit-field particle-vx)
    (inherit-field particle-vy)

    ;; fields for selection and updating the position of the particle
    (inherit-field selected?)

    ;; field where the ball is supposed to bounce 
    (inherit-field wall-width)
    (inherit-field wall-height)
    (inherit-field wall-half-width)
    (inherit-field wall-half-height)

    ;; inherit the handle selected flag from DraggableController%
    (inherit-field handle-selected?)

    ;; Relative distance of the controller from mouse click position
    (inherit-field saved-mx)
    (inherit-field saved-my)

    (super-new)

    ;; after-tick: -> trap
    ;; EFFECT: Returns a trap message for after tick
    (define/override (after-tick) 'field-after-tick-trap)

    ;; current-color: -> String
    ;; RETURNS: the color for rendering the elements
    ;; STRATEGY: Using cases on selected?
    (define (current-color)
      (if selected? "red" "black"))

    ;; handle-color: -> String
    ;; RETURNS: the color for rendering the handle
    ;; STRATEGY: Using cases on selected?
    (define (handle-color)
      (if handle-selected? "red" "black"))

    ;; viewer-image: -> Image
    ;; EFFECT: Assembles the image of the viewer
    ;; RETURNS: the final image to be rendered on the scene
    (define/public (viewer-image)
      (overlay
       (rectangle wall-width wall-height WALL-BORDER-STYLE
                  WALL-BORDER-COLOR)
       (overlay/align "left" "top"
        (rectangle 10 10 "outline" (handle-color))
       (rectangle width height CONTROLLER-BORDER-STYLE
                  CONTROLLER-BORDER-COLOR))))))