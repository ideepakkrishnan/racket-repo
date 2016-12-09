#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require "interfaces.rkt")
(require "DraggableController.rkt")

(provide DataController%)

;; A DataController% is a (new DataController% [model Model<%>])
(define DataController%
  (class* DraggableController% (Controller<%>)

    (inherit-field model)  ; the model

    (inherit-field width height)

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)

    ;; the velocity of the particle in x and y coordinates
    (inherit-field particle-vx)
    (inherit-field particle-vy)

    ;; fields for selection and updating the position of the particle
    (inherit-field selected?)
    (inherit-field handle-selected?)

    (super-new)
    
    (define/override (after-tick) 'viewer2-after-tick-trap)

    (define (current-color)
      (if selected? "red" "black"))

    (define (handle-color)
      (if handle-selected? "red" "black"))

    (define (text-color)
      (if selected?
          (if handle-selected?
              "black"
              "red")
          "black"))

    ;; assemble the image of the viewer
    (define/public (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay 
          the-data-image
          (overlay/align "left" "top"
             (rectangle 10 10 "outline" (handle-color))
            (rectangle
              width
              height
              CONTROLLER-BORDER-STYLE
              "black")))))

    (define (data-image)
      (above
       (text "Arrow keys change position" FONT-SIZE (text-color))
       (text (string-append "X = " (number->string particle-x)
                            " Y = " (number->string particle-y))
             FONT-SIZE (text-color))
       (text (string-append "VX = " (number->string particle-vx)
                            " VY = " (number->string particle-vy))
             FONT-SIZE (text-color))))))